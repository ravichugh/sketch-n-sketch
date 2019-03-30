module FillPBEHole exposing (..)

import Eval
import Lang exposing (..)
import LangTools
import MathExp
import Solver
import Syntax
import Types
import Update
import Utils

import Dict


-- Sketch leaves
-- No name collisions with actual vars b/c spaces
-- Six kinds of leaves to fill in
exampleVal1      = eHoleNamed "exampleVal1"
exampleVal2      = eHoleNamed "exampleVal2"
var              = eVar "env var"
num1             = eVar "num 1"
num2             = eVar "num 2"
num3             = eVar "num 3"
valFromVarDomain = eHoleNamed "valFromVarDomain"
-- Some constant leaves
zero             = eConstFrozen 0
two              = eConstFrozen 2


isSketchFilled : Exp -> Bool
isSketchFilled sketchExp =
  not <| containsNode (\e -> List.member e [exampleVal1, exampleVal2, var, num1, num2, num3, valFromVarDomain]) sketchExp


-- The sketches
sketches : List Exp
sketches =
  -- MathExp sketches (currently cannot contain exampleVal)
  -- num1 and num2 filled by solver
  [ var
  , eOp Plus  [var, num1]
  , eOp Minus [var, num1]
  , eOp Minus [num1, var]
  , eOp Mult [var, num1]
  , eOp Div [var, num1]
  , eOp Div [num1, var]
  , eOp Plus [num1, eOp Mult [var, num2]] -- base + i * sep
  ] ++
  -- Non-MathExp sketches
  -- Leaves are always exampleVal
  -- So filled by partial evaluation example by example until filled
  [ exampleVal1
  , eIfSingleLine (eOp Eq [var, valFromVarDomain]) exampleVal1 exampleVal2
  , eIfSingleLine (eOp Lt [var, valFromVarDomain]) exampleVal1 exampleVal2
  , eIfSingleLine (eOp Eq [eOp Mod [var, two], zero]) exampleVal1 exampleVal2 -- alternating
  ]


pbeHoleFillings : Solver.SolutionsCache -> List Eval.PBEHoleSeen -> List Exp
pbeHoleFillings solutionsCache pbeHolesSeen =
  let (_, envs, results) = Utils.unzip3 pbeHolesSeen in
  case Utils.projOk results of
    Ok ((firstExampleVal::_) as exampleVals) ->
      let filledSketches =
        let possiblyRelevantVariables =
          envs
          |> List.map Utils.removeShadowedKeys
          |> Utils.removeCommonSuffix -- quick first pass to eliminate prelude, vals should be === in JS so should plow through the list quickly
          |> List.concat
          |> Utils.pairsToDictOfLists
          |> Dict.toList
          |> List.filter (\(ident, vals) -> List.length (Utils.dedupBy Update.valToString vals) > 0 && (List.head vals |> Maybe.andThen valToMaybeFuncBodyExp |> (==) Nothing))
        in
        sketches
        |> List.concatMap
            (\sketch ->
              if containsNode ((==) var) sketch then
                possiblyRelevantVariables
                |> List.concatMap
                    (\(ident, varVals) ->
                      let varDomain = Utils.dedupBy Update.valToString varVals in
                      let initialSketches =
                        if containsNode ((==) valFromVarDomain) sketch then
                          -- valFromVarDomain not filled in all possible combinations (okay since sketches so far only use it at most once)
                          varDomain
                          |> List.map (\val -> sketch |> mapExpNodesMatching ((==) valFromVarDomain) (always (eHoleVal val)))
                        else
                          [sketch]
                      in
                      initialSketches
                      |> List.concatMap
                          (\initialSketch ->
                            case (MathExp.expToMaybeMathExp initialSketch, List.map valToMaybeNum exampleVals |> Utils.projJusts, List.map valToMaybeNum varVals |> Utils.projJusts) of
                              (Just (mathExp, identToVarId), Just exampleNums, Just varNums) ->
                                -- Sketch is a mathExp and the example and and variable are all numbers
                                if containsNode ((==) num1) initialSketch && containsNode ((==) num2) initialSketch && containsNode ((==) num3) initialSketch then
                                  -- Two unknowns
                                  -- Try to solve based on two differing examples.
                                  case Utils.zip exampleNums varNums |> Utils.dedup of
                                    (exampleNum1, varNum1)::(exampleNum2, varNum2)::(exampleNum3, varNum3)::_ ->
                                      let
                                        envVarId  = Utils.justGet_ "PBE hole filling identToVarId shouldn't happen" "env var" identToVarId
                                        num1VarId = Utils.justGet_ "PBE hole filling identToVarId shouldn't happen" "num 1" identToVarId
                                        num2VarId = Utils.justGet_ "PBE hole filling identToVarId shouldn't happen" "num 2" identToVarId
                                        num3VarId = Utils.justGet_ "PBE hole filling identToVarId shouldn't happen" "num 3" identToVarId
                                        eqns =
                                          [ (MathNum exampleNum1, mathExp |> MathExp.applySubst (Dict.singleton envVarId varNum1))
                                          , (MathNum exampleNum2, mathExp |> MathExp.applySubst (Dict.singleton envVarId varNum2))
                                          , (MathNum exampleNum3, mathExp |> MathExp.applySubst (Dict.singleton envVarId varNum3))
                                          ]
                                        targetVarIds = [num1VarId, num2VarId, num3VarId]
                                      in
                                      Solver.solve solutionsCache eqns targetVarIds
                                      |> List.concatMap
                                          (\solution -> -- List (MathExp, VarId)
                                            case (Utils.maybeFind num1VarId (List.map Utils.flip solution), Utils.maybeFind num2VarId (List.map Utils.flip solution), Utils.maybeFind num3VarId (List.map Utils.flip solution)) of
                                              (Just (MathNum num1Num), Just (MathNum num2Num), Just (MathNum num3Num)) ->
                                                [
                                                  initialSketch
                                                  |> mapExpNodesMatching ((==) var)  (always (eVar ident))
                                                  |> mapExpNodesMatching ((==) num1) (always (eConstDummyLoc num1Num))
                                                  |> mapExpNodesMatching ((==) num2) (always (eConstDummyLoc num2Num))
                                                  |> mapExpNodesMatching ((==) num3) (always (eConstDummyLoc num3Num))
                                                ]
                                              _ ->
                                                []
                                          )

                                    _ ->
                                      []

                                else if containsNode ((==) num1) initialSketch && containsNode ((==) num2) initialSketch then
                                  -- Two unknowns
                                  -- Try to solve based on two differing examples.
                                  case Utils.zip exampleNums varNums |> Utils.dedup of
                                    (exampleNum1, varNum1)::(exampleNum2, varNum2)::_ ->
                                      let
                                        envVarId  = Utils.justGet_ "PBE hole filling identToVarId shouldn't happen" "env var" identToVarId
                                        num1VarId = Utils.justGet_ "PBE hole filling identToVarId shouldn't happen" "num 1" identToVarId
                                        num2VarId = Utils.justGet_ "PBE hole filling identToVarId shouldn't happen" "num 2" identToVarId
                                        eqns =
                                          [ (MathNum exampleNum1, mathExp |> MathExp.applySubst (Dict.singleton envVarId varNum1))
                                          , (MathNum exampleNum2, mathExp |> MathExp.applySubst (Dict.singleton envVarId varNum2))
                                          ]
                                        targetVarIds = [num1VarId, num2VarId]
                                      in
                                      Solver.solve solutionsCache eqns targetVarIds
                                      |> List.concatMap
                                          (\solution -> -- List (MathExp, VarId)
                                            case (Utils.maybeFind num1VarId (List.map Utils.flip solution), Utils.maybeFind num2VarId (List.map Utils.flip solution)) of
                                              (Just (MathNum num1Num), Just (MathNum num2Num)) ->
                                                [
                                                  initialSketch
                                                  |> mapExpNodesMatching ((==) var)  (always (eVar ident))
                                                  |> mapExpNodesMatching ((==) num1) (always (eConstDummyLoc num1Num))
                                                  |> mapExpNodesMatching ((==) num2) (always (eConstDummyLoc num2Num))
                                                ]
                                              _ ->
                                                []
                                          )

                                    _ ->
                                      []

                                else if containsNode ((==) num1) initialSketch then
                                  -- One unknown
                                  -- Try to solve based on the first example.
                                  case (exampleNums, varNums) of
                                    (exampleNum1::_, varNum1::_) ->
                                      let
                                        envVarId  = Utils.justGet_ "PBE hole filling identToVarId shouldn't happen" "env var" identToVarId
                                        num1VarId = Utils.justGet_ "PBE hole filling identToVarId shouldn't happen" "num 1" identToVarId
                                        eqns =
                                          [ (MathNum exampleNum1, mathExp |> MathExp.applySubst (Dict.singleton envVarId varNum1))
                                          ]
                                        targetVarIds = [num1VarId]
                                      in
                                      Solver.solve solutionsCache eqns targetVarIds
                                      |> List.concatMap
                                          (\solution -> -- List (MathExp, VarId)
                                            case solution of
                                              [(MathNum num1Num, _)] ->
                                                [
                                                  initialSketch
                                                  |> mapExpNodesMatching ((==) var)  (always (eVar ident))
                                                  |> mapExpNodesMatching ((==) num1) (always (eConstDummyLoc num1Num))
                                                ]
                                              _ ->
                                                []
                                          )

                                    _ ->
                                      []
                                else if initialSketch == var then
                                  -- No unknowns (it's just lone var)
                                  [eVar ident]
                                else
                                  let _ = Utils.log "fillPBEHoles did not expect to hit this branch" in
                                  []

                              (Just _, _, _) ->
                                if initialSketch == var then
                                  -- There's a type shape check later in the solution filter.
                                  [eVar ident]
                                else
                                  -- Sketch is a mathExp but either the examples or the var are not all numbers
                                  []

                              _ ->
                                -- Sketch is not a mathExp
                                -- Fill sketch example-by-example by partial evaluation
                                let filledSketch =
                                  Utils.zip varVals exampleVals
                                  |> Utils.foldl
                                      (initialSketch |> mapExpNodesMatching ((==) var) (always (eVar ident)))
                                      (\(varVal, exampleVal) sketchSoFar->
                                        let env = [(ident, varVal)] in
                                        -- If we hit a named hole, evaluation will fail...
                                        case Eval.doEval False Syntax.Elm env sketchSoFar of
                                          Err errStr ->
                                            -- ...then we'll fill that hole with the current example value
                                            if errStr |> String.contains "empty hole exampleVal1!" then
                                              sketchSoFar
                                              |> mapExpNodesMatching ((==) exampleVal1) (always (eHoleVal exampleVal))
                                            else if errStr |> String.contains "empty hole exampleVal2!" then
                                              sketchSoFar
                                              |> mapExpNodesMatching ((==) exampleVal2) (always (eHoleVal exampleVal))
                                            else
                                              let _ = Utils.log <| "Unexpected error during PBE sketch filling: " ++ errStr in
                                              sketchSoFar

                                          Ok _ ->
                                            sketchSoFar
                                      )
                                in
                                if isSketchFilled filledSketch
                                then [filledSketch]
                                else []
                          )
                    )
              else if sketch == exampleVal1 then
                -- Should be the only sketch that doesn't use the env var
                [eHoleVal firstExampleVal]
              else
                let _ = Utils.log <| "Unexpected sketch that doesn't use env var: " ++ Syntax.unparser Syntax.Elm sketch in
                []
            )
      in
      let filteredAndFinalizedFilledSketches =
        filledSketches
        |> List.filter
            (\sketch ->
              -- "Relate" tool criteria (`isGoodEnough` in ValueBasedTransform) is
              -- value within 20% of expected
              -- AND distance to each other original example value must be within 20% of what it was before
              --
              -- The second criteria will always reject the [exampleVal1] sketch unless all exampleVals were completely identital
              --
              -- So let's just follow the first criteria for now
              Utils.zip envs exampleVals
              |> List.all
                  (\(env, originalExampleVal) ->
                    case Eval.doEval False Syntax.Elm env sketch of
                      Ok ((newExampleVal, _), _, _) ->
                        Types.valToMaybeType originalExampleVal == Types.valToMaybeType newExampleVal &&
                        (
                          Utils.maybeZip
                            (flattenValTree originalExampleVal |> List.filterMap valToMaybeNum)
                            (flattenValTree newExampleVal      |> List.filterMap valToMaybeNum)
                          |> Maybe.map
                              (List.all
                                  (\(oldNum, newNum) ->
                                    let diff = newNum - oldNum in
                                    if newNum == 0 then diff == 0 else abs (diff / oldNum) < 0.2
                                  )
                              )
                          |> Maybe.withDefault False
                        )
                      Err _ ->
                        False
                  )
            )
        |> List.map
            (\filteredSketch ->
              -- Replace val holes with an exp for the val.
              filteredSketch
              |> mapExp
                  (\e ->
                    case e.val.e__ of
                      EHole _ (HoleVal valInHole) -> Update.val_to_exp space1 valInHole
                      _                           -> e
                  )
            )
        |> List.filter
            (\sketch ->
              -- Remove sketches that have identical branches.
              let leaves = LangTools.effectiveBranches sketch in
              List.length leaves == List.length (Utils.dedup leaves)
            )
      in
      filteredAndFinalizedFilledSketches

    Ok [] ->
      let _ = Utils.log <| "Can't fill PBE hole because no examples" in
      []

    Err errStr ->
      let _ = Utils.log <| "Can't fill PBE hole because not all examples evaluated\n" ++ errStr in
      []

