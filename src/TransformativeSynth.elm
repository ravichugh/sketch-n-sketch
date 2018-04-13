module TransformativeSynth exposing (..)

import CodeMotion
import Eval
import Lang exposing (..)
import LangTools
import LangUnparser
import Provenance
import Sync
import Syntax
import Utils
import ValUnparser

import Set exposing (Set)


type alias Rule = Env -> Exp -> List Exp
type alias Metric a = { metric : Env -> Exp -> a, distance : a -> Num } -- Satisified if distance == 0

-- This would be simpler if one didn't have to universally quantify one's type variables at the outermost level for Metric.
type OutputConstraint =
  ValueSetConstraint (List Rule) (Metric (Maybe (List Val, List Val)))


-- Unparse only works for deduping if there are no value holes.
unparse : Exp -> String
unparse exp = LangUnparser.unparseWithUniformWhitespace False False exp


eval : Env -> Exp -> Maybe Val
eval funcEnv funcBody =
  Eval.eval_ Syntax.Elm funcEnv [] funcBody
  |> Result.toMaybe
  |> Maybe.map (\(val, widgets) -> val)


-- May make this into rules (plural)
applyConstraintRules : OutputConstraint -> Env -> Exp -> List Exp
applyConstraintRules constraint funcEnv funcBody =
  case constraint of
    ValueSetConstraint rules _ -> rules |> List.concatMap (\rule -> rule funcEnv funcBody)


constraintDistance : OutputConstraint -> Env -> Exp -> Num
constraintDistance constraint funcEnv funcBody =
  case constraint of
    ValueSetConstraint _ metric -> metricDistance metric funcEnv funcBody


metricDistance : Metric a -> Env -> Exp -> Num
metricDistance metric funcEnv funcBody =
  metric.metric funcEnv funcBody |> metric.distance


makeValueSetConstraint : Sync.Options -> List Val -> OutputConstraint
makeValueSetConstraint syncOptions outputValsDesired =
  let
    valDiff val removeVals =
      valDiff_ val (List.concatMap flattenValTree removeVals)

    valDiff_ val removeVals =
      -- Not sure what understanding of equality to use here
      if List.any (Provenance.valEqFast val) removeVals then
        []
      else
        let
          childLeftovers =
            childVals val |> List.map (\childVal -> valDiff_ childVal removeVals)

          nothingRemoved =
            Utils.zip (childVals val) childLeftovers
            |> List.all (\(childVal, childLeftovers) -> [childVal] == childLeftovers)
        in
        if nothingRemoved
        then [val]
        else List.concat childLeftovers

    valsDiff vals removeVals =
      vals |> List.concatMap (\val -> valDiff val removeVals)

    metricFromOutput val =
      let
        additionsNeeded = valsDiff outputValsDesired [val]
        deletionsNeeded = valsDiff [val] outputValsDesired
      in
      (additionsNeeded, deletionsNeeded)

    metric funcEnv funcBody =
      eval funcEnv funcBody |> Maybe.map metricFromOutput

    distance metric =
      case metric of
        Just (additionsNeeded, deletionsNeeded) -> List.length additionsNeeded + List.length deletionsNeeded |> toFloat
        Nothing                                 -> Utils.infinity

    rule funcEnv funcBody =
      case metric funcEnv funcBody of
        Just (additionsNeeded, deletionsNeeded) ->
          let
            _ = Debug.log "additionsNeeded" (List.map ValUnparser.strVal additionsNeeded)

            -- Uggggh I don't want to write another AST walker
            expsPerhapsWithNeededValueWithDups =
              flattenExpTree funcBody
              |> List.filter (not << isLet) -- We'll catch the let body.
              |> List.map (.val >> .eid)
              |> List.concatMap
                  (\eid ->
                    additionsNeeded
                    |> Provenance.dedupSameVals
                    |> List.concatMap
                        (\val ->
                          let
                            candidate1 = funcBody |> mapExpNode eid (\exp -> eList [eHoleVal0 val] (Just exp) |> copyPrecedingWhitespace exp)
                            candidate2 = funcBody |> mapExpNode eid (\exp -> eTuple [eHoleVal0 val, exp]      |> copyPrecedingWhitespace exp)
                          in
                          [candidate1,  candidate2]
                          |> List.concatMap (CodeMotion.resolveValueHoles syncOptions (Just funcEnv))
                          |> List.filter (flattenExpTree >> (not << List.any isValHole))
                        )
                  )

            _ = Debug.log "expsPerhapsWithNeededValueDups" (List.length expsPerhapsWithNeededValueWithDups)

            expsPerhapsWithNeededValue =
              expsPerhapsWithNeededValueWithDups
              |> Utils.dedupBy unparse

            _ = Debug.log "expsPerhapsWithNeededValue" (List.length expsPerhapsWithNeededValue)

            expsPerhapsWithPerhapsExtraValuesRemoved =
              let
                expsToMaybeRemove =
                  deletionsNeeded
                  |> List.concatMap Provenance.valToSameVals
                  |> List.map valExp
                  |> List.filter (.val >> .eid >> flip List.member (allEIds funcBody))
                  |> Utils.dedup
              in
              expsToMaybeRemove
              |> List.map
                  (\expToRemove ->
                    funcBody
                    |> mapExp
                        (\exp ->
                          case exp.val.e__ of
                            EList ws1 wsAndHeads ws2 maybeTail ws3 ->
                              case wsAndHeads |> Utils.maybeFindAndRemoveFirst (\(_, headExp) -> headExp.val.eid == expToRemove.val.eid) of
                                Just (_, newWsAndHeads) -> replaceE__ exp (EList ws1 newWsAndHeads ws2 maybeTail ws3)
                                _ ->
                                  exp
                            _ ->
                              exp
                        )
                  )
              |> List.filter (unparse >> (/=) (unparse funcBody))
          in
          expsPerhapsWithNeededValue ++ expsPerhapsWithPerhapsExtraValuesRemoved

        Nothing ->
          []
  in
  ValueSetConstraint [rule] { metric = metric, distance = distance }


makeFuncEnv : Env -> Maybe Ident -> List Pat -> List Val -> Exp -> Maybe Env
makeFuncEnv env maybeRecursiveName funcPats argVals funcBody =
  let
    valNoProvenance v_ = { v_ = v_, provenance = dummyProvenance, parents = Parents [] } -- Yes this duplicated elsewhere. I'm afraid to make it public.

    closureEnv =
      case maybeRecursiveName of
        Just funcName ->
          let funcVal = valNoProvenance (VClosure maybeRecursiveName funcPats funcBody env) in
          (funcName, funcVal)::env
        Nothing       -> env

  in
  Utils.maybeZip funcPats argVals
  |> Maybe.andThen (List.foldl Eval.cons (Just closureEnv))


noProgressDepthLimit = 5


-- env, let f = (\x y -> e) in f arg1 arg2 ==> let f = (\x y -> e') in f arg1' arg2' s.t. valsToAdd ⊆ output
-- Although, no argument modifications for now.
search : Env -> Maybe Ident -> Exp -> List Val -> List OutputConstraint -> List Exp
search
  env
  maybeRecursiveName
  funcExp
  initArgVals
  outputConstraints
  =
  -- Pre-loop:
  -- Compute metrics for initial candidate
  case LangTools.expToMaybeFuncPatsAndBody funcExp of
    Just (funcPats, funcBody) ->
      case makeFuncEnv env maybeRecursiveName funcPats initArgVals funcBody of
        Just funcEnv ->
          let
            candidates = [(funcBody, outputConstraints |> List.map (\constraint -> constraintDistance constraint funcEnv funcBody) |> List.sum)]
          in
          -- Start synthesis loop
          -- Stops at set depth or when one or more candidates satisfy all constraints
          search_ noProgressDepthLimit env maybeRecursiveName funcPats initArgVals outputConstraints candidates Set.empty
          |> List.filterMap (\(newFuncBody, distance) -> if distance == 0 then Just newFuncBody else Nothing)

        Nothing -> []
    Nothing -> []


search_ : Int -> Env -> Maybe Ident -> List Pat -> List Val -> List OutputConstraint -> List (Exp, Float) -> Set String -> List (Exp, Float)
search_ depthRemaining env maybeRecursiveName funcPats argVals outputConstraints priorCandidates visited =
  -- Synthesis loop:
    -- Check if constraints satisfied (if yes, return)
    -- Find candidates with lowest metric(s) (A* search)
    -- Apply rewrite rules to make new candidates
    -- Evaluate candidates with metric(s)
  let victories = priorCandidates |> List.filter (\(_, distance) -> distance == 0) in
  if victories /= [] then
    victories
  else if depthRemaining <= 0 then
    []
  else
    let
      shortestDistance                        = priorCandidates |> List.map (\(_, distance) -> distance) |> List.minimum |> Maybe.withDefault Utils.infinity
      (candidatesToExpand, candidatesToDefer) = priorCandidates |> List.partition (\(_, distance) -> distance == shortestDistance)

      _ = Debug.log "(candidatesToExpand, candidatesToDefer)" (List.length candidatesToExpand, List.length candidatesToDefer)

      (expandedCandidateExps, newVisitedSet) =
        candidatesToExpand
        |> Utils.foldl
            ([], visited)
            (\(candidateExp, _) (expandedCandidateExps, visited) ->
              let candidateUnparsed = unparse candidateExp in
              if Set.member candidateUnparsed visited then
                (expandedCandidateExps, visited)
              else
                let
                  newVisitedSet = Set.insert candidateUnparsed visited

                  newlyExpandedCandidateExps =
                    outputConstraints
                    |> List.concatMap
                        (\constraint ->
                          case makeFuncEnv env maybeRecursiveName funcPats argVals candidateExp of
                            Just funcEnv -> applyConstraintRules constraint funcEnv candidateExp
                            Nothing      -> []
                        )
                in
                (newlyExpandedCandidateExps ++ expandedCandidateExps, newVisitedSet)
            )

      _ = Debug.log "(expandedCandidateExps, newVisitedSet)" (List.length expandedCandidateExps, Set.size newVisitedSet)

      -- _ =
      --   expandedCandidateExps
      --   |> List.map (\candidateExp -> Utils.log <| unparse candidateExp)

      scoredExpandedCandidateExps =
        expandedCandidateExps
        |> List.filterMap
            (\candidateExp ->
              -- let _ = Utils.log <| "Scoring " ++ unparse candidateExp in
              makeFuncEnv env maybeRecursiveName funcPats argVals candidateExp
              |> Maybe.map
                  (\funcEnv ->
                    ( candidateExp
                    , outputConstraints |> List.map (\constraint -> constraintDistance constraint funcEnv candidateExp) |> List.sum
                    )
                  )
            )
        |> List.filter (\(_, distance) -> distance < Utils.infinity)
        |> List.sortBy (\(_, distance) -> distance)

      _ = Utils.log "scoredExpandedCandidateExps:"

      _ =
        scoredExpandedCandidateExps
        |> List.map (\(candidateExp, distance) -> Debug.log (unparse candidateExp) distance)

      _ = Utils.log "Recursing!"
    in
    search_ (depthRemaining - 1) env maybeRecursiveName funcPats argVals outputConstraints scoredExpandedCandidateExps newVisitedSet
