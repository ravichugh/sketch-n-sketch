--------------------------------------------------------------------------------
-- This module provides "glue" code for the View to access the transformations
-- that CodeMotion (and friends) provides.
--------------------------------------------------------------------------------

module DeuceTools exposing
  ( DeuceTool
  , deuceTools
  , isRenamer
  , isActive
  , noneActive
  )

import String
import Dict

import Either exposing (..)
import Utils
import ColorNum

import InterfaceModel exposing (..)

import Lang exposing (..)
import LangTools
import LangUnparser

import DeuceWidgets exposing
  ( DeuceWidget(..)
  )

import CodeMotion
import ExpressionBasedTransform

--------------------------------------------------------------------------------
-- Selections
--------------------------------------------------------------------------------

type alias Selections =
  ( List (LocId, (WS, Num, Loc, WidgetDecl))  -- number literals
  , List (EId, (WS, EBaseVal))                -- other base value literals
  , List EId                                  -- expressions (including literals)
  , List PathedPatternId                      -- patterns
  , List EId                                  -- equations
  , List ExpTargetPosition                    -- expression target positions
  , List PatTargetPosition                    -- pattern target positions
  )

selectedNumsAndBaseVals
    : Model
   -> ( List (LocId, (WS, Num, Loc, WidgetDecl))
      , List (EId, (WS, EBaseVal))
      )
selectedNumsAndBaseVals model =
  let noMatches = ([], []) in
  -- TODO may want to distinguish between different kinds of selected
  -- items earlier
  model.deuceState.selectedWidgets
  |> List.map (\deuceWidget ->
       case deuceWidget of
         DeuceExp eid ->
           case findExpByEId model.inputExp eid of
             Just ePlucked ->
               case ePlucked.val.e__ of
                 EConst ws n loc wd -> ([(eid, (ws, n, loc, wd))], [])
                 EBase ws baseVal   -> ([], [(eid, (ws, baseVal))])

                 _ -> noMatches
             _ -> noMatches
         _ -> noMatches
     )
  |> List.unzip
  |> (\(l1,l2) -> (List.concat l1, List.concat l2))

selectedNums : Model -> List (LocId, (WS, Num, Loc, WidgetDecl))
selectedNums =
  selectedNumsAndBaseVals >> Tuple.first

selectedBaseVals : Model -> List (EId, (WS, EBaseVal))
selectedBaseVals =
  selectedNumsAndBaseVals >> Tuple.second

selectedExps : List DeuceWidget -> List EId
selectedExps deuceWidgets =
  flip List.concatMap deuceWidgets <| \deuceWidget ->
    case deuceWidget of
      DeuceExp x -> [x]
      _ -> []

selectedPats : List DeuceWidget -> List PathedPatternId
selectedPats deuceWidgets =
  flip List.concatMap deuceWidgets <| \deuceWidget ->
    case deuceWidget of
      DeucePat x -> [x]
      _ -> []

selectedEquations : List DeuceWidget -> List EId
selectedEquations deuceWidgets =
  flip List.concatMap deuceWidgets <| \deuceWidget ->
    case deuceWidget of
      DeuceLetBindingEquation x -> [x]
      _ -> []

selectedExpTargets : List DeuceWidget -> List ExpTargetPosition
selectedExpTargets deuceWidgets =
  flip List.concatMap deuceWidgets <| \deuceWidget ->
    case deuceWidget of
      DeuceExpTarget x -> [x]
      _ -> []

selectedPatTargets : List DeuceWidget -> List PatTargetPosition
selectedPatTargets deuceWidgets =
  flip List.concatMap deuceWidgets <| \deuceWidget ->
    case deuceWidget of
      DeucePatTarget x -> [x]
      _ -> []

--------------------------------------------------------------------------------
-- Selection Helper Functions
--------------------------------------------------------------------------------

oneOrMoreNumsOnly : Selections -> Bool
oneOrMoreNumsOnly selections =
  case selections of
    (nums, [], exps, [], [], [], []) ->
      List.length nums >= 1 && List.length nums == List.length exps
    _ ->
      False

--------------------------------------------------------------------------------
-- Predicates
--------------------------------------------------------------------------------

type PredicateValue
    -- Good to go, and can accept no more arguments
  = FullySatisfied
    -- Good to go, but can accept more arguments if necessary
  | Satisfied
    -- Not yet good to go, but with more arguments may be okay
  | Possible
    -- Not good to go, and no additional arguments will make a difference
  | Impossible

-- NOTE: Descriptions should be an *action* in sentence case with no period at
--       the end, e.g.:
--         * Select a boolean value
--         * Select 4 integers
type alias Predicate =
  { description : String
  , value : PredicateValue
  }

satisfied : Predicate -> Bool
satisfied pred =
  case pred.value of
    FullySatisfied ->
      True
    Satisfied ->
      True
    Possible ->
      False
    Impossible ->
      False

allSatisfied : List Predicate -> Bool
allSatisfied =
  List.all satisfied

--------------------------------------------------------------------------------
-- Deuce Tools
--------------------------------------------------------------------------------

type alias DeuceTransformation =
  () -> List SynthesisResult

type alias DeuceTool =
  { name : String
  , func : Maybe DeuceTransformation
  , reqs : List Predicate -- requirements to run the tool
  }

--------------------------------------------------------------------------------
-- Make Equal
--------------------------------------------------------------------------------

makeEqualTool : Model -> Selections -> DeuceTool
makeEqualTool model selections =
  -- TODO allow optional target position
  -- TODO define a helper to factor Introduce Var and this
  let
    (func, literalPredVal) =
      case selections of
        (nums, baseVals, exps, [], [], [], []) ->
          let
            literals =
              List.map Left nums ++ List.map Right baseVals
          in
            if List.length literals /= List.length exps then
              (Nothing, Impossible)
            else if List.length literals < 2 then
              (Nothing, Possible)
            else
              ( CodeMotion.makeEqualTransformation model literals
              , Satisfied
              )
        _ ->
          (Nothing, Impossible)
  in
    { name = "Make Equal (New Variable)"
    , func = func
    , reqs =
        [ { description =
              "Select two or more literals"
          , value =
              literalPredVal
          }
        ]
    }


--------------------------------------------------------------------------------
-- Flip Boolean
--------------------------------------------------------------------------------
-- TODO This function does not rely on CodeMotion, so it is probably doing too
--      much.
--------------------------------------------------------------------------------

flipBooleanTool : Model -> Selections -> DeuceTool
flipBooleanTool model selections =
  let
    (func, boolPredVal) =
      case selections of
        ([], [_], [eId], [], [], [], []) ->
          case findExpByEId model.inputExp eId of
            Just ePlucked ->
              case ePlucked.val.e__ of
                EBase ws (EBool bool) ->
                  let
                    flipped =
                      withDummyExpInfo (EBase ws (EBool (not bool)))
                    newExp =
                      replaceExpNode eId flipped model.inputExp
                  in
                    (Just <| \() -> oneSafeResult newExp, FullySatisfied)
                _ ->
                  (Nothing, Impossible)
            _ ->
              (Nothing, Impossible)
        ([], [], [], [], [], [], []) ->
          (Nothing, Possible)
        _ ->
          (Nothing, Impossible)
  in
    { name = "Flip Boolean"
    , func = func
    , reqs =
        [ { description =
              "Select a boolean value"
          , value =
              boolPredVal
          }
        ]
    }

--------------------------------------------------------------------------------
-- Rename Pattern
--------------------------------------------------------------------------------

renamePatternTool : Model -> Selections -> DeuceTool
renamePatternTool model selections =
  let
    disabledName =
      "Rename Pattern"
    (name, func, patPredVal) =
      case selections of
        ([], [], [], [pathedPatId], [], [], []) ->
          case
            LangTools.findPat pathedPatId model.inputExp
              |> Maybe.andThen LangTools.patToMaybeIdent
          of
            Just ident ->
              ( "Rename " ++ ident
              , Just <|
                  \() ->
                    let
                      newName =
                        model.deuceState.renameVarTextBox
                    in
                      CodeMotion.renamePat pathedPatId newName model.inputExp
              , FullySatisfied
              )
            _ ->
              (disabledName, Nothing, Impossible)
        ([], [], [], [], [], [], []) ->
          (disabledName, Nothing, Possible)
        _ ->
          (disabledName, Nothing, Impossible)
  in
    { name = name
    , func = func
    , reqs =
        [ { description =
              "Select a pattern identifier"
          , value =
              patPredVal
          }
        ]
    }

--------------------------------------------------------------------------------
-- Rename Variable
--------------------------------------------------------------------------------

renameVariableTool : Model -> Selections -> DeuceTool
renameVariableTool model selections =
  let
    disabledName =
      "Rename Variable"
    (name, func, varPredVal) =
      case selections of
        ([], [], [eId], [], [], [], []) ->
          case findExpByEId model.inputExp eId of
            Just ePlucked ->
              case ePlucked.val.e__ of
                EVar _ ident ->
                  let
                     newName =
                      model.deuceState.renameVarTextBox
                  in
                    ( "Rename All " ++ ident
                    , Just <|
                        \() ->
                          CodeMotion.renameVar eId newName model.inputExp
                    , FullySatisfied
                    )
                _ -> (disabledName, Nothing, Impossible)
            _ -> (disabledName, Nothing, Impossible)
        ([], [], [], [], [], [], []) ->
          (disabledName, Nothing, Possible)
        _ ->
          (disabledName, Nothing, Impossible)
  in
    { name = name
    , func = func
    , reqs =
        [ { description =
              "Select a variable"
          , value =
              varPredVal
          }
        ]
    }

--------------------------------------------------------------------------------
-- Swap Names and Usages
--------------------------------------------------------------------------------
-- Swap Usages
--------------------------------------------------------------------------------

swapNamesAndUsagesTool : Model -> Selections -> DeuceTool
swapNamesAndUsagesTool model selections =
  let makeThunk (pathedPatId1, name1) (pathedPatId2, name2) () =
    model.inputExp
      |> CodeMotion.composeTransformations
           ("Swap names " ++ name1 ++ " and " ++ name2)
           [ CodeMotion.renamePat
               pathedPatId1
               "IMPROBABLE_TEMPORARY_NAME_FOR_SAFETY_CHECK!!!"
           , CodeMotion.renamePat
               pathedPatId2
               name1
           , CodeMotion.renamePat
               pathedPatId1
               name2
           ]
  in
  selectTwoVars "Swap Names and Usages" makeThunk model selections

swapUsagesTool : Model -> Selections -> DeuceTool
swapUsagesTool model selections =
  let makeThunk (pathedPatId1, _) (pathedPatId2, _) () =
    CodeMotion.swapUsages pathedPatId1 pathedPatId2 model.inputExp
  in
  selectTwoVars "Swap Usages" makeThunk model selections

selectTwoVars toolName makeThunk model selections =
  let
    (func, predVal) =
      case selections of
        ([], [], [], [], [], [], []) ->
          (Nothing, Possible)
        ([], [], [], [_], [], [], []) ->
          (Nothing, Possible) -- could check whether pathedPatId1 is a var
        ([], [], [], [pathedPatId1, pathedPatId2], [], [], []) ->
          let maybeNames =
            [pathedPatId1, pathedPatId2]
              |> List.map (\ppid -> LangTools.findPat ppid model.inputExp)
              |> List.map (Maybe.andThen LangTools.patToMaybeIdent)
          in
          case maybeNames of
            [Just name1, Just name2] ->
              ( Just (makeThunk (pathedPatId1, name1) (pathedPatId2, name2))
              , FullySatisfied
              )
            _ ->
              (Nothing, Impossible)
        _ ->
          (Nothing, Impossible)
  in
    { name = toolName
    , func = func
    , reqs = [ { description = "Select two variables", value = predVal } ]
    }

--------------------------------------------------------------------------------
-- Inline Definition
--------------------------------------------------------------------------------

inlineDefinitionTool : Model -> Selections -> DeuceTool
inlineDefinitionTool model selections =
  let
    toolName = "Inline Definition"

    (name, func, predVal) =
      case selections of
        ([], [], [], [], [], [], []) ->
          (toolName, Nothing, Possible)
        (_, _, _, [], [], _, _) ->
          (toolName, Nothing, Impossible)
        ([], [], [], pathedPatIds, [], [], []) ->
          ( Utils.maybePluralize toolName pathedPatIds
          , Just <| \() ->
              CodeMotion.inlineDefinitions pathedPatIds model.inputExp
          , Satisfied
          )
        ([], [], [], [], letEIds, [], []) ->
          ( Utils.maybePluralize toolName letEIds
          , Just <| \() ->
              CodeMotion.inlineDefinitions
                (letEIds |> List.map (\letEId -> ((letEId, 1), [])))
                model.inputExp
          , Satisfied
          )
        _ ->
          (toolName, Nothing, Impossible)
  in
    { name = name
    , func = func
    , reqs =
        [ { description =
              "Select 1 or more patterns, or 1 or more variable definitions"
          , value =
              predVal
          }
        ]
    }


--------------------------------------------------------------------------------
-- Twiddle Shapes
--------------------------------------------------------------------------------
-- TODO
--------------------------------------------------------------------------------
--
--matchOne str (strRegex, f) =
--  case Regex.find (Regex.AtMost 1) (Regex.regex strRegex) str of
--    [match] ->
--      case Utils.projJusts match.submatches of
--        Nothing -> []
--        Just xs -> f xs
--    _ ->
--      []
--
--makeTwiddleTools m eId eShape =
--
--  let strShape = unparse eShape in
--
--  let evaluateRulesUntilMatch rules =
--    case rules of
--      [] -> []
--      rule :: rest ->
--        case matchOne strShape rule of
--          []    -> evaluateRulesUntilMatch rest
--          tools -> tools
--  in
--
--  let rewriteAndReturn newShape =
--    let newExp =
--      replaceExpNodePreservingPrecedingWhitespace eId newShape m.inputExp
--    in
--    [synthesisResult "XXX" newExp]
--  in
--
--  let rewriteRectRawToStretchy args =
--    if List.length args /= 8 then []
--    else
--      let (x, y, w, h, fill, stroke, strokeWidth, rot) =
--        Utils.unwrap8 (List.map Utils.parseInt args)
--      in
--      [ ("Rewrite Stretchy", \() ->
--          let newShape =
--            stencilStretchyRect x y (x + w) (y + h) fill stroke strokeWidth rot in
--          rewriteAndReturn newShape
--        ) ]
--  in
--
--  let rewriteRectStretchyToRaw args =
--    if List.length args /= 8 then []
--    else
--      let (left, top, right, bot, fill, stroke, strokeWidth, rot) =
--        Utils.unwrap8 (List.map Utils.parseInt args)
--      in
--      [ ("Rewrite Raw", \() ->
--          let newShape =
--            stencilRawRect left top (right - left) (bot - top) fill stroke strokeWidth rot in
--          rewriteAndReturn newShape
--        ) ]
--  in
--
--  evaluateRulesUntilMatch <|
--    [ (reStretchyRect, rewriteRectStretchyToRaw)
--    , (reRawRect, rewriteRectRawToStretchy)
--    ]
--
--twiddleShapesTool : Model -> Selections -> DeuceTool
--twiddleShapesTool model selections = case selections of
--  ([], [], [eId], [], [], [], []) ->
--    case findExpByEId m.inputExp eId of
--      Just ePlucked ->
--        let tools = Draw.makeTwiddleTools m eId ePlucked in
--        case tools of
--          _::_ -> tools
--          [] -> []
--      _ -> []
--  _ -> []
--

--------------------------------------------------------------------------------
-- Introduce Variable
--------------------------------------------------------------------------------

introduceVariableTool : Model -> Selections -> DeuceTool
introduceVariableTool model selections =
  -- TODO allow target position to be omitted
  let
    toolName = "Introduce Variable"

    (name, func, predVal) =
      case selections of
        ([], [], [], [], [], [], []) ->
          (toolName, Nothing, Possible)
        (_, _, [], _, _, _, _) ->
          (toolName, Nothing, Impossible)
        (_, _, exps, [], [], [], [patTarget]) ->
          ( Utils.maybePluralize "Introduce Variable" exps
          , CodeMotion.introduceVarTransformation
              model
              exps
              (PatTargetPosition patTarget)
          , Satisfied
          )
        (_, _, exps, [], [], [expTarget], []) ->
          ( Utils.maybePluralize "Introduce Variable" exps
          , CodeMotion.introduceVarTransformation
              model
              exps
              (ExpTargetPosition expTarget)
          , Satisfied
          )
        _ ->
          (toolName, Nothing, Impossible)
  in
    { name = name
    , func = func
    , reqs =
        [ { description =
              "Select 1 or more constants and 1 optional target position"
          , value =
              predVal
          }
        ]
    }

--------------------------------------------------------------------------------
-- Elminate Common Sub-Expression
--------------------------------------------------------------------------------
-- Copy Expression
--------------------------------------------------------------------------------

eliminateCommonSubExpressionTool : Model -> Selections -> DeuceTool
eliminateCommonSubExpressionTool model selections =
  selectOneNonLiteralExpression
    "Eliminate Common Sub-Expression"
    CodeMotion.eliminateCommonSubExpressionTransformation
    model
    selections

copyExpressionTool : Model -> Selections -> DeuceTool
copyExpressionTool model selections =
  selectOneNonLiteralExpression
    "Copy Expression"
    CodeMotion.copyExpressionTransformation
    model
    selections

selectOneNonLiteralExpression toolName makeThunk model selections =
  let
    (func, predVal) =
      case selections of
        ([], [], [], [], [], [], []) ->
          (Nothing, Possible)
        (_, _, [], _, _, _, _) ->
          (Nothing, Impossible)
        (_, _, [_], _, _, _, _) ->
          (Nothing, Impossible)
        (nums, baseVals, i::js, [], [], [], []) ->
          let
            atLeastOneNonLiteral =
              List.length (i::js) > (List.length nums + List.length baseVals)
          in
            if atLeastOneNonLiteral then
              ( makeThunk model i js
              , Satisfied
              )
            else
              (Nothing, Impossible)
        _ ->
          (Nothing, Impossible)
  in
    { name = toolName
    , func = func
    , reqs =
        [ { description =
              "Select 2 or more expressions, including at least 1 that is not a constant"
          , value =
              predVal
          }
        ]
    }

--------------------------------------------------------------------------------
-- Move Definition
--------------------------------------------------------------------------------

moveDefinitionTool : Model -> Selections -> DeuceTool
moveDefinitionTool model selections =
  let
    toolName = "Move Definition"

    (name, func, predVal) =
      case selections of
        ([], [], [], [], [], [], []) ->
          (toolName, Nothing, Possible)
        (_, _, _, [], [], _, _) ->
          (toolName, Nothing, Impossible)
        ([], [], [], pathedPatIds, [], [(Before, eId)], []) ->
          ( Utils.maybePluralize toolName pathedPatIds
          , Just <| \() ->
              CodeMotion.moveDefinitionsBeforeEId
                pathedPatIds
                eId
                model.inputExp
          , Satisfied
          )
        ([], [], [], pathedPatIds, [], [], [patTarget]) ->
          let
            targetPathedPatId =
              patTargetPositionToTargetPathedPatId patTarget
          in
            case
              ( findExpByEId
                  model.inputExp
                  (pathedPatIdToScopeEId targetPathedPatId)
              ) |> Maybe.map (.val >> .e__)
            of
              Just (ELet _ _ _ _ _ _ _) ->
                ( Utils.maybePluralize toolName pathedPatIds
                , Just <| \() ->
                    CodeMotion.moveDefinitionsPat
                      pathedPatIds
                      targetPathedPatId
                      model.inputExp
                , Satisfied
                )
              _ ->
                (toolName, Nothing, Impossible)
        ([], [], [], [], [letEId], [(Before, eId)], []) ->
          ( toolName
          , Just <| \() ->
              -- Better result names if we hand the singular case directly to
              -- moveDefinitionsBeforeEId.
              CodeMotion.moveDefinitionsBeforeEId
                [((letEId, 1), [])]
                eId
                model.inputExp
          , Satisfied
          )
        ([], [], [], [], letEIds, [(Before, eId)], []) ->
          ( Utils.maybePluralize toolName letEIds
          , Just <| \() ->
              CodeMotion.moveEquationsBeforeEId
                letEIds
                eId
                model.inputExp
          , Satisfied
          )
        _ ->
          (toolName, Nothing, Impossible)
  in
    { name = toolName
    , func = func
    , reqs =
        [ { description =
              """Select 2 or more patterns or 2 or more variable definitions,
                 and 1 target position"""
          , value =
              predVal
          }
        ]
    }

--------------------------------------------------------------------------------
-- Duplicate Definition
--------------------------------------------------------------------------------

duplicateDefinitionTool : Model -> Selections -> DeuceTool
duplicateDefinitionTool model selections =
  let
    toolName = "Duplicate Definition"

    (name, func, predVal) =
      case selections of
        ([], [], [], [], [], [], []) ->
          (toolName, Nothing, Possible)
        (_, _, _, [], _, _, _) ->
          (toolName, Nothing, Impossible)
        ([], [], [], pathedPatIds, [], [(Before, eId)], []) ->
          ( Utils.maybePluralize toolName pathedPatIds
          , Just <| \() ->
              CodeMotion.duplicateDefinitionsBeforeEId
                pathedPatIds
                eId
                model.inputExp
          , Satisfied
          )
        ([], [], [], pathedPatIds, [], [], [patTarget]) ->
          let
            targetPathedPatId =
              patTargetPositionToTargetPathedPatId patTarget
          in
            case
              ( findExpByEId
                  model.inputExp
                  (pathedPatIdToScopeEId targetPathedPatId)
              ) |> Maybe.map (.val >> .e__)
            of
              Just (ELet _ _ _ _ _ _ _) ->
                ( Utils.maybePluralize toolName pathedPatIds
                , Just <| \() ->
                    CodeMotion.duplicateDefinitionsPat
                      pathedPatIds
                      targetPathedPatId
                      model.inputExp
                , Satisfied
                )
              _ ->
                (toolName, Nothing, Impossible)
        _ ->
          (toolName, Nothing, Impossible)
  in
    { name = toolName
    , func = func
    , reqs =
        [ { description =
              "Select 1 or more patterns and 1 target position"
          , value =
              predVal
          }
        ]
    }


--------------------------------------------------------------------------------
-- Thaw/Freeze
--------------------------------------------------------------------------------
-- TODO This function does not rely on CodeMotion, so it is probably doing too
--      much.
--------------------------------------------------------------------------------

thawFreezeTool : Model -> Selections -> DeuceTool
thawFreezeTool model selections =
  let
    (nums, _, _, _, _, _, _) =
      selections

    mode =
      let
        freezeAnnotations =
          List.map (\(_,(_,_,(_,frzn,_),_)) -> frzn) nums
      in
        if not (oneOrMoreNumsOnly selections) then
          Nothing
        else
          case Utils.dedup freezeAnnotations of
            [frzn] ->
              if model.syncOptions.thawedByDefault then
                if frzn == unann then
                  Just ("Freeze", frozen)
                else if frzn == frozen then
                  Just ("Thaw", unann)
                else if frzn == thawed then
                  Just ("Freeze", frozen)
                else
                  Nothing
              else
                if frzn == unann then
                  Just ("Thaw", thawed)
                else if frzn == frozen then
                  Just ("Thaw", thawed)
                else if frzn == thawed then
                  Just ("Freeze", unann)
                else
                  Nothing
            _ ->
              if model.syncOptions.thawedByDefault then
                Just ("Freeze", frozen)
              else
                Just ("Thaw", thawed)

    (name, func, predVal) =
      case mode of
        Nothing ->
          ("Thaw/Freeze", Nothing, Impossible)
        Just (toolName, newAnnotation) ->
          ( toolName
          , Just <| \() ->
              let
                eSubst =
                  List.foldl
                    ( \(eId,(ws,n,(locid,_,x),wd)) acc ->
                        Dict.insert
                          eId
                          (EConst ws n (locid, newAnnotation, x) wd)
                          acc
                    )
                    Dict.empty
                    nums
              in
                [ synthesisResult
                    toolName
                    (applyESubst eSubst model.inputExp)
                ]
          , Satisfied
          )
  in
    { name = name
    , func = func
    , reqs = [ { description = "Select 1 or more numbers", value = predVal } ]
    }

--------------------------------------------------------------------------------
-- Show/Hide Range
--------------------------------------------------------------------------------
-- TODO This function does not rely on CodeMotion, so it is probably doing too
--      much.
--------------------------------------------------------------------------------

showHideRangeTool : Model -> Selections -> DeuceTool
showHideRangeTool model selections =
  let
    (nums, _, _, _, _, _, _) =
      selections

    mode =
      if not (oneOrMoreNumsOnly selections) then
        Nothing
      else
        let
          freezeAnnotations =
            flip List.map nums <|
              \(_,(_,_,_,wd)) ->
                case wd.val of
                  IntSlider _ _ _ _ b ->
                    Just b
                  NumSlider _ _ _ _ b ->
                    Just b
                  _ ->
                    Nothing
        in
          case Utils.dedup freezeAnnotations of
            [Just b] ->
              Just b
            _ ->
              Nothing

    (name, func, predVal) =
      case mode of
        Nothing ->
          ("Show/Hide Sliders", Nothing, Impossible)
        Just hidden ->
          let
            toolName =
              case (hidden, List.length nums) of
                (True, 1) ->
                  "Show Slider"
                (True, _) ->
                  "Show Sliders"
                (False, 1) ->
                  "Hide Slider"
                (False, _) ->
                  "Hide Sliders"
          in
            ( toolName
            , Just <| \() ->
                let
                  eSubst =
                    List.foldl
                      ( \(eId,(ws,n,loc,wd)) acc ->
                          let
                            wd_ =
                              case wd.val of
                                IntSlider a b c d _ ->
                                  IntSlider a b c d (not hidden)
                                NumSlider a b c d _ ->
                                  NumSlider a b c d (not hidden)
                                _ ->
                                  wd.val
                          in
                            Dict.insert
                              eId
                              (EConst ws n loc { wd | val = wd_ })
                              acc
                      )
                      Dict.empty
                      nums
                in
                  [ synthesisResult
                      toolName
                      (applyESubst eSubst model.inputExp)
                  ]
            , Satisfied
            )
  in
    { name = name
    , func = func
    , reqs = [ { description = "Select 1 or more numbers", value = predVal } ]
    }

--------------------------------------------------------------------------------
-- Add/Remove Range
--------------------------------------------------------------------------------
-- TODO This function does not rely on CodeMotion, so it is probably doing too
--      much.
--------------------------------------------------------------------------------

-- Helper function
rangeAround : number -> (number, number)
rangeAround n =
  let i = n - 3 * n in
  let j = n + 3 * n in
  (i, j)

addRemoveRangeTool : Model -> Selections -> DeuceTool
addRemoveRangeTool model selections =
  let
    (nums, _, _, _, _, _, _) =
      selections

    mode =
      let
        freezeAnnotations =
          flip List.map nums <|
            \(_,(_,_,_,wd)) ->
              case wd.val of
                NoWidgetDecl ->
                  True
                _ ->
                  False
      in
        case Utils.dedup freezeAnnotations of
          [b] ->
            Just b
          _   ->
            Nothing

    (name, func, predVal) =
      case mode of
        Nothing ->
          ("Add/Remove Sliders", Nothing, Impossible)
        Just noRanges ->
          let
            toolName =
              case (noRanges, List.length nums) of
                (True, 1) ->
                  "Add Slider"
                (True, _) ->
                  "Add Sliders"
                (False, 1) ->
                  "Remove Slider"
                (False, _) ->
                  "Remove Sliders"
          in
            ( toolName
            , Just <| \() ->
                let
                  eSubst =
                    List.foldl
                      ( \(eId,(ws,n,loc,_)) acc ->
                          let
                            wd =
                              if noRanges then
                                if Utils.between n (0.001, 1) then
                                  numSlider 0.001 1
                                else
                                  let
                                    (i, j) =
                                      rangeAround n
                                  in
                                    if toFloat (round n) == n then
                                      intSlider (max 0 (round i)) (round j)
                                    else
                                      numSlider (max 0 i) j
                              else
                                withDummyRange NoWidgetDecl
                          in
                            Dict.insert
                              eId
                              (EConst ws n loc wd)
                              acc
                      )
                      Dict.empty
                      nums
                in
                  [ synthesisResult
                      toolName
                      (applyESubst eSubst model.inputExp)
                  ]
            , Satisfied
            )
  in
    { name = name
    , func = func
    , reqs = [ { description = "Select 1 or more numbers", value = predVal } ]
    }

--------------------------------------------------------------------------------
-- Rewrite Offset
--------------------------------------------------------------------------------

rewriteOffsetTool : Model -> Selections -> DeuceTool
rewriteOffsetTool model selections =
  let
    toolName = "Rewrite as Offset"

    (name, func, predVal)  =
      case selections of
        ([], [], [], [], [], [], []) ->
          (toolName, Nothing, Possible)
        ([], _, _, _, _, _, _) ->
          (toolName, Nothing, Impossible)
        (nums, [], exps, [ppid], [], [], []) ->
          if List.length nums == List.length exps then
            ( Utils.maybePluralize toolName nums
            , CodeMotion.rewriteOffsetTransformation model ppid nums
            , Satisfied
            )
          else
            (toolName, Nothing, Impossible)
        _ ->
          (toolName, Nothing, Impossible)
  in
    { name = name
    , func = func
    , reqs =
        [ { description = "Select 1 variable and 1 or more numbers"
          , value = predVal
          }
        ]
    }

--------------------------------------------------------------------------------
-- Convert Color String
--------------------------------------------------------------------------------
-- TODO This function does not rely on CodeMotion, so it is probably doing too
--      much.
--------------------------------------------------------------------------------

-- convertColorStringTool : Model -> Selections -> DeuceTool
-- convertColorStringTool model selections =
--   let
--     disabledTool =
--       { name = "Convert Color String"
--       , func = Nothing
--       }
--   in
--     case selections of
--       (_, [], _, _, _, _, _) ->
--         disabledTool
--       ([], literals, exps, [], [], [], []) ->
--         if List.length exps /= List.length literals then
--           disabledTool
--         else
--           let
--             maybeStrings =
--               List.map
--                 ( \(eid, (_, baseVal)) ->
--                     case baseVal of
--                       EString _ string ->
--                         Just (eid, string)
--                       _ ->
--                         Nothing
--                 )
--                 literals
--           in
--             Utils.bindMaybesToList maybeStrings <| \idsAndStrings ->
--               let
--                 maybeConverted =
--                   List.map ColorNum.convertStringToRgbAndHue idsAndStrings
--               in
--                 Utils.bindMaybesToList maybeConverted <| \converted ->
--                   let
--                     (newExp1, newExp2) =
--                       List.foldl
--                         ( \(eid,(r,g,b),hue) (acc1,acc2) ->
--                             let
--                               replaceString =
--                                 replaceExpNodePreservingPrecedingWhitespace eid
--                               eRgba =
--                                 eList (listOfNums [r,g,b,1.0]) Nothing
--                               eColorNum =
--                                 eConst hue dummyLoc
--                             in
--                               ( replaceString eRgba acc1
--                               , replaceString eColorNum acc2
--                               )
--                         )
--                         (model.inputExp, model.inputExp)
--                         converted
--                   in
--                     { name =
--                         Utils.maybePluralize "Convert Color String" literals
--                     , func =
--                         Just <|
--                           \() ->
--                             [ newExp1
--                                 |> synthesisResult "RGBA"
--                             , newExp2
--                                 |> synthesisResult "Color Number (Hue Only)"
--                                 |> setResultSafe False
--                             ]
--                     }
--       _ ->
--         disabledTool

--------------------------------------------------------------------------------
-- Create Function
--------------------------------------------------------------------------------

createFunctionTool : Model -> Selections -> DeuceTool
createFunctionTool model selections =
  let
    (func, predVal) =
      case selections of
        ([], [], [], [], [], [], []) ->
          (Nothing, Possible)
        ([], [], [], [pathedPatId], [], [], []) ->
          case
            LangTools.findScopeExpAndPat pathedPatId model.inputExp
              |> Maybe.map (\(e,p) -> (e.val.e__, p.val.p__))
          of
            Just (ELet _ _ _ _ _ _ _, PVar _ ident _) ->
              ( Just <| \() ->
                  CodeMotion.abstractPVar pathedPatId model.inputExp
              , FullySatisfied
              )
            _ ->
              (Nothing, Impossible)
        (_, _, [eid], [], [], [], []) ->
          let
            maybeExpToAbstract =
              findExpByEId model.inputExp eid
            expToAbstractParts =
              maybeExpToAbstract
                |> Maybe.map flattenExpTree
                |> Maybe.withDefault []
            parameterCount =
              ( Utils.count
                  ( \e ->
                      CodeMotion.shouldBeParameterIsConstant
                        e model.inputExp
                  )
                  expToAbstractParts
              ) +
              ( Utils.count
                  ( \e ->
                      CodeMotion.shouldBeParameterIsNamedUnfrozenConstant
                        e model.inputExp
                  )
                  expToAbstractParts
              )
            expSize =
              maybeExpToAbstract
                |> Maybe.map LangTools.nodeCount
                |> Maybe.withDefault 0
          in
            if parameterCount > 0 && expSize >= 3 then
              ( Just <| \() -> CodeMotion.abstractExp eid model.inputExp
              , FullySatisfied
              )
            else
              (Nothing, Impossible)
        ([], [], [], [], [letEId], [], []) ->
          case
            LangTools.justFindExpByEId model.inputExp letEId
              |> LangTools.expToMaybeLetPat
              |> Maybe.map (.val >> .p__)
          of
            Just (PVar _ _ _) ->
              ( Just <|
                  \() ->
                    let
                       pathedPatId = ((letEId, 1), [])
                    in
                      CodeMotion.abstractPVar pathedPatId model.inputExp
              , FullySatisfied
              )
            _ ->
              (Nothing, Impossible)
        _ ->
          (Nothing, Impossible)
  in
    { name = "Create Function"
    , func = func
    , reqs =
        [ { description = "Select 1 definition or variable or expression"
          , value = predVal
          }
        ]
    }

--------------------------------------------------------------------------------
-- Merge
--------------------------------------------------------------------------------

mergeTool : Model -> Selections -> DeuceTool
mergeTool model selections =
  let
    tryMerge eids =
      let
        mergeResults =
          let
            candidateExpFilter e =
              List.member e.val.eid eids
            minCloneCount =
              List.length eids
          in
            ExpressionBasedTransform.cloneEliminationSythesisResults
              candidateExpFilter minCloneCount 2 model.inputExp
      in
        if mergeResults /= [] then
          ( Just <| \() -> mergeResults
          , Satisfied
          )
        else
          (Nothing, Impossible)


    (func, predVal) =
      case selections of
        ([], [], [], [], [], [], []) ->
          (Nothing, Possible)

        (_, _, [], [], letEId1::letEId2::restLetEIds, [], []) ->
          let boundExpEIds =
            letEId1::letEId2::restLetEIds
            |> List.map (LangTools.justFindExpByEId model.inputExp >> LangTools.expToLetBoundExp >> .val >> .eid)
          in
          tryMerge boundExpEIds

        (_, _, eid1::eid2::restEIds, [], [], [], []) ->
          tryMerge (eid1::eid2::restEIds)

        _ ->
          (Nothing, Impossible)
  in
    { name = "Merge Expressions into Function"
    , func = func
    , reqs =
        [ { description = "Select 2 or more expressions"
          , value = predVal
          }
        ]
    }

--------------------------------------------------------------------------------
-- Add Arguments
--------------------------------------------------------------------------------

addArgumentsTool : Model -> Selections -> DeuceTool
addArgumentsTool model selections =
  let
    toolName = "Add Argument"

    -- this helper helps avoid changing existing structure of this function
    makeReqs predVal =
      [ { description =
            """Select 1 or more expressions in a function and
               1 target position in the function's argument list"""
        , value = predVal
        }
      ]

    defaultTool predVal =
      { name = toolName
      , func = Nothing
      , reqs = makeReqs predVal
      }

    disabledTool = defaultTool Impossible
  in
    case selections of
      ([], [], [], [], [], [], []) ->
        defaultTool Possible
      (_, _, firstEId::restEIds, [], [], [], [patTarget]) ->
        let
          eids =
            firstEId::restEIds
          targetPathedPatId =
            patTargetPositionToTargetPathedPatId patTarget
          maybeScopeExp =
            findExpByEId
              model.inputExp
              (pathedPatIdToScopeEId targetPathedPatId)
        in
          case
            maybeScopeExp
              |> Maybe.map (.val >> .e__)
          of
            Just (EFun _ _ fbody _) ->
              let
                isAllSelectedExpsInFBody =
                  eids |> List.all (\eid -> findExpByEId fbody eid /= Nothing)
              in
                if isAllSelectedExpsInFBody && List.length eids == 1 then
                  { name = "Add Argument" -- Fowler calls this "Add Parameter"
                  , func =
                      Just <|
                        \() ->
                          CodeMotion.addArg
                            (Utils.head_ eids)
                            targetPathedPatId
                            model.inputExp
                  , reqs = makeReqs Satisfied
                  }
                else if isAllSelectedExpsInFBody then
                  { name = "Add Arguments" -- Fowler calls this "Add Parameter"
                  , func =
                      Just <|
                        \() ->
                          CodeMotion.addArgs
                            eids
                            targetPathedPatId
                            model.inputExp
                  , reqs = makeReqs Satisfied
                  }
                else
                  disabledTool
            _ ->
              disabledTool
      ( _, _, []
      , firstArgSourcePathedPatId::restArgSourcePathedPatId
      , [], [], [patTarget]
      ) ->
        let
          argSourcePathedPatIds =
            firstArgSourcePathedPatId::restArgSourcePathedPatId
          targetPathedPatId =
            patTargetPositionToTargetPathedPatId patTarget
          maybeScopeExp =
            findExpByEId model.inputExp (pathedPatIdToScopeEId targetPathedPatId)
        in
          case
            maybeScopeExp
              |> Maybe.map (.val >> .e__)
          of
            Just (EFun _ _ fbody _) ->
              let
                isAllSelectedPatsInFBody =
                  argSourcePathedPatIds
                    |> List.all
                         ( \argSourcePathedPatId ->
                             LangTools.findScopeExpAndPat
                               argSourcePathedPatId fbody /= Nothing
                         )
              in
                if
                  isAllSelectedPatsInFBody &&
                  List.length argSourcePathedPatIds == 1
                then
                  { name = "Add Argument" -- Fowler calls this "Add Parameter"
                  , func =
                      Just <|
                        \() ->
                          CodeMotion.addArgFromPat
                            (Utils.head_ argSourcePathedPatIds)
                            targetPathedPatId
                            model.inputExp
                  , reqs = makeReqs Satisfied
                  }
                else if isAllSelectedPatsInFBody then
                  { name = "Add Arguments" -- Fowler calls this "Add Parameter"
                  , func =
                      Just <|
                        \() ->
                          CodeMotion.addArgsFromPats
                            argSourcePathedPatIds
                            targetPathedPatId
                            model.inputExp
                  , reqs = makeReqs Satisfied
                  }
                else
                  disabledTool
            _ ->
              disabledTool
      _ ->
        disabledTool

--------------------------------------------------------------------------------
-- Remove Arguments
--------------------------------------------------------------------------------

removeArgumentsTool : Model -> Selections -> DeuceTool
removeArgumentsTool model selections =
  let
    toolName = "Remove Argument"

    -- this helper helps avoid changing existing structure of this function
    makeReqs predVal =
      [ { description =
            "Select function arguments at definitions or call-sites"
        , value = predVal
        }
      ]

    defaultTool predVal =
      { name = toolName
      , func = Nothing
      , reqs = makeReqs predVal
      }

    disabledTool = defaultTool Impossible
  in
    case selections of
      ([], [], [], [], [], [], []) ->
        defaultTool Possible
      (_, _, [], [], _, _, _) ->
        disabledTool
      ([], [], [], pathedPatIds, [], [], []) ->
        let
          isAllArgumentSelected =
            pathedPatIds
              |> List.all
                   ( \pathedPatId ->
                       let
                         scopeExp =
                           findExpByEId
                             model.inputExp
                             (pathedPatIdToScopeEId pathedPatId)
                       in
                         case
                           scopeExp
                             |> Maybe.map (.val >> .e__)
                         of
                           Just (EFun _ _ _ _) ->
                             True
                           _ ->
                             False
                   )
        in
          if isAllArgumentSelected && List.length pathedPatIds == 1 then
            { name = "Remove Argument"
            , func =
                Just <|
                  \() ->
                    CodeMotion.removeArg
                      (Utils.head_ pathedPatIds)
                      model.inputExp
            , reqs = makeReqs Satisfied
            }
          else if isAllArgumentSelected then
            { name = "Remove Arguments"
            , func =
                Just <|
                  \() ->
                    CodeMotion.removeArgs
                      pathedPatIds
                      model.inputExp
            , reqs = makeReqs Satisfied
            }
          else
            disabledTool
      (_, _, eids, [], [], [], []) ->
        case
          eids
            |> List.map
                 ( LangTools.eidToMaybeCorrespondingArgumentPathedPatId
                     model.inputExp
                 )
            |> Utils.projJusts
        of
          Just [argPathedPatId] ->
            { name = "Remove Argument"
            , func =
                Just <|
                  \() ->
                    CodeMotion.removeArg
                      argPathedPatId
                      model.inputExp
            , reqs = makeReqs Satisfied
            }
          Just argPathedPatIds ->
            { name = "Remove Arguments"
            , func =
                Just <|
                  \() ->
                    CodeMotion.removeArgs
                    argPathedPatIds
                    model.inputExp
            , reqs = makeReqs Satisfied
            }
          _ ->
            disabledTool
      _ ->
        disabledTool

--------------------------------------------------------------------------------
-- Reorder Arguments
--------------------------------------------------------------------------------

reorderArgumentsTool : Model -> Selections -> DeuceTool
reorderArgumentsTool model selections =
  let
    -- this helper helps avoid changing existing structure of this function
    makeReqs predVal =
      [ { description =
            """Select one or more function arguments (either at the definition
               or a call-site, and 1 target position in that list"""
        , value = predVal
        }
      ]

    func =
      case selections of
        (_, _, [], [], _, _, _) ->
          Nothing
        ([], [], [], pathedPatIds, [], [], [patTarget]) ->
          let
            targetPathedPatId =
              patTargetPositionToTargetPathedPatId patTarget
            allScopesSame =
              List.map pathedPatIdToScopeId (targetPathedPatId::pathedPatIds)
                |> Utils.allSame
          in
            case
              ( allScopesSame
              , ( findExpByEId
                    model.inputExp
                    (pathedPatIdToScopeEId targetPathedPatId)
                ) |> Maybe.map (.val >> .e__)
              )
            of
              (True, Just (EFun _ _ _ _)) ->
                Just <|
                  \() ->
                    CodeMotion.reorderFunctionArgs
                        (pathedPatIdToScopeEId targetPathedPatId)
                        (List.map pathedPatIdToPath pathedPatIds)
                        (pathedPatIdToPath targetPathedPatId)
                        model.inputExp
              _ ->
                Nothing
        (_, _, eids, [], [], [(beforeAfter, eid)], []) ->
          case
            (eid::eids)
              |> List.map
                   ( LangTools.eidToMaybeCorrespondingArgumentPathedPatId
                       model.inputExp
                   )
              |> Utils.projJusts
          of
            Just (targetReferencePathedPatId::pathedPatIds) ->
              let
                targetPathedPatId =
                  patTargetPositionToTargetPathedPatId
                    (beforeAfter, targetReferencePathedPatId)
                allScopesSame =
                  ( List.map
                      pathedPatIdToScopeId
                      (targetPathedPatId::pathedPatIds)
                  ) |> Utils.allSame
              in
                case
                  ( allScopesSame
                  , ( findExpByEId
                        model.inputExp
                        (pathedPatIdToScopeEId targetPathedPatId)
                    ) |> Maybe.map (.val >> .e__)
                  )
                of
                  (True, Just (EFun _ _ _ _)) ->
                    Just <|
                      \() ->
                        CodeMotion.reorderFunctionArgs
                            (pathedPatIdToScopeEId targetPathedPatId)
                            (List.map pathedPatIdToPath pathedPatIds)
                            (pathedPatIdToPath targetPathedPatId)
                            model.inputExp
                  _ ->
                    Nothing
            _ ->
              Nothing
        _ ->
          Nothing
  in
  { name = "Reorder Arguments"
  , func = func
  , reqs =
      case selections of
        ([], [], [], [], [], [], []) -> makeReqs Possible
        _ ->
          case func of
            Just _  -> makeReqs Satisfied
            Nothing -> makeReqs Impossible
  }

--------------------------------------------------------------------------------
-- Reorder List
--------------------------------------------------------------------------------

reorderListTool : Model -> Selections -> DeuceTool
reorderListTool model selections =
  { name = "Reorder List"
  , func =
      CodeMotion.reorderEListTransformation model selections
  , reqs = [] -- TODO reqs
  }

--------------------------------------------------------------------------------
-- Make Single Line
--------------------------------------------------------------------------------
-- TODO This function does not rely on CodeMotion, so it is probably doing too
--      much.
--------------------------------------------------------------------------------

makeSingleLineTool : Model -> Selections -> DeuceTool
makeSingleLineTool model selections =
  { name = "Make Single Line"
  , func =
      let
        maybeEIdToDeLineAndWhetherToPreservePrecedingWhitespace =
          case selections of
            (_, _, [eid], [], [], [], []) ->
              Just (eid, True)
            ([], [], [], [], [letEId], [], []) ->
              findExpByEId model.inputExp letEId
                |> Maybe.andThen LangTools.expToMaybeLetBoundExp
                |> Maybe.map (\letBoundExp -> (letBoundExp.val.eid, False))
            _ ->
              Nothing
      in
        case maybeEIdToDeLineAndWhetherToPreservePrecedingWhitespace of
          Nothing ->
            Nothing
          Just (eid, shouldPreservePrecedingWhitespace) ->
            let
              perhapsLeftTrimmer =
                if shouldPreservePrecedingWhitespace then
                  String.trimLeft
                else
                  identity
            in
              if
                LangTools.justFindExpByEId model.inputExp eid
                  |> LangUnparser.unparse
                  |> perhapsLeftTrimmer
                  |> String.contains "\n"
              then
                Just <|
                  \() ->
                    let
                      deLine ws =
                        if String.contains "\n" ws.val then
                          space1
                        else
                          ws
                      deLineP__ p__ =
                        case p__ of
                          PVar ws ident wd ->
                            PVar
                              (deLine ws)
                              ident
                              wd
                          PConst ws n ->
                            PConst
                              (deLine ws)
                              n
                          PBase ws v ->
                            PBase
                              (deLine ws)
                              v
                          PList ws1 ps ws2 rest ws3 ->
                            PList
                              (deLine ws1)
                              (setPatListWhitespace "" " " ps)
                              (deLine ws2)
                              rest
                              space0
                          PAs ws1 ident ws2 p ->
                            PAs
                              (deLine ws1)
                              ident
                              space1
                              p
                      deLinePat p =
                        mapPatTopDown (mapNodeP__ deLineP__) p
                      deLineE__ e__ =
                        case e__ of
                          EBase ws v ->
                            EBase (deLine ws) v
                          EConst ws n l wd ->
                            EConst (deLine ws) n l wd
                          EVar ws x ->
                            EVar (deLine ws) x
                          EFun ws1 ps e1 ws2 ->
                            EFun
                              (deLine ws1)
                              ( ps
                                  |> List.map deLinePat
                                  |> setPatListWhitespace "" " "
                              )
                              e1
                              space0
                          EApp ws1 e1 es ws2 ->
                            EApp
                              (deLine ws1)
                              (replacePrecedingWhitespace "" e1)
                              es
                              space0
                          EList ws1 es ws2 rest ws3 ->
                            EList
                              (deLine ws1)
                              (setExpListWhitespace "" " " es)
                              (deLine ws2)
                              rest
                              space0
                          EOp ws1 op es ws2 ->
                            EOp (deLine ws1) op es space0
                          EIf ws1 e1 e2 e3 ws2 ->
                            EIf (deLine ws1) e1 e2 e3 space0
                          ELet ws1 kind rec p e1 e2 ws2 ->
                            ELet (deLine ws1) kind rec p e1 e2 space0
                          ECase ws1 e1 bs ws2 ->
                            ECase (deLine ws1) e1 bs space0
                          ETypeCase ws1 pat bs ws2 ->
                            ETypeCase (deLine ws1) pat bs space0
                          EComment ws s e1 ->
                            EComment ws s e1
                          EOption ws1 s1 ws2 s2 e1 ->
                            EOption ws1 s1 space1 s2 e1
                          ETyp ws1 pat tipe e ws2 ->
                            ETyp (deLine ws1) pat tipe e space0
                          EColonType ws1 e ws2 tipe ws3 ->
                            EColonType (deLine ws1) e (deLine ws2) tipe space0
                          ETypeAlias ws1 pat tipe e ws2 ->
                            ETypeAlias (deLine ws1) pat tipe e space0
                      deLineExp e =
                        mapExp (mapNodeE__ deLineE__) e
                    in
                      model.inputExp
                        |> mapExpNode
                             eid
                             ( \e ->
                                 e
                                   |> deLineExp
                                   |> ( if
                                          shouldPreservePrecedingWhitespace
                                        then
                                          copyPrecedingWhitespace e
                                        else
                                          replacePrecedingWhitespace " "
                                      )
                             )
                        |> synthesisResult "Make Single Line"
                        |> Utils.singleton
              else
                Nothing
    , reqs =
        [ { description = "Select 1 expression or definition"
          , value =
              -- just duplicating this case from above
              case selections of
                ([], [], [], [], [], [], []) ->
                  Possible
                (_, _, [eid], [], [], [], []) ->
                  FullySatisfied
                ([], [], [], [], [letEId], [], []) ->
                  FullySatisfied
                _ ->
                  Impossible
           }
        ]
    }

--------------------------------------------------------------------------------
-- Make Multi-line
--------------------------------------------------------------------------------
-- TODO This function does not rely on CodeMotion, so it is probably doing too
--      much.
--------------------------------------------------------------------------------

makeMultiLineTool : Model -> Selections -> DeuceTool
makeMultiLineTool model selections =
  { name = "Make Multi-line"
  , func =
      case selections of
        (_, _, [eid], [], [], [], []) ->
          let
            exp =
              LangTools.justFindExpByEId model.inputExp eid
          in
            case exp.val.e__ of
              EList ws1 es ws2 Nothing ws3 ->
                if
                  es |>
                    List.all (precedingWhitespace >> String.contains "\n")
                then
                  Nothing
                else
                  Just <|
                    \() ->
                      let
                        indentation =
                          indentationAt eid model.inputExp
                      in
                        model.inputExp
                          |> replaceExpNodeE__ByEId
                               eid
                               ( EList
                                   ws1
                                   ( setExpListWhitespace
                                       ("\n" ++ indentation ++ "  ")
                                       ("\n" ++ indentation ++ "  ")
                                       es
                                   )
                                   ws2
                                   Nothing
                                   ( ws <| "\n" ++ indentation )
                               )
                          |> synthesisResult "Make Multi-line"
                          |> Utils.singleton
              EApp ws1 e es ws2 ->
                if
                  es |>
                    List.all (precedingWhitespace >> String.contains "\n")
                then
                  Nothing
                else
                  Just <|
                    \() ->
                      let
                        indentation =
                          String.repeat (e.end.col) " "
                      in
                        model.inputExp
                          |> replaceExpNodeE__ByEId
                               eid
                               ( EApp
                                   ws1
                                   e
                                   ( setExpListWhitespace
                                       " "
                                       ("\n" ++ indentation)
                                       es
                                   )
                                   space0
                               )
                          |> synthesisResult "Make Multi-line"
                          |> Utils.singleton
              _ ->
                Nothing
        _ ->
          Nothing
  , reqs =
      [ { description = "Select 1 expression"
        , value =
            -- just duplicating this case from above
            case selections of
              ([], [], [], [], [], [], []) ->
                Possible
              (_, _, [eid], [], [], [], []) ->
                FullySatisfied
              _ ->
                Impossible
         }
      ]
  }

--------------------------------------------------------------------------------
-- Align Expressions
--------------------------------------------------------------------------------
-- TODO This function does not rely on CodeMotion, so it is probably doing too
--      much.
--------------------------------------------------------------------------------

alignExpressionsTool : Model -> Selections -> DeuceTool
alignExpressionsTool model selections =
  { name = "Align Expressions"
  , func =
      case selections of
        (_, _, eid1::eid2::restEIds, [], [], [], []) ->
          let
            eids =
              eid1::eid2::restEIds
            exps =
              eids |>
                List.map (LangTools.justFindExpByEId model.inputExp)
            lineNums =
              exps |>
                List.map (.start >> .line)
          in
            if lineNums /= Utils.dedup lineNums then
              Nothing
            else
              Just <|
                \() ->
                  let
                    maxCol =
                      exps
                        |> List.map (.start >> .col)
                        |> List.maximum
                        |> Utils.fromJust_
                             "DeuceTools.alignExpressionsTool maxCol"
                  in
                    model.inputExp
                      |> mapExp
                          ( \e ->
                              if List.member e.val.eid eids then
                                let
                                  wsDelta =
                                    maxCol - e.start.col
                                in
                                  e |>
                                    pushRight (String.repeat wsDelta " ")
                              else
                                e
                          )
                      |> synthesisResult "Align Expressions"
                      |> Utils.singleton
        _ ->
          Nothing
  , reqs =
      [ { description = "Select 2 or more expressions"
        , value =
            -- just duplicating this case from above
            case selections of
              ([], [], [], [], [], [], []) ->
                Possible
              (_, _, eid1::eid2::restEIds, [], [], [], []) ->
                Satisfied
              _ ->
                Impossible
         }
      ]
  }

--==============================================================================
--= EXPORTS
--==============================================================================

--------------------------------------------------------------------------------
-- All Tools
--------------------------------------------------------------------------------

deuceTools : Model -> List (List DeuceTool)
deuceTools model =
  let
    {selectedWidgets} =
      model.deuceState
    nums =
      selectedNums model
    baseVals =
      selectedBaseVals model
    exps =
      selectedExps selectedWidgets
    pathedPatIds =
      selectedPats selectedWidgets
    letBindingEquations =
      selectedEquations selectedWidgets
    expTargets =
      selectedExpTargets selectedWidgets
    patTargets =
      selectedPatTargets selectedWidgets
    selections =
      ( nums
      , baseVals
      , exps
      , pathedPatIds
      , letBindingEquations
      , expTargets
      , patTargets
      )
  in
    List.map (List.map (\tool -> tool model selections))
      [ [ createFunctionTool
        , mergeTool
        ]
      , [ addArgumentsTool
        , removeArgumentsTool
        , reorderArgumentsTool
        ]
      , [ moveDefinitionTool
        , introduceVariableTool
        ]
      , [ eliminateCommonSubExpressionTool
        , renameVariableTool
        , renamePatternTool
        , swapNamesAndUsagesTool
        , inlineDefinitionTool
        , duplicateDefinitionTool
        ]
      , [ makeSingleLineTool
        , makeMultiLineTool
        , alignExpressionsTool
        ]
      , [ makeEqualTool
        , copyExpressionTool
        , reorderListTool
        , swapUsagesTool
        ]
      , [ thawFreezeTool
        , addRemoveRangeTool
        , showHideRangeTool
        , rewriteOffsetTool
        -- TODO
        -- , convertColorStringsTool
        , flipBooleanTool
        -- TODO ?
        -- , twiddleShapesTool
      ]
    ]

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

isRenamer : DeuceTool -> Bool
isRenamer =
  String.startsWith "Rename" << .name

isActive : DeuceTool -> Bool
isActive tool =
  not <| tool.func == Nothing

noneActive : Model -> Bool
noneActive model =
  model
    |> deuceTools
    |> List.concat
    |> List.filter isActive
    |> List.isEmpty
