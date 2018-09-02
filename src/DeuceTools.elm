------------------------------------------------------------------------------
-- This module provides "glue" code for the View to access the transformations
-- that CodeMotion (and friends) provides.
--------------------------------------------------------------------------------

module DeuceTools exposing
  ( createToolCache
  , createToolCacheMultipleInterpretations
  , reselectDeuceTool
  , updateRenameToolsInCache
  , isActive
  , isRenamer
  )

import String
import Dict

import Either exposing (..)
import Utils
import ImpureGoodies
import ColorNum

import InterfaceModel as Model exposing
  ( Model
  , oneSafeResult
  , setResultSafe
  , CachedDeuceTool
  )

import Lang exposing (..)
import LangTools
import Syntax
import Types2

import DeuceWidgets exposing
  ( DeuceWidget(..)
  )

import CodeMotion
import ExpressionBasedTransform

--------------------------------------------------------------------------------
-- Selections
--------------------------------------------------------------------------------

type alias Selections = Lang.DeuceSelections

selectedNumsAndBaseVals
    : Exp
   -> List DeuceWidget
   -> ( List (LocId, (WS, Num, Loc, WidgetDecl))
      , List (EId, (WS, EBaseVal))
      )
selectedNumsAndBaseVals program selectedWidgets =
  let noMatches = ([], []) in
  -- TODO may want to distinguish between different kinds of selected
  -- items earlier
  selectedWidgets
  |> List.map (\deuceWidget ->
       case deuceWidget of
         DeuceExp eid ->
           case findExpByEId program eid of
             Just ePlucked ->
               case (unwrapExp ePlucked) of
                 EConst ws n loc wd -> ([(eid, (ws, n, loc, wd))], [])
                 EBase ws baseVal   -> ([], [(eid, (ws, baseVal))])

                 _ -> noMatches
             _ -> noMatches
         _ -> noMatches
     )
  |> List.unzip
  |> (\(l1,l2) -> (List.concat l1, List.concat l2))

selectedNums : Exp -> List DeuceWidget -> List (LocId, (WS, Num, Loc, WidgetDecl))
selectedNums program selectedWidgets =
  selectedNumsAndBaseVals program selectedWidgets |> Tuple.first

selectedBaseVals : Exp -> List DeuceWidget -> List (EId, (WS, EBaseVal))
selectedBaseVals program selectedWidgets =
  selectedNumsAndBaseVals program selectedWidgets |> Tuple.second

selectedEIds : List DeuceWidget -> List EId
selectedEIds deuceWidgets =
  flip List.concatMap deuceWidgets <| \deuceWidget ->
    case deuceWidget of
      DeuceExp x -> [x]
      _ -> []

selectedPathedPatIds : List DeuceWidget -> List PathedPatternId
selectedPathedPatIds deuceWidgets =
  flip List.concatMap deuceWidgets <| \deuceWidget ->
    case deuceWidget of
      DeucePat x -> [x]
      _ -> []

selectedEquationEIds : List DeuceWidget -> List (EId, BindingNumber)
selectedEquationEIds deuceWidgets =
  flip List.concatMap deuceWidgets <| \deuceWidget ->
    case deuceWidget of
      DeuceLetBindingEquation x -> [x]
      _ -> []

selectedDeclTargets : List DeuceWidget -> List DeclarationTargetPosition
selectedDeclTargets deuceWidgets =
  flip List.concatMap deuceWidgets <| \deuceWidget ->
    case deuceWidget of
      DeuceDeclTarget x -> [x]
      _ -> []

selectedEIdTargets : List DeuceWidget -> List ExpTargetPosition
selectedEIdTargets deuceWidgets =
  flip List.concatMap deuceWidgets <| \deuceWidget ->
    case deuceWidget of
      DeuceExpTarget x -> [x]
      _ -> []

selectedPathedPatIdTargets : List DeuceWidget -> List PatTargetPosition
selectedPathedPatIdTargets deuceWidgets =
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
    (nums, [], exps, [], [], [], [], []) ->
      List.length nums >= 1 && List.length nums == List.length exps
    _ ->
      False

--------------------------------------------------------------------------------
-- Make Equal
--------------------------------------------------------------------------------

makeEqualTool : Model -> Selections -> DeuceTool
makeEqualTool model selections =
  -- TODO allow optional target position
  -- TODO define a helper to factor Introduce Var and this
  let
    (func, expsPredVal) =
      case selections of
        (_, _, [], [], [], _, _, _) -> (Nothing, Possible)
        (_, _, _, _::_, _, _, _, _) -> (Nothing, Impossible) -- no pattern selection allowed (yet)
        (_, _, _, _, _::_, _, _, _) -> (Nothing, Impossible) -- no equation selection allowed (yet?)
        (_, _, [_], _, _, _, _, _)  -> (Nothing, Possible)
        (_, _, eids, [], [], [], [], []) ->
          ( CodeMotion.makeEqualTransformation model.inputExp eids Nothing
          , Satisfied
          )
        (_, _, eids, [], [], [], [], [patTarget]) ->
          ( CodeMotion.makeEqualTransformation model.inputExp eids (Just (PatTargetPosition patTarget))
          , Satisfied
          )
        (_, _, eids, [], [], [], [expTarget], []) ->
          ( CodeMotion.makeEqualTransformation model.inputExp eids (Just (ExpTargetPosition expTarget))
          , Satisfied
          )
        _ ->
          (Nothing, Impossible)
  in
    { name = "Make Equal with Single Variable"
    , func = func
    , reqs =
        [ { description =
              "Select two or more expressions and, optionally, a target position (i.e. whitespace)."
          , value =
              expsPredVal
          }
        ]
    , id = "makeEqual"
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
        ([], [_], [eId], [], [], [], [], []) ->
          case findExpByEId model.inputExp eId of
            Just ePlucked ->
              case (unwrapExp ePlucked) of
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
        ([], [], [], [], [], [], [], []) ->
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
    , id = "flipBoolean"
    }

--------------------------------------------------------------------------------
-- Rename Variable at Definition
--------------------------------------------------------------------------------
-- Rename Variable At Use
--------------------------------------------------------------------------------

renameVariableTool : Model -> Selections -> DeuceTool
renameVariableTool model selections =
  let
    disabledName =
      "Rename Variable"
    nameMaker s =
      "Rename '" ++ s ++ "'"
    (name, func, predVal) =
      case selections of
        ([], [], [], [pathedPatId], [], [], [], []) ->
          case
            LangTools.findPatByPathedPatternId pathedPatId model.inputExp
              |> Maybe.andThen LangTools.patToMaybeIdent -- Rename tool only allowed for PVar and PAs
          of
            Just ident ->
              let
                newName =
                  model.deuceState.renameVarTextBox
              in
                ( nameMaker ident
                , Just <| \() -> CodeMotion.renamePat pathedPatId newName model.inputExp
                , FullySatisfied
                )
            _ ->
              (disabledName, Nothing, Impossible)
        ([], [], [eId], [], [], [], [], []) ->
          case findExpByEId model.inputExp eId of
            Just ePlucked ->
              case (unwrapExp ePlucked) of
                EVar _ ident ->
                  let
                     newName =
                      model.deuceState.renameVarTextBox
                  in
                    ( nameMaker ident
                    , Just <| \() -> CodeMotion.renameVar eId newName model.inputExp
                    , FullySatisfied
                    )
                _ -> (disabledName, Nothing, Impossible)
            _ -> (disabledName, Nothing, Impossible)
        ([], [], [], [], [], [], [], []) ->
          (disabledName, Nothing, Possible)
        _ ->
          (disabledName, Nothing, Impossible)
  in
    { name = name
    , func = func
    , reqs =
        [ { description =
              "Select a variable definition or use"
          , value =
              predVal
          }
        ]
    , id = "renameVariable"
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
  selectTwoVars
    "Swap Variable Names and Usages" "swapNamesAndUsages" makeThunk model selections

swapUsagesTool : Model -> Selections -> DeuceTool
swapUsagesTool model selections =
  let makeThunk (pathedPatId1, _) (pathedPatId2, _) () =
    CodeMotion.swapUsages pathedPatId1 pathedPatId2 model.inputExp
  in
  selectTwoVars
    "Swap Variable Usages" "swapUsages" makeThunk model selections

selectTwoVars toolName toolId makeThunk model selections =
  let
    (func, predVal) =
      case selections of
        ([], [], [], [], [], [], [], []) ->
          (Nothing, Possible)
        ([], [], [], [_], [], [], [], []) ->
          (Nothing, Possible) -- could check whether pathedPatId1 is a var
        ([], [], [], [pathedPatId1, pathedPatId2], [], [], [], []) ->
          let maybeNames =
            [pathedPatId1, pathedPatId2]
              |> List.map (\ppid -> LangTools.findPatByPathedPatternId ppid model.inputExp)
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
    , id = toolId
    }

--------------------------------------------------------------------------------
-- Swap Expressions
--------------------------------------------------------------------------------

swapExpressionsTool : Model -> Selections -> DeuceTool
swapExpressionsTool model selections =
  let
    (func, predVal) =
      case selections of
        (_, _, [], [], [], [], [], [])           -> (Nothing, Possible)
        (_, _, _, _::_, _, _, _, _)             -> (Nothing, Impossible) -- no pattern selection allowed (yet)
        (_, _, _, _, _::_, _, _, _)             -> (Nothing, Impossible) -- no equation selection allowed (yet?)
        (_, _, [_], _, _, [], [], [])            -> (Nothing, Possible)
        (_, _, [eid1, eid2], [], [], [], [], []) -> (CodeMotion.swapExpressionsTransformation model.syntax model.inputExp eid1 eid2, Satisfied)
        _                                    -> (Nothing, Impossible)
  in
    { name = "Swap Expressions"
    , func = func
    , reqs =
        [ { description =
              "Select two expressions."
          , value =
              predVal
          }
        ]
    , id = "swapExpressions"
    }


--------------------------------------------------------------------------------
-- Swap Definitions
--------------------------------------------------------------------------------

swapDefinitionsTool : Model -> Selections -> DeuceTool
swapDefinitionsTool model selections =
  let ppidIsInLet ppid =
    case LangTools.findScopeExpAndPatByPathedPatternId ppid model.inputExp of
       Just ((scopeExp, _), _) -> isLet scopeExp
       Nothing            -> False
  in
  let letEIdToTopPId letEId bn =
    LangTools.justFindExpByEId model.inputExp letEId |>
    LangTools.expToLetPat |> flip Utils.nth bn |> Utils.fromOk "DeuceTools.swapDefinitionsTool" |>
    .val |> .pid
  in
  let
    (func, predVal) =
      case selections of
        (_, _, [], [], [], [], [], [])                 -> (Nothing, Possible)
        (_, _, [], [ppid], [], [], [], [])             -> if ppidIsInLet ppid then (Nothing, Possible) else (Nothing, Impossible)
        (_, _, [], [], [letEId], [], [], [])           -> (Nothing, Possible)
        (_, _, [], [ppid1, ppid2], [], [], [], [])     -> (CodeMotion.swapDefinitionsTransformation model.syntax model.inputExp (LangTools.pathedPatternIdToPId ppid1 model.inputExp |> Utils.fromJust_ "CodeMotion.swapDefinitionsTool") (LangTools.pathedPatternIdToPId ppid2 model.inputExp |> Utils.fromJust_ "CodeMotion.swapDefinitionsTool"), Satisfied)
        (_, _, [], [], [(letEId1, bn1), (letEId2, bn2)], [], [], []) ->
                                                      (CodeMotion.swapDefinitionsTransformation model.syntax model.inputExp (letEIdToTopPId letEId1 bn1) (letEIdToTopPId letEId2 bn2), Satisfied)
        _                                          -> (Nothing, Impossible)
  in
    { name = "Swap Definitions"
    , func = func
    , reqs =
        [ { description =
              "Select two variable definitions."
          , value =
              predVal
          }
        ]
    , id = "swapDefinitions"
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
        ([], [], [], [], [], [], [], []) ->
          (toolName, Nothing, Possible)
        (_, _, _, [], [], _, _, _) ->
          (toolName, Nothing, Impossible)
        ([], [], [], pathedPatIds, [], [], [], []) ->
          ( Utils.perhapsPluralizeList toolName pathedPatIds
          , Just <| \() ->
              CodeMotion.inlineDefinitions model.syntax pathedPatIds model.inputExp
          , Satisfied
          )
        ([], [], [], [], letEIds, [], [], []) ->
          ( Utils.perhapsPluralizeList toolName letEIds
          , Just <| \() ->
              CodeMotion.inlineDefinitions
                model.syntax
                (letEIds |> List.map (\letEId -> (letEId, [])))
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
              "Select one or more variable definitions"
          , value =
              predVal
          }
        ]
    , id = "inlineDefinition"
    }

--------------------------------------------------------------------------------
-- Introduce Variable
--------------------------------------------------------------------------------

introduceVariableTool : Model -> Selections -> DeuceTool
introduceVariableTool model selections =
  -- TODO allow target position to be omitted
  let
    toolName = "Introduce Local Variable"

    (name, func, predVal) =
      case Debug.log "selections" selections of
        ([], [], [], [], [], [], [], []) ->
          (toolName, Nothing, Possible)
        (_, _, [], _, _, _, _, _) ->
          (toolName, Nothing, Impossible)
        (_, _, exps, [], [], [], [], []) ->
          ( Utils.perhapsPluralizeList "Introduce Variable" exps
          , CodeMotion.introduceVarTransformation
              model
              exps
              Nothing
          , Satisfied
          )
        (_, _, exps, [], [], [], [], [patTarget]) ->
          ( Utils.perhapsPluralizeList "Introduce Variable" exps
          , CodeMotion.introduceVarTransformation
              model
              exps
              (Just (PatTargetPosition patTarget))
          , Satisfied
          )
        (_, _, exps, [], [], [], [expTarget], []) ->
          ( Utils.perhapsPluralizeList "Introduce Variable" exps
          , CodeMotion.introduceVarTransformation
              model
              exps
              (Just (ExpTargetPosition expTarget))
          , Satisfied
          )
        (_, _, exps, [], [], [declTarget], [], []) ->
          ( Utils.perhapsPluralizeList "Introduce Variable" exps
          , CodeMotion.introduceVarTransformation
              model
              exps
              (Just (DeclarationTargetPosition declTarget))
          , Satisfied
          )
        (_, _, exps, [], [decl], [], [], []) ->
          ( Utils.perhapsPluralizeList "Introduce Variable" exps
          , CodeMotion.introduceVarTransformation
              model
              exps
              (Just (DeclarationTargetPosition (Before, decl)))
          , Satisfied
          )
        _ ->
          (toolName, Nothing, Impossible)
  in
    { name = name
    , func = func
    , reqs =
        [ { description =
              "Select one or more expressions and, optionally, one target position (i.e. whitespace)"
          , value =
              predVal
          }
        ]
    , id = "introduceVariable"
    }

--------------------------------------------------------------------------------
-- Copy Expression
--------------------------------------------------------------------------------

copyExpressionTool : Model -> Selections -> DeuceTool
copyExpressionTool model selections =
  let
    (func, predVal) =
      case selections of
        (_, _, [], [], [], [], [], [])   -> (Nothing, Possible)
        (_, _, _, _::_, _, _, _, _)     -> (Nothing, Impossible) -- no pattern selection allowed (yet)
        (_, _, _, _, _::_, _, _, _)     -> (Nothing, Impossible) -- no equation selection allowed (yet?)
        (_, _, [_], _, _,  [], [], [])    -> (Nothing, Possible)
        (_, _, eids, [], [], [], [], []) -> (CodeMotion.copyExpressionTransformation model.syntax model.inputExp eids, Satisfied)
        _                            -> (Nothing, Impossible)
  in
    { name = "Make Equal by Copying"
    , func = func
    , reqs =
        [ { description =
              "Select two or more expressions."
          , value =
              predVal
          }
        ]
    , id = "copyExpression"
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
        ([], [], [], [], [], [], [], []) ->
          (toolName, Nothing, Possible)
        ([], [], [], _::_, [], [], [], []) ->
          (toolName, Nothing, Possible)
        ([], [], [], [], _::_, [], [], []) ->
          (toolName, Nothing, Possible)
        ([], [], [], [], [], [], [_], []) ->
          (toolName, Nothing, Possible)
        ([], [], [], [], [], [], [], [_]) ->
          (toolName, Nothing, Possible)
        ([], [], [], firstPatId::restPatIds, [], [], [(Before, eId)], []) ->
          let pathedPatIds = firstPatId::restPatIds in
          ( Utils.perhapsPluralizeList toolName pathedPatIds
          , Just <| \() ->
              CodeMotion.moveDefinitionsBeforeEId
                model.syntax
                pathedPatIds
                eId
                model.inputExp
          , Satisfied
          )
        ([], [], [], firstPatId::restPatIds, [], [], [], [patTarget]) ->
          let pathedPatIds = firstPatId::restPatIds in
          let
            targetPathedPatId =
              patTargetPositionToTargetPathedPatId patTarget
          in
            case
              ( findExpByEId
                  model.inputExp
                  (pathedPatIdToScopeEId targetPathedPatId)
              ) |> Maybe.map unwrapExp
            of
              Just (ELet _ _ _ _ _) ->
                ( Utils.perhapsPluralizeList toolName pathedPatIds
                , Just <| \() ->
                    CodeMotion.moveDefinitionsPat
                      model.syntax
                      pathedPatIds
                      targetPathedPatId
                      model.inputExp
                , Satisfied
                )
              _ ->
                (toolName, Nothing, Impossible)
        ([], [], [], [], [letEIdBinding], [], [(Before, eId)], []) ->
          ( toolName
          , Just <| \() ->
              -- Better result names if we hand the singular case directly to
              -- moveDefinitionsBeforeEId.
              CodeMotion.moveDefinitionsBeforeEId
                model.syntax
                [(letEIdBinding, [])]
                eId
                model.inputExp
          , Satisfied
          )
        ([], [], [], [], letEIds, [], [(Before, eId)], []) ->
          ( Utils.perhapsPluralizeList toolName letEIds
          , Just <| \() ->
              CodeMotion.moveEquationsBeforeEId
                model.syntax
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
              "Select one or more variable definitions and one target position (i.e. whitespace)"
          , value =
              predVal
          }
        ]
    , id = "moveDefinition"
    }

--------------------------------------------------------------------------------
-- Duplicate Definition
--------------------------------------------------------------------------------

duplicateDefinitionTool : Model -> Selections -> DeuceTool
duplicateDefinitionTool model selections =
  let
    toolName = "Duplicate Definition"

    eidIsLet eid =
      (findExpByEId model.inputExp eid |> Maybe.map isLet) == Just True

    (name, func, predVal) =
      case selections of
        ([], [], [], [], [], [], [], []) ->
          (toolName, Nothing, Possible)
        (_, _, _, [], _, _, _, _) ->
          (toolName, Nothing, Impossible)
        ([], [], [], pathedPatIds, [], [], [(Before, eId)], []) ->
          let allAreLets = pathedPatIds |> List.all (pathedPatIdToScopeEId >> eidIsLet) in
          if allAreLets then
            ( Utils.perhapsPluralizeList toolName pathedPatIds
            , Just <| \() ->
                CodeMotion.duplicateDefinitionsBeforeEId
                  model.syntax
                  pathedPatIds
                  eId
                  model.inputExp
            , Satisfied
            )
          else
            (toolName, Nothing, Impossible)

        ([], [], [], pathedPatIds, [], [], [], [patTarget]) ->
          let
            targetPathedPatId =
              patTargetPositionToTargetPathedPatId patTarget

            allAreLets =
              (pathedPatIds |> List.all (pathedPatIdToScopeEId >> eidIsLet)) &&
              eidIsLet (pathedPatIdToScopeEId targetPathedPatId)
          in
          if allAreLets then
            ( Utils.perhapsPluralizeList toolName pathedPatIds
            , Just <| \() ->
                CodeMotion.duplicateDefinitionsPat
                  model.syntax
                  pathedPatIds
                  targetPathedPatId
                  model.inputExp
            , Satisfied
            )
          else
            (toolName, Nothing, Impossible)
        _ ->
          (toolName, Nothing, Impossible)
  in
    { name = toolName
    , func = func
    , reqs =
        [ { description =
              "Select one or more patterns and one target position (i.e. whitespace)"
          , value =
              predVal
          }
        ]
    , id = "duplicateDefinition"
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
    (nums, _, _, _, _, _, _, _) =
      selections

    (mode, predVal) =
      let
        freezeAnnotations =
          List.map (\(_,(_,_,(_,frzn,_),_)) -> frzn) nums
      in
        if selections == ([], [], [], [], [], [], [], []) then
          (Nothing, Possible)
        else if not (oneOrMoreNumsOnly selections) then
          (Nothing, Impossible)
        else
          let
            mode_ =
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
          in
            case mode_ of
              Just _ ->
                (mode_, Satisfied)
              Nothing ->
                (mode_, Impossible)

    (name, func) =
      case mode of
        Nothing ->
          ("Thaw/Freeze", Nothing)
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
          )
  in
    { name = name
    , func = func
    , reqs = [ { description = "Select one or more numbers", value = predVal } ]
    , id = "thawFreeze"
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
    (nums, _, _, _, _, _, _, _) =
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

    predVal =
      if selections == ([], [], [], [], [], [], [], []) then
        Possible
      else if mode /= Nothing then
        Satisfied
      else
        Impossible

    (name, func) =
      case mode of
        Nothing ->
          ( "Show/Hide Sliders"
          , Nothing
          )
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
            )
  in
    { name = name
    , func = func
    , reqs = [ { description = "Select one or more numbers with sliders", value = predVal } ]
    , id = "showHideRange"
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
    (nums, _, _, _, _, _, _, _) =
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
        Utils.dedup freezeAnnotations
        |> Utils.maybeUnpackSingleton

    predVal =
      if selections == ([], [], [], [], [], [], [], []) then
        Possible
      else if oneOrMoreNumsOnly selections then
        Satisfied
      else
        Impossible

    (name, func) =
      case mode of
        Nothing ->
          ( "Add/Remove Sliders"
          , Nothing
          )
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
            )
  in
    { name = name
    , func = func
    , reqs = [ { description = "Select one or more numbers", value = predVal } ]
    , id = "addRemoveRange"
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
        ([], [], [], [], [], [], [], []) ->
          (toolName, Nothing, Possible)
        ([], _, _, _, _, _, _, _) ->
          (toolName, Nothing, Impossible)
        (nums, [], exps, [ppid], [], [], [], []) ->
          case findExpByEId model.inputExp (pathedPatIdToScopeEId ppid) of
            Just scopeExp ->
              if isLet scopeExp && List.length nums == List.length exps then
                ( Utils.perhapsPluralizeList toolName nums
                , CodeMotion.rewriteOffsetTransformation model ppid nums
                , Satisfied
                )
              else
                (toolName, Nothing, Impossible)

            _ ->
              (toolName, Nothing, Impossible)

        _ ->
          (toolName, Nothing, Impossible)
  in
    { name = name
    , func = func
    , reqs =
        [ { description = "Select one variable and one or more numbers"
          , value = predVal
          }
        ]
    , id = "rewriteOffset"
    }

--------------------------------------------------------------------------------
-- Convert Color String
--------------------------------------------------------------------------------
-- TODO This function does not rely on CodeMotion, so it is probably doing too
--      much.
--------------------------------------------------------------------------------

convertColorStringTool : Model -> Selections -> DeuceTool
convertColorStringTool model selections =
  let
    baseName =
      "Convert Color String"

    impossible =
      ( baseName
      , Nothing
      , Impossible
      )

    (name, func, predVal) =
      case selections of
        ([], [], [], [], [], [], [], []) ->
          ( baseName
          , Nothing
          , Possible
          )
        ([], literals, exps, [], [], [], [], []) ->
          let
            expCount =
              List.length exps
            literalCount =
              List.length literals
          in
            if expCount /= literalCount then
              impossible
            else
              let
                idStringPairs =
                  List.filterMap
                    ( \(eid, (_, baseVal)) ->
                        case baseVal of
                          EString _ string ->
                            Just (eid, string)
                          _ ->
                            Nothing
                    )
                    literals
                idStringPairCount =
                  List.length idStringPairs
              in
                if literalCount /= idStringPairCount then
                  impossible
                else
                  let
                    convertedStrings =
                      List.filterMap
                        ColorNum.convertStringToRgbAndHue
                        idStringPairs
                    convertedStringCount =
                      List.length convertedStrings
                  in
                    if idStringPairCount /= convertedStringCount then
                      impossible
                    else
                      let
                        (newExp1, newExp2) =
                          List.foldl
                            ( \(eid, (r, g, b), hue) (acc1, acc2) ->
                                let
                                  replaceString =
                                    replaceExpNodePreservingPrecedingWhitespace
                                      eid
                                  eRgba =
                                    eList (listOfNums [r, g, b, 1.0]) Nothing
                                  eColorNum =
                                    eConst hue dummyLoc
                                in
                                  ( replaceString eRgba acc1
                                  , replaceString eColorNum acc2
                                  )
                            )
                            (model.inputExp, model.inputExp)
                            convertedStrings
                      in
                        ( Utils.perhapsPluralizeList baseName literals
                        , Just <|
                            \() ->
                              [ newExp1
                                  |> synthesisResult "RGBA"
                              , newExp2
                                  |> synthesisResult "Color Number (Hue Only)"
                                  |> setResultSafe False
                              ]
                        , Satisfied
                        )
        _ ->
          impossible
  in
    { name = name
    , func = func
    , reqs =
        [ { description = "Select one or more color strings"
          , value = predVal
          }
        ]
    , id = "convertColorString"
    }
--------------------------------------------------------------------------------
-- Create Function
--------------------------------------------------------------------------------

createFunctionTool : Model -> Selections -> DeuceTool
createFunctionTool model selections =
  let
    (func, predVal) =
      case selections of
        ([], [], [], [], [], [], [], []) ->
          (Nothing, Possible)
        ([], [], [], [pathedPatId], [], [], [], []) ->
          case
            LangTools.findScopeExpAndPatByPathedPatternId pathedPatId model.inputExp
              |> Maybe.map (\((e, id), p) -> (unwrapExp e, id, p.val.p__))
          of
            Just (ELet _ _ _ _ _, id, PVar _ ident _) ->
              ( Just <| \() ->
                  CodeMotion.abstractPVar model.syntax pathedPatId [] model.inputExp
              , FullySatisfied
              )
            _ ->
              (Nothing, Impossible)
        (_, _, [eid], [], [], [], [], []) ->
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
              ( Just <| \() -> CodeMotion.abstractExp model.syntax eid model.inputExp
              , FullySatisfied
              )
            else
              (Nothing, Impossible)
        ([], [], [], [], [(letEId, bindingNum)], [], [], []) ->
          case
            LangTools.justFindExpByEId model.inputExp letEId
              |> LangTools.expToMaybeLetPat
              |> Maybe.andThen (
                flip Utils.nth bindingNum >>
                Result.toMaybe)
              |> Maybe.map (.val >> .p__)
          of
            Just (PVar _ _ _) ->
              ( Just <|
                  \() ->
                    let
                       pathedPatId = ((letEId, 1), [])
                    in
                      CodeMotion.abstractPVar model.syntax pathedPatId [] model.inputExp
              , FullySatisfied
              )
            _ ->
              (Nothing, Impossible)
        _ ->
          (Nothing, Impossible)
  in
    { name = "Create Function from Definition"
    , func = func
    , reqs =
        [ { description = "Select one variable definition or expression"
          , value = predVal
          }
        ]
    , id = "createFunction"
    }


createFunctionFromArgsTool : Model -> Selections -> DeuceTool
createFunctionFromArgsTool model selections =
  let
    (func, predVal) =
      case selections of
        ([], [], [], [], [], [], [], []) ->
          (Nothing, Possible)
        (_, _, eids, ppids, [], [], [], []) ->
          case ppids |> List.map (\ppid -> LangTools.findBoundExpByPathedPatternId ppid model.inputExp) |> Utils.projJusts of
            Just boundExps ->
              let argEIds = Utils.dedup <| eids ++ List.map expEId boundExps in
              let enclosingPPIds =
                let ancestors = commonAncestors (\e -> List.member (expEId e) argEIds) model.inputExp in
                let ancestorEIds = ancestors |> List.map expEId in
                ancestors
                |> List.concatMap (\e -> case unwrapExp e of
                   ELet _ _ (Declarations _ _ _ letexps) _ _ ->
                     elemsOf letexps |> List.filter (\(LetExp _ _ p _ _ _ ) ->
                       p |> LangTools.patToMaybePVarIdent |> (/=) (Just "main")
                     ) |> List.map ((,) e)
                   _ -> [])
                |> List.concatMap
                    (\(letExp, LetExp _ _ p _ _ e1) ->
                      LangTools.tryMatchExpPatToPaths p e1
                      |> List.filter (\(path, boundExp) -> not (isFunc boundExp))
                      |> List.filter (\(path, boundExp) -> List.member (expEId boundExp) ancestorEIds) -- Incidentally, this also filters out trivial abstractions (e.g. (let x 5) -> (let x (\n -> n))) b/c boundExp must be ancestor of an arg, not an arg itself.
                      |> List.map    (\(path, boundExp) -> (((expEId letExp), 1), path))
                    )
              in
              case enclosingPPIds of
                [] ->
                  (Nothing, Impossible)

                _ ->
                  ( Just (\() -> enclosingPPIds |> List.concatMap (\ppid -> CodeMotion.abstractPVar model.syntax ppid argEIds model.inputExp))
                  , Satisfied
                  )

            _ ->
              (Nothing, Impossible)

        _ ->
          (Nothing, Impossible)
  in
    { name = "Create Function from Arguments"
    , func = func
    , reqs =
        [ { description = "Select expressions or patterns to become arguments to a new function."
          , value = predVal
          }
        ]
    , id = "createFunctionFromArguments"
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
              List.member (expEId e) eids
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
        ([], [], [], [], [], [], [], []) ->
          (Nothing, Possible)

        ([], [], [_], [], [], [], [], []) ->
          (Nothing, Possible)

        ([], [], [], [], [_], [], [], []) ->
          (Nothing, Possible)

        (_, _, [], [], ((letEId1, bind1)::(letEId2, bind2)::restLetEIds) as list, [], [], []) ->
          let boundExpEIds =
            list |> List.map (\(eid, bind) ->
               LangTools.justFindExpByEId model.inputExp eid |>
               LangTools.expToLetBoundExp |>
               flip Utils.nth bind |>
               Utils.fromOk "DeuceTools: bind should have been a integer in range !" |>
               expEId)
          in
          tryMerge boundExpEIds

        (_, _, eid1::eid2::restEIds, [], [], [], [], []) ->
          tryMerge (eid1::eid2::restEIds)

        _ ->
          (Nothing, Impossible)
  in
    { name = "Create Function by Merging Definitions"
    , func = func
    , reqs =
        [ { description = "Select two or more expressions"
          , value = predVal
          }
        ]
    , id = "merge"
    }

--------------------------------------------------------------------------------
-- Add Arguments
--------------------------------------------------------------------------------

addArgumentsTool : Model -> Selections -> DeuceTool
addArgumentsTool model selections =
  let
    toolName = "Add Argument"
    id = "addArguments"

    -- this helper helps avoid changing existing structure of this function
    makeReqs predVal =
      [ { description =
            """Select one or more expressions in a function and, optionally,
               one target position (i.e. whitespace) in the function's argument list"""
        , value = predVal
        }
      ]

    defaultTool predVal =
      { name = toolName
      , func = Nothing
      , reqs = makeReqs predVal
      , id = id
      }

    disabledTool = defaultTool Impossible
  in
    case selections of
      (_, _, [], [], [], [], [], []) ->
        defaultTool Possible

      (_, _, [], [], [], [], [], [patTarget]) ->
        defaultTool Possible

      (_, _, firstEId::restEIds, [], [], [], [], patTargets) ->
        let eids = firstEId::restEIds in
        let enclosingFuncs =
          commonAncestors (\e -> List.member (expEId e) eids) model.inputExp
          |> List.filter isFunc
        in
        -- Is each target in an arg list? (Filtered to only zero or one target below in targetPPIdsToTry.)
        let targetPPIdsWithValidity =
          patTargets
          |> List.map patTargetPositionToTargetPathedPatId
          |> List.map
              (\targetPPId ->
                ( targetPPId
                , enclosingFuncs |> List.any (eidIs (pathedPatIdToScopeEId targetPPId))
                )
              )
        in
        let targetPPIdsToTry =
          let funcExpToArgPPId funcExp =
            ( ((expEId funcExp), 1)
            , [ 1 + List.length (LangTools.expToFuncPats funcExp) ] -- By default, insert argument at the end
            )
          in
          case targetPPIdsWithValidity of
            []                   -> enclosingFuncs |> List.map funcExpToArgPPId
            [(targetPPId, True)] -> [targetPPId]
            _                    -> []
        in
        case (eids, targetPPIdsToTry) of
          ([eid], _::_) ->
            { name = "Add Argument" -- Fowler calls this "Add Parameter"
            , func =
                Just <|
                  \() ->
                    targetPPIdsToTry
                    |> List.concatMap (\targetPPId -> CodeMotion.addArg model.syntax eid targetPPId model.inputExp)
            , reqs = makeReqs Satisfied
            , id = id
            }

          (_::_::_, _::_) ->
            { name = "Add Arguments" -- Fowler calls this "Add Parameter"
            , func =
                Just <|
                  \() ->
                    targetPPIdsToTry
                    |> List.concatMap (\targetPPId -> CodeMotion.addArgs model.syntax eids targetPPId model.inputExp)
            , reqs = makeReqs Satisfied
            , id = id
            }

          _ ->
            disabledTool

      ( _, _, [], firstArgSourcePathedPatId::restArgSourcePathedPatId, [], [], [], patTargets) ->
        let argSourcePathedPatIds = firstArgSourcePathedPatId::restArgSourcePathedPatId in
        let argSourceScopeEIds = argSourcePathedPatIds |> List.map pathedPatIdToScopeEId in
        let areSourcesAllLets = argSourceScopeEIds |> List.all (findExpByEId model.inputExp >> Maybe.map isLet >> (==) (Just True)) in
        let enclosingFuncs =
          commonAncestors (\e -> List.member (expEId e) argSourceScopeEIds) model.inputExp
          |> List.filter isFunc
        in
        -- Is each target in an arg list? (Filtered to only zero or one target below in targetPPIdsToTry.)
        let targetPPIdsWithValidity =
          patTargets
          |> List.map patTargetPositionToTargetPathedPatId
          |> List.map
              (\targetPPId ->
                ( targetPPId
                , enclosingFuncs |> List.any (eidIs (pathedPatIdToScopeEId targetPPId))
                )
              )
        in
        let targetPPIdsToTry =
          let funcExpToArgPPId funcExp =
            ( ((expEId funcExp), 1)
            , [ 1 + List.length (LangTools.expToFuncPats funcExp) ] -- By default, insert argument at the end
            )
          in
          case targetPPIdsWithValidity of
            []                   -> enclosingFuncs |> List.map funcExpToArgPPId
            [(targetPPId, True)] -> [targetPPId]
            _                    -> []
        in
        case (argSourcePathedPatIds, targetPPIdsToTry, areSourcesAllLets) of
          ([argSourcePathedPatId], _::_, True) ->
            { name = "Add Argument" -- Fowler calls this "Add Parameter"
            , func =
                Just <|
                  \() ->
                    targetPPIdsToTry
                    |> List.concatMap (\targetPPId -> CodeMotion.addArgFromPat model.syntax argSourcePathedPatId targetPPId model.inputExp)
            , reqs = makeReqs Satisfied
            , id = id
            }

          (_::_::_, _::_, True) ->
            { name = "Add Arguments" -- Fowler calls this "Add Parameter"
            , func =
                Just <|
                  \() ->
                    targetPPIdsToTry
                    |> List.concatMap (\targetPPId -> CodeMotion.addArgsFromPats model.syntax argSourcePathedPatIds targetPPId model.inputExp)
            , reqs = makeReqs Satisfied
            , id = id
            }

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
    id = "removeArguments"

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
      , id = id
      }

    disabledTool = defaultTool Impossible
  in
    case selections of
      ([], [], [], [], [], [], [], []) ->
        defaultTool Possible
      (_, _, [], [], _, _, _, _) ->
        disabledTool
      ([], [], [], pathedPatIds, [], [], [], []) ->
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
                             |> Maybe.map unwrapExp
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
                      model.syntax
                      (Utils.head_ pathedPatIds)
                      model.inputExp
            , reqs = makeReqs Satisfied
            , id = id
            }
          else if isAllArgumentSelected then
            { name = "Remove Arguments"
            , func =
                Just <|
                  \() ->
                    CodeMotion.removeArgs
                      model.syntax
                      pathedPatIds
                      model.inputExp
            , reqs = makeReqs Satisfied
            , id = id
            }
          else
            disabledTool
      (_, _, eids, [], [], [], [], []) ->
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
                      model.syntax
                      argPathedPatId
                      model.inputExp
            , reqs = makeReqs Satisfied
            , id = id
            }
          Just argPathedPatIds ->
            { name = "Remove Arguments"
            , func =
                Just <|
                  \() ->
                    CodeMotion.removeArgs
                      model.syntax
                      argPathedPatIds
                      model.inputExp
            , reqs = makeReqs Satisfied
            , id = id
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
               or a call-site) and one target position (i.e. whitespace) in that list"""
        , value = predVal
        }
      ]

    (func, possibility) =
      case selections of
        (_, _, [], [], [], [], [], []) ->
          (Nothing, Possible)

        (_, _, [], pathedPatIds, [], [], [], []) ->
          let scopeEIds = List.map pathedPatIdToScopeEId pathedPatIds in
          if Utils.allSame scopeEIds && (scopeEIds |> List.all (findExpByEId model.inputExp >> Maybe.map isFunc >> (==) (Just True)))
          then (Nothing, Possible)
          else (Nothing, Impossible)

        (_, _, eids, [], [], [], [], []) ->
          case eids |> List.map (LangTools.eidToMaybeCorrespondingArgumentPathedPatId model.inputExp) |> Utils.projJusts of
            Just _  -> (Nothing, Possible)
            Nothing -> (Nothing, Impossible)

        ([], [], [], pathedPatIds, [], [], [], [patTarget]) ->
          let targetPathedPatId = patTargetPositionToTargetPathedPatId patTarget in
          let scopeIds          = List.map pathedPatIdToScopeId (targetPathedPatId::pathedPatIds) in
          let targetScopeEId    = pathedPatIdToScopeEId targetPathedPatId in
          case (Utils.allSame scopeIds, targetScopeEId |> findExpByEId model.inputExp |> Maybe.map unwrapExp) of
            (True, Just (EFun _ _ _ _)) ->
              ( Just <|
                  \() ->
                    CodeMotion.reorderFunctionArgs
                        targetScopeEId
                        (List.map pathedPatIdToPath pathedPatIds)
                        (pathedPatIdToPath targetPathedPatId)
                        model.inputExp

              , Satisfied
              )

            _ ->
                (Nothing, Impossible)

        (_, _, eids, [], [], [], [(beforeAfter, eid)], []) ->
          case eid::eids |> List.map (LangTools.eidToMaybeCorrespondingArgumentPathedPatId model.inputExp) |> Utils.projJusts of
            Just (targetReferencePathedPatId::pathedPatIds) ->
              let targetPathedPatId = patTargetPositionToTargetPathedPatId (beforeAfter, targetReferencePathedPatId) in
              let scopeIds = List.map pathedPatIdToScopeId (targetPathedPatId::pathedPatIds) in
              let targetEId = pathedPatIdToScopeEId targetPathedPatId in
              case (Utils.allSame scopeIds, targetEId |> findExpByEId model.inputExp |> Maybe.map unwrapExp) of
                (True, Just (EFun _ _ _ _)) ->
                  ( Just <|
                      \() ->
                        CodeMotion.reorderFunctionArgs
                            targetEId
                            (List.map pathedPatIdToPath pathedPatIds)
                            (pathedPatIdToPath targetPathedPatId)
                            model.inputExp
                  , Satisfied
                  )
                _ ->
                (Nothing, Impossible)
            _ ->
              (Nothing, Impossible)
        _ ->
          (Nothing, Impossible)
  in
  { name = "Reorder Arguments"
  , func = func
  , reqs = makeReqs possibility
  , id = "reorderArguments"
  }

--------------------------------------------------------------------------------
-- Reorder List
--------------------------------------------------------------------------------

reorderExpressionsTool : Model -> Selections -> DeuceTool
reorderExpressionsTool model selections =
  { name = "Reorder Expressions"
  , func = CodeMotion.reorderExpressionsTransformation model.inputExp selections
  , reqs = [] -- TODO reqs
  , id = "reorderExpressions"
  }

--------------------------------------------------------------------------------
-- Make Single Line
--------------------------------------------------------------------------------
-- TODO This function does not rely on CodeMotion, so it is probably doing too
--      much.
--------------------------------------------------------------------------------

-- TODO make tool this Inactive when the resulting line would be too long

makeSingleLineTool : Model -> Selections -> DeuceTool
makeSingleLineTool model selections =
  { name = "Make Single Line"
  , func =
      let
        maybeEIdToDeLineAndWhetherToPreservePrecedingWhitespace =
          case selections of
            (_, _, [eid], [], [], [], [], []) ->
              Just (eid, True)
            ([], [], [], [], [(letEId, bindingNum)], [], [], []) ->
              findExpByEId model.inputExp letEId
                |> Maybe.andThen (\e -> findLetexpByBindingNumber e bindingNum)
                |> Maybe.map bindingOfLetExp
                |> Maybe.map (\letBoundExp -> (expEId letBoundExp, False))
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
                  |> Syntax.unparser model.syntax
                  |> perhapsLeftTrimmer
                  |> String.contains "\n"
              then
                Just <|
                  \() ->
                    let
                      deLineDecls ((Declarations po tps anns lex) as decls) =
                        -- Make sure the first definition does not have a comma.
                        case po of
                          [] -> decls
                          noCommaIndex::_ ->
                            let annsOffset = List.length tps in
                            let lexOffset = annsOffset + List.length anns in
                            let commaIfNotFirst index = if index == noCommaIndex then Nothing else Just (ws "") in
                            let tps2 = List.indexedMap (\i (LetType spComma spAlias spP pat fs wsEq e) ->
                                 LetType (commaIfNotFirst i) (deLine spAlias) (Maybe.map deLine spP) (deLinePat pat) fs (deLine wsEq) e
                               ) (elemsOf tps) in
                            let anns2 = List.indexedMap (\i (LetAnnotation spComma spP pat fs wsEq e) ->
                                  LetAnnotation (commaIfNotFirst (i + annsOffset)) (deLine spP) (deLinePat pat) fs (deLine wsEq) e
                               ) anns in
                            let lex2 = List.indexedMap (\i (LetExp spComma spP pat fs wsEq e) ->
                                  LetExp (commaIfNotFirst (i + lexOffset)) (deLine spP) (deLinePat pat) fs (deLine wsEq) e
                               ) (elemsOf lex) in
                            Declarations po (regroup tps tps2) anns2 (regroup lex lex2)
                      deLine ws =
                        if String.contains "\n" ws.val then
                          space1
                        else
                          ws
                      deLineP__ p__ =
                        case p__ of
                          PWildcard ws ->
                            PWildcard
                              (deLine ws)
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
                          PRecord ws1 ps ws2 ->
                            PRecord
                              (deLine ws1)
                              (Utils.recordValuesMake ps (setPatListWhitespace "" " " (Utils.recordValues ps)))
                              (deLine ws2)
                          PAs ws1 p1 ws2 p2 ->
                            PAs
                              (deLine ws1)
                              p1
                              space1
                              p2
                          PParens ws1 p ws2 ->
                            PParens
                              (deLine ws1)
                              p
                              ws2
                          PColonType ws1 p ws2 t ->
                            PColonType (deLine ws1) p (deLine ws2) t
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
                          EApp ws1 e1 es appType ws2 ->
                            EApp
                              (deLine ws1)
                              (replacePrecedingWhitespace "" e1)
                              es
                              appType
                              space0
                          EList ws1 es ws2 rest ws3 ->
                            EList
                              (deLine ws1)
                              (Utils.listValuesMake es (setExpListWhitespace "" " " (Utils.listValues es)))
                              (deLine ws2)
                              rest
                              space0
                          ERecord ws1 mb decls ws2 ->
                            ERecord
                              (deLine ws1)
                              mb
                              (deLineDecls decls)
                              (deLine ws2)
                          ESelect ws0 e1 ws1 ws2 n ->
                            ESelect (deLine ws0) e1 (deLine ws1) (deLine ws2) n
                          EOp ws1 wso op es ws2 ->
                            EOp (deLine ws1) (deLine wso) op es space0
                          EIf ws1 e1 ws2 e2 ws3 e3 ws4 ->
                            EIf (deLine ws1) e1 ws2 e2 ws3 e3 space0
                          ELet ws1 kind decls ws2 e2 ->
                            ELet (deLine ws1) kind (deLineDecls decls) (deLine ws2) e2
                          ECase ws1 e1 bs ws2 ->
                            ECase (deLine ws1) e1 bs space0
                          EColonType ws1 e ws2 tipe ws3 ->
                            EColonType (deLine ws1) e (deLine ws2) tipe space0
                          EParens ws1 e pStyle ws2 ->
                            EParens (deLine ws1) e pStyle (deLine ws2)
                          EHole ws h ->
                            EHole (deLine ws) h
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
                        |> List.singleton
              else
                Nothing
    , reqs =
        [ { description = "Select one expression or definition"
          , value =
              -- just duplicating this case from above
              case selections of
                ([], [], [], [], [], [], [], []) ->
                  Possible
                (_, _, [eid], [], [], [], [], []) ->
                  FullySatisfied
                ([], [], [], [], [letEId], [], [], []) ->
                  FullySatisfied
                _ ->
                  Impossible
           }
        ]
    , id = "makeSingleLine"
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
        (_, _, [eid], [], [], [], [], []) ->
          let
            exp =
              LangTools.justFindExpByEId model.inputExp eid
          in
            case (unwrapExp exp) of
              EList ws1 es ws2 Nothing ws3 ->
                if
                  Utils.listValues es |>
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
                                   (Utils.zip
                                      (List.map Tuple.first es)
                                      ( setExpListWhitespace
                                          ("\n" ++ indentation ++ "  ")
                                          ("\n" ++ indentation ++ "  ")
                                          (Utils.listValues es)
                                      )
                                   )
                                   ws2
                                   Nothing
                                   ( ws <| "\n" ++ indentation )
                               )
                          |> synthesisResult "Make Multi-line"
                          |> List.singleton
              EApp ws1 (Expr e) es appType ws2 ->
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
                                   (Expr e)
                                   ( setExpListWhitespace
                                       " "
                                       ("\n" ++ indentation)
                                       es
                                   )
                                   appType
                                   space0
                               )
                          |> synthesisResult "Make Multi-line"
                          |> List.singleton
              _ ->
                Nothing
        _ ->
          Nothing
  , reqs =
      [ { description = "Select one expression"
        , value =
            -- just duplicating this case from above
            case selections of
              ([], [], [], [], [], [], [], []) ->
                Possible
              (_, _, [eid], [], [], [], [], []) ->
                FullySatisfied
              _ ->
                Impossible
         }
      ]
  , id = "makeMultiLine"
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
        (_, _, eid1::eid2::restEIds, [], [], [], [], []) ->
          let
            eids =
              eid1::eid2::restEIds
            exps =
              eids |>
                List.map (LangTools.justFindExpByEId model.inputExp)
            lineNums =
              exps |>
                List.map (\(Expr e) -> e.start.line)
          in
            if lineNums /= Utils.dedup lineNums then
              Nothing
            else
              Just <|
                \() ->
                  let
                    maxCol =
                      exps
                        |> List.map (\(Expr e) -> e.start.col)
                        |> List.maximum
                        |> Utils.fromJust_
                             "DeuceTools.alignExpressionsTool maxCol"
                  in
                    model.inputExp
                      |> mapExp
                          ( \(Expr e_) ->
                              let e = Expr e_ in
                              if List.member (expEId e) eids then
                                let
                                  wsDelta =
                                    maxCol - e_.start.col
                                in
                                  e |>
                                    pushRight (String.repeat wsDelta " ")
                              else
                                e
                          )
                      |> synthesisResult "Align Expressions"
                      |> List.singleton
        _ ->
          Nothing
  , reqs =
      [ { description = "Select two or more expressions"
        , value =
            -- just duplicating this case from above
            case selections of
              ([], [], [], [], [], [], [], []) ->
                Possible
              (_, _, eid1::eid2::restEIds, [], [], [], [], []) ->
                Satisfied
              _ ->
                Impossible
         }
      ]
  , id = "alignExpressions"
  }


--------------------------------------------------------------------------------
-- Type System Tool
--------------------------------------------------------------------------------

typesTool : Model -> DeuceSelections -> DeuceTool
typesTool model selections =
  let
    (func, boolPredVal) =
      case selections of
        ([], [], [], [], [], [], [], []) ->
          (Nothing, Possible)

        (_, _, [eId], [], [], [], [], []) ->
          let
            exp = LangTools.justFindExpByEId model.inputExp eId
          in
          (Just (Types2.makeDeuceExpTool model.inputExp exp), Satisfied)

        (_, _, _, [pathedPatId], [], [], [], []) ->
          let
            pat =
              LangTools.findPatByPathedPatternId pathedPatId model.inputExp
                |> Utils.fromJust_ "typesTool findPat"
          in
          (Just (Types2.makeDeucePatTool model.inputExp pat), Satisfied)

        _ ->
          (Nothing, Impossible)
  in
    { name = "Type Information"
    , func = func
    , reqs = [ { description = "Select something.", value = boolPredVal } ]
    , id = "typeInfo"
    }


--==============================================================================
--= EXPORTS
--==============================================================================

--------------------------------------------------------------------------------
-- All Tools
--------------------------------------------------------------------------------


selectionsTuple : Exp -> List DeuceWidget -> Selections
selectionsTuple program selectedWidgets =
  ( selectedNums program selectedWidgets
  , selectedBaseVals program selectedWidgets
  , selectedEIds selectedWidgets
  , selectedPathedPatIds selectedWidgets
  , selectedEquationEIds selectedWidgets
  , selectedDeclTargets selectedWidgets
  , selectedEIdTargets selectedWidgets
  , selectedPathedPatIdTargets selectedWidgets
  )

toolList =
  [ [ typesTool
    ]
  ]
-- TODO: get Deuce tools to work with the ELet AST
{-
  [ [ createFunctionTool
    , createFunctionFromArgsTool
    , mergeTool
    ]
  , [ addArgumentsTool
    , removeArgumentsTool
    , reorderArgumentsTool
    ]
  , [ renameVariableTool
    , introduceVariableTool
    , swapNamesAndUsagesTool
    , swapUsagesTool
    ]
  , [ makeEqualTool
    , copyExpressionTool
    ]
  , [ moveDefinitionTool
    , swapDefinitionsTool
    , inlineDefinitionTool
    , duplicateDefinitionTool
    ]
  , [ reorderExpressionsTool
    , swapExpressionsTool
    ]
  , [ makeSingleLineTool
    , makeMultiLineTool
    , alignExpressionsTool
    ]
  , [ thawFreezeTool
    , addRemoveRangeTool
    , showHideRangeTool
    , rewriteOffsetTool
    , convertColorStringTool
    ]
  , [ flipBooleanTool
    ]
  ]
-}

deuceToolsOf : Model -> List (List DeuceTool)
deuceToolsOf model =
  let
    selections = selectionsTuple model.inputExp model.deuceState.selectedWidgets
  in
  toolList
  |> List.map (List.map (\tool -> tool model selections))

createToolCache : Model -> List (List CachedDeuceTool)
createToolCache model =
  createToolCache_ model
  -- ImpureGoodies.logTimedRun "DeuceTools.createToolCachce" (\() ->
  --   createToolCache_ model
  -- )

createToolCache_ : Model -> List (List CachedDeuceTool)
createToolCache_ model =
  deuceToolsOf model |> List.map (
    List.map (\deuceTool ->
      case runTool deuceTool of
        Just results -> (deuceTool, results, False)
        Nothing      -> (deuceTool, [], True)
    )
  )

-- This function is not used.
createToolCacheMultipleInterpretations : Model -> List (List DeuceWidget)-> List (List CachedDeuceTool)
createToolCacheMultipleInterpretations model interpretations =
  let selectionInterpretations =
    interpretations
    |> List.map (selectionsTuple model.inputExp)
  in
  let toolToCacheResults tool =
    let
      toolInterpretations =
        selectionInterpretations |> List.map (\selections -> tool model selections)
      toolResults =
        toolInterpretations |> List.map runTool |> Utils.filterJusts |> List.concat
      -- I don't think the context-sensitive menu uses any tool properties that vary between intepretations.
      deuceTool = Utils.head "createToolCacheMultipleInterpretations" toolInterpretations
    in
    (deuceTool, toolResults, toolResults == [])
  in
  toolList
  |> List.map (List.map toolToCacheResults)

reselectDeuceTool : Model -> Model
reselectDeuceTool model =
  let
    newSelectedDeuceTool =
      case model.selectedDeuceTool of
        Just (selectedDeuceTool, _, _) ->
          Utils.findFirst
            ( \(deuceTool, _, _) ->
                deuceTool.id == selectedDeuceTool.id
            )
            ( List.concat model.deuceToolsAndResults
            )
        Nothing ->
          Nothing
  in
    { model | selectedDeuceTool = newSelectedDeuceTool }

updateRenameToolsInCache almostNewModel =
  let
    cachedAndNewDeuceTools =
      Utils.zipWith Utils.zip
        almostNewModel.deuceToolsAndResults
        (deuceToolsOf almostNewModel)
          -- assumes that the new tools computed by deuceTools
          -- are the same as the cached ones
  in
  cachedAndNewDeuceTools |> List.map (
    List.map (\((cachedDeuceTool, cachedResults, cachedBool), newDeuceTool) ->
      if isRenamer cachedDeuceTool then
        case runTool newDeuceTool of
          Just results -> (newDeuceTool, results, False)
          Nothing      -> (newDeuceTool, [], True)
      else
        (cachedDeuceTool, cachedResults, cachedBool)
    )
  )

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- Run a tool, and maybe get some results back (if it is active)
runTool : DeuceTool -> Maybe (List SynthesisResult)
runTool deuceTool =
  -- let _ = Utils.log <| "running tool " ++ deuceTool.name in
  case deuceTool.func of
    Just thunk ->
      case ImpureGoodies.crashToError thunk of
        Ok results -> Just results
        Err errMsg ->
          let _ = Debug.log ("Deuce Tool Crash \"" ++ deuceTool.name ++ "\"") (toString errMsg) in
          Nothing

    _ ->
      Nothing

-- Check if a tool is active without running it
isActive : DeuceTool -> Bool
isActive deuceTool =
  deuceTool.func /= Nothing

-- Check if a given tool is a renaming tool
isRenamer : DeuceTool -> Bool
isRenamer =
  String.startsWith "Rename" << .name
