--------------------------------------------------------------------------------
-- This module provides "glue" code for the View to access the transformations
-- that CodeMotion provides.
--------------------------------------------------------------------------------

module DeuceTools exposing
  ( DeuceTool
  , deuceTools
  )

import Either exposing (..)
import Utils

import InterfaceModel exposing (..)

import Lang exposing (..)
import LangTools

import DeuceWidgets exposing
  ( DeuceWidget(..)
  )

import CodeMotion

--------------------------------------------------------------------------------
-- Selections
--------------------------------------------------------------------------------

type alias Selections =
  ( List (LocId, (WS, Num, Loc, WidgetDecl))
  , List (EId, (WS, EBaseVal))
  , List EId
  , List PathedPatternId
  , List EId
  , List ExpTargetPosition
  , List PatTargetPosition
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
-- Deuce Tools
--------------------------------------------------------------------------------

type alias DeuceTransformation =
  () -> List SynthesisResult

type alias DeuceTool =
  { name : String
  , func : Maybe DeuceTransformation
  }

--------------------------------------------------------------------------------
-- Make Equal
--------------------------------------------------------------------------------

makeEqualTool : Model -> Selections -> DeuceTool
makeEqualTool model selections =
  { name = "Make Equal with New Var"
  , func =
      case selections of
        (nums, baseVals, exps, [], [], [], []) ->
          let
            literals =
              List.map Left nums ++ List.map Right baseVals
          in
            if
              List.length literals >= 2 &&
              List.length literals == List.length exps
            then
              CodeMotion.makeMakeEqualTool model literals
            else
              Nothing
        _ -> Nothing
  }

--------------------------------------------------------------------------------
-- Flip Boolean
--------------------------------------------------------------------------------
-- NOTE: This tool does not rely on CodeMotion, so this function is probably
--       doing too much.
--------------------------------------------------------------------------------

flipBooleanTool : Model -> Selections -> DeuceTool
flipBooleanTool model selections =
  { name = "Flip Boolean"
  , func =
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
                    Just <| \() -> oneSafeResult newExp
                _ -> Nothing
            _ -> Nothing
        _ -> Nothing
  }

--------------------------------------------------------------------------------
-- Rename Pattern
--------------------------------------------------------------------------------

renamePatternTool : Model -> Selections -> DeuceTool
renamePatternTool model selections =
  let
    disabledTool =
      { name = "Rename Pattern"
      , func = Nothing
      }
  in
    case selections of
      ([], [], [], [pathedPatId], [], [], []) ->
        case
          LangTools.findPat pathedPatId model.inputExp
            |> Maybe.andThen LangTools.patToMaybeIdent
        of
          Just ident ->
            { name = "Rename " ++ ident
            , func =
                Just <|
                  \() ->
                    let
                      newName =
                        model.deuceState.renameVarTextBox
                    in
                      CodeMotion.renamePat pathedPatId newName model.inputExp
            }
          _ -> disabledTool
      _ -> disabledTool

--------------------------------------------------------------------------------
-- Rename Variable
--------------------------------------------------------------------------------

renameVariableTool : Model -> Selections -> DeuceTool
renameVariableTool model selections =
  let
    disabledTool =
      { name = "Rename Variable"
      , func = Nothing
      }
  in
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
                  { name = "Rename All " ++ ident
                  , func =
                      Just <|
                        \() ->
                          CodeMotion.renameVar eId newName model.inputExp
                  }
              _ -> disabledTool
          _ -> disabledTool
      _ -> disabledTool

--------------------------------------------------------------------------------
-- Swap Usages
--------------------------------------------------------------------------------

swapUsagesTool : Model -> Selections -> DeuceTool
swapUsagesTool model selections =
  { name = "Swap Usages"
  , func =
      case selections of
        ([], [], [], pathedPatId1::pathedPatId2::[], [], [], []) ->
          case
            [ LangTools.findPat pathedPatId1 model.inputExp
            , LangTools.findPat pathedPatId2 model.inputExp
            ] |> List.map (Maybe.andThen LangTools.patToMaybeIdent)
          of
            [Just name1, Just name2] ->
              Just <|
                \() ->
                  CodeMotion.swapUsages pathedPatId1 pathedPatId2 model.inputExp
            _ -> Nothing
        _ -> Nothing
  }

--------------------------------------------------------------------------------
-- Swap Names and Usages
--------------------------------------------------------------------------------

swapNamesAndUsagesTool : Model -> Selections -> DeuceTool
swapNamesAndUsagesTool model selections =
  { name = "Swap Names and Usages"
  , func =
      case selections of
        ([], [], [], pathedPatId1::pathedPatId2::[], [], [], []) ->
          case
            [ LangTools.findPat pathedPatId1 model.inputExp
            , LangTools.findPat pathedPatId2 model.inputExp
            ] |> List.map (Maybe.andThen LangTools.patToMaybeIdent)
          of
            [Just name1, Just name2] ->
              Just <|
                \() ->
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
            _ -> Nothing
        _ -> Nothing
  }

--------------------------------------------------------------------------------
-- Inline Definition
--------------------------------------------------------------------------------

inlineDefinitionTool : Model -> Selections -> DeuceTool
inlineDefinitionTool model selections =
  let
    disabledTool =
      { name = "Rename Pattern"
      , func = Nothing
      }
  in
    case selections of
      (_, _, _, [], [], _, _) ->
        disabledTool
      ([], [], [], pathedPatIds, [], [], []) ->
        { name =
            Utils.maybePluralize "Inline Definition" pathedPatIds
        , func =
            Just <|
              \() ->
                CodeMotion.inlineDefinitions pathedPatIds model.inputExp
        }
      ([], [], [], [], letEIds, [], []) ->
        { name =
            Utils.maybePluralize "Inline Definition" letEIds
        , func =
            Just <|
              \() ->
                CodeMotion.inlineDefinitions
                  (letEIds |> List.map (\letEId -> ((letEId, 1), [])))
                  model.inputExp
        }
      _ ->
        disabledTool

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
  let
    disabledTool =
      { name = "Introduce Variable"
      , func = Nothing
      }
  in
    case selections of
      (_, _, [], _, _, _, _) ->
        disabledTool
      (_, _, exps, [], [], [], [patTarget]) ->
        { name =
            Utils.maybePluralize "Introduce Variable" exps
        , func =
            CodeMotion.makeIntroduceVarTool
              model
              exps
              (PatTargetPosition patTarget)
        }
      (_, _, exps, [], [], [expTarget], []) ->
        { name =
            Utils.maybePluralize "Introduce Variable" exps
        , func =
            CodeMotion.makeIntroduceVarTool
              model
              exps
              (ExpTargetPosition expTarget)
        }
      _ ->
        disabledTool

--------------------------------------------------------------------------------
-- Elminate Common Sub-Expression
--------------------------------------------------------------------------------

eliminateCommonSubExpressionTool : Model -> Selections -> DeuceTool
eliminateCommonSubExpressionTool model selections =
  { name = "Eliminate Common Sub-Expression"
  , func =
      case selections of
        (_, _, [], _, _, _, _) ->
          Nothing
        (_, _, [_], _, _, _, _) ->
          Nothing
        (nums, baseVals, i::js, [], [], [], []) ->
          let
            atLeastOneNonLiteral =
              List.length (i::js) > (List.length nums + List.length baseVals)
          in
            if atLeastOneNonLiteral then
              CodeMotion.makeEliminateCommonSubExpressionTool model i js
            else
              Nothing
        _ ->
          Nothing
  }

--------------------------------------------------------------------------------
-- Copy Expression
--------------------------------------------------------------------------------

copyExpressionTool : Model -> Selections -> DeuceTool
copyExpressionTool model selections =
  { name = "Copy Expression"
  , func =
      case selections of
        (_, _, [], _, _, _, _) ->
          Nothing
        (_, _, [_], _, _, _, _) ->
          Nothing
        (nums, baseVals, i::js, [], [], [], []) ->
          let
            atLeastOneNonLiteral =
              List.length (i::js) > (List.length nums + List.length baseVals)
          in
            if atLeastOneNonLiteral then
              CodeMotion.makeCopyExpressionTool model i js
            else
              Nothing
        _ ->
          Nothing
  }

--------------------------------------------------------------------------------
-- Move Definition
--------------------------------------------------------------------------------

moveDefinitionTool : Model -> Selections -> DeuceTool
moveDefinitionTool model selections =
  let
    disabledTool =
      { name = "Rename Pattern"
      , func = Nothing
      }
  in
    case selections of
      (_, _, _, [], [], _, _) ->
        disabledTool
      ([], [], [], pathedPatIds, [], [(Before, eId)], []) ->
        { name =
            Utils.maybePluralize "Move Definition" pathedPatIds
        , func =
            Just <|
              \() ->
                CodeMotion.moveDefinitionsBeforeEId
                  pathedPatIds
                  eId
                  model.inputExp
        }
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
              { name =
                  Utils.maybePluralize "Move Definition" pathedPatIds
              , func =
                  Just <|
                    \() ->
                      CodeMotion.moveDefinitionsPat
                        pathedPatIds
                        targetPathedPatId
                        model.inputExp
              }
            _ -> disabledTool
      ([], [], [], [], [letEId], [(Before, eId)], []) ->
        { name = "Move Definition"
        , func =
            Just <|
              \() ->
                -- Better result names if we hand the singular case directly to
                -- moveDefinitionsBeforeEId.
                CodeMotion.moveDefinitionsBeforeEId
                  [((letEId, 1), [])]
                  eId
                  model.inputExp
        }
      ([], [], [], [], letEIds, [(Before, eId)], []) ->
        { name = "Move Definitions"
        , func =
            Just <|
              \() ->
                CodeMotion.moveEquationsBeforeEId
                  letEIds
                  eId
                  model.inputExp
        }
      _ ->
        disabledTool

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
    List.map (List.map ((|>) model >> (|>) selections))
      [ [ makeEqualTool
        , flipBooleanTool
        ]
      , [ renamePatternTool
        ]
      , [ renameVariableTool
        , swapUsagesTool
        , swapNamesAndUsagesTool
        , inlineDefinitionTool
        , introduceVariableTool
        , eliminateCommonSubExpressionTool
        , copyExpressionTool
        , moveDefinitionTool
        ]
      ]
