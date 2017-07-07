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
  { name = "Make Equal (New Variable)"
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
              CodeMotion.makeEqualTransformation model literals
            else
              Nothing
        _ -> Nothing
  }

--------------------------------------------------------------------------------
-- Flip Boolean
--------------------------------------------------------------------------------
-- TODO This function does not rely on CodeMotion, so it is probably doing too
--      much.
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
            CodeMotion.introduceVarTransformation
              model
              exps
              (PatTargetPosition patTarget)
        }
      (_, _, exps, [], [], [expTarget], []) ->
        { name =
            Utils.maybePluralize "Introduce Variable" exps
        , func =
            CodeMotion.introduceVarTransformation
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
              CodeMotion.eliminateCommonSubExpressionTransformation
                model i js
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
              CodeMotion.copyExpressionTransformation model i js
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
-- Duplicate Definition
--------------------------------------------------------------------------------

duplicateDefinitionTool : Model -> Selections -> DeuceTool
duplicateDefinitionTool model selections =
  let
    disabledTool =
      { name = "Duplicate Definition"
      , func = Nothing
      }
  in
    case selections of
    (_, _, _, [], _, _, _) ->
      disabledTool
    ([], [], [], pathedPatIds, [], [(Before, eId)], []) ->
      { name =
          Utils.maybePluralize "Duplicate Definition" pathedPatIds
      , func =
          Just <|
            \() ->
              CodeMotion.duplicateDefinitionsBeforeEId
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
                Utils.maybePluralize "Duplicate Definition" pathedPatIds
            , func =
                Just <|
                  \() ->
                    CodeMotion.duplicateDefinitionsPat
                      pathedPatIds
                      targetPathedPatId
                      model.inputExp
            }
          _ -> disabledTool
    _ -> disabledTool

--------------------------------------------------------------------------------
-- Thaw/Freeze
--------------------------------------------------------------------------------
-- TODO This function does not rely on CodeMotion, so it is probably doing too
--      much.
--------------------------------------------------------------------------------

thawFreezeTool : Model -> Selections -> DeuceTool
thawFreezeTool model selections =
  let
    disabledTool =
      { name = "Thaw/Freeze"
      , func = Nothing
      }
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
  in
    case mode of
      Nothing ->
        disabledTool
      Just (toolName, newAnnotation) ->
        { name = toolName
        , func =
            Just <|
              \() ->
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
    disabledTool =
      { name = "Show/Hide Sliders"
      , func = Nothing
      }
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
  in
    case mode of
      Nothing ->
        disabledTool
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
          { name = toolName
          , func =
              Just <|
                \() ->
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
    disabledTool =
      { name = "Add/Remove Sliders"
      , func = Nothing
      }
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
  in
    case mode of
      Nothing ->
        disabledTool
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
          { name = toolName
          , func =
              Just <|
                \() ->
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
          }

--------------------------------------------------------------------------------
-- Rewrite Offset
--------------------------------------------------------------------------------

rewriteOffsetTool : Model -> Selections -> DeuceTool
rewriteOffsetTool model selections =
  let
    func =
      case selections of
        ([], _, _, _, _, _, _) ->
          Nothing
        (nums, [], exps, [ppid], [], [], []) ->
          if List.length nums == List.length exps then
            CodeMotion.rewriteOffsetTransformation model ppid nums
          else
            Nothing
        _ ->
          Nothing
  in
    { name =
        let
          (nums, _, _, _, _, _, _) =
            selections
        in
          case func of
            Nothing ->
              "Rewrite as Offset"
            Just _ ->
              Utils.maybePluralize "Rewrite as Offset" nums
    , func =
        func
    }

--------------------------------------------------------------------------------
-- Convert Color String
--------------------------------------------------------------------------------
-- TODO This function does not rely on CodeMotion, so it is probably doing too
--      much.
--------------------------------------------------------------------------------
-- TODO
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
  { name = "Create Function"
  , func =
      case selections of
        ([], [], [], [pathedPatId], [], [], []) ->
          case
            LangTools.findScopeExpAndPat pathedPatId model.inputExp
              |> Maybe.map (\(e,p) -> (e.val.e__, p.val.p__))
          of
            Just (ELet _ _ _ _ _ _ _, PVar _ ident _) ->
              Just <|
                \() ->
                  CodeMotion.abstractPVar pathedPatId model.inputExp
            _ ->
              Nothing
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
              Just <|
                \() ->
                  CodeMotion.abstractExp eid model.inputExp
            else
              Nothing
        ([], [], [], [], [letEId], [], []) ->
          case
            LangTools.justFindExpByEId model.inputExp letEId
              |> LangTools.expToMaybeLetPat
              |> Maybe.map (.val >> .p__)
          of
            Just (PVar _ _ _) ->
              Just <|
                \() ->
                  let
                     pathedPatId = ((letEId, 1), [])
                  in
                    CodeMotion.abstractPVar pathedPatId model.inputExp
            _ ->
              Nothing
        _ ->
          Nothing
  }

--------------------------------------------------------------------------------
-- Merge
--------------------------------------------------------------------------------

mergeTool : Model -> Selections -> DeuceTool
mergeTool model selections =
  { name = "Merge"
  , func =
      case selections of
        (_, _, eid1::eid2::restEIds, [], [], [], []) ->
          let
            eids =
              eid1::eid2::restEIds
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
              Just <|
                \() ->
                  mergeResults
            else
              Nothing
        _ ->
          Nothing
  }

--------------------------------------------------------------------------------
-- Add Arguments
--------------------------------------------------------------------------------

addArgumentsTool : Model -> Selections -> DeuceTool
addArgumentsTool model selections =
  let
    disabledTool =
      { name = "Add Argument"
      , func = Nothing
      }
  in
    case selections of
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
    disabledTool =
      { name = "Remove Argument"
      , func = Nothing
      }
  in
    case selections of
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
            { name = "Remove Aggument"
            , func =
                Just <|
                  \() ->
                    CodeMotion.removeArg
                      (Utils.head_ pathedPatIds)
                      model.inputExp
            }
          else if isAllArgumentSelected then
            { name = "Remove Arguments"
            , func =
                Just <|
                  \() ->
                    CodeMotion.removeArgs
                      pathedPatIds
                      model.inputExp
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
            }
          Just argPathedPatIds ->
            { name = "Remove Arguments"
            , func =
                Just <|
                  \() ->
                    CodeMotion.removeArgs
                    argPathedPatIds
                    model.inputExp
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
  { name = "Reorder Arguments"
  , func =
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
  }

--------------------------------------------------------------------------------
-- Reorder List
--------------------------------------------------------------------------------

reorderListTool : Model -> Selections -> DeuceTool
reorderListTool model selections =
  { name = "Reorder List"
  , func =
      CodeMotion.reorderEListTransformation model selections
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
    List.map (List.map ((|>) model >> (|>) selections))
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
