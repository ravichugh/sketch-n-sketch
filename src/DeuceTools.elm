------------------------------------------------------------------------------
-- This module provides "glue" code for the View to access the transformations
-- that CodeMotion (and friends) provides.
--------------------------------------------------------------------------------

module DeuceTools exposing
  ( typesToolId
  , createToolCache
  , reselectDeuceTool
  , updateInputSensitiveToolsInCache
  , isActive
  -- Hotkey exposure
  , renameVariableViaHotkey
  , expandFormatViaHotkey
  , addToEndViaHotkey
  , smartCompleteHole
  , replaceWithParens
  , replaceWithList
  , replaceWithRecord
  , replaceWithLambda
  , replaceWithApp
  , replaceWithCase
  , replaceWithLet
  , replaceWithCond
  )

import String
import Dict
import Regex

import Either exposing (..)
import Utils
import ImpureGoodies
import ColorNum

import Model exposing
  ( Model
  , oneSafeResult
  , setResultSafe
  , CachedDeuceTool
  )

import Info
import Lang exposing (..)
import LangTools
import LeoParser
import Syntax
import Types2

import DeuceWidgets exposing
  ( DeuceWidget(..)
  , DeuceState
  , emptyDeuceState
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

selectedTIds : List DeuceWidget -> List TId
selectedTIds deuceWidgets =
  flip List.concatMap deuceWidgets <| \deuceWidget ->
    case deuceWidget of
      DeuceType x -> [x]
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
    (nums, [], exps, [], [], [], [], [], []) ->
      List.length nums >= 1 && List.length nums == List.length exps
    _ ->
      False

--------------------------------------------------------------------------------
-- Create elements in place of holes
--------------------------------------------------------------------------------

inBodyPosition : Exp -> Exp -> Bool
inBodyPosition root e =
  LangTools.performActionIfBody root e (\_ _ _ -> True) (always True) False

doesChildNeedParens : Exp -> Bool
doesChildNeedParens e =
  case unwrapExp e of
    EApp _ _ _ _ _ -> True
    EOp _ _ _ _ _ -> True
    EList _ _ _ _ _ -> True -- Some children do and some don't, so let's be conservative
    EColonType _ _ _ _ _ -> True
    ESelect _ _ _ _ _ -> True
    _ -> False

shouldHaveParensOnAccountOfParent : Exp -> EId -> Bool
shouldHaveParensOnAccountOfParent root expEId =
  parentByEId root expEId |>
    Utils.fromJust_ "Couldn't find hole EId" |>
      Maybe.map doesChildNeedParens |>
        Maybe.withDefault False

addToEnd : Model -> Selections -> Maybe Exp
addToEnd model selections =
  let mbExp =
    case selections of
      (_, _, [eId], [], [], [], [], [], []) ->
        findExpByEId model.inputExp eId
      (_, _, [], [ppid], [], [], [], [], []) ->
        LangTools.findScopeExpAndPatByPathedPatternId ppid model.inputExp
        |> Maybe.map Tuple.first |> Maybe.map Tuple.first
      _ ->
        Nothing
  in
  Maybe.andThen (addToEnd_ model True) mbExp

addToEnd_ model tryParent exp =
  let
    root = model.inputExp
    eId = expEId exp
    return e__ = replaceExpNode eId (replaceE__ exp e__) root |> Just
    wsOfLast wsGetter l =
      ws <|
      case Utils.maybeLast l of
        Nothing   -> ""
        Just last -> wsGetter last |> ensureWhitespace
  in
  case unwrapExp exp of
    EApp wsb f args appType ws1 ->
      let
        argWsb = wsOfLast precedingWhitespace args
        newArg = EHole argWsb EEmptyHole |> withDummyExpInfo
      in
      EApp wsb f (args ++ [newArg]) appType ws1
      |> return
    EFun wsb pats body ws1 ->
      let
        patWsb = wsOfLast precedingWhitespacePat pats
        -- TODO this should probably actually be a PWildcard
        newPat =
          withDummyPatInfo <| PVar patWsb "_" <| Info.withDummyInfo NoWidgetDecl
      in
      EFun wsb (pats ++ [newPat]) body ws1
      |> return
    _ ->
      if tryParent then
        parentByEId root eId |>
        Maybe.andThen (Maybe.andThen <| addToEnd_ model False)
      else
        Nothing

genericReplaceHoleTool : String -> String -> (Exp -> Exp -> DeuceTransformation) -> Model -> Selections -> DeuceTool
genericReplaceHoleTool   toolID    name      transformer                            model    selections =
  let
    (func, predVal) =
      let
        impossible = (InactiveDeuceTransform, Impossible)
        root = model.inputExp
      in
      case selections of
        (_, _, [], [], [], [], [], [], []) ->
          (InactiveDeuceTransform, Possible)
        (_, _, [holeEId], [], [], [], [], [], []) ->
          let holeExp = LangTools.justFindExpByEId root holeEId in
          case unwrapExp holeExp of
            EHole _ EEmptyHole ->
              let transformation = transformer root holeExp in
              (transformation, if transformation == InactiveDeuceTransform then Impossible else FullySatisfied)
            _ ->
              impossible
        _ ->
          impossible
  in
    { name = name
    , func = func
    , reqs =
        [ { description =
              "Select a hole"
          , value =
              predVal
          }
        ]
    , id = toolID
    }

makeReplaceHoleTool : String -> String -> (Exp -> Exp -> Maybe Exp) -> Model -> Selections -> DeuceTool
makeReplaceHoleTool   toolID    replDesc  programModifier =
  genericReplaceHoleTool
    toolID
    ("Create " ++ replDesc)
    (\root holeExp ->
      let mbNewProgram = programModifier root holeExp in
      case mbNewProgram of
        Nothing ->
          InactiveDeuceTransform
        Just newProgram ->
          basicDeuceTransform <| [basicTransformationResult ("Replace with " ++ replDesc) newProgram]
    )

makeSimpleReplaceHoleTool : String -> String -> Bool ->         (WS -> Exp__) -> Model -> Selections -> DeuceTool
makeSimpleReplaceHoleTool   toolID    replDesc  mightNeedParens wsToExp__ =
  makeReplaceHoleTool toolID replDesc (\root holeExp ->
    let
      holeEId = expEId holeExp
      wsb = ws <| precedingWhitespace holeExp
      nextID = LeoParser.maxId root + 1
      newExp =
        if mightNeedParens && shouldHaveParensOnAccountOfParent root holeEId
        then
          Expr <|
            withDummyExpInfoEId nextID <|
              EParens wsb (Expr <| withDummyExpInfoEId holeEId <| wsToExp__ space0) Parens space0
        else
          Expr <| withDummyExpInfoEId holeEId <| wsToExp__ wsb
    in
    Just <| replaceExpNode holeEId newExp root
  )

-- TODO text box or slider (via the WidgetDecl) to specify the value
createNumTool =
  makeSimpleReplaceHoleTool
    "createNumFromHole"
    "a number"
    False
    (\wsb -> EConst wsb 0.0 dummyLoc noWidgetDecl)

createTrueTool =
  makeSimpleReplaceHoleTool
    "createTrueFromHole"
    "True"
    False
    (\wsb -> EBase wsb <| EBool True)

createFalseTool =
  makeSimpleReplaceHoleTool
    "createFalseFromHole"
    "False"
    False
    (\wsb -> EBase wsb <| EBool False)

createEmptyStringTool =
  makeSimpleReplaceHoleTool
    "createEmptyStringFromHole"
    "an empty string"
    False
    (\wsb -> EBase wsb <| EString defaultQuoteChar "")

createVarTool =
  genericReplaceHoleTool
    "createVarReferenceFromHole"
    "Create a variable reference"
    (\root holeExp ->
      let
        holeEId = expEId holeExp
        wsb = precedingWhitespace holeExp |> ws
        visibleVars = visibleIdents root holeExp |> Utils.fromJust_ "holeExp not found"
      in
      SmartCompleteDeuceTransform <| \smartCompleteText ->
        List.filter (String.startsWith smartCompleteText) visibleVars |>
          List.map (\vIdent ->
            replaceExpNode holeEId (Expr <| withDummyExpInfoEId holeEId <| EVar wsb vIdent) root
            |> basicTransformationResult vIdent
          )
    )

createLambdaTool =
  makeSimpleReplaceHoleTool
    "createLambdaFromHole"
    "an anonymous function"
    True
    (\wsb -> EFun wsb [pWildcard0] eEmptyHoleVal space0)

createApplicationTool =
  makeSimpleReplaceHoleTool
    "createAppFromHole"
    "an application"
    True
    (\wsb -> EApp wsb eEmptyHoleVal0 [eEmptyHoleVal] SpaceApp space0)

{- TODO we need some sort of autocompleter for ops
  | EOp WS WS Op (List t1) WS
type Op_
  -- nullary ops
  = Pi
  | DictEmpty
  | CurrentEnv
  -- unary ops
  | DictFromList
  | Cos | Sin | ArcCos | ArcSin
  | Floor | Ceil | Round
  | ToStr
  | ToStrExceptStr -- Keeps strings, but use ToStr for everything else
  | Sqrt
  | Explode
  | DebugLog
  | NoWidgets
  -- binary ops
  | Plus | Minus | Mult | Div
  | Lt | Eq
  | Mod | Pow
  | ArcTan2
  | DictGet
  | DictRemove
  -- trinary ops
  | DictInsert
  | RegexExtractFirstIn
--
opFromIdentifier : Ident -> Maybe Op_
opFromIdentifier identifier =
  case identifier of
    "pi" ->
      Just Pi
    "__DictEmpty__" ->
      Just DictEmpty
    "__CurrentEnv__" ->
      Just CurrentEnv
    "__DictFromList__" ->
      Just DictFromList
    "cos" ->
      Just Cos
    "sin" ->
      Just Sin
    "arccos" ->
      Just ArcCos
    "arcsin" ->
      Just ArcSin
    "floor" ->
      Just Floor
    "ceiling" ->
      Just Ceil
    "round" ->
      Just Round
    "toString" ->
      Just ToStr
    "sqrt" ->
      Just Sqrt
    "explode" ->
      Just Explode
    "+" ->
      Just Plus
    "-" ->
      Just Minus
    "*" ->
      Just Mult
    "/" ->
      Just Div
    "<" ->
      Just Lt
    "==" ->
      Just Eq
    "mod" ->
      Just Mod
    "^" ->
      Just Pow
    "arctan2" ->
      Just ArcTan2
    "__DictInsert__" ->
      Just DictInsert
    "__DictGet__" ->
      Just DictGet
    "__DictRemove__" ->
      Just DictRemove
    "debug" ->
      Just DebugLog
    "noWidgets" ->
      Just NoWidgets
    "extractFirstIn" ->
      Just RegexExtractFirstIn
    _ ->
      Nothing
-}

createEmptyListTool =
  makeSimpleReplaceHoleTool
    "createEmptyListFromHole"
    "empty list"
    False
    (\wsb -> EList wsb [] space0 Nothing space0)

createCondTool =
  makeSimpleReplaceHoleTool
    "createCondFromHole"
    "a conditional"
    True
    (\wsb -> EIf wsb eEmptyHoleVal space1 eEmptyHoleVal space1 eEmptyHoleVal space0)

createCaseTool =
  makeReplaceHoleTool
    "createCaseFromHole"
    "a case clause"
    (\oldRoot holeExp ->
      let
        holeEId = expEId holeExp
        wsb = precedingWhitespace holeExp
        maxID = LeoParser.maxId oldRoot
        (caseID, patID) = Utils.mapBoth ((+) maxID) (1, 2)
        ofExp = Expr <| withDummyExpInfoEId holeEId <| EHole space1 EEmptyHole
        pat = withDummyPatInfoPId patID <| PWildcard space0
        holeExpWithOneSpace = replacePrecedingWhitespace " " holeExp
        caseExp wsBeforeCase wsBeforeBranch =
          let branch =
            withDummyBranchInfo <|
              Branch_ (ws wsBeforeBranch) pat holeExpWithOneSpace space0
          in
          Expr <|
            withDummyExpInfoEId caseID <|
              ECase (ws wsBeforeCase) ofExp [branch] space1
        caseExpSeparateLine caseIndent =
          let
            wsbNewlineCount = max 1 <| newlineCount wsb
            wsBeforeCase = String.repeat wsbNewlineCount "\n" ++ caseIndent
            wsBeforeBranch = "\n" ++ caseIndent ++ "  "
          in
          caseExp wsBeforeCase wsBeforeBranch
        mbCaseIndentIfBody = LangTools.getProperIndentationIfBody oldRoot holeExp
      in
      Maybe.map (\newExp -> replaceExpNode holeEId newExp oldRoot) <|
      case (mbCaseIndentIfBody, parentByEId oldRoot holeEId) of
        (Just caseIndentIfBody, _) ->
          Just <| caseExpSeparateLine caseIndentIfBody
        (Nothing, Just (Just parent)) ->
          let parentEId = expEId parent in
          case unwrapExp parent of
            ELet _ _ _ _ _ ->
              -- If we get here, we already know that holeExp is a bound exp of the ELet
              Just <| caseExpSeparateLine <| indentationAt parentEId oldRoot ++ "  "
            EParens _ _ _ _ ->
              Just <|
                if String.contains "\n" wsb then
                  caseExpSeparateLine <| indentationAt holeEId oldRoot
                else
                  caseExp wsb <| "\n" ++ indentationAt parentEId oldRoot ++ "  "
            _ ->
              Nothing
        (Nothing, Just Nothing) ->
          Just <| caseExpSeparateLine <| indentationAt holeEId oldRoot
        (Nothing, Nothing) ->
          Nothing
    )

createLetTool =
  makeReplaceHoleTool
    "createLetFromHole"
    "a let clause"
    (\oldRoot holeExp ->
      let
        holeEId = expEId holeExp
        wsb = ws <| precedingWhitespace holeExp
        maxID = LeoParser.maxId oldRoot
        (letID, patID, parensID) = Utils.mapThree ((+) maxID) (1, 2, 3)
        isTopLevel = LangTools.isTopLevelEId holeEId oldRoot
        pat = withDummyPatInfoPId patID <| PWildcard <| if isTopLevel then space0 else space1
        boundExp = Expr <| withDummyExpInfoEId holeEId <| EHole space1 EEmptyHole
        newExp =
          let
            letOrDef = if isTopLevel then Def else Let
            holeExpWithOneSpace = replacePrecedingWhitespace " " holeExp
            flatLet wsBeforeLet =
              Expr <|
                withDummyExpInfoEId letID <|
                  eLet__ wsBeforeLet letOrDef False pat space1 boundExp space1 holeExpWithOneSpace space0
          in
          if shouldHaveParensOnAccountOfParent oldRoot holeEId then
            Expr <|
              withDummyExpInfoEId parensID <|
                EParens wsb (flatLet space0) Parens space0
          else if inBodyPosition oldRoot holeExp || newlineCount (precedingWhitespace holeExp) /= 0 then
            LangTools.newLetFancyWhitespace letID False pat boundExp holeExp oldRoot
          else
            flatLet wsb
      in
      Just <| replaceExpNode holeEId newExp oldRoot
    )

createTypeAscriptionTool =
  makeSimpleReplaceHoleTool
    "createTypeAscriptionFromHole"
    "a type ascription"
    True
    -- TODO dummyType should be changed to hole type
    (\wsb -> EColonType wsb eEmptyHoleVal0 space1 (dummyType space1) space0)

createParenthesizedTool =
  makeSimpleReplaceHoleTool
    "createParenthesizedFromHole"
    "parenthesis"
    False
    (\wsb -> EParens wsb eEmptyHoleVal0 Parens space0)

createRecordTool =
  makeSimpleReplaceHoleTool
    "createRecordFromHole"
    "record"
    False
    (\wsb -> ERecord wsb Nothing (Declarations [] [] [] []) space0)

addToEndViaHotkey : Model -> DeuceWidget -> Maybe Exp
addToEndViaHotkey = runFunctionViaHotkey addToEnd

smartCompleteHole : Model -> DeuceWidget -> (String, String -> List TransformationResult)
smartCompleteHole = runInputBasedToolViaHotkey createVarTool

replaceWithParens : Model -> DeuceWidget -> Maybe Exp
replaceWithParens = runToolViaHotkey createParenthesizedTool

replaceWithList : Model -> DeuceWidget -> Maybe Exp
replaceWithList = runToolViaHotkey createEmptyListTool

replaceWithRecord : Model -> DeuceWidget -> Maybe Exp
replaceWithRecord = runToolViaHotkey createRecordTool

replaceWithLambda : Model -> DeuceWidget -> Maybe Exp
replaceWithLambda = runToolViaHotkey createLambdaTool

replaceWithApp : Model -> DeuceWidget -> Maybe Exp
replaceWithApp = runToolViaHotkey createApplicationTool

replaceWithCase : Model -> DeuceWidget -> Maybe Exp
replaceWithCase = runToolViaHotkey createCaseTool

replaceWithLet : Model -> DeuceWidget -> Maybe Exp
replaceWithLet = runToolViaHotkey createLetTool

replaceWithCond : Model -> DeuceWidget -> Maybe Exp
replaceWithCond = runToolViaHotkey createCondTool

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
        (_, _, [], [], [], [], _, _, _) -> (InactiveDeuceTransform, Possible)
        (_, _, _, _::_, _, _, _, _, _) -> (InactiveDeuceTransform, Impossible) -- no pattern selection allowed (yet)
        (_, _, _, _, _, _::_, _, _, _) -> (InactiveDeuceTransform, Impossible) -- no equation selection allowed (yet?)
        (_, _, [_], _, _, _, _, _, _)  -> (InactiveDeuceTransform, Possible)
        (_, _, eids, [], [], [], [], [], []) ->
          ( CodeMotion.makeEqualTransformation model.inputExp eids Nothing |> mbThunkToTransform
          -- TODO It seems incorrect to return `Satisfied` if the func is `Nothing` - I think this may be a source of bugs
          , Satisfied
          )
        (_, _, eids, [], [], [], [], [], [patTarget]) ->
          ( CodeMotion.makeEqualTransformation model.inputExp eids (Just (PatTargetPosition patTarget)) |> mbThunkToTransform
          , Satisfied
          )
        (_, _, eids, [], [], [], [], [expTarget], []) ->
          ( CodeMotion.makeEqualTransformation model.inputExp eids (Just (ExpTargetPosition expTarget)) |> mbThunkToTransform
          , Satisfied
          )
        (_, _, eids, [], [], [], [declTarget], [], []) ->
          ( CodeMotion.makeEqualTransformation model.inputExp eids (Just (DeclarationTargetPosition declTarget)) |> mbThunkToTransform
          , Satisfied
          )
        _ ->
          (InactiveDeuceTransform, Impossible)
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
        ([], [_], [eId], [], [], [], [], [], []) ->
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
                    (basicDeuceTransform <| oneSafeResult newExp, FullySatisfied)
                _ ->
                  (InactiveDeuceTransform, Impossible)
            _ ->
              (InactiveDeuceTransform, Impossible)
        ([], [], [], [], [], [], [], [], []) ->
          (InactiveDeuceTransform, Possible)
        _ ->
          (InactiveDeuceTransform, Impossible)
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
        ([], [], [], [pathedPatId], [], [], [], [], []) ->
          case
            LangTools.findPatByPathedPatternId pathedPatId model.inputExp
              |> Maybe.andThen LangTools.patToMaybeIdent -- Rename tool only allowed for PVar and PAs
          of
            Just ident ->
              ( nameMaker ident
              , RenameDeuceTransform <| \newName -> CodeMotion.renamePat pathedPatId newName model.inputExp
              , FullySatisfied
              )
            _ ->
              (disabledName, InactiveDeuceTransform, Impossible)
        ([], [], [eId], [], [], [], [], [], []) ->
          case findExpByEId model.inputExp eId of
            Just ePlucked ->
              case (unwrapExp ePlucked) of
                EVar _ ident ->
                  ( nameMaker ident
                  , RenameDeuceTransform <| \newName -> CodeMotion.renameVar eId newName model.inputExp
                  , FullySatisfied
                  )
                _ -> (disabledName, InactiveDeuceTransform, Impossible)
            _ -> (disabledName, InactiveDeuceTransform, Impossible)
        ([], [], [], [], [], [], [], [], []) ->
          (disabledName, InactiveDeuceTransform, Possible)
        _ ->
          (disabledName, InactiveDeuceTransform, Impossible)
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

renameVariableViaHotkey : Model -> DeuceWidget -> (String, String -> List TransformationResult)
renameVariableViaHotkey = runInputBasedToolViaHotkey renameVariableTool

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
        ([], [], [], [], [], [], [], [], []) ->
          (InactiveDeuceTransform, Possible)
        ([], [], [], [_], [], [], [], [], []) ->
          (InactiveDeuceTransform, Possible) -- could check whether pathedPatId1 is a var
        ([], [], [], [pathedPatId1, pathedPatId2], [], [], [], [], []) ->
          let maybeNames =
            [pathedPatId1, pathedPatId2]
              |> List.map (\ppid -> LangTools.findPatByPathedPatternId ppid model.inputExp)
              |> List.map (Maybe.andThen LangTools.patToMaybeIdent)
          in
          case maybeNames of
            [Just name1, Just name2] ->
              ( NoInputDeuceTransform (makeThunk (pathedPatId1, name1) (pathedPatId2, name2))
              , FullySatisfied
              )
            _ ->
              (InactiveDeuceTransform, Impossible)
        _ ->
          (InactiveDeuceTransform, Impossible)
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
        (_, _, [], [], [], [], [], [], [])           -> (InactiveDeuceTransform, Possible)
        (_, _, _, _::_, _, _, _, _, _)             -> (InactiveDeuceTransform, Impossible) -- no pattern selection allowed (yet)
        (_, _, _, _, _, _::_, _, _, _)             -> (InactiveDeuceTransform, Impossible) -- no equation selection allowed (yet?)
        (_, _, [_], _, _, _, [], [], [])            -> (InactiveDeuceTransform, Possible)
        (_, _, [eid1, eid2], [], [], [], [], [], []) -> (CodeMotion.swapExpressionsTransformation model.syntax model.inputExp eid1 eid2 |> mbThunkToTransform, Satisfied)
        _                                    -> (InactiveDeuceTransform, Impossible)
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
        (_, _, [], [], [], [], [], [], []) ->
          (InactiveDeuceTransform, Possible)
        (_, _, [], [ppid], [], [], [], [], []) ->
          (InactiveDeuceTransform, if ppidIsInLet ppid then Possible else Impossible)
        (_, _, [], [], [], [letEId], [], [], []) ->
          (InactiveDeuceTransform, Possible)
        (_, _, [], [ppid1, ppid2], [], [], [], [], []) ->
          ( CodeMotion.swapDefinitionsTransformation model.syntax model.inputExp
              (LangTools.pathedPatternIdToPId ppid1 model.inputExp |> Utils.fromJust_ "CodeMotion.swapDefinitionsTool")
              (LangTools.pathedPatternIdToPId ppid2 model.inputExp |> Utils.fromJust_ "CodeMotion.swapDefinitionsTool")
              |> mbThunkToTransform
          , Satisfied
          )
        (_, _, [], [], [], [(letEId1, bn1), (letEId2, bn2)], [], [], []) ->
          ( CodeMotion.swapDefinitionsTransformation model.syntax model.inputExp (letEIdToTopPId letEId1 bn1) (letEIdToTopPId letEId2 bn2) |> mbThunkToTransform
          , Satisfied
          )
        _ ->
          (InactiveDeuceTransform, Impossible)
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
        ([], [], [], [], [], [], [], [], []) ->
          (toolName, InactiveDeuceTransform, Possible)
        (_, _, _, _, _, [], _, _, _) ->
          (toolName, InactiveDeuceTransform, Impossible)
        ([], [], [], [], [], letEIds, [], [], []) ->
          case CodeMotion.inlineDefinitions letEIds model.inputExp of
            Nothing ->
              (toolName , InactiveDeuceTransform , Impossible)
            Just (inlinedIdents, newProgram) ->
              ( Utils.perhapsPluralizeList toolName letEIds
              , basicDeuceTransform [basicTransformationResult ("Inline " ++ toString inlinedIdents) newProgram]
              , Satisfied
              )
        _ ->
          (toolName, InactiveDeuceTransform, Impossible)
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
        ([], [], [], [], [], [], [], [], []) ->
          (toolName, InactiveDeuceTransform, Possible)
        (_, _, [], _, _, _, _, _, _) ->
          (toolName, InactiveDeuceTransform, Impossible)
        (_, _, exps, [], [], [], [], [], []) ->
          ( Utils.perhapsPluralizeList "Introduce Variable" exps
          , CodeMotion.introduceVarTransformation
              model
              exps
              Nothing
            |> mbThunkToTransform
          -- TODO It seems incorrect to return `Satisfied` if there is no func - I think this may be a source of bugs
          , Satisfied
          )
        (_, _, exps, [], [], [], [], [], [patTarget]) ->
          ( Utils.perhapsPluralizeList "Introduce Variable" exps
          , CodeMotion.introduceVarTransformation
              model
              exps
              (Just (PatTargetPosition patTarget))
            |> mbThunkToTransform
          , Satisfied
          )
        (_, _, exps, [], [], [], [], [expTarget], []) ->
          ( Utils.perhapsPluralizeList "Introduce Variable" exps
          , CodeMotion.introduceVarTransformation
              model
              exps
              (Just (ExpTargetPosition expTarget))
            |> mbThunkToTransform
          , Satisfied
          )
        (_, _, exps, [], [], [], [declTarget], [], []) ->
          ( Utils.perhapsPluralizeList "Introduce Variable" exps
          , CodeMotion.introduceVarTransformation
              model
              exps
              (Just (DeclarationTargetPosition declTarget))
            |> mbThunkToTransform
          , Satisfied
          )
        (_, _, exps, [], [], [decl], [], [], []) ->
          ( Utils.perhapsPluralizeList "Introduce Variable" exps
          , CodeMotion.introduceVarTransformation
              model
              exps
              (Just (DeclarationTargetPosition (Before, decl)))
            |> mbThunkToTransform
          , Satisfied
          )
        _ ->
          (toolName, InactiveDeuceTransform, Impossible)
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
        (_, _, [], [], [], [], [], [], [])   -> (InactiveDeuceTransform, Possible)
        (_, _, _, _::_, _, _, _, _, _)     -> (InactiveDeuceTransform, Impossible) -- no pattern selection allowed (yet)
        (_, _, _, _, _, _::_, _, _, _)     -> (InactiveDeuceTransform, Impossible) -- no equation selection allowed (yet?)
        (_, _, [_], _, _, _,  [], [], [])    -> (InactiveDeuceTransform, Possible)
        (_, _, eids, [], [], [], [], [], []) -> (CodeMotion.copyExpressionTransformation model.syntax model.inputExp eids |> mbThunkToTransform, Satisfied)
        _                            -> (InactiveDeuceTransform, Impossible)
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

patIdToEIdBindingNum: Exp -> PathedPatternId -> Maybe (EId, BindingNumber)
patIdToEIdBindingNum originalExp ((eid, bindingNum), path) =
  if path /= [] then Nothing
  else
    if    findExpByEId originalExp eid
       |> Maybe.map isLet
       |> Maybe.withDefault False then Just (eid, bindingNum)
    else Nothing

moveDefinitionTool : Model -> Selections -> DeuceTool
moveDefinitionTool model selections =
  let
    toolName = "Move Definition"

    (name, func, predVal) =
      case selections of
        ([], [], [], [], [], [], [], [], []) ->
          (toolName, InactiveDeuceTransform, Possible)
        ([], [], [], _::_, [], [], [], [], []) ->
          (toolName, InactiveDeuceTransform, Possible)
        ([], [], [], [], [], _::_, [], [], []) ->
          (toolName, InactiveDeuceTransform, Possible)
        ([], [], [], [], [], [], [], [_], []) ->
          (toolName, InactiveDeuceTransform, Possible)
        ([], [], [], [], [], [], [], [], [_]) ->
          (toolName, InactiveDeuceTransform, Possible)
        {-
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
          )-}
        ([], [], [], patIds, [], letEIds, declTargets, eTargets, patTargets) ->
          let mbAllLetIds = patIds
                |> List.map (patIdToEIdBindingNum model.inputExp)
                |> Utils.projJusts
                |> Maybe.map (flip (++) letEIds) in
          case mbAllLetIds of
            Nothing -> (toolName, InactiveDeuceTransform, Impossible)
            Just [] -> (toolName, InactiveDeuceTransform, Impossible)
            Just allLetEIds ->
               case (declTargets, eTargets, patTargets) of
                ([declTarget], [], []) ->
                   ( Utils.perhapsPluralizeList toolName allLetEIds
                   , NoInputDeuceTransform <| \() ->
                       CodeMotion.moveDeclarations
                         allLetEIds
                         (Tuple.mapFirst InsertDeclarationLevel declTarget)
                         model.inputExp
                   , Satisfied
                   )
                ([], [(Before, eId)], []) ->
                  ( Utils.perhapsPluralizeList toolName allLetEIds
                  , NoInputDeuceTransform <| \() ->
                      CodeMotion.moveDeclarations
                        allLetEIds
                        (InsertDeclarationLevel Before, (eId, 0))
                        model.inputExp
                  , Satisfied
                  )
                ([], [], [(beforeAfter, (insertionPosition, path), pId)]) ->
                       ( Utils.perhapsPluralizeList toolName allLetEIds
                       , NoInputDeuceTransform <| \() ->
                           CodeMotion.moveDeclarations
                             allLetEIds
                             (InsertPatternLevel beforeAfter path, insertionPosition)
                             model.inputExp
                       , Satisfied
                       )
                _ ->
                  (toolName, InactiveDeuceTransform, Impossible)
        _ ->
          (toolName, InactiveDeuceTransform, Impossible)
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
        ([], [], [], [], [], [], [], [], []) ->
          (toolName, InactiveDeuceTransform, Possible)
        (_, _, _, [], _, _, _, _, _) ->
          (toolName, InactiveDeuceTransform, Impossible)
        ([], [], [], pathedPatIds, [], [], [], [(Before, eId)], []) ->
          let allAreLets = pathedPatIds |> List.all (pathedPatIdToScopeEId >> eidIsLet) in
          if allAreLets then
            ( Utils.perhapsPluralizeList toolName pathedPatIds
            , NoInputDeuceTransform <| \() ->
                CodeMotion.duplicateDefinitionsBeforeEId
                  model.syntax
                  pathedPatIds
                  eId
                  model.inputExp
            , Satisfied
            )
          else
            (toolName, InactiveDeuceTransform, Impossible)

        ([], [], [], pathedPatIds, [], [], [], [], [patTarget]) ->
          let
            targetPathedPatId =
              patTargetPositionToTargetPathedPatId patTarget

            allAreLets =
              (pathedPatIds |> List.all (pathedPatIdToScopeEId >> eidIsLet)) &&
              eidIsLet (pathedPatIdToScopeEId targetPathedPatId)
          in
          if allAreLets then
            ( Utils.perhapsPluralizeList toolName pathedPatIds
            , NoInputDeuceTransform <| \() ->
                CodeMotion.duplicateDefinitionsPat
                  model.syntax
                  pathedPatIds
                  targetPathedPatId
                  model.inputExp
            , Satisfied
            )
          else
            (toolName, InactiveDeuceTransform, Impossible)
        _ ->
          (toolName, InactiveDeuceTransform, Impossible)
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
    (nums, _, _, _, _, _, _, _, _) =
      selections

    (mode, predVal) =
      let
        freezeAnnotations =
          List.map (\(_,(_,_,(_,frzn,_),_)) -> frzn) nums
      in
        if selections == ([], [], [], [], [], [], [], [], []) then
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
          ("Thaw/Freeze", InactiveDeuceTransform)
        Just (toolName, newAnnotation) ->
          ( toolName
          , NoInputDeuceTransform <| \() ->
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
                [ basicTransformationResult
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
    (nums, _, _, _, _, _, _, _, _) =
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
      if selections == ([], [], [], [], [], [], [], [], []) then
        Possible
      else if mode /= Nothing then
        Satisfied
      else
        Impossible

    (name, func) =
      case mode of
        Nothing ->
          ( "Show/Hide Sliders"
          , InactiveDeuceTransform
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
            , NoInputDeuceTransform <| \() ->
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
                  [ basicTransformationResult
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
    (nums, _, _, _, _, _, _, _, _) =
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
      if selections == ([], [], [], [], [], [], [], [], []) then
        Possible
      else if oneOrMoreNumsOnly selections then
        Satisfied
      else
        Impossible

    (name, func) =
      case mode of
        Nothing ->
          ( "Add/Remove Sliders"
          , InactiveDeuceTransform
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
            , NoInputDeuceTransform <| \() ->
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
                  [ basicTransformationResult
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
        ([], [], [], [], [], [], [], [], []) ->
          (toolName, InactiveDeuceTransform, Possible)
        ([], _, _, _, _, _, _, _, _) ->
          (toolName, InactiveDeuceTransform, Impossible)
        (nums, [], exps, [ppid], [], [], [], [], []) ->
          case findExpByEId model.inputExp (pathedPatIdToScopeEId ppid) of
            Just scopeExp ->
              if isLet scopeExp && List.length nums == List.length exps then
                ( Utils.perhapsPluralizeList toolName nums
                , CodeMotion.rewriteOffsetTransformation model ppid nums |> mbThunkToTransform
                , Satisfied
                )
              else
                (toolName, InactiveDeuceTransform, Impossible)

            _ ->
              (toolName, InactiveDeuceTransform, Impossible)

        _ ->
          (toolName, InactiveDeuceTransform, Impossible)
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
      , InactiveDeuceTransform
      , Impossible
      )

    (name, func, predVal) =
      case selections of
        ([], [], [], [], [], [], [], [], []) ->
          ( baseName
          , InactiveDeuceTransform
          , Possible
          )
        ([], literals, exps, [], [], [], [], [], []) ->
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
                        , NoInputDeuceTransform <|
                            \() ->
                              [ newExp1
                                  |> basicTransformationResult "RGBA"
                              , newExp2
                                  |> basicTransformationResult "Color Number (Hue Only)"
                                  |> mapSynthesisResult (setResultSafe False)
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
        ([], [], [], [], [], [], [], [], []) ->
          (InactiveDeuceTransform, Possible)
        ([], [], [], [pathedPatId], [], [], [], [], []) ->
          case
            LangTools.findScopeExpAndPatByPathedPatternId pathedPatId model.inputExp
              |> Maybe.map (\((e, id), p) -> (unwrapExp e, id, p.val.p__))
          of
            Just (ELet _ _ _ _ _, id, PVar _ ident _) ->
              ( NoInputDeuceTransform <| \() ->
                  CodeMotion.abstractPVar model.syntax pathedPatId [] model.inputExp
              , FullySatisfied
              )
            _ ->
              (InactiveDeuceTransform, Impossible)
        (_, _, [eid], [], [], [], [], [], []) ->
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
              ( NoInputDeuceTransform <| \() -> CodeMotion.abstractExp model.syntax eid model.inputExp
              , FullySatisfied
              )
            else
              (InactiveDeuceTransform, Impossible)
        ([], [], [], [], [], [(letEId, bindingNum)], [], [], []) ->
          case
            LangTools.justFindExpByEId model.inputExp letEId
              |> LangTools.expToMaybeLetPat
              |> Maybe.andThen (
                flip Utils.nth bindingNum >>
                Result.toMaybe)
              |> Maybe.map (.val >> .p__)
          of
            Just (PVar _ _ _) ->
              ( NoInputDeuceTransform <|
                  \() ->
                    let
                       pathedPatId = ((letEId, 1), [])
                    in
                      CodeMotion.abstractPVar model.syntax pathedPatId [] model.inputExp
              , FullySatisfied
              )
            _ ->
              (InactiveDeuceTransform, Impossible)
        _ ->
          (InactiveDeuceTransform, Impossible)
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
        ([], [], [], [], [], [], [], [], []) ->
          (InactiveDeuceTransform, Possible)
        (_, _, eids, ppids, [], [], [], [], []) ->
          case ppids |> List.map (\ppid -> LangTools.findBoundExpByPathedPatternId ppid model.inputExp) |> Utils.projJusts of
            Just boundExps ->
              let argEIds = Utils.dedup <| eids ++ List.map expEId boundExps in
              let enclosingPPIds =
                let ancestors = commonAncestors (\e -> List.member (expEId e) argEIds) model.inputExp in
                let ancestorEIds = ancestors |> List.map expEId in
                ancestors
                |> List.concatMap (\e -> case unwrapExp e of
                   ELet _ _ (Declarations _ _ _ letexps) _ _ ->
                     elemsOf letexps |> List.filterMap (\(LetExp _ _ p _ _ boundExp) ->
                       let shouldKeep =
                         (p |> LangTools.patToMaybePVarIdent |> flip List.member [Nothing, Just "main"] >> not) &&
                         (not <| isFunc boundExp) &&
                         List.member (expEId boundExp) ancestorEIds
                       in
                       if shouldKeep then
                         Just p
                       else
                         Nothing
                     ) |> List.filterMap (.val >> .pid >> pidToPathedPatternId model.inputExp)
                   _ -> [])
                {- TODO the matching stuff likely needs to be updated in light of declarations
                |> List.concatMap
                    (\(letExp, LetExp _ _ p _ _ e1) ->
                      LangTools.tryMatchExpPatToPaths p e1
                      |> List.filter (\(path, boundExp) -> not (isFunc boundExp))
                      |> List.filter (\(path, boundExp) -> List.member (expEId boundExp) ancestorEIds) -- Incidentally, this also filters out trivial abstractions (e.g. (let x 5) -> (let x (\n -> n))) b/c boundExp must be ancestor of an arg, not an arg itself.
                      |> List.map    (\(path, boundExp) -> (((expEId letExp), 1), path))
                    )
                -}
              in
              case enclosingPPIds of
                [] ->
                  (InactiveDeuceTransform, Impossible)

                _ ->
                  ( NoInputDeuceTransform (\() -> enclosingPPIds |> List.concatMap (\ppid -> CodeMotion.abstractPVar model.syntax ppid argEIds model.inputExp))
                  , Satisfied
                  )

            _ ->
              (InactiveDeuceTransform, Impossible)

        _ ->
          (InactiveDeuceTransform, Impossible)
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
            List.map Basic <|
              ExpressionBasedTransform.cloneEliminationSythesisResults
                candidateExpFilter minCloneCount 2 model.inputExp
      in
        if mergeResults /= [] then
          ( NoInputDeuceTransform <| \() -> mergeResults
          , Satisfied
          )
        else
          (InactiveDeuceTransform, Impossible)

    (func, predVal) =
      case selections of
        ([], [], [], [], [], [], [], [], []) ->
          (InactiveDeuceTransform, Possible)

        ([], [], [_], [], [], [], [], [], []) ->
          (InactiveDeuceTransform, Possible)

        ([], [], [], [], [], [_], [], [], []) ->
          (InactiveDeuceTransform, Possible)

        (_, _, [], [], [], ((letEId1, bind1)::(letEId2, bind2)::restLetEIds) as list, [], [], []) ->
          let boundExpEIds =
            list |> List.map (\(eid, bind) ->
               LangTools.justFindExpByEId model.inputExp eid |>
               LangTools.expToLetBoundExp |>
               flip Utils.nth bind |>
               Utils.fromOk "DeuceTools: bind should have been a integer in range !" |>
               expEId)
          in
          tryMerge boundExpEIds

        (_, _, eid1::eid2::restEIds, [], [], [], [], [], []) ->
          tryMerge (eid1::eid2::restEIds)

        _ ->
          (InactiveDeuceTransform, Impossible)
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
      , func = InactiveDeuceTransform
      , reqs = makeReqs predVal
      , id = id
      }

    disabledTool = defaultTool Impossible
  in
    case selections of
      (_, _, [], [], [], [], [], [], []) ->
        defaultTool Possible

      (_, _, [], [], [], [], [], [], [patTarget]) ->
        defaultTool Possible

      (_, _, firstEId::restEIds, [], [], [], [], [], patTargets) ->
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
                NoInputDeuceTransform <|
                  \() ->
                    targetPPIdsToTry
                    |> List.concatMap (\targetPPId -> CodeMotion.addArg model.syntax eid targetPPId model.inputExp)
            , reqs = makeReqs Satisfied
            , id = id
            }

          (_::_::_, _::_) ->
            { name = "Add Arguments" -- Fowler calls this "Add Parameter"
            , func =
                NoInputDeuceTransform <|
                  \() ->
                    targetPPIdsToTry
                    |> List.concatMap (\targetPPId -> CodeMotion.addArgs model.syntax eids targetPPId model.inputExp)
            , reqs = makeReqs Satisfied
            , id = id
            }

          _ ->
            disabledTool

      ( _, _, [], firstArgSourcePathedPatId::restArgSourcePathedPatId, [], [], [], [], patTargets) ->
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
                NoInputDeuceTransform <|
                  \() ->
                    targetPPIdsToTry
                    |> List.concatMap (\targetPPId -> CodeMotion.addArgFromPat model.syntax argSourcePathedPatId targetPPId model.inputExp)
            , reqs = makeReqs Satisfied
            , id = id
            }

          (_::_::_, _::_, True) ->
            { name = "Add Arguments" -- Fowler calls this "Add Parameter"
            , func =
                NoInputDeuceTransform <|
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
      , func = InactiveDeuceTransform
      , reqs = makeReqs predVal
      , id = id
      }

    disabledTool = defaultTool Impossible
  in
    case selections of
      ([], [], [], [], [], [], [], [], []) ->
        defaultTool Possible
      (_, _, [], [], _, _, _, _, _) ->
        disabledTool
      ([], [], [], pathedPatIds, [], [], [], [], []) ->
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
                NoInputDeuceTransform <|
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
                NoInputDeuceTransform <|
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
      (_, _, eids, [], [], [], [], [], []) ->
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
                NoInputDeuceTransform <|
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
                NoInputDeuceTransform <|
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
        (_, _, [], [], [], [], [], [], []) ->
          (InactiveDeuceTransform, Possible)

        (_, _, [], pathedPatIds, [], [], [], [], []) ->
          let scopeEIds = List.map pathedPatIdToScopeEId pathedPatIds in
          if Utils.allSame scopeEIds && (scopeEIds |> List.all (findExpByEId model.inputExp >> Maybe.map isFunc >> (==) (Just True)))
          then (InactiveDeuceTransform, Possible)
          else (InactiveDeuceTransform, Impossible)

        (_, _, eids, [], [], [], [], [], []) ->
          case eids |> List.map (LangTools.eidToMaybeCorrespondingArgumentPathedPatId model.inputExp) |> Utils.projJusts of
            Just _  -> (InactiveDeuceTransform, Possible)
            Nothing -> (InactiveDeuceTransform, Impossible)

        ([], [], [], pathedPatIds, [], [], [], [], [patTarget]) ->
          let targetPathedPatId = patTargetPositionToTargetPathedPatId patTarget in
          let scopeIds          = List.map pathedPatIdToScopeId (targetPathedPatId::pathedPatIds) in
          let targetScopeEId    = pathedPatIdToScopeEId targetPathedPatId in
          case (Utils.allSame scopeIds, targetScopeEId |> findExpByEId model.inputExp |> Maybe.map unwrapExp) of
            (True, Just (EFun _ _ _ _)) ->
              ( NoInputDeuceTransform <|
                  \() ->
                    CodeMotion.reorderFunctionArgs
                        targetScopeEId
                        (List.map pathedPatIdToPath pathedPatIds)
                        (pathedPatIdToPath targetPathedPatId)
                        model.inputExp

              , Satisfied
              )

            _ ->
                (InactiveDeuceTransform, Impossible)

        (_, _, eids, [], [], [], [], [(beforeAfter, eid)], []) ->
          case eid::eids |> List.map (LangTools.eidToMaybeCorrespondingArgumentPathedPatId model.inputExp) |> Utils.projJusts of
            Just (targetReferencePathedPatId::pathedPatIds) ->
              let targetPathedPatId = patTargetPositionToTargetPathedPatId (beforeAfter, targetReferencePathedPatId, 0) in
              let scopeIds = List.map pathedPatIdToScopeId (targetPathedPatId::pathedPatIds) in
              let targetEId = pathedPatIdToScopeEId targetPathedPatId in
              case (Utils.allSame scopeIds, targetEId |> findExpByEId model.inputExp |> Maybe.map unwrapExp) of
                (True, Just (EFun _ _ _ _)) ->
                  ( NoInputDeuceTransform <|
                      \() ->
                        CodeMotion.reorderFunctionArgs
                            targetEId
                            (List.map pathedPatIdToPath pathedPatIds)
                            (pathedPatIdToPath targetPathedPatId)
                            model.inputExp
                  , Satisfied
                  )
                _ ->
                (InactiveDeuceTransform, Impossible)
            _ ->
              (InactiveDeuceTransform, Impossible)
        _ ->
          (InactiveDeuceTransform, Impossible)
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
  , func = CodeMotion.reorderExpressionsTransformation model.inputExp selections |> mbThunkToTransform
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
            (_, _, [eid], [], [], [], [], [], []) ->
              Just (eid, True)
            ([], [], [], [], [], [(letEId, bindingNum)], [], [], []) ->
              findExpByEId model.inputExp letEId
                |> Maybe.andThen (\e -> findLetexpByBindingNumber e bindingNum)
                |> Maybe.map bindingOfLetExp
                |> Maybe.map (\letBoundExp -> (expEId letBoundExp, False))
            _ ->
              Nothing
      in
        case maybeEIdToDeLineAndWhetherToPreservePrecedingWhitespace of
          Nothing ->
            InactiveDeuceTransform
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
                NoInputDeuceTransform <|
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
                        |> basicTransformationResult "Make Single Line"
                        |> List.singleton
              else
                InactiveDeuceTransform
    , reqs =
        [ { description = "Select one expression or definition"
          , value =
              -- just duplicating this case from above
              case selections of
                ([], [], [], [], [], [], [], [], []) ->
                  Possible
                (_, _, [eid], [], [], [], [], [], []) ->
                  FullySatisfied
                ([], [], [], [], [], [letEId], [], [], []) ->
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
        (_, _, [eid], [], [], [], [], [], []) ->
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
                  InactiveDeuceTransform
                else
                  NoInputDeuceTransform <|
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
                          |> basicTransformationResult "Make Multi-line"
                          |> List.singleton
              EApp ws1 (Expr e) es appType ws2 ->
                if
                  es |>
                    List.all (precedingWhitespace >> String.contains "\n")
                then
                  InactiveDeuceTransform
                else
                  NoInputDeuceTransform <|
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
                          |> basicTransformationResult "Make Multi-line"
                          |> List.singleton
              _ ->
                InactiveDeuceTransform
        _ ->
          InactiveDeuceTransform
  , reqs =
      [ { description = "Select one expression"
        , value =
            -- just duplicating this case from above
            case selections of
              ([], [], [], [], [], [], [], [], []) ->
                Possible
              (_, _, [eid], [], [], [], [], [], []) ->
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
        (_, _, eid1::eid2::restEIds, [], [], [], [], [], []) ->
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
              InactiveDeuceTransform
            else
              NoInputDeuceTransform <|
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
                      |> basicTransformationResult "Align Expressions"
                      |> List.singleton
        _ ->
          InactiveDeuceTransform
  , reqs =
      [ { description = "Select two or more expressions"
        , value =
            -- just duplicating this case from above
            case selections of
              ([], [], [], [], [], [], [], [], []) ->
                Possible
              (_, _, eid1::eid2::restEIds, [], [], [], [], [], []) ->
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

typesToolId : String
typesToolId =
  "typeInfo"

typesTool : Model -> DeuceSelections -> DeuceTool
typesTool model selections =
  let
    (func, boolPredVal) =
      case selections of
        ([], [], [], [], [], [], [], [], []) ->
          (InactiveDeuceTransform, Possible)

        -- single expression, nothing else
        (_, _, [eId], [], [], [], [], [], []) ->
          let
            exp = LangTools.justFindExpByEId model.inputExp eId
          in
          (NoInputDeuceTransform (Types2.makeDeuceExpTool model.inputExp exp), Satisfied)

        -- single pattern, nothing else
        (_, _, [], [pathedPatId], [], [], [], [], []) ->
          -- TODO ensure this works?
          case LangTools.findPatByPathedPatternId pathedPatId model.inputExp of
            Nothing ->
              (InactiveDeuceTransform, Impossible)

            Just pat ->
              (NoInputDeuceTransform (Types2.makeDeucePatTool model.inputExp pat), Satisfied)

        -- single type, nothing else
        (_, _, [], [], [tId], [], [], [], []) ->
          -- let
          --   transformationResults () =
          --     [ Label <| PlainText <| toString tId
          --     ]
          -- in
          -- (NoInputDeuceTransform transformationResults, Satisfied)
          --
          (InactiveDeuceTransform, Impossible)

        _ ->
          (InactiveDeuceTransform, Impossible)
  in
    { name = "Type Information"
    , func = func
    , reqs = [ { description = "Select something.", value = boolPredVal } ]
    , id = typesToolId
    }


--------------------------------------------------------------------------------
-- Format Tool
--------------------------------------------------------------------------------

-- Not currently factoring and composing the formatting of different
-- kinds of Exp nodes.

formatTool : Model -> DeuceSelections -> DeuceTool
formatTool model selections =
  let
    (func, boolPredVal) =
      case selections of
        ([], [], [], [], [], [], [], [], []) ->
          (InactiveDeuceTransform, Possible)

        (_, _, [eId], [], [], [], [], [], []) ->
          let
            exp =
              LangTools.justFindExpByEId model.inputExp eId

            replaceThisExp__ newE__ =
              replaceExpNode eId (replaceE__ exp newE__) model.inputExp
          in
            case (unExpr exp).val.e__ of
              EList ws1 wsExps ws2 maybeTail ws3 ->
                let
                  rebuild wsExps ws3 =
                    EList ws1 wsExps ws2 maybeTail ws3
                      |> replaceThisExp__

                  simpleSingleLine =
                    let
                      newWsExps =
                        wsExps
                          |> Utils.mapi0 (\(i, (_, e_i)) ->
                               let ws_i = if i == 0 then "" else " " in
                               ( space0
                               , replacePrecedingWhitespace ws_i e_i
                               )
                             )
                    in
                      rebuild newWsExps space0

                  simpleMultiLine =
                    let
                      startCol =
                        (unExpr exp).start.col

                      breakAndIndent =
                        "\n" ++ String.repeat (startCol - 1) " "

                      newWsExps =
                        wsExps
                          |> Utils.mapi0 (\(i, (_, e_i)) ->
                               ( ws <| if i == 0 then " " else breakAndIndent
                               , replacePrecedingWhitespace " " e_i
                               )
                             )
                    in
                      rebuild newWsExps (ws breakAndIndent)

                  func () =
                    [ basicTransformationResult "Single-Line" simpleSingleLine
                    , basicTransformationResult "Multi-Line" simpleMultiLine
                    ]
                in
                  (NoInputDeuceTransform func, Satisfied)

              ELet wsBeforeLet letKind (Declarations po letTypes letAnnots letExps) wsBeforeIn body ->
                let
                  mapEachLetExp f =
                    letExps |> List.map (Tuple.mapSecond (List.map f))

                  rebuild : (String -> String) -> Maybe Int -> Maybe String -> Maybe String -> Exp

                  rebuild transformWsBeforeBeforeEachPat -- (String -> String)
                          maybePadWsBeforeEachEqualsTo   -- Maybe Int
                          maybeNewWsBeforeEachExp        -- Maybe String
                          maybeNewWsBeforeIn =           -- Maybe String
                    let
                      newLetExps =
                        mapEachLetExp (\(LetExp ws0 wsBeforeBeforePat pat fas ws2 expEquation) ->
                          let
                            newWsBeforeBeforePat =
                              wsBeforeBeforePat
                                |> mapWs transformWsBeforeBeforeEachPat

                            newPat =
                              -- there shouldn't be any whitespace before pat anyway
                              pat

                            newWsBeforeEquals =
                              maybePadWsBeforeEachEqualsTo
                                |> Maybe.map (\maxCol -> ws (String.repeat (1 + maxCol - ws2.start.col) " "))
                                |> Maybe.withDefault ws2

                            newWsBeforeExp =
                              maybeNewWsBeforeEachExp
                                |> Maybe.withDefault (precedingWhitespace expEquation)

                            newExpEquation =
                              replacePrecedingWhitespace newWsBeforeExp expEquation
                          in
                            LetExp ws0 newWsBeforeBeforePat newPat fas newWsBeforeEquals newExpEquation
                        )

                      newWsBeforeIn =
                        maybeNewWsBeforeIn
                          |> Maybe.map ws
                          |> Maybe.withDefault wsBeforeIn

                      newDecls =
                        Declarations po letTypes letAnnots newLetExps
                    in
                      ELet wsBeforeLet letKind newDecls newWsBeforeIn body
                        |> replaceThisExp__

                  (listEqualsSignCol, listWsBeforeExp) =
                    letExps
                      |> List.map Tuple.second
                      |> List.concat
                      |> List.map (\(LetExp _ _ _ _ wsBeforeEquals e) ->
                           (wsBeforeEquals.start.col, precedingWhitespace e)
                         )
                      |> List.unzip
                      |> Tuple.mapSecond Utils.dedup

                  makeSingleLine =
                    basicTransformationResult "Single Line"
                      (rebuild (always space1.val) Nothing (Just space1.val) (Just space1.val))

                  alignEqualsSigns =
                    let
                      maxEqualsSignCol =
                        List.maximum listEqualsSignCol
                          |> Utils.fromJust_ "maxEqualsSignCol"
                    in
                      basicTransformationResult "Remove Line Breaks and Align Equals Signs"
                        (rebuild (Regex.replace Regex.All (Regex.regex "(\\n)*\\n") (always "\n"))
                                 (Just maxEqualsSignCol)
                                 (Just space1.val)
                                 Nothing)

                  alignAllToSome =
                    listWsBeforeExp
                      |> List.map (\s ->
                           let caption =
                             if String.contains "\n" s then
                               "Match Indentation"
                             else
                               "Remove Line Breaks"
                           in
                             basicTransformationResult caption
                               (rebuild identity Nothing (Just s) Nothing)
                         )

                  numLetExp =
                    letExps
                      |> List.map Tuple.second
                      |> List.concat
                      |> List.length

                  func () =
                    if numLetExp == 1 then
                      [makeSingleLine]
                    else
                      alignAllToSome ++ [alignEqualsSigns]
                in
                (NoInputDeuceTransform func, Satisfied)

              EApp _ _ _ _ _ ->
                let
                  lineBreakAndIndent k =
                    "\n" ++ indent k

                  indent k =
                    String.repeat ((unExpr exp).start.col - 1) " " ++ String.repeat k "  "

                  addBackwardPipeline maybeIndent e =
                    case (unExpr e).val.e__ of
                      EApp ws1 eFunc eArgs SpaceApp ws2 ->
                        case Utils.split (List.length eArgs - 1) eArgs of
                          (prefixArgs, [lastArg]) ->
                            case (unExpr lastArg).val.e__ of
                              EParens _ innerLastArg Parens _ ->
                                let
                                  maybeIndentPlusOne =
                                    maybeIndent
                                      |> Maybe.map ((+) 1)

                                  wsNewLastArg =
                                    maybeIndent
                                      |> Maybe.map lineBreakAndIndent
                                      |> Maybe.withDefault " "

                                  newLastArg =
                                    addBackwardPipeline maybeIndentPlusOne innerLastArg
                                      |> replacePrecedingWhitespace wsNewLastArg

                                  newFunc =
                                    -- HACK: going through EVar
                                    withDummyExpInfo <|
                                      EVar space0 <|
                                        String.join " " <|
                                          List.map
                                            (String.trim << Syntax.unparser model.syntax)
                                            (eFunc::prefixArgs)
                                in
                                  EApp ws1 newFunc [newLastArg] (LeftApp space1) ws2
                                    |> replaceE__ e

                              _ ->
                                e
                          _ ->
                            e
                      _ ->
                        e

                  addForwardPipeline multi e =
                    case (unExpr e).val.e__ of
                      EApp ws1 eFunc eArgs SpaceApp ws2 ->
                        case Utils.split (List.length eArgs - 1) eArgs of
                          (prefixArgs, [lastArg]) ->
                            case (unExpr lastArg).val.e__ of
                              EParens _ innerLastArg Parens _ ->
                                let
                                  newLastArg =
                                    addForwardPipeline multi innerLastArg

                                  wsBeforePipe =
                                    if multi then
                                      ws (lineBreakAndIndent 1)
                                    else
                                      space1

                                  newFunc =
                                    -- HACK: going through EVar
                                    withDummyExpInfo <|
                                      EVar space1 <|
                                        String.join " " <|
                                          List.map
                                            (String.trim << Syntax.unparser model.syntax)
                                            (eFunc::prefixArgs)
                                in
                                  EApp space0 newFunc [newLastArg] (RightApp wsBeforePipe) ws2
                                    |> replaceE__ e

                              _ ->
                                e |> replacePrecedingWhitespace (lineBreakAndIndent 0)
                          _ ->
                            e |> replacePrecedingWhitespace (lineBreakAndIndent 0)
                      _ ->
                        e |> replacePrecedingWhitespace (lineBreakAndIndent 0)

                  rewriteAndFinish f =
                    (unExpr (f exp)).val.e__
                      |> replaceThisExp__
                      |> Syntax.unparser model.syntax
                      |> Syntax.parser model.syntax
                      |> Result.withDefault (eStr "Bad Format EApp. Bad editor. Bad")

                  transforms =
                    [ ( "Forward Pipeline Single-Line"
                      , rewriteAndFinish (addForwardPipeline False)
                      )
                    , ( "Forward Pipeline Multi-Line"
                      , rewriteAndFinish (addForwardPipeline True)
                      )
                    , ( "Backward Pipeline Single-Line"
                      , rewriteAndFinish (addBackwardPipeline Nothing)
                      )
                    , ( "Backward Pipeline Multi-Line"
                      , rewriteAndFinish (addBackwardPipeline (Just 1))
                      )
                    ]
                    |> List.filter (\(_, newExp) ->
                         String.trim (Syntax.unparser model.syntax newExp) /= String.trim model.code
                       )
                in
                case transforms of
                  [] ->
                    (InactiveDeuceTransform, Impossible)

                  _ ->
                    let func () =
                      transforms
                        |> List.map (\(text, newExp) ->
                             Fancy
                               (synthesisResult "DUMMY DESCRIPTION" newExp)
                               (PlainText text)
                           )
                    in
                    (NoInputDeuceTransform func, Satisfied)

              _ ->
                (InactiveDeuceTransform, Impossible)

        (_, _, [], [], [], [], [], [], []) ->
          (InactiveDeuceTransform, Impossible)

        (_, _, [], [], [], [_], [], [], []) ->
          (InactiveDeuceTransform, Impossible)

        (_, _, [], [], [], equations, [], [], []) ->
          let
            (eIds, bindingNums) =
              List.unzip equations
          in
            case Utils.dedup eIds of
              [eId] ->
                let
                  expLet =
                    findExpByEId model.inputExp eId
                      |> Utils.fromJust_ "DeuceTools Format findExpByEId"

                  listLetExp : List LetExp
                  listLetExp =
                    bindingNums
                      |> List.map (findLetexpByBindingNumber expLet)
                      |> List.map (Utils.fromJust_ "DeuceTools Format findLetexpByBindingNumber")

                  -- Too much work for now to support individual BindingNums
                  -- of an ELet. Just use the version that formats every LetExp
                  -- in an ELet.
                in
                (InactiveDeuceTransform, Impossible)

              _ ->
                (InactiveDeuceTransform, Impossible)

        _ ->
          (InactiveDeuceTransform, Impossible)
  in
    { name = "Format"
    , func = func
    , reqs = [ { description = "Select something.", value = boolPredVal } ]
    , id = "format"
    }

expandFormatViaHotkey : Model -> DeuceWidget -> Maybe Exp
expandFormatViaHotkey = runFunctionViaHotkey expandFormat

expandFormat old selections =
  case selections of
    (_, _, [eId], [], [], [], [], [], []) ->
      let
        oldRoot = old.inputExp
        exp = LangTools.justFindExpByEId oldRoot eId
        naturalIndent =
          LangTools.getProperIndentationIfBody oldRoot exp
          |> Maybe.withDefault (indentationAt eId oldRoot ++ "  ")
        naturalIndentNewLine = "\n" ++ naturalIndent
        return e__ = replaceExpNode eId (replaceE__ exp e__) oldRoot |> Just
      in
      case unwrapExp exp of
        EFun wsb ps body wsa ->
          EFun wsb ps (replacePrecedingWhitespace naturalIndentNewLine body) wsa
          |> return
        ELet _ lk decls wsIn body ->
          replacePrecedingWhitespace naturalIndentNewLine exp
          |> Just
        EIf wsb cond wsBeforeThen t wsBeforeElse f wsa ->
          let
            elseIndent = "\n" ++ indentationAt eId oldRoot
            childIndent = elseIndent ++ "  "
            newTrue = replacePrecedingWhitespace childIndent t
            newFalse = replacePrecedingWhitespace childIndent f
          in
          EIf wsb cond wsBeforeThen newTrue (ws elseIndent) newFalse wsa
          |> return
        EList ws1 wsExps ws2 maybeTail ws3 ->
          let
            startCol = (unExpr exp).start.col
            breakAndIndent = "\n" ++ String.repeat (startCol - 1) " "
            newWsExps =
              wsExps
                |> Utils.mapi0 (\(i, (_, e_i)) ->
                     ( ws <| if i == 0 then " " else breakAndIndent
                     , replacePrecedingWhitespace " " e_i
                     )
                   )
          in
          EList ws1 newWsExps ws2 maybeTail (ws breakAndIndent)
          |> return
        _ ->
          Nothing
    _ ->
      Nothing

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
  , selectedTIds selectedWidgets
  , selectedEquationEIds selectedWidgets
  , selectedDeclTargets selectedWidgets
  , selectedEIdTargets selectedWidgets
  , selectedPathedPatIdTargets selectedWidgets
  )

toolNeedsHovers : Model -> String -> Bool
toolNeedsHovers model toolId =
  Utils.and
    [ model.codeEditorMode == Model.CETypeInspector
    , Model.noCodeWidgetsSelected model
    , toolId == typesToolId
    ]

toolList =
  [ [ typesTool
    , (\model selections -> Types2.introduceTypeAliasTool model.inputExp selections)
    , (\model selections -> Types2.renameTypeTool model.inputExp selections)
    , (\model selections -> Types2.renameDataConstructorTool model.inputExp selections)
    , (\model selections -> Types2.duplicateDataConstructorTool model.inputExp selections)
    , (\model selections -> Types2.convertToDataTypeTool model.inputExp selections)
    ]
  , ( mergeTools "holeReplacementMerger" "Replace hole (select from menu)" "Select a hole"
    [ createTrueTool
    , createFalseTool
    , createEmptyStringTool
    , createLambdaTool
    , createApplicationTool
    , createEmptyListTool
    , createCondTool
    , createCaseTool
    , createLetTool
    , createTypeAscriptionTool
    , createParenthesizedTool
    , createRecordTool
    ]) ::
    [ createVarTool ]

-- TODO: get Deuce tools to work with the ELet AST
  , [ createFunctionTool
    , createFunctionFromArgsTool
    -- , mergeTool
    ]
  , [ addArgumentsTool ]
{-
    , removeArgumentsTool
    , reorderArgumentsTool
    ]
-}
  , [ renameVariableTool
    , introduceVariableTool
    ]
{-
    , swapNamesAndUsagesTool
    , swapUsagesTool
    ]
-}  , [ makeEqualTool
    --, copyExpressionTool
    ]
  , [ moveDefinitionTool
    --, swapDefinitionsTool
    , inlineDefinitionTool
    --, duplicateDefinitionTool
    ]
 {- , [ reorderExpressionsTool
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
-}
  , [ formatTool
    ]
  ]

deuceToolsOf : Model -> List (List DeuceTool)
deuceToolsOf model =
  let
    selectedDeuceWidgets =
      Model.getAllSelected model

    selections =
      selectionsTuple
        model.inputExp
        selectedDeuceWidgets
  in
    List.map
      ( List.map
          ( \tool ->
              let
                appliedTool =
                  tool model selections
              in
                if toolNeedsHovers model appliedTool.id then
                  let
                    hoveredDeuceWidgets =
                      model.deuceState.hoveredWidgets

                    selectionsAndHovers =
                      selectionsTuple
                        model.inputExp
                        ( Utils.removeDuplicates <|
                            selectedDeuceWidgets ++ hoveredDeuceWidgets
                        )
                  in
                    tool model selectionsAndHovers
                else
                  appliedTool
          )
      )
      toolList

createToolCache : Model -> List (List CachedDeuceTool)
createToolCache model =
  deuceToolsOf model |> List.map (
    List.map (flip runTool emptyDeuceState)
  )

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

updateInputSensitiveToolsInCache almostNewModel =
  let
    cachedAndNewDeuceTools =
      Utils.zipWith Utils.zip
        almostNewModel.deuceToolsAndResults
        (deuceToolsOf almostNewModel)
          -- assumes that the new tools computed by deuceTools
          -- are the same as the cached ones
    reRun newDeuceTool = runTool newDeuceTool almostNewModel.deuceState
  in
  cachedAndNewDeuceTools |> List.map (
    List.map (\((cachedDeuceTool, _, _) as cached, newDeuceTool) ->
      case cachedDeuceTool.func of
        InactiveDeuceTransform        -> reRun newDeuceTool
        NoInputDeuceTransform _       -> cached
        RenameDeuceTransform _        -> reRun newDeuceTool
        SmartCompleteDeuceTransform _ -> reRun newDeuceTool
    )
  )

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

basicDeuceTransform : List TransformationResult -> DeuceTransformation
basicDeuceTransform result =
  NoInputDeuceTransform <| \() -> result

mbThunkToTransform : Maybe (() -> List TransformationResult) -> DeuceTransformation
mbThunkToTransform mbThunk =
  case mbThunk of
    Nothing -> InactiveDeuceTransform
    Just thunk -> NoInputDeuceTransform thunk

mergeTools : String -> String -> String -> List (Model -> Selections -> DeuceTool) -> Model -> Selections -> DeuceTool
mergeTools id name description toolBuilders model selections =
  let
    isSatisfied val =
      List.member val [Satisfied, FullySatisfied]
    (thunk, predVal) =
      List.foldl (\toolBuilder (thunkAcc, predValAcc) ->
        let
          {func, reqs} = toolBuilder model selections
          req = Utils.head_ reqs
          predValCur = req.value
          mbThunkCur = case func of
            NoInputDeuceTransform thunk -> Just thunk
            _                           -> Nothing
          combineThunks a b () =
            a () ++ b ()
        in
        if not <| isSatisfied predValCur || isSatisfied predValAcc then
          if predValCur == Possible || predValAcc == Possible then
            (thunkAcc, Possible)
          else
            (thunkAcc, Impossible)
        else case (mbThunkCur, predValCur, predValAcc) of
          (Nothing, _, _) ->
            (thunkAcc, predValAcc)
          (Just thunkCur, FullySatisfied, FullySatisfied) ->
            (combineThunks thunkAcc thunkCur, FullySatisfied)
          (Just thunkCur, _, _) ->
            (combineThunks thunkAcc thunkCur, Satisfied)
      ) (always [], Impossible) toolBuilders
    func =
      if isSatisfied predVal then
        NoInputDeuceTransform thunk
      else
        InactiveDeuceTransform
  in
  { name = name
  , func = func
  , reqs = [ { description = description, value = predVal } ]
  , id = id
  }

runFunctionViaHotkey : (Model -> Selections -> Maybe Exp) -> Model -> DeuceWidget -> Maybe Exp
runFunctionViaHotkey function old selectedWidget =
  let thunk () =
    function old <| selectionsTuple old.inputExp [selectedWidget]
  in
  case ImpureGoodies.crashToError thunk of
    Ok exp ->
      exp
    Err errMsg ->
      let _ =
        Debug.log <| "Deuce Function Via Hotkey Crash :" ++ toString errMsg
      in
      Nothing

runInputBasedToolViaHotkey :
  (Model -> Selections -> DeuceTool) -> Model -> DeuceWidget -> (String, String -> List TransformationResult)
runInputBasedToolViaHotkey toolBuilder old selectedWidget =
  let
    deuceTool = toolBuilder old <| selectionsTuple old.inputExp [selectedWidget]
    textToResults =
      case deuceTool.func of
        RenameDeuceTransform renameVarTextToResults ->
          renameVarTextToResults
        SmartCompleteDeuceTransform smartCompleteTextToResults ->
          smartCompleteTextToResults
        _ ->
          always []
  in
  ( deuceTool.name
  , (\text ->
      case ImpureGoodies.crashToError (\() -> textToResults text) of
        Ok results ->
          results
        Err errMsg ->
          let _ =
            Debug.log ("Deuce Input Based Tool Via Hotkey Crash '" ++ deuceTool.name ++ "'") (toString errMsg)
          in
          []
    )
  )

runToolViaHotkey : (Model -> Selections -> DeuceTool) -> Model -> DeuceWidget -> Maybe Exp
runToolViaHotkey toolBuilder old selectedWidget =
  let
    deuceTool : DeuceTool
    deuceTool = toolBuilder old <| selectionsTuple old.inputExp [selectedWidget]

    run : (() -> List TransformationResult) -> Maybe Exp
    run thunk =
      case ImpureGoodies.crashToError thunk of
        Ok [singleResult] ->
          extractSynthesisResultWith Model.resultExp singleResult
        Ok results ->
          let _ =
            Debug.log <| "Deuce Tool Via Hotkey too many results: " ++ toString results
          in
          Nothing
        Err errMsg ->
          let _ =
            Debug.log ("Deuce Tool Via Hotkey Crash '" ++ deuceTool.name ++ "'") (toString errMsg)
          in
          Nothing
  in
  case deuceTool.func of
    NoInputDeuceTransform thunk -> run thunk
    _                           -> Nothing

-- Run a tool: get results back if it is active, otherwise no results
runTool : DeuceTool -> DeuceState -> CachedDeuceTool
runTool deuceTool deuceState =
  let run thunk =
    case ImpureGoodies.crashToError thunk of
      Ok results ->
        (deuceTool, results, False)
      Err errMsg ->
        let _ = Debug.log ("Deuce Tool Crash \"" ++ deuceTool.name ++ "\"") (toString errMsg) in
        (deuceTool, [], True)
  in
  case deuceTool.func of
    InactiveDeuceTransform ->
      (deuceTool, [], True)
    NoInputDeuceTransform thunk ->
      run thunk
    RenameDeuceTransform renameVarTextToResults ->
      run <| \() -> renameVarTextToResults deuceState.renameVarText
    SmartCompleteDeuceTransform smartCompleteTextToResults ->
      run <| \() -> smartCompleteTextToResults deuceState.smartCompleteText

-- Check if a tool is active without running it
isActive : Model.CodeEditorMode -> DeuceTool -> Bool
isActive mode deuceTool =
  deuceTool.func /= InactiveDeuceTransform
    && (mode /= Model.CETypeInspector || deuceTool.id == typesToolId)
