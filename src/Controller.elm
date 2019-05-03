port module Controller exposing
  ( update
  , msgNoop
  , msgWindowDimensions
  , msgVisibilityChange
  , msgCodeUpdate
  , msgKeyPress, msgKeyDown, msgKeyUp
  , msgMouseIsDown, msgMousePosition
  , msgRun, upstateRun, msgTryParseRun
  , msgUpdateValueEditor, msgUpdateHTMLEditor, msgCallUpdate
  , msgAceUpdate
  , msgUserHasTyped
  , msgOutputCanvasUpdate
  , msgUndo, msgRedo, msgCleanCode
  , msgDigHole, msgMakeEqual, msgRelate, msgIndexedRelate, msgBuildAbstraction
  , msgSelectSynthesisResult, msgClearSynthesisResults
  , msgStartAutoSynthesis, msgStopAutoSynthesisAndClear
  , msgHoverSynthesisResult, msgPreview, msgClearPreview
  , msgActivateRenameInOutput, msgUpdateRenameInOutputTextBox, msgDoRename
  , msgAddArg, msgRemoveArg
  , msgGroupBlobs, msgDuplicate, msgMergeBlobs, msgAbstractBlobs
  , msgReplicateBlob
  , msgToggleCodeBox
  , msgSetOutputGraphics, msgSetOutputHtmlText, doSetOutputHtmlText, msgSetOutputValueText, msgSetOutputPBE, msgSetOutputStructuredEditor
  , msgSetHeuristicsBiased, msgSetHeuristicsNone, msgSetHeuristicsFair
  , msgStartAnimation, msgRedraw, msgTickDelta
  , msgNextSlide, msgPreviousSlide
  , msgNextMovie, msgPreviousMovie
  , msgPauseResumeMovie
  , msgOpenDialogBox, msgCloseDialogBox
  , msgUpdateFilenameInput
  , msgLoadIcon
  , fileMessageHandler, fileMessageError
  , msgNew, msgSaveAs, msgSave, msgOpen, msgDelete
  , msgAskNew, msgAskOpen
  , msgConfirmFileOperation, msgCancelFileOperation
  , msgExportCode, msgExportHtml
  , msgImportCode, msgAskImportCode
  , msgMouseEnterCodeBox, msgMouseLeaveCodeBox
  , msgReceiveDotImage
  , msgMouseClickDeuceWidget
  , msgMouseEnterDeuceWidget, msgMouseLeaveDeuceWidget
  , msgChooseDeuceExp
  , msgHideMenu, msgToggleMenu -- , msgShowMenu
  , msgUpdateFontSize
  , msgSetToolMode
  , msgSetGhostsShown
  , msgDeuceTextBoxSetFocus
  , msgHoverDeuceResult
  , msgLeaveDeuceResult
  , msgUpdateRenameVarTextBox
  , msgUpdateDeuceKeyboardTextBox
  , msgUpdateSmartCompleteTextBox
  , msgClearDrag
  , msgDragDeucePopupPanel
  , msgDragAutoOutputToolsPopupPanel
  , msgDragEditCodePopupPanel
  , msgDragDeuceRightClickMenu
  , msgDragPbePopupPanel
  , msgTextSelect
  , msgClearPreviewDiff
  , msgSetExampleByName
  , msgSetEnableDeuceBoxSelection
  , msgSetEnableDeuceTextSelection
  , msgSetCodeToolsMenuMode
  , msgSetTextSelectMode
  , msgSetEnableTextEdits
  , msgSetAllowMultipleTargetPositions
  , msgSetSelectedDeuceTool
  , msgDeuceRightClick
  , msgDragMainResizer
  , msgResetInterfaceLayout
  , msgReceiveDeucePopupPanelInfo
  , msgSetColorScheme
  , msgSetSyntax
  , msgAskNextTemplate, msgAskPreviousTemplate
  , msgValuePathUpdate
  , msgAutoSync
  , msgSetCodeEditorMode
  , msgSetDoTypeChecking
  , msgAddSelectedHole
  , msgRemoveSelectedHole
  , msgSetSelectedUnExp
  , msgUnsetSelectedUnExp
  , msgUpdateHoleExampleInput
  , msgUpdateBackpropExampleInput
  , msgCollectAndSolve
  , msgShowUnExpPreview
  , msgClearUnExpPreview
  , msgSelectTSEFLLPPath, msgDeselectTSEFLLPPath, msgTSEFLLPApplyReplacement
  )

import Updatable exposing (Updatable)
import Info
import Lang exposing (..) --For access to what makes up the Vals
import Types2
import Ace
import ParserUtils exposing (showError)
-- import FastParser exposing (freshen)
import Parser as P
import LeoParser as Parser
import LeoUnparser
import LangTools
import LangUtils
import LangSimplify
import ValueBasedTransform
import Blobs exposing (..)
import Draw
import ExpressionBasedTransform as ETransform
import Sync
import Eval
import Evaluator
import TriEval
import UnLang as U exposing (UnExp, Constraints, World)
import PBESynthesis
import Update exposing (vStr, vList)
import UpdateUtils
import UpdateStack
import EvalUpdate
import Results exposing (Results)
import LazyList exposing (LazyList)
import Utils
import Keys
import Model exposing (..)
import Layout exposing
  ( canvasPosition
  , clickToCanvasPoint
  , deucePopupPanelMouseOffset
  , deuceRightClickMenuMouseOffset
  )
import AceCodeBox
import OutputCanvas exposing (DiffTimer)
import AnimationLoop
import FileHandler exposing (ExternalFileMessage(..), InternalFileMessage(..))
import File exposing (FileExtension, Filename, File, FileIndex)
import DeucePopupPanelInfo exposing (DeucePopupPanelInfo)
import ColorScheme
import SyntaxHighlight exposing (ExternalSyntaxHighlightMessage(..))
import LangSvg
import ShapeWidgets exposing (SelectableFeature(..), GenericFeature(..), ShapeFeature(..), RealZone(..), PointFeature(..), DistanceFeature(..), OtherFeature(..))
import ExamplesGenerated as Examples
import Config exposing (params)
import Either exposing (Either(..))
import DefaultIconTheme
import DependenceGraph exposing (lookupIdent)
import CodeMotion
import DeuceWidgets exposing (..) -- TODO
import DeuceTools
import Deuce
import ColorNum
import Syntax exposing (Syntax)
import LangUnparser -- for comparing expressions for equivalence
import History exposing (History)
import NonDet
import TinyStructuredEditorsForLowLowPrices

import ImpureGoodies exposing (nativeDict)

import VirtualDom

--Core Libraries
import List
import Dict exposing (Dict)
import Regex
import Set
import String
import Char
import Time

--Html Libraries
import Html
import Html.Attributes as Attr
import Html.Events as Events

--Svg Libraries
import Svg
import Svg.Attributes
import Svg.Events
import Svg.Lazy

import Mouse

--Error Checking Libraries
import Debug

--Other libraries
import PageVisibility exposing (Visibility(..))

import UpdatedEnv

import Json.Decode as JSDecode

import Lazy

import ValUnbuilder as Vu

--------------------------------------------------------------------------------

debugLog = Config.debugLog Config.debugController

--------------------------------------------------------------------------------

-- Returns:
--   ( Type-checked expression
--   , Hole environment
--   , Ace type information
--   , whether or not the expression type-checks
--   )
maybeTypeCheck : Bool -> Exp -> (Exp, Types2.HoleEnv, Types2.AceTypeInfo, Bool)
maybeTypeCheck doTypeChecking inputExp =
  if doTypeChecking then
    let
      (newInputExp, holeEnv) =
        Types2.typecheck inputExp

      aceTypeInfo =
        Types2.aceTypeInfo newInputExp

      typeChecks =
        Types2.typeChecks newInputExp
    in
      (newInputExp, holeEnv, aceTypeInfo, typeChecks)
  else
    (inputExp, Types2.emptyHoleEnv, Types2.dummyAceTypeInfo, True)

-- replace most uses of maybeTypeCheck with this.
--
maybeTypeCheckAndUpdateModel : Model -> Model
maybeTypeCheckAndUpdateModel model =
  let
    (newInputExp, holeEnv, aceTypeInfo, typeChecks) =
      maybeTypeCheck model.doTypeChecking model.inputExp
  in
    { model
        | inputExp =
            newInputExp
        , holeEnv =
            holeEnv
        , codeBoxInfo =
            updateCodeBoxInfo aceTypeInfo model
    }
      |> showTypeInspectorIfNecessary typeChecks

showTypeInspectorIfNecessary : Bool -> Model -> Model
showTypeInspectorIfNecessary typeChecks model =
  { model
      | codeEditorMode =
          -- if typeChecks then
          if True then
            model.codeEditorMode
          else
            CETypeInspector
  }

msgSetDoTypeChecking : Bool -> Msg
msgSetDoTypeChecking bool =
  NewModelAndCmd "Toggle Type Checking" <| \model ->
    let
      (newInputExp, holeEnv, aceTypeInfo, typeChecks) =
        maybeTypeCheck bool model.inputExp

      newModel =
        { model
            | doTypeChecking =
                bool
            , inputExp =
                newInputExp
            , holeEnv =
                holeEnv
            , codeBoxInfo =
                updateCodeBoxInfo aceTypeInfo model
        }
          |> showTypeInspectorIfNecessary typeChecks
          |> resetDeuceState
    in
      (newModel, Cmd.none)

refreshLiveInfo m =
  case mkLive
         m.syntax
         m.syncOptions
         m.slideNumber m.movieNumber m.movieTime
         m.inputExp
         (m.inputVal, m.widgets) of

    Ok liveInfo ->
      liveInfo

    Err s ->
      let _ = Debug.log "refreshLiveInfo Error" (toString s) in
      { initSubstPlus = Parser.substPlusOf m.inputExp
      , triggers = Dict.empty
      }

-- TODO refresh type highlights, too
refreshHighlights zoneKey model =
  let codeBoxInfo = model.codeBoxInfo in
  let hi = liveInfoToHighlights zoneKey model in
  { model | codeBoxInfo = { codeBoxInfo | highlights = hi } }

between1 i (j,k) = Utils.between i (j+1, k+1)

{-
cleanExp =
  mapExpViaExp__ <| \e__ -> case e__ of
    EApp _ e0 [e1,_,_] _ -> case (unwrapExp e0) of
      EVar _ "inferred"  -> (unwrapExp e1)
      _                  -> e__
    EApp _ e0 [_,e1] _   -> case (unwrapExp e0) of
      EVar _ "flow"      -> (unwrapExp e1)
      _                  -> e__
    EOp _ op [e1,e2] _   ->
      case (op.val, (unwrapExp e2)) of
        (Plus, EConst _ 0 _ _) -> (unwrapExp e1)
        _                      -> e__
    _                    -> e__
-}

{-
addSlateAndCode old (exp, val) =
  slateAndCode old (exp, val)
  |> Result.map (\(slate, code) -> (exp, val, slate, code))
-}

clearSelections : Model -> Model
clearSelections old =
  { old | selectedFeatures = Set.empty
        , selectedShapes   = Set.empty
        , selectedBlobs    = Dict.empty
        }


-- We may want to revisit our error handling so we don't have different error types floating around.
discardErrorAnnotations : Result (String, Ace.Annotation) a -> Result String a
discardErrorAnnotations result =
  result |> Result.mapError (\(string, annot) -> string)

runWithErrorHandling model exp onOk =
  let result =
    -- runWithErrorHandling is called after synthesis. Recompute line numbers.
    let reparsedResult = Syntax.unparser model.syntax exp |> Syntax.parser model.syntax |> (Result.mapError showError) in
    reparsedResult
    |> Result.andThen (\reparsed ->
       runAndResolve model reparsed
       |> Result.map (\(val, widgets, env, slate, code) -> onOk reparsed val env widgets slate code)
    )
  in
  handleError model result

handleError : Model -> Result String Model -> Model
handleError oldModel result =
  case result of
    Ok newModel -> newModel
    Err s       -> { oldModel | errorBox = Just s }

updateCodeBoxInfo : Types2.AceTypeInfo -> Model -> CodeBoxInfo
updateCodeBoxInfo ati m =
  let codeBoxInfo = m.codeBoxInfo in
  { codeBoxInfo | annotations = ati.annotations
                , highlights = ati.highlights
                , tooltips = ati.tooltips }

updateCodeBoxWithParseError annot codeBoxInfo =
  { codeBoxInfo | annotations = [annot] , highlights = [] , tooltips = [] }

switchToCursorTool old =
  { old | mouseMode = MouseNothing , tool = Cursor }

-- rewrite the innermost let-body eInner to (let main eInner main).
-- note that there's nothing special about calling this temp binding "main".
--
rewriteInnerMostExpToMain exp =
  case (unwrapExp exp) of
    ELet ws1 lk decls ws3 e2 ->
      replaceE__ exp (ELet ws1 lk decls ws3 (rewriteInnerMostExpToMain e2))
    _ ->
      eLets [("main", exp)] (eVar "main")


--------------------------------------------------------------------------------
-- Mouse Events

onMouseClick clickPos old maybeClickable =
  let (isOnCanvas, (canvasX, canvasY) as pointOnCanvas) = clickToCanvasPoint old clickPos in
  case (old.tool, old.mouseMode) of

    -- zone not yet dragged
    (Cursor, MouseDragZone (i, k, z) _ False _) ->
      onClickPrimaryZone i k z { old | mouseMode = MouseNothing }

    (Cursor, _) ->
      { old | mouseMode = MouseNothing }

    (Poly stk, MouseDrawNew polyPoints) ->
      let pointToAdd =
        case maybeClickable of
          Just (PointWithProvenance xVal yVal) ->
            ( (round (valToNum xVal), SnapVal xVal)
            , (round (valToNum yVal), SnapVal yVal)
            )
          Nothing ->
            ((canvasX, NoSnap), (canvasY, NoSnap))
      in
      case polyPoints of
        NoPointsYet   -> { old | mouseMode = MouseDrawNew (PolyPoints [pointToAdd]) }
        PolyPoints [] -> Debug.crash "invalid state, PolyPoints should always be nonempty"
        PolyPoints points ->
          let ((initialX, _), (initialY, _)) = Utils.last_ points in
          let ((thisX, _), (thisY, _))       = pointToAdd in
          if Utils.distanceInt (thisX, thisY) (initialX, initialY) > Draw.drawDotSize then { old | mouseMode = MouseDrawNew (PolyPoints (pointToAdd :: points)) }
          else if List.length points == 2 then { old | mouseMode = MouseNothing }
          else if List.length points == 1 then switchToCursorTool old
          else upstateRun <| switchToCursorTool <| Draw.addPolygon stk old points
        _ -> Debug.crash "invalid state, points should be NoPointsYet or PolyPoints for polygon tool"

    (Path stk, MouseDrawNew NoPointsYet) ->
      { old | mouseMode = MouseDrawNew (PathPoints [(old.keysDown, pointOnCanvas)]) }

    (Path stk, MouseDrawNew (PathPoints points)) ->
      let add new =
        let points_ = (old.keysDown, new) :: points in
        (points_, { old | mouseMode = MouseDrawNew (PathPoints points_) })
      in
      case points of
        [] -> Debug.crash "invalid state, PathPoints should always be nonempty"
        (_,firstClick) :: [] ->
          if Utils.distanceInt pointOnCanvas firstClick < Draw.drawDotSize
          then switchToCursorTool old
          else Tuple.second (add pointOnCanvas)
        (_,lastClick) :: _ ->
          if Utils.distanceInt pointOnCanvas lastClick < Draw.drawDotSize
          then upstateRun <| Draw.addPath stk old points
          else
            let (_,firstClick) = Utils.last_ points in
            if Utils.distanceInt pointOnCanvas firstClick < Draw.drawDotSize
            then
              let (points_,old_) = add firstClick in
              upstateRun <| switchToCursorTool <| Draw.addPath stk old_ points_
            else
              Tuple.second (add pointOnCanvas)

    (PointOrOffset, _) ->
      if isOnCanvas then
        upstateRun <| Draw.addPoint old pointOnCanvas
      else
        old

    (_, MouseDrawNew NoPointsYet) -> switchToCursorTool old

    _ ->
      old


onClickPrimaryZone : LangSvg.NodeId -> LangSvg.ShapeKind -> ShapeWidgets.RealZone -> Model -> Model
onClickPrimaryZone i k realZone old =
  let hoveredCrosshairs_ =
    case ShapeWidgets.zoneToMaybePointFeature realZone of
      Just pointFeature ->
        Set.insert (i, pointFeature) old.hoveredCrosshairs
      _ ->
        old.hoveredCrosshairs
  in
  let (selectedFeatures_, selectedShapes_, selectedBlobs_) =
    if i < -2 then -- Clicked a widget
      if realZone == ZOffset1D then
        let selectableFeatureToToggle = ShapeFeature i (DFeat Offset) in
        let update = if Set.member selectableFeatureToToggle old.selectedFeatures then Set.remove else Set.insert in
        (update selectableFeatureToToggle old.selectedFeatures, old.selectedShapes, old.selectedBlobs)
      else
        (old.selectedFeatures, old.selectedShapes, old.selectedBlobs)
    else
      let toggleThisShape () =
        Set.insert i <|
          if old.keysDown == [Keys.keyShift]
          then old.selectedShapes
          else Set.empty
      in
      let selectBlob blobId =
        Dict.insert blobId i <|
          if old.keysDown == [Keys.keyShift]
          then old.selectedBlobs
          else Dict.empty
      in
      let maybeBlobId =
        case Dict.get i (Tuple.second old.slate) |> Maybe.map .interpreted of
          Just (LangSvg.SvgNode _ l _) -> LangSvg.maybeFindBlobId l
          _                            -> Debug.crash "onClickPrimaryZone"
      in
      case (k, realZone, maybeBlobId) of
        ("line", ZLineEdge, Just blobId) -> (old.selectedFeatures, toggleThisShape (), selectBlob blobId)
        (_,      ZInterior, Just blobId) -> (old.selectedFeatures, toggleThisShape (), selectBlob blobId)
        ("line", ZLineEdge, Nothing)     -> (old.selectedFeatures, toggleThisShape (), old.selectedBlobs)
        (_,      ZInterior, Nothing)     -> (old.selectedFeatures, toggleThisShape (), old.selectedBlobs)
        _                                -> (old.selectedFeatures, old.selectedShapes, old.selectedBlobs)
  in
  { old | hoveredCrosshairs = hoveredCrosshairs_
        , selectedFeatures = selectedFeatures_
        , selectedShapes = selectedShapes_
        , selectedBlobs = selectedBlobs_
        }


onMouseDrag : {x:Int, y:Int} -> {x:Int, y:Int} -> Model -> (Model, Cmd Msg)
onMouseDrag lastPosition newPosition old =
  let (mx0, my0) = (newPosition.x, newPosition.y) in
  let (isOnCanvas, (mx, my)) = clickToCanvasPoint old newPosition in
  let (_, (mxLast, myLast))  = clickToCanvasPoint old lastPosition in
  let noCommand newModel = (newModel, Cmd.none) in
  case old.mouseMode of

    MouseNothing -> noCommand old

    MouseDragLayoutWidget f ->
      noCommand <| f (mx0, my0) old

    MouseDrag f ->
      noCommand <| f lastPosition newPosition old

    MouseDragZone zoneKey (mx0, my0) _ trigger ->
      case old.syncMode of
        TracesAndTriggers True ->
          noCommand <|
            applyTrigger old.solutionsCache zoneKey trigger (mx0, my0) (mx, my) old
        TracesAndTriggers False ->
          -- TODO: define and run a version of trigger that applies to the
          -- single value being manipulated, rather than the entire program.
          noCommand old
        ValueBackprop _ ->
          adHocZone.drag zoneKey (mx0,my0) (mx,my) old

    MouseDragSelect initialPosition initialSelectedShapes initialSelectedFeatures initialSelectedBlobs ->
      noCommand <|
      let pos1 = canvasPosition old initialPosition in
      let pos2 = canvasPosition old (mousePosition old) in
      let selectTop   = min pos1.y pos2.y in
      let selectLeft  = min pos1.x pos2.x in
      let selectBot   = max pos1.y pos2.y in
      let selectRight = max pos1.x pos2.x in
      let (root, shapeTree) = old.slate in
      let selectableShapeFeaturesAndPositions =
        shapeTree
        |> Dict.toList
        |> List.concatMap
            (\(nodeId, svgNode) ->
              case svgNode.interpreted of
                LangSvg.TextNode _ -> []
                LangSvg.SvgNode shapeKind shapeAttrs childIds ->
                  ShapeWidgets.pointFeaturesOfShape shapeKind shapeAttrs
                  |> List.concatMap
                      (\pf ->
                        case ShapeWidgets.maybeEvaluateShapePointFeature shapeKind shapeAttrs pf of
                          Just (x, y) -> [((nodeId, XFeat pf), (x, y)), ((nodeId, YFeat pf), (x, y))]
                          Nothing     -> []
                      )
            )
      in
      let selectableWidgetFeaturesAndPositions =
        old.widgets
        |> Utils.zipi1
        |> List.concatMap
            (\(widgetI, widget) ->
              let idAsShape = -2 - widgetI in
              ShapeWidgets.pointFeaturesOfWidget widget
              |> List.concatMap
                  (\pf ->
                    case ShapeWidgets.maybeEvaluateWidgetPointFeature widget pf of
                      Just (x, y) -> [((idAsShape, XFeat pf), (x, y)), ((idAsShape, YFeat pf), (x, y))]
                      Nothing     -> []
                  )
            )
      in
      let blobsAndBounds = [] in -- Ignore for now. Blobs are going to go bye-bye.
      let blobsToSelect = [] in  -- Ignore for now. Blobs are going to go bye-bye.
      let shapesToSelect =
        shapeTree
        |> Dict.filter
            (\nodeId svgNode ->
              case ShapeWidgets.maybeShapeBounds svgNode of
                Just (left, top, right, bot) -> selectLeft <= round left && round right <= selectRight && selectTop <= round top && round bot <= selectBot
                Nothing                      -> False
            )
        |> Dict.keys -- List of node ids
      in
      let featuresToSelect =
        selectableShapeFeaturesAndPositions ++ selectableWidgetFeaturesAndPositions
        |> List.filter (\((nodeId, shapeFeature), (x, y)) -> not (List.member nodeId shapesToSelect) && selectLeft <= round x && round x <= selectRight && selectTop <= round y && round y <= selectBot)
        |> List.map    (\((nodeId, shapeFeature), (x, y)) -> ShapeFeature nodeId shapeFeature)
      in
      if old.keysDown == [Keys.keyShift] then
        { old | selectedShapes   = Utils.multiToggleSet (Set.fromList shapesToSelect) initialSelectedShapes
              , selectedFeatures = Utils.multiToggleSet (Set.fromList featuresToSelect) initialSelectedFeatures
              , selectedBlobs    = initialSelectedBlobs }
      else
        { old | selectedShapes   = Set.fromList shapesToSelect
              , selectedFeatures = Set.fromList featuresToSelect
              , selectedBlobs    = Dict.fromList blobsToSelect }


    MouseDrawNew shapeBeingDrawn ->
      noCommand <|
      case (old.tool, shapeBeingDrawn) of
        (Poly _, _) -> old -- handled by onMouseClick instead
        (Path _, _) -> old -- handled by onMouseClick instead

        (_, NoPointsYet) ->
          let pointOnCanvas     = (old.keysDown, (mx, my)) in
          let lastPointOnCanvas = (old.keysDown, (mxLast, myLast)) in
          { old | mouseMode = MouseDrawNew (TwoPoints pointOnCanvas lastPointOnCanvas) }

        (_, TwoPoints _ point1) ->
          let pointOnCanvas = (old.keysDown, (mx, my)) in
          { old | mouseMode = MouseDrawNew (TwoPoints pointOnCanvas point1) }

        (_, Offset1DFromExisting _ _ ((x0Val, y0Val) as basePoint)) ->
          let ((effectiveMX, effectiveMY), snap) =
            let (x0, y0) = (valToNum x0Val, valToNum y0Val) in
            let (dxRaw, dyRaw) = (mx - round x0, my - round y0) in
            -- Hmm, shoudn't assume this here. Yolo.
            let (axis, sign, amount) = Draw.horizontalVerticalSnap (0, 0) (dxRaw, dyRaw) in
            -- Only snap to other offsets (for now)
            let possibleSnaps =
              old.widgets
              |> List.filterMap
                  (\widget ->
                    case widget of
                      WOffset1D _ _ _ _ (amount, _) amountVal _ _ -> Just (round amount, amountVal)
                      _                                           -> Nothing
                  )
              |> List.sortBy Tuple.first
            in
            let pixelsPerSnap = 20 in
            let snapRanges =
              possibleSnaps
              |> Utils.mapi0
                  (\(i, (numberToSnapTo, val))->
                    ((numberToSnapTo + pixelsPerSnap * i, numberToSnapTo + pixelsPerSnap * (i+1)), numberToSnapTo, val)
                  )
            in
            let maybeInSnapRange = Utils.findFirst (\((low, high), _, _) -> amount >= low && amount < high) snapRanges in
            case maybeInSnapRange of
              Just (_, snapToNumber, val) ->
                if      axis == X && sign == Positive then ((round x0 + snapToNumber, my), SnapVal val)
                else if axis == X && sign == Negative then ((round x0 - snapToNumber, my), SnapVal val)
                else if axis == Y && sign == Positive then ((mx, round y0 + snapToNumber), SnapVal val)
                else                                       ((mx, round y0 - snapToNumber), SnapVal val)
              Nothing ->
                let numberOfSnapsPassed = Utils.count (\((_, high), _, _) -> amount >= high) snapRanges in
                if      axis == X && sign == Positive then ((mx - numberOfSnapsPassed*pixelsPerSnap, my), NoSnap)
                else if axis == X && sign == Negative then ((mx + numberOfSnapsPassed*pixelsPerSnap, my), NoSnap)
                else if axis == Y && sign == Positive then ((mx, my - numberOfSnapsPassed*pixelsPerSnap), NoSnap)
                else                                       ((mx, my + numberOfSnapsPassed*pixelsPerSnap), NoSnap)
          in
          { old | mouseMode = MouseDrawNew (Offset1DFromExisting (effectiveMX, effectiveMY) snap basePoint) }

        _ -> old

    MouseDownInCodebox pos ->
      noCommand old

onMouseUp old =
  case (old.outputMode, old.mouseMode) of

    (HtmlText _, _) -> old

    (PrintScopeGraph _, _) -> old

    (Graphics, MouseDragZone zoneKey (mx0, my0) _ trigger) ->
      case old.syncMode of
        TracesAndTriggers True ->
          finishTrigger zoneKey old
        TracesAndTriggers False ->
          let (isOnCanvas, (mx, my)) = clickToCanvasPoint old (mousePosition old) in
          old
            |> applyTrigger old.solutionsCache zoneKey trigger (mx0, my0) (mx, my)
            |> finishTrigger zoneKey
        ValueBackprop _ ->
          let (_, (mx, my)) = clickToCanvasPoint old (mousePosition old) in
          adHocZone.finishDrag zoneKey (mx0,my0) (mx,my) old

    (_, MouseDrawNew points) ->
      let resetMouseMode model = { model | mouseMode = MouseNothing } in
      case (old.tool, points, old.keysDown == [Keys.keyShift]) of

        (Line _,     TwoPoints pt2 pt1, _) -> upstateRun <| resetMouseMode <| Draw.addLine old pt2 pt1
        (HelperLine, TwoPoints pt2 pt1, _) -> upstateRun <| resetMouseMode <| Draw.addLine old pt2 pt1

        (Rect Raw,      TwoPoints pt2 pt1, False) -> upstateRun <| resetMouseMode <| Draw.addRawRect old pt2 pt1
        (Rect Raw,      TwoPoints pt2 pt1, True)  -> upstateRun <| resetMouseMode <| Draw.addRawSquare old pt2 pt1
        (Rect Stretchy, TwoPoints pt2 pt1, False) -> upstateRun <| resetMouseMode <| Draw.addStretchyRect old pt2 pt1
        (Rect Stretchy, TwoPoints pt2 pt1, True)  -> upstateRun <| resetMouseMode <| Draw.addStretchySquare old pt2 pt1

        (Oval Raw,      TwoPoints pt2 pt1, False) -> upstateRun <| resetMouseMode <| Draw.addRawOval old pt2 pt1
        (Oval Raw,      TwoPoints pt2 pt1, True)  -> upstateRun <| resetMouseMode <| Draw.addRawCircle old pt2 pt1
        (Oval Stretchy, TwoPoints pt2 pt1, False) -> upstateRun <| resetMouseMode <| Draw.addStretchyOval old pt2 pt1
        (Oval Stretchy, TwoPoints pt2 pt1, True)  -> upstateRun <| resetMouseMode <| Draw.addStretchyCircle old pt2 pt1

        (Lambda i,       TwoPoints pt2 pt1, _) -> upstateRun <| resetMouseMode <| Draw.addLambda i old pt2 pt1
        (Function fName, TwoPoints pt2 pt1, _) -> upstateRun <| resetMouseMode <| Draw.addFunction fName old pt2 pt1

        (Poly _, _, _) -> old
        (Path _, _, _) -> old

        (Text, TwoPoints pt2 pt1, _) -> upstateRun <| resetMouseMode <| Draw.addTextBox old pt2 pt1

        (PointOrOffset, TwoPoints (_, pt2) (_, (x1,y1)), _)         -> upstateRun <| resetMouseMode <| Draw.addOffsetAndPoint old NoSnap (toFloat x1, toFloat y1) pt2
        (PointOrOffset, Offset1DFromExisting pt2 snap basePoint, _) -> upstateRun <| resetMouseMode <| Draw.addOffset old snap basePoint pt2

        (_, NoPointsYet, _)     -> switchToCursorTool old

        _              -> resetMouseMode old

    _ -> { old | mouseMode = MouseNothing, liveSyncInfo = refreshLiveInfo old }

applyTrigger solutionsCache zoneKey trigger (mx0, my0) (mx, my) old =
  let dx = if old.keysDown == Keys.y then 0 else (mx - mx0) in
  let dy = if old.keysDown == Keys.x then 0 else (my - my0) in

  let (newExp, highlights) = trigger solutionsCache (mx0, my0) (dx, dy) in

  let codeBoxInfo_ =
    let codeBoxInfo = old.codeBoxInfo in
    { codeBoxInfo | highlights = highlights }
  in

  EvalUpdate.runWithEnv old.syntax newExp |> Result.andThen (\((newVal, newWidgets), newEnv) ->
  LangSvg.resolveToRootedIndexedTree old.syntax old.slideNumber old.movieNumber old.movieTime newVal |> Result.map (\newSlate ->
    let newCode = Syntax.unparser old.syntax newExp in
    { old | code = newCode
          , lastParsedCode = newCode
          , lastRunCode = newCode
          , inputExp = newExp
          , inputVal = newVal
          , inputEnv = newEnv
          , valueEditorString = LangUtils.valToString newVal
          , outputMode        = maybeUpdateOutputMode old newSlate
          , htmlEditorString = Nothing
          , slate = newSlate
          , slateCount = 1 + old.slateCount
          , widgets = newWidgets
          , codeBoxInfo = codeBoxInfo_
          , mouseMode = MouseDragZone zoneKey (mx0, my0) True trigger
          }
  )) |> handleError old

finishTrigger zoneKey old =
  --
  -- Sync.highlightChanges takes into account character length
  -- changes to constants during live sync (via applyTrigger).
  -- but Sync.yellowAndGrayHighlights does not, so workaround
  -- is to re-parse and refreshHighlights.
  --
  --   let e = Utils.fromOkay "onMouseUp" <| Syntax.parser old.syntax old.code in
  --   let old_ = { old | inputExp = e } in
  --
  -- But this requires another EvalUpdate.run, which can be slow.
  -- Removing this workaround (which means the yellow highlights
  -- can be off after a live sync, until the next parse).
  --
  -- TODO: make a Sync.finishTrigger function that takes care of this.
  --
  let old_ = old in
  refreshHighlights zoneKey
    { old_ | mouseMode = MouseNothing, liveSyncInfo = refreshLiveInfo old_
           , history = modelCommit old.code [] Nothing old_.history
           , synthesisResultsDict = Dict.empty
           }


--------------------------------------------------------------------------------

adHocZone =
  let
    getLatestAttrs i old =
      case Dict.get i old.shapeUpdatesViaZones of

        -- shape i hasn't been modified since evaluation
        Nothing ->
          let (_,originalAttrs,_) =
            LangSvg.justGetSvgNode "getLatestAttrs" i old.slate
          in
          originalAttrs

        -- shape i has been modified since evaluation
        Just attrs ->
          attrs

    drag zoneKey (mx0,my0) (mx,my) old =
      let dx = toFloat <| if old.keysDown == Keys.y then 0 else (mx - mx0) in
      let dy = toFloat <| if old.keysDown == Keys.x then 0 else (my - my0) in
      let
        (i, _, _) =
          zoneKey

        latestAttrs =
          getLatestAttrs i old

        newAttrs =
          Sync.updateAttrs latestAttrs zoneKey dx dy

        cmds =
          newAttrs |> List.map (\(k,av) ->
            case av.interpreted of
              LangSvg.ANum (n,_) ->
                OutputCanvas.setDomShapeAttribute
                  { nodeId = i
                  , attrName = k
                  , attrValue = toString n
                  }
              LangSvg.APoints points ->
                OutputCanvas.setDomShapeAttribute
                  { nodeId = i
                  , attrName = "points"
                  , attrValue = LangSvg.strPoints points
                  }
              -- LangSvg.APath2
              -- TODO: add this when Sync.updateAttrs handles "path"
              aval_ ->
                let _ = Debug.log "WARN: adHocZone.drag not supported yet" aval_ in
                Cmd.none
          )
      in
      (old, Cmd.batch cmds)

    finishDrag zoneKey (mx0,my0) (mx,my) old =
      let dx = toFloat <| if old.keysDown == Keys.y then 0 else (mx - mx0) in
      let dy = toFloat <| if old.keysDown == Keys.x then 0 else (my - my0) in
      let
        (i, _, _) =
          zoneKey

        latestAttrs =
          getLatestAttrs i old

        newAttrs =
          Sync.updateAttrs latestAttrs zoneKey dx dy

        newUpdatedAttrs =
          List.foldl
            (\(k,av) acc -> Utils.update (k, av) acc)
            latestAttrs
            newAttrs

        newModel =
          { old
             | mouseMode =
                 MouseNothing
             , shapeUpdatesViaZones =
                 Dict.insert i newUpdatedAttrs old.shapeUpdatesViaZones
          }
      in
      newModel
  in
  { drag = drag
  , finishDrag = finishDrag
  }


--------------------------------------------------------------------------------

tryRun : Model -> Result (Model, String, Maybe Ace.Annotation) Model
tryRun old =
  let
    oldWithUpdatedHistory =
      let
        updatedHistory =
          modelCommit old.code [] Nothing old.history
      in
        { old | history = updatedHistory }
  in
    case (ImpureGoodies.logTimedRun "parsing time" <| \() -> Syntax.parser old.syntax old.code, old.outputMode) of
      (Err err, _) ->
        Err (oldWithUpdatedHistory, showError err, Nothing)
      (Ok parsedExp, PBE) ->
        let
          result =
            TriEval.eval parsedExp
              |> Result.mapError (\s -> "[Error] " ++ s)
        in
          Ok <|
            refreshInputExp
              { old
                  | unExpOutput = result
                  , errorBox = Nothing
              }

      (Ok parsedExp, _) ->
        let
          (e, holeEnv, aceTypeInfo, typeChecks) =
            maybeTypeCheck old.doTypeChecking parsedExp
        in
        let resultThunk () =

          -- want final environment of top-level definitions when evaluating e,
          -- for the purposes of running Little code to generate icons.
          -- but can't just use the output environment from eval directly.
          -- for example, if the last expression was a function call (either
          -- within the program or in Prelude), the final environment is from
          -- that function body. so instead, calling rewriteInnerMostExpToMain
          -- because the output environment from (let main eFinalBody main)
          -- will be the top-level definitions (and main).
          --
          let rewrittenE = rewriteInnerMostExpToMain e in

          EvalUpdate.runWithEnv old.syntax rewrittenE |>
          Result.andThen (\((newVal,ws),finalEnv) ->
            LangSvg.fetchEverything old.syntax old.slideNumber old.movieNumber 0.0 newVal
            |> Result.map (\(newSlideCount, newMovieCount, newMovieDuration, newMovieContinue, newSlate) ->
              let newCode = Syntax.unparser old.syntax e in -- unnecessary, if parse/unparse were inverses
              let lambdaTools_ =
                -- TODO should put program into Model
                -- TODO actually, ideally not. caching introduces bugs
                let program = splitExp e in
                Draw.lambdaToolOptionsOf old.syntax program finalEnv ++ initModel.lambdaTools
              in
              let new =
                loadLambdaToolIcons finalEnv { old | lambdaTools = lambdaTools_ }
                |> clearSynthesisResults
              in
              let new_ =
                { new | inputExp      = e
                      , inputVal      = newVal
                      , inputEnv      = finalEnv
                      , valueEditorString = LangUtils.valToString newVal
                      , outputMode    = maybeUpdateOutputMode old newSlate
                      , htmlEditorString = Nothing
                      , code          = newCode
                      , lastParsedCode = newCode
                      , lastRunCode   = newCode
                      , slideCount    = newSlideCount
                      , movieCount    = newMovieCount
                      , movieTime     = 0
                      , movieDuration = newMovieDuration
                      , movieContinue = newMovieContinue
                      , runAnimation  = newMovieDuration > 0
                      , slate         = newSlate
                      , slateCount    = 1 + old.slateCount
                      , widgets       = ws
                      , history       = modelCommit newCode [] Nothing old.history
                      , caption       = Nothing
                      , syncOptions   = Sync.syncOptionsOf old.syncOptions e
                      , lambdaTools   = lambdaTools_
                      , errorBox      = Nothing
                      , scopeGraph    = DependenceGraph.compute e
                      , preview       = Nothing
                      , previewdiffs  = Nothing
                      , synthesisResultsDict = Dict.singleton "Auto-Synthesis" (perhapsRunAutoSynthesis old e)
                      , shapeUpdatesViaZones = Dict.empty
                      , tinyStructuredEditorsForLowLowPricesState =
                          if old.outputMode == StructuredEditor then
                            let maybeValType =
                              -- The correct incantation would be (unExpr e).val.typ but the type checker can't type the empty list.
                              -- Look for an explicit type annotation.
                              expEffectiveExps e
                              |> Utils.mapFirstSuccess (LangTools.expToMaybeEColonType)
                            in
                            TinyStructuredEditorsForLowLowPrices.prepare
                                old.tinyStructuredEditorsForLowLowPricesState
                                finalEnv
                                e
                                maybeValType
                                newVal
                          else
                            old.tinyStructuredEditorsForLowLowPricesState
                }
              in
                { new_
                    | liveSyncInfo = refreshLiveInfo new_
                    , codeBoxInfo = updateCodeBoxInfo aceTypeInfo new_
                }
                  |> showTypeInspectorIfNecessary typeChecks
                  |> resetDeuceState
            )
          )
        in
          let oldWithUpdatedHistoryAndTypes =
            { oldWithUpdatedHistory
                | codeBoxInfo =
                    updateCodeBoxInfo aceTypeInfo oldWithUpdatedHistory
            }
          in
          case ImpureGoodies.crashToError resultThunk of
            Err s         -> Err (oldWithUpdatedHistoryAndTypes, s, Nothing)
            Ok (Err s)    -> Err (oldWithUpdatedHistoryAndTypes, s, Nothing)
            Ok (Ok model) -> Ok model


--------------------------------------------------------------------------------
-- Updating the Model

-- 1. Compute a "pure" update (only the newModel), and then
-- 2. Decide whether to issue a command based on simple predicates
--    (name of Msg, and simple comparison of oldModel and newModel).
--
update : Msg -> Model -> (Model, Cmd Msg)
update msg oldModel =
  let msgCaption = case msg of
    Msg caption _            -> caption
    NewModelAndCmd caption _ -> caption
    ResponseFromSolver _     -> "RESPONSE FROM SOLVER"
  in
  case
    ( (oldModel.pendingFileOperation, oldModel.fileOperationConfirmed)
    , (oldModel.pendingGiveUpMsg, oldModel.giveUpConfirmed)
    )
  of
    ((Just msg2, True), _) ->
      update
        msg2
        { oldModel | pendingFileOperation = Nothing
                   , fileOperationConfirmed = False }
    (_, (Just msg2, True)) ->
      update
      msg2
      { oldModel | pendingGiveUpMsg = Nothing
                 , giveUpConfirmed = False }
    _ ->
      let (newModel, newCmd) =
        let (caption, (newModel, cmd)) =
          case msg of
            Msg caption f -> (caption, (f oldModel, Cmd.none))
            NewModelAndCmd caption f -> (caption, f oldModel)
            ResponseFromSolver _ ->
              Debug.crash "Controller.update: solver response messages should never hit here"
        in
        let _ = debugLog "Msg (or MsgNewModelAndCmd)" caption in
        (newModel, cmd)
      in
      let newAddDummyDivAroundCanvas =
        if String.startsWith "Ace" msgCaption then newModel.addDummyDivAroundCanvas else
        case newModel.addDummyDivAroundCanvas of
           Just True  -> Just False
           Just False -> Nothing
           Nothing    -> Nothing
      in
      let augmentSlateCondition =
        (newAddDummyDivAroundCanvas /= oldModel.addDummyDivAroundCanvas && newAddDummyDivAroundCanvas == Nothing) ||
        Utils.maybeIsEmpty (oldModel.preview) /= Utils.maybeIsEmpty (newModel.preview)
      in
      let finalModel =
        { newModel
            | addDummyDivAroundCanvas = newAddDummyDivAroundCanvas
            , slateCount = if augmentSlateCondition then newModel.slateCount + 1 else newModel.slateCount
            }
      in
      let anotherCmd = issueCommandBasedOnCaption msgCaption oldModel finalModel in
      let (hookedModel, hookedCommands) = applyAllHooks oldModel finalModel in
      ( Model.setAllUpdated hookedModel
      , Cmd.batch <|
          newCmd :: anotherCmd :: updateCommands finalModel ++ hookedCommands
      )

-- deprecate if/when Msg is removed in favor of just NewModelAndCmd
upstate : Msg -> Model -> Model
upstate msg old =
  case msg of
    Msg caption updateModel ->
      -- let _ = Debug.log "" (caption, old.userStudyTaskStartTime, old.userStudyTaskCurrentTime) in
      -- let _ = Debug.log "Msg" caption in
      -- let _ = if {-String.contains "Key" caption-} True then Debug.log caption (old.mouseMode, old.mouseState) else (old.mouseMode, old.mouseState) in
      let _ = debugLog "Msg" caption in
      updateModel old

    NewModelAndCmd caption _ ->
      Debug.crash <|
        "Controller.upstate: shouldn't be called with a NewModelAndCmd: " ++ caption

    ResponseFromSolver _ ->
      Debug.crash "Controller.upstate: solver response messages should never hit here"

--------------------------------------------------------------------------------
-- Hooks to be run after every message

hooks : List (Model -> Model -> (Model, Cmd Msg))
hooks =
  [ handleSavedSelectionsHook
  , handleSyntaxHook
  , handleOutputSelectionChanges
  , handleDeuceCache
  , focusJustShownRenameBox
  , focusAsNeeded
  ]

applyAllHooks : Model -> Model -> (Model, List (Cmd Msg))
applyAllHooks oldModel newModel =
  List.foldl
    ( \f (newestModel, cmds) ->
        f oldModel newestModel
          |> Tuple.mapSecond (flip (::) cmds)
    )
    (newModel, [])
    hooks

handleSavedSelectionsHook : Model -> Model -> (Model, Cmd Msg)
handleSavedSelectionsHook oldModel newModel =
  -- Update selections before/after previewing
  if oldModel.preview == Nothing && newModel.preview /= Nothing then
    ( { newModel | savedSelections = Just newModel.codeBoxInfo.selections }
    , Cmd.none
    )
  else if oldModel.preview /= Nothing && newModel.preview == Nothing then
    case newModel.savedSelections of
      Just selections ->
        ( { newModel | savedSelections = Nothing }
        , AceCodeBox.setSelections selections
        )
      Nothing ->
        (newModel, Cmd.none)
  else
    (newModel, Cmd.none)

handleSyntaxHook : Model -> Model -> (Model, Cmd Msg)
handleSyntaxHook oldModel newModel =
  let
    cmd =
      if newModel.syntax /= oldModel.syntax then
        SyntaxHighlight.sendMessage <|
          SetSyntax newModel.syntax
      else
        Cmd.none
  in
    (newModel, cmd)

handleOutputSelectionChanges : Model -> Model -> (Model, Cmd Msg)
handleOutputSelectionChanges oldModel newModel =
  if oldModel.selectedFeatures == newModel.selectedFeatures &&
     oldModel.selectedShapes == newModel.selectedShapes &&
     oldModel.selectedBlobs == newModel.selectedBlobs
  then
    (newModel, Cmd.none)
  else
    let finalModel = { newModel | synthesisResultsDict = Dict.empty } in
    (finalModel, Cmd.none)

deuceOverlayMsgs : Deuce.Messages Msg
deuceOverlayMsgs =
  { onMouseOver = msgMouseEnterDeuceWidget
  , onMouseOut = msgMouseLeaveDeuceWidget
  , onClick = msgMouseClickDeuceWidget
  }

handleDeuceCache : Model -> Model -> (Model, Cmd Msg)
handleDeuceCache oldModel newModel =
  if Model.deuceCacheNeedsUpdate oldModel newModel
  then
    ( { newModel
          | deuceOverlayCache = Just <|
              Deuce.overlay deuceOverlayMsgs newModel
      }
    , Cmd.none
    )
  else
    (newModel, Cmd.none)

port doFocusJustShownRenameBox : () -> Cmd msg

focusJustShownRenameBox : Model -> Model -> (Model, Cmd Msg)
focusJustShownRenameBox oldModel newModel =
  if oldModel.renamingInOutput == Nothing && newModel.renamingInOutput /= Nothing then
    (newModel, doFocusJustShownRenameBox ())
  else
    (newModel, Cmd.none)

port doFocusAsNeeded : String -> Cmd msg

focusAsNeeded : Model -> Model -> (Model, Cmd Msg)
focusAsNeeded oldModel newModel =
  case newModel.needsToFocusOn of
    Nothing ->
      (newModel, Cmd.none)
    Just id ->
      ( { newModel | needsToFocusOn = Nothing }, doFocusAsNeeded id )

debugModel : (Model -> a) -> Model -> Model -> (Model, Cmd Msg)
debugModel get old new =
  let
    oldProperty =
      get old
    newProperty =
      get new
    returnValue =
      (new, Cmd.none)
  in
    if oldProperty /= newProperty then
      let
        _ = Debug.log "" newProperty
      in
        returnValue
    else
      returnValue

--------------------------------------------------------------------------------

updateCommands : Model -> List (Cmd Msg)
updateCommands model =
  let
    ifNeedsUpdate : (Model -> Updatable a) -> (a -> Cmd Msg) -> List (Cmd Msg)
    ifNeedsUpdate get f =
      let
        up =
          get model
      in
        if Updatable.needsUpdate up then
          [ f <| Updatable.extract up ]
        else
          []
  in
    List.concat
      [ ifNeedsUpdate .enableTextEdits <|
          AceCodeBox.setReadOnly << not
      ]

getAutoSyncDelay: Model -> Int
getAutoSyncDelay m =
  case Dict.fromList (getTopLevelOptions m.inputExp) |> Dict.get "updatedelay" of
    Just x -> String.toInt x |> Result.withDefault m.autoSyncDelay
    Nothing -> m.autoSyncDelay

issueCommandBasedOnCaption : String -> Model -> Model -> Cmd Msg
issueCommandBasedOnCaption kind oldModel newModel =
  case kind of
    "Toggle Code Box" ->
      if newModel.basicCodeBox
        then Cmd.none
        else AceCodeBox.initializeAndDisplay newModel
          -- TODO crash: "Uncaught Error: ace.edit can't find div #editor"

    "Save As" ->
      if newModel.filename.name /= Model.bufferName then
        FileHandler.sendMessage <|
          Write newModel.filename newModel.code
      else
        Cmd.none

    "Save" ->
      if newModel.filename.name /= Model.bufferName then
        FileHandler.sendMessage <|
          Write newModel.filename newModel.code
      else
        FileHandler.sendMessage RequestFileIndex

    "Confirm Write" ->
      Cmd.batch <| iconCommand newModel.filename

    "Open" ->
      FileHandler.sendMessage <|
        RequestFile newModel.filename

    "Delete" ->
      FileHandler.sendMessage <|
        Delete newModel.fileToDelete

    "Confirm Delete" ->
      Cmd.batch <| iconCommand newModel.fileToDelete

    "Export Code" ->
      -- not just using prettyFilename WithExtension, because that
      -- function handles templates and local differently
      let ext = if newModel.syntax == Syntax.Leo then ".elm" else ".little" in
      FileHandler.sendMessage <|
        Download
          (Model.prettyFilename WithoutExtension newModel ++ ext)
          newModel.code

    "Export HTML" ->
      let (prefix, suffix, extension) =
        case vHtmlNodeUnapply newModel.inputVal of
           Just ("svg", _, _) -> ("", "", ".svg")
           _ -> ("<html>\n<body>", "</body>\n</html>", ".html")
      in
      FileHandler.sendMessage <|
        Download
          (Model.prettyFilename WithoutExtension newModel ++ extension)
          (prefix ++ LangSvg.printRawHTML newModel.showGhosts newModel.slate ++ suffix)

    "Import Code" ->
      FileHandler.sendMessage <|
        RequestUploadedFile Model.importCodeFileInputId

    -- Do not send changes back to the editor, because this is the command where
    -- we receieve changes (if this is removed, an infinite feedback loop
    -- occurs).
    "Ace Update" ->
      if newModel.backupRecovery then
        let
          backup =
            File.backupFilename newModel.filename
        in
          if newModel.needsSave then
            FileHandler.sendMessage <|
              Write backup ("{-\n" ++ newModel.code ++ "\n-}\n\nmain = svg []")
          else
            FileHandler.sendMessage <|
              Delete backup
      else
        Cmd.none

    "Enable Text Edits" ->
      AceCodeBox.setReadOnly False

    "Disable Text Edits" ->
      AceCodeBox.setReadOnly True

    "Show UnExp Preview" ->
      AceCodeBox.display newModel

    "Clear UnExp Preview" ->
      AceCodeBox.display
        { newModel
            | code =
                newModel.codeAtPbeSynthesis
                  |> Maybe.withDefault newModel.code
        }

    _ ->
      let dispatchIfChanged: (Model -> a) -> (a -> Cmd Msg) -> Cmd Msg
          dispatchIfChanged submodel cmdBuilder =
            let n = submodel newModel in
            if submodel oldModel /= n then cmdBuilder n else Cmd.none
      in
      let dispatchIfNonemptyChanged: (Model -> Maybe a) -> (a -> Cmd Msg) -> Cmd Msg
          dispatchIfNonemptyChanged submodel cmdBuilder =
            let n = submodel newModel in
            case n of
              Nothing -> Cmd.none
              Just a ->
                if submodel oldModel /= n then cmdBuilder a else Cmd.none
      in
      Cmd.batch
        [ dispatchIfChanged
            (\m -> m.syncMode)
            (\syncMode -> case syncMode of
               ValueBackprop True -> OutputCanvas.enableAutoSync True
               _                  -> OutputCanvas.enableAutoSync False
            )
        , dispatchIfChanged getAutoSyncDelay OutputCanvas.setAutoSyncDelay
        , dispatchIfChanged (\m -> m.preview |> Utils.maybeIsEmpty |> not) OutputCanvas.setPreviewMode
        , dispatchIfChanged (\m -> m.previewdiffs |> Utils.maybeIsEmpty |> not) (OutputCanvas.setDiffTimer << DiffTimer newModel.previewdiffsDelay)
        , dispatchIfNonemptyChanged (\m -> m.caretPosition) OutputCanvas.setCaretPosition
        --, dispatchIfNonemptyChanged (\m ->
        --   m.previewdiffs |> Utils.maybeOrElse (Maybe.map (\(_, exps, _) -> exps) m.preview) |>
        --     Maybe.andThen List.head |> Maybe.map (\expHead -> expHead.start)) AceCodeBox.aceCodeBoxScroll
        , if kind == "Update Font Size" then
            AceCodeBox.updateFontSize newModel
          else if
            newModel.code /= oldModel.code ||
            newModel.codeBoxInfo /= oldModel.codeBoxInfo ||
            newModel.preview /= oldModel.preview ||
            kind == "Turn Off Caption" ||
            kind == "Mouse Enter CodeBox" ||
            kind == "Mouse Leave CodeBox" ||
            kind == "Call Update"
             {- ||
             String.startsWith "Key Up" kind ||
             String.startsWith "Key Down" kind
             -}
               -- ideally this last condition would not be necessary.
               -- and onMouseLeave from point/crosshair zones still leave
               -- stale yellow highlights.
          then
            AceCodeBox.display newModel
          else if kind == "Drag Layout Widget Trigger" then
            -- TODO: only want to do this for resize code box widget.
            -- and need to resize during and after the MouseDragLayout trigger.
            -- (onMouseUp). workaround for now: click widget again.
            AceCodeBox.resize newModel
          else if kind == "Toggle Output" && newModel.outputMode == PrintScopeGraph Nothing then
            DependenceGraph.render newModel.scopeGraph
          else if newModel.runAnimation then
            AnimationLoop.requestFrame ()
          else
            Cmd.none
        , if String.startsWith "New" kind then
            Cmd.batch
              [ AceCodeBox.resetScroll newModel
              , OutputCanvas.resetScroll
              ]
          else
            Cmd.none
        , if String.startsWith "Read File" kind then
            OutputCanvas.resetScroll
          else
            Cmd.none
        , if String.startsWith "msgMouseClickDeuceWidget" kind then
            DeucePopupPanelInfo.requestDeucePopupPanelInfo ()
          else
            Cmd.none
        , if String.startsWith "Open Dialog Box" kind then
            FileHandler.sendMessage RequestFileIndex
          else
            Cmd.none
        , if kind == "Set Color Scheme" then
            ColorScheme.updateColorScheme newModel.colorScheme
          else
            Cmd.none
        ]


iconCommand filename =
  let
    potentialIconName =
      filename.name
  in
    if List.member potentialIconName Model.iconNames then
      [ FileHandler.sendMessage <|
          RequestIcon potentialIconName
      ]
    else
      []

--------------------------------------------------------------------------------

msgNoop = Msg "Noop" identity

msgWindowDimensions wh = Msg "Window Dimensions" <| \old ->
  { old | dimensions = wh }

msgCodeUpdate s = Msg "Code Update" <| \old ->
  { old | code = s }

--------------------------------------------------------------------------------

msgVisibilityChange : Visibility -> Msg
msgVisibilityChange visibility =
  Msg "Visibility Change" <| \model ->
    case visibility of
      -- If changing to visible, do nothing
      Visible ->
        model
      -- If changing to hidden, unpress all keys
      Hidden ->
        { model | keysDown = [] }

--------------------------------------------------------------------------------

msgRun = Msg "Run" <| \old -> upstateRun old

msgAceUpdate aceCodeBoxInfo = Msg "Ace Update" <| \old ->
  if old.preview /= Nothing then
    old
  else
    let
      needsSave =
        old.lastSaveState /= Just aceCodeBoxInfo.code
    in
      { old
          | code =
              aceCodeBoxInfo.code
          , codeBoxInfo =
              aceCodeBoxInfo.codeBoxInfo
          , needsSave =
              needsSave
      }

msgUserHasTyped : Msg
msgUserHasTyped =
  Msg "User Has Typed" <| \model ->
    { model
        | deuceState =
            emptyDeuceState
    }

msgOutputCanvasUpdate outputCanvasInfo = Msg "Output Canvas Update" <| \old ->
  { old | outputCanvasInfo = outputCanvasInfo }

upstateRun old =
  let newFailuresInARowAfterFail    = if old.runFailuresInARowCount < 0 then 1 else old.runFailuresInARowCount + 1 in
  let newFailuresInARowAfterSuccess = if old.runFailuresInARowCount > 0 then 0 else old.runFailuresInARowCount - 1 in
  case tryRun old of
    Err (oldWithUpdatedHistory, err, Just annot) ->
      { oldWithUpdatedHistory
          | errorBox = Just err
          , codeBoxInfo = updateCodeBoxWithParseError annot old.codeBoxInfo
          , runFailuresInARowCount = newFailuresInARowAfterFail
      }
    Err (oldWithUpdatedHistory, err, Nothing) ->
      { oldWithUpdatedHistory
          | errorBox = Just err
          , runFailuresInARowCount = newFailuresInARowAfterFail
      }
    Ok newModel ->
      { newModel | runFailuresInARowCount = newFailuresInARowAfterSuccess }

msgTryParseRun newModel = Msg "Try Parse Run" <| \old ->
  case tryRun newModel of
    Err (oldWithUpdatedHistory, err, Just annot) ->
      { oldWithUpdatedHistory
          | caption = Just (LangError err)
          , codeBoxInfo = updateCodeBoxWithParseError annot old.codeBoxInfo
      }
    Err (oldWithUpdatedHistory, err, Nothing) ->
      { oldWithUpdatedHistory
          | caption = Just (LangError err)
      }
    Ok modelAfterRun ->
      modelAfterRun

--------------------------------------------------------------------------------

resetDeuceCacheAndReselect : Model -> (Model -> List (List CachedDeuceTool)) -> Model
resetDeuceCacheAndReselect almostNewModel cacheRefresher =
  { almostNewModel
      | deuceToolsAndResults =
          cacheRefresher almostNewModel
      , deuceToolResultPreviews =
          Dict.empty
  } |> DeuceTools.reselectDeuceTool

resetDeucePopupPanelPosition : Model -> Model
resetDeucePopupPanelPosition m =
  let
    oldPopupPanelPositions =
      m.popupPanelPositions
    newPopupPanelPositions =
      { oldPopupPanelPositions | deuce = (400, 400) }
  in
    { m | popupPanelPositions = newPopupPanelPositions }

updateTrackedValues : History TrackedValues -> TrackedValues -> Model -> Model
updateTrackedValues newHistory recent old =
  let
    toBeRun =
      { old
          | code = recent.code
      }
        |> Model.hideDeuceRightClickMenu
        |> resetDeuceState
    ran =
      upstateRun toBeRun
    ranDeuceState =
      ran.deuceState
    newDeuceState =
      { ranDeuceState
          | selectedWidgets =
              recent.selectedDeuceWidgets
          , mbKeyboardFocusedWidget =
              recent.mbKeyboardFocusedWidget
      }
    almostNew =
      { ran
          | deuceState =
              newDeuceState
          , history =
              newHistory
      }
  in
  resetDeuceCacheAndReselect almostNew DeuceTools.createToolCache |>
  resetDeucePopupPanelPosition

msgUndo = Msg "Undo" doUndo

doUndo : Model -> Model
doUndo old =
  case History.backward old.history of
    Just newHistory ->
      case History.mostRecent newHistory of
        Just recent ->
          updateTrackedValues newHistory recent old

        Nothing ->
          old

    Nothing ->
      old

--  case old.history of
--    ([], _) ->
--      old
--    ([firstRun], _) ->
--      old
--    (lastRun::secondToLast::older, future) ->
--      let
--        new =
--          { old
--              | history =
--                  (secondToLast::older, lastRun::future)
--              , code =
--                  secondToLast
--          }
--            |> Model.hideDeuceRightClickMenu
--            |> resetDeuceState
--      in
--        upstateRun new

msgRedo = Msg "Redo" doRedo

doRedo : Model -> Model
doRedo old =
  case History.forward old.history of
    Just newHistory ->
      case History.mostRecent newHistory of
        Just recent ->
          updateTrackedValues newHistory recent old

        Nothing ->
          old

    Nothing ->
      old

--  case old.history of
--    (_, []) ->
--      old
--    (past, next::future) ->
--      let
--        new =
--          { old
--              | history =
--                  (next::past, future)
--              , code =
--                  next
--          }
--            |> Model.hideDeuceRightClickMenu
--            |> resetDeuceState
--      in
--        upstateRun new

--------------------------------------------------------------------------------

msgMouseIsDown b = Msg ("MouseIsDown " ++ toString b) <| \old ->
  let new =
    let {x,y} = mousePosition old in
    let lightestColor = 470 in
    { old | randomColor = (old.randomColor + x + y) % lightestColor }
  in
  case (b, new.mouseState) of

    (True, (Nothing, pos, _)) -> -- mouse down
      let _ = debugLog "mouse down" () in
      if old.hoveringCodeBox
      -- TODO disabling MouseDownInCodeBox because onMouseDragged is disabled
{-
      then { new | mouseState = (Just False, pos),
                   mouseMode = MouseDownInCodebox pos }
-}
      then { new | mouseState = (Just False, pos, Nothing) }
      else { new | mouseState = (Just False, pos, Nothing) }

    (False, (Just False, pos, maybeClickable)) -> -- click (mouse up after not being dragged)
      let _ = debugLog "mouse click" () in
      onMouseClick pos { new | mouseState = (Nothing, pos, Nothing) } maybeClickable

    (False, (Just True, pos, _)) -> -- mouse up (after being dragged)
      let _ = debugLog "mouse up" () in
      onMouseUp { new | mouseState = (Nothing, pos, Nothing) }

    (False, (Nothing, _, _)) ->
      let _ = debugLog "mouse down was preempted by a handler in View" () in
      new

    -- (True, (Just _, _)) -> Debug.crash "upstate MouseIsDown: impossible"
    (True, (Just _, _, _)) ->
      let _ = Debug.log "upstate MouseIsDown: impossible" () in
      new

msgMousePosition pos_ =
  let
    mouseStateUpdater old =
      case old.mouseState of
        (Nothing, _, _) ->
          ({ old | mouseState = (Nothing, pos_, Nothing) }, Cmd.none)
        (Just _, oldPos_, maybeClickable) ->
          onMouseDrag oldPos_ pos_ { old | mouseState = (Just True, pos_, maybeClickable) }

    maybeDeucePopupPanelPositionUpdater old =
      if noCodeWidgetsSelected old then
        deucePopupPanelPositionUpdater pos_ old
      else
        old
  in
    NewModelAndCmd
      ("MousePosition " ++ toString pos_)
      (mouseStateUpdater >> Tuple.mapFirst maybeDeucePopupPanelPositionUpdater)

deucePopupPanelPositionUpdater : Mouse.Position -> Model -> Model
deucePopupPanelPositionUpdater pos old =
  let
    newDeucePopupPanelPosition =
      ( pos.x + deucePopupPanelMouseOffset.x
      , pos.y + deucePopupPanelMouseOffset.y
      )

    oldPopupPanelPositions =
      old.popupPanelPositions

    newPopupPanelPositions =
      { oldPopupPanelPositions | deuce = newDeucePopupPanelPosition }
  in
    { old | popupPanelPositions = newPopupPanelPositions }

--------------------------------------------------------------------------------

tryPreserveListIds_ : (Int -> a -> a -> Maybe (a, Int)) -> Int -> List a -> List a -> Maybe (List a, Int)
tryPreserveListIds_ preserver nextAvailId oldList newList =
  Utils.maybeZip oldList newList |> Maybe.andThen (
  Utils.foldlMaybe (\(old, new) (reversedAcc, updatedAvailId) ->
    preserver updatedAvailId old new
    |> Maybe.map (Tuple.mapFirst <| flip (::) reversedAcc)
  )
  (Just ([], nextAvailId))       >> Maybe.map (
  Tuple.mapFirst List.reverse))

tryPreserveDecl_ nextAvailId oldDecl newDecl =
  case (oldDecl, newDecl) of
    (DeclExp (LetExp _ _ oldP _ _ oldE), DeclExp (LetExp ocs wsb newP fas ws1 newE)) ->
      tryPreservePIds_ oldP newP            |> Maybe.andThen (\preservedP ->
      tryPreserveIDs_ nextAvailId oldE newE |> Maybe.map (\(preservedE, nextAvailId) ->
        (DeclExp <| LetExp ocs wsb preservedP fas ws1 preservedE, nextAvailId)
      ))
    (DeclType (LetType _ _ _ oldP _ _ oldT), DeclType (LetType ocs wsb aS newP fas ws1 newT)) ->
      tryPreservePIds_ oldP newP |> Maybe.andThen (\preservedP ->
      tryPreserveTIds_ oldT newT |> Maybe.map (\preservedT ->
        (DeclType <| LetType ocs wsb aS preservedP fas ws1 preservedT, nextAvailId)
      ))
    (DeclAnnotation (LetAnnotation _ _ oldP _ _ oldT), DeclAnnotation (LetAnnotation ocs wsb newP fas ws1 newT)) ->
      tryPreservePIds_ oldP newP |> Maybe.andThen (\preservedP ->
      tryPreserveTIds_ oldT newT |> Maybe.map (\preservedT ->
        (DeclAnnotation <| LetAnnotation ocs wsb preservedP fas ws1 preservedT, nextAvailId)
      ))
    _ ->
      Nothing

tryPreserveDecls_ nextAvailId oldDecls newDecls =
  let
    oldDeclsOrdered = getDeclarationsInOrder oldDecls
    newDeclsOrdered = getDeclarationsInOrder newDecls
  in
  tryPreserveListIds_ tryPreserveDecl_ nextAvailId oldDeclsOrdered newDeclsOrdered
  |> Maybe.andThen (\(preservedDeclsFlat, nextAvailId) ->
  Parser.reorderDeclarations preservedDeclsFlat |> Result.toMaybe
  |> Maybe.map (\preservedDecls ->
  (preservedDecls, nextAvailId)
  ))

tryPreservePIds_ : Pat -> Pat -> Maybe Pat
tryPreservePIds_ oldPat newPat =
  let
    oldPId = oldPat.val.pid
    preserveLeaf = setPId oldPId newPat |> Just
    return p__ = replaceP__ newPat p__ |> setPId oldPId |> Just
  in
  case (oldPat.val.p__, newPat.val.p__) of
    (PVar _ _ _, PVar _ _ _)    -> preserveLeaf
    (PWildcard _, PVar _ "_" _) -> preserveLeaf
    (PConst _ _, PConst _ _)    -> preserveLeaf
    (PBase _ _, PBase _ _)      -> preserveLeaf
    (PWildcard _, PWildcard _)  -> preserveLeaf
    (PVar _ "_" _, PWildcard _) -> preserveLeaf
    (PList _ oldPats _ oldM _, PList wsb newPats ws1 newM ws2) ->
      tryPreservePatsIds_ oldPats newPats |> Maybe.andThen (\preservedPats ->
      (case (oldM, newM) of
        (Just oldT, Just newT) -> tryPreservePIds_ oldT newT |> Maybe.map Just
        (Nothing, Nothing)     -> Just Nothing
        _                      -> Nothing
      )                                   |> Maybe.andThen (\preservedM ->
        PList wsb preservedPats ws1 preservedM ws2 |> return
      ))
    (PAs _ oldBound _ oldAs, PAs wsb newBound ws1 newAs) ->
      tryPreservePIds_ oldBound newBound |> Maybe.andThen (\preservedBound ->
      tryPreservePIds_ oldAs newAs       |> Maybe.andThen (\preservedAs ->
        PAs wsb preservedBound ws1 preservedAs |> return
      ))
    (PParens _ oldInner _, PParens wsb newInner wsa) ->
      tryPreservePIds_ oldInner newInner |> Maybe.andThen (\preservedInner ->
        PParens wsb preservedInner wsa |> return
      )
    (PRecord _ oldFields _, PRecord wsb newFields wsa) ->
      Utils.zip oldFields newFields
      |> List.map (\((_, _, oldIdent, _, oldField), (mbWs, wsbf, newIdent, ws1, newField)) ->
        if oldIdent == newIdent then
          tryPreservePIds_ oldField newField
          |> Maybe.map (\preservedField -> (mbWs, wsbf, newIdent, ws1, preservedField))
        else
          Nothing
      )
      |> Utils.projJusts
      |> Maybe.andThen (\preservedFields ->
        PRecord wsb preservedFields wsa |> return
      )
    (PColonType _ oldInner _ oldType, PColonType wsb newInner ws1 newType) ->
      tryPreservePIds_ oldInner newInner |> Maybe.andThen (\preservedInner ->
      tryPreserveTIds_ oldType newType   |> Maybe.andThen (\preservedType ->
        PColonType wsb preservedInner ws1 preservedType |> return
      ))
    _ ->
      Nothing

tryPreserveTIds_ : Type -> Type -> Maybe Type
-- TODO implement properly
tryPreserveTIds_ oldType newType = Just newType

tryPreservePatsIds_ : List Pat -> List Pat -> Maybe (List Pat)
tryPreservePatsIds_ oldPats newPats =
  let preserver _ oldPat newPat =
    tryPreservePIds_ oldPat newPat |> Maybe.map (flip (,) 0)
  in
  tryPreserveListIds_ preserver 0 oldPats newPats
  |> Maybe.map Tuple.first

tryPreserveExpsIds_ : Int -> List Exp -> List Exp -> Maybe (List Exp, Int)
tryPreserveExpsIds_ = tryPreserveListIds_ tryPreserveIDs_

tryPreserveIDs_ : Int -> Exp -> Exp -> Maybe (Exp, Int)
tryPreserveIDs_ nextAvailId old new =
  let
    oldEId = expEId old
    withEId newEId e__ = replaceE__ new e__ |> setEId newEId
    preserveLeaf = Just (setEId oldEId new, nextAvailId)
    return updatedAvailId e__ = Just (withEId oldEId e__, updatedAvailId)
  in
  case (unwrapExp old, unwrapExp new) of
    (EConst _ _ _ _, EConst _ _ _ _) -> preserveLeaf
    (EBase _ _, EBase _ _)           -> preserveLeaf
    (EVar _ _, EVar _ _)             -> preserveLeaf
    (EFun _ oldPats oldBody _, EFun wsb newPats newBody wsa) ->
      tryPreservePatsIds_ oldPats newPats         |> Maybe.andThen (\preservedPats ->
      tryPreserveIDs_ nextAvailId oldBody newBody |> Maybe.andThen (\(preservedBody, nextAvailId) ->
        EFun wsb preservedPats preservedBody wsa
        |> return nextAvailId
      ))
    (EApp _ oldF oldArgs _ _, EApp wsb newF newArgs appType wsa) ->
      tryPreserveIDs_ nextAvailId oldF newF           |> Maybe.andThen (\(preservedF, nextAvailId) ->
      tryPreserveExpsIds_ nextAvailId oldArgs newArgs |> Maybe.andThen (\(preservedArgs, nextAvailId) ->
        EApp wsb preservedF preservedArgs appType wsa
        |> return nextAvailId
      ))
    (EOp _ _ _ oldOps _, EOp wsb wsc op newOps wsa) ->
      tryPreserveExpsIds_ nextAvailId oldOps newOps |> Maybe.andThen (\(preservedOps, nextAvailId) ->
        EOp wsb wsc op preservedOps wsa
        |> return nextAvailId
      )
    (EList _ oldChildren _ oldM _, EList wsb newChildren ws1 newM ws2) ->
      let tryPreserveChild nextAvailId (_, oldChild) (wsC, newChild) =
        tryPreserveIDs_ nextAvailId oldChild newChild
        |> Maybe.map (\(preservedChild, nextAvailId) ->
          ((wsC, preservedChild), nextAvailId)
        )
      in
      tryPreserveListIds_ tryPreserveChild nextAvailId oldChildren newChildren
      |> Maybe.andThen (\(preservedChildren, nextAvailId) ->
        (case (oldM, newM) of
          (Just oldT, Just newT) ->
            tryPreserveIDs_ nextAvailId oldT newT
            |> Maybe.map (Tuple.mapFirst Just)
          (Nothing, Nothing) ->
            Just (Nothing, nextAvailId)
          _ ->
            Nothing
        )
      |> Maybe.andThen (\(preservedM, nextAvailId) ->
        EList wsb preservedChildren ws1 preservedM ws2
        |> return nextAvailId
      ))
    (EIf _ oldC _ oldT _ oldF _, EIf wsb newC wsT newT wsE newF wsa) ->
      tryPreserveExpsIds_ nextAvailId [oldC, oldT, oldF] [newC, newT, newF]
      |> Maybe.andThen (\(preserved, nextAvailId) ->
        let (preservedC, preservedT, preservedF) =
          Utils.mapThree (flip Utils.geti preserved) (1, 2, 3)
        in
        EIf wsb preservedC wsT preservedT wsE preservedF wsa
        |> return nextAvailId
      )
    (ECase _ oldS oldBranches _, ECase wsb newS newBranches wsa) ->
      let tryPreserveBranch nextAvailId oldB newB =
        let (Branch_ wsb newP newE wsa) = newB.val in
        tryPreservePIds_ (branchPat oldB) newP            |> Maybe.andThen (\preservedP ->
        tryPreserveIDs_ nextAvailId (branchExp oldB) newE |> Maybe.map (\(preservedE, nextAvailId) ->
          (Branch_ wsb preservedP preservedE wsa |> Info.replaceInfo newB, nextAvailId)
        ))
      in
      tryPreserveIDs_ nextAvailId oldS newS
      |> Maybe.andThen (\(preservedS, nextAvailId) ->
      tryPreserveListIds_ tryPreserveBranch nextAvailId oldBranches newBranches
      |> Maybe.andThen (\(preservedBranches, nextAvailId) ->
        ECase wsb preservedS preservedBranches wsa
        |> return nextAvailId
      ))
    (ELet _ _ oldDecls _ oldBody, ELet wsb lk newDecls ws1 newBody) ->
      tryPreserveDecls_ nextAvailId oldDecls newDecls |> Maybe.andThen (\(preservedDecls, nextAvailId) ->
      tryPreserveIDs_ nextAvailId oldBody newBody     |> Maybe.andThen (\(preservedBody, nextAvailId) ->
        ELet wsb lk preservedDecls ws1 preservedBody
        |> return nextAvailId
      ))
    (EColonType _ oldE _ oldT _, EColonType wsb newE ws1 newT wsa) ->
      tryPreserveIDs_ nextAvailId oldE newE |> Maybe.andThen (\(preservedE, nextAvailId) ->
      tryPreserveTIds_ oldT newT            |> Maybe.andThen (\preservedT ->
        EColonType wsb preservedE ws1 preservedT wsa
        |> return nextAvailId
      ))
    (EParens _ oldE _ _, EParens wsb newE ps wsa) ->
      tryPreserveIDs_ nextAvailId oldE newE |> Maybe.andThen (\(preservedE, nextAvailId) ->
        EParens wsb preservedE ps wsa
        |> return nextAvailId
      )
    (EHole _ (EEmptyHole _), EHole _ (EEmptyHole _))       -> preserveLeaf
    (EHole _ (ESnapHole _), EHole _ (ESnapHole _)) -> preserveLeaf
    (ERecord _ oldM oldDecls _, ERecord wsb newM newDecls wsa) ->
      (case (oldM, newM) of
        (Just (oldS, _), Just (newS, wsAfterS)) ->
          tryPreserveIDs_ nextAvailId oldS newS
          |> Maybe.map (Tuple.mapFirst <| flip (,) wsAfterS >> Just)
        (Nothing, Nothing) ->
          Just (Nothing, nextAvailId)
        _ ->
          Nothing
      )
      |> Maybe.andThen (\(preservedM, nextAvailId) ->
      tryPreserveDecls_ nextAvailId oldDecls newDecls
      |> Maybe.andThen (\(preservedDecls, nextAvailId) ->
        ERecord wsb preservedM preservedDecls wsa
        |> return nextAvailId
      ))
    (ESelect _ oldSel _ _ _, ESelect wsb newSel ws1 ws2 ident) ->
      tryPreserveIDs_ nextAvailId oldSel newSel |> Maybe.andThen (\(preservedSel, nextAvailId) ->
        ESelect wsb preservedSel ws1 ws2 ident
        |> return nextAvailId
      )
    -- Accounts for when the unparser adds extra parens
    (_, EParens wsb newInnerExp ps wsa) ->
      tryPreserveIDs_ (nextAvailId + 1) old newInnerExp
      |> Maybe.map (\(preservedInnerExp, nextAvailId) ->
        let newPreserved =
          EParens wsb preservedInnerExp ps wsa |> withEId nextAvailId
        in
        (newPreserved, nextAvailId)
      )
    _ ->
      Nothing

-- currently doesn't preserve loc ids
tryPreserveIDs : Exp -> Exp -> Exp
tryPreserveIDs old new =
  let freshenedOld = Parser.freshen old in
  case tryPreserveIDs_ (Parser.maxId freshenedOld + 1) freshenedOld new of
    Just (e, _) -> e
    Nothing     -> new

deuceRefresh : Exp -> Model -> Model
deuceRefresh preUnparsedExp old =
  let
    parseResult = ImpureGoodies.logTimedRun "parsing time refresh" <| \() ->
      Syntax.parser old.syntax old.code

    (newInputExp, newLastParsedCode) =
      case parseResult of
        Ok parsedExp ->
          let inputExp = tryPreserveIDs preUnparsedExp parsedExp in
          (inputExp, old.code)

        Err _ ->
          (old.inputExp, old.lastParsedCode)

    oldMbKeyboardFocusedWidget = old.deuceState.mbKeyboardFocusedWidget
    newMbKeyboardFocusedWidget =
      if isDeuceWidgetValid newInputExp oldMbKeyboardFocusedWidget then
        oldMbKeyboardFocusedWidget
      else
        Nothing
    oldDeuceState = old.deuceState
    newDeuceState =
      { oldDeuceState |
          mbKeyboardFocusedWidget = newMbKeyboardFocusedWidget
      }

  in
    { old
        | inputExp = newInputExp
        , lastParsedCode = newLastParsedCode
        , deuceState = newDeuceState
    }

refreshInputExp : Model -> Model
refreshInputExp old =
  let
    parseResult = ImpureGoodies.logTimedRun "parsing time refresh" <| \() ->
      Syntax.parser old.syntax old.code

    (newInputExp, holeEnv, newCodeBoxInfo, newLastParsedCode, typeChecks) =
      case parseResult of
        Ok parsedExp ->
          let
            (typedInputExp, holeEnv, aceTypeInfo, typeChecks) =
              maybeTypeCheck old.doTypeChecking parsedExp
            inputExp = tryPreserveIDs old.inputExp typedInputExp
          in
            (inputExp, holeEnv, updateCodeBoxInfo aceTypeInfo old, old.code, typeChecks)

        Err _ ->
          (old.inputExp, old.holeEnv, old.codeBoxInfo, old.lastParsedCode, True)
  in
    { old
        | inputExp =
            newInputExp
        , holeEnv =
            holeEnv
        , lastParsedCode =
            newLastParsedCode
        , codeBoxInfo =
            newCodeBoxInfo
    }
      |> showTypeInspectorIfNecessary typeChecks

--------------------------------------------------------------------------------

getKeyboardFocusedWidget : Model -> Maybe DeuceWidget
getKeyboardFocusedWidget model =
  let ds = model.deuceState in
  case (ds.mbKeyboardFocusedWidget, ds.selectedWidgets) of
    (Just keyboardFocusedWidget, _) -> Just keyboardFocusedWidget
    (_, selected :: _) -> Just selected
    _ -> Nothing

isKeyDown : Int -> Model -> Bool
isKeyDown keyCode model =
  List.member keyCode model.keysDown

msgKeyPress keyCode = Msg ("Key Press " ++ toString keyCode) <| \old ->
  old

msgKeyDown keyCode =
  let
    addKey old =
      let
        currentKeyDown =
          isKeyDown keyCode old
      in
        if not currentKeyDown then
          { old | keysDown = keyCode :: old.keysDown }
        else
          old
    func old =
      let
        currentKeyDown =
          isKeyDown keyCode old
        noughtSelectedInOutput =
          nothingSelectedInOutput old
        somethingSelectedInOutput =
          not noughtSelectedInOutput
        mbKeyboardFocusedWidget =
          getKeyboardFocusedWidget old
      in
        if keyCode == Keys.keyEsc then
          if Model.anyDialogShown old then
            Model.closeAllDialogBoxes old

          else if noughtSelectedInOutput
                    && not (deucePopupPanelShown old)
                    && (old.codeEditorMode == CEDeuceClick || old.codeEditorMode == CETypeInspector)
                    && not currentKeyDown then
            { old | codeEditorMode = CEText }

          else
            let
              new =
                { old | renamingInOutput = Nothing }
                  |> Model.hideDeuceRightClickMenu
                  |> resetDeuceState
                  |> resetDeuceKeyboardInfo
                  |> \m -> { m | deucePopupPanelAbove = True }
            in
              case (old.tool, old.mouseMode) of
                (Cursor, _)         -> clearSelections new
                (_, MouseNothing)   -> { new | tool = Cursor }
                (_, MouseDrawNew _) -> { new | mouseMode = MouseNothing }
                _                   -> new

        else if somethingSelectedInOutput && keyCode == Keys.keyE && List.any Keys.isCommandKey old.keysDown && List.length old.keysDown == 1 then
          let newModel = doMakeEqual old in
          newModel.synthesisResultsDict
          |> Dict.get "Make Equal"
          |> Maybe.andThen (Utils.findFirst isResultSafe)
          |> Maybe.map (\synthesisResult -> { newModel | code = Syntax.unparser old.syntax  (resultExp synthesisResult) } |> clearSynthesisResults |> upstateRun )
          |> Maybe.withDefault old
        else if somethingSelectedInOutput && keyCode == Keys.keyBackspace then
          deleteInOutput old
        else if somethingSelectedInOutput && keyCode == Keys.keyD && List.any Keys.isCommandKey old.keysDown && List.length old.keysDown == 1 then
          doDuplicate old
        else if somethingSelectedInOutput && keyCode == Keys.keyG && List.any Keys.isCommandKey old.keysDown && List.length old.keysDown == 1 then
          doGroup old
        else if somethingSelectedInOutput && keyCode == Keys.keyZ && List.any Keys.isCommandKey old.keysDown && List.length old.keysDown == 1 then
          doUndo old
        else if somethingSelectedInOutput && keyCode == Keys.keyZ && List.any Keys.isCommandKey old.keysDown && List.any ((==) Keys.keyShift) old.keysDown && List.length old.keysDown == 2 then
          doRedo old

        else if
          List.any Keys.isCommandKey old.keysDown &&
          List.any ((==) Keys.keyShift) old.keysDown &&
          keyCode == Keys.keyDigit 8
        then
          { old | mainResizerX = Just <| Layout.mainResizerLeftBound old }

        else if
          List.any Keys.isCommandKey old.keysDown &&
          List.any ((==) Keys.keyShift) old.keysDown &&
          keyCode == Keys.keyDigit 9
        then
          { old | mainResizerX = Nothing }

        else if
          List.any Keys.isCommandKey old.keysDown &&
          List.any ((==) Keys.keyShift) old.keysDown &&
          keyCode == Keys.keyDigit 0
        then
          { old | mainResizerX = Just <| Layout.mainResizerRightBound old }

        else if keyCode == Keys.keyEnter && List.any Keys.isCommandKey old.keysDown && List.length old.keysDown == 1 then
          case old.outputMode of
            Graphics ->
              upstateRun old
            HtmlText _ ->
              let _ = Debug.log "TODO Enter" () in
              upstateRun old
            PBE ->
              upstateRun old
            -- ShowValue -> doCallUpdate old
            _ ->
              old

        else if old.outputMode == Graphics && keyCode == Keys.keyDown &&
                List.any Keys.isCommandKey old.keysDown && List.length old.keysDown == 1 then
          { old | outputMode = HtmlText (LangSvg.printHTML old.showGhosts old.slate) }
        else if isHtmlText old.outputMode && keyCode == Keys.keyDown &&
                List.any Keys.isCommandKey old.keysDown && List.length old.keysDown == 1 then
          { old | outputMode = ValueText }
        else if old.outputMode == ValueText && keyCode == Keys.keyUp &&
                List.any Keys.isCommandKey old.keysDown && List.length old.keysDown == 1 then
          { old | outputMode = HtmlText (LangSvg.printHTML old.showGhosts old.slate) }
        else if isHtmlText old.outputMode && keyCode == Keys.keyUp &&
                List.any Keys.isCommandKey old.keysDown && List.length old.keysDown == 1 then
          { old | outputMode = Graphics }

        else if noughtSelectedInOutput && old.codeEditorMode == CEDeuceClick
                && not currentKeyDown && mbKeyboardFocusedWidget /= Nothing then
          let keyCodes = keyCode :: old.keysDown |> List.sort in
          if old.isDeuceTextBoxFocused then
            if old.mbDeuceKeyboardInfo /= Nothing then
              handleDeuceTextBoxCommand old keyCodes
            else
              old
          else
            handleDeuceHotKey old keyCodes <| Utils.fromJust_ "Impossible" mbKeyboardFocusedWidget

        else if
          not currentKeyDown &&
          (keyCode == Keys.keyShift || keyCode == Keys.keyAlt)
        then
          refreshInputExp old
        else
          old
  in
    Msg ("Key Down " ++ toString keyCode) <|
      addKey << func

msgKeyUp keyCode = Msg ("Key Up " ++ toString keyCode) <| \old ->
  -- let _ = Debug.log "Key Up" (keyCode, old.keysDown) in
  if Keys.isCommandKey keyCode then
    -- When keyMeta (command key) is down and another key k is downed,
    -- there will not be a key up event for k.
    --   (This is not true for all keys, for example, up and down...)
    -- So remove k when keyMeta goes up.
    { old | keysDown = List.filter ((==) Keys.keyShift) old.keysDown }
  else
    { old | keysDown = Utils.removeAsSet keyCode old.keysDown }



--------------------------------------------------------------------------------

cleanSynthesisResult (SynthesisResult {description, exp, isSafe, sortKey, children, nextLazy}) =
  SynthesisResult <|
    { description = description ++ " → Cleaned"
    , exp         = LangSimplify.cleanCode exp
    , diffs       = []
    , isSafe      = isSafe
    , sortKey     = sortKey
    , children    = children
    , nextLazy    = nextLazy
    }

cleanDedupSortSynthesisResults model synthesisResults =
  synthesisResults
  -- |> List.map cleanSynthesisResult
  |> Utils.dedupBy (\(SynthesisResult {description, exp, sortKey, children}) -> Syntax.unparser model.syntax exp)
  |> List.sortBy (\(SynthesisResult {description, exp, sortKey, children}) -> (LangTools.nodeCount exp, sortKey, description))

perhapsRunAutoSynthesis model program =
  if model.autoSynthesis
    then cleanDedupSortSynthesisResults model (ETransform.passiveSynthesisSearch model program)
    else []

msgCleanCode = Msg "Clean Code" <| \old ->
  case Syntax.parser old.syntax old.code of
    Err err ->
      { old | caption = Just (LangError (showError err)) }
    Ok reparsed ->
      let cleanedExp = LangSimplify.cleanCode reparsed in
      let code_ = Syntax.unparser old.syntax cleanedExp in
      if old.code == code_ then old
      else
        let _ = debugLog "Cleaned: " code_ in
        upstateRun { old | inputExp = cleanedExp, code = code_ }

msgDigHole = Msg "Dig Hole" <| \old ->
  let newExp =
    ValueBasedTransform.digHole old.inputExp old.selectedFeatures old.slate old.widgets old.syncOptions
  in
  runWithErrorHandling old newExp (\reparsed newVal newEnv newWidgets newSlate newCode ->
    debugLog "new model" <|
    clearSelections <|
      { old | code             = newCode
            , inputExp         = reparsed
            , inputVal         = newVal
            , inputEnv         = newEnv
            , history          = modelCommit newCode [] Nothing old.history
            , slate            = newSlate
            , slateCount       = 1 + old.slateCount
            , widgets          = newWidgets
            , preview          = Nothing
              -- we already ran it successfully once so it shouldn't crash the second time
            , liveSyncInfo     = Utils.fromOk "DigHole MkLive" <|
                                   mkLive old.syntax old.syncOptions
                                     old.slideNumber old.movieNumber old.movieTime reparsed
                                     (newVal, newWidgets)
      }
  )

msgMakeEqual = Msg "Make Equal" doMakeEqual

doMakeEqual old =
  let synthesisResults =
    ValueBasedTransform.makeEqual
        old.syntax
        old.solutionsCache
        old.inputExp
        old.selectedFeatures
        old.slideNumber
        old.movieNumber
        old.movieTime
        old.syncOptions
  in
  { old | synthesisResultsDict = Dict.insert "Make Equal" (cleanDedupSortSynthesisResults old synthesisResults) old.synthesisResultsDict }

msgRelate = Msg "Relate" <| \old ->
  let synthesisResults =
    ValueBasedTransform.relate
        old.syntax
        old.solutionsCache
        old.inputExp
        old.selectedFeatures
        old.slideNumber
        old.movieNumber
        old.movieTime
        old.syncOptions
  in
  { old | synthesisResultsDict = Dict.insert "Relate" (cleanDedupSortSynthesisResults old synthesisResults) old.synthesisResultsDict }

msgIndexedRelate = Msg "Indexed Relate" <| \old ->
  let synthesisResults =
    ValueBasedTransform.indexedRelate
        old.syntax
        old.inputExp
        old.selectedFeatures
        old.selectedShapes
        old.slideNumber
        old.movieNumber
        old.movieTime
        old.syncOptions
  in
  { old | synthesisResultsDict = Dict.insert "Indexed Relate" (cleanDedupSortSynthesisResults old synthesisResults) old.synthesisResultsDict }

-- msgMakeEquidistant = Msg "Make Equidistant" <| \old ->
--   let newExp =
--     ValueBasedTransform.makeEquidistant
--         old.inputExp
--         old.selectedFeatures
--         old.slideNumber
--         old.movieNumber
--         old.movieTime
--         old.slate
--         old.syncOptions
--   in
--   runWithErrorHandling old newExp (\reparsed newVal newWidgets newSlate newCode ->
--     debugLog "new model" <|
--       { old | code             = newCode
--             , inputExp         = reparsed
--             , inputVal         = newVal
--             , inputEnv         = newEnv
--             , history          = addToHistory old.code old.history
--             , slate            = newSlate
--             , widgets          = newWidgets
--             , preview          = Nothing
--               -- we already ran it successfully once so it shouldn't crash the second time
--             , mode             = Utils.fromOk "MakeEquidistant MkLive" <|
--                                    mkLive old.syncOptions
--                                      old.slideNumber old.movieNumber old.movieTime reparsed
--                                      (newVal, newWidgets)
--             , selectedFeatures = Set.empty
--       }
--   )

msgBuildAbstraction = Msg "Build Abstraction" <| \old ->
  let synthesisResults =
    ValueBasedTransform.buildAbstraction
        old.syntax
        old.inputExp
        old.selectedFeatures
        old.selectedShapes
        old.selectedBlobs
        old.slideNumber
        old.movieNumber
        old.movieTime
        old.syncOptions
  in
  { old | synthesisResultsDict = Dict.insert "Build Abstraction" (cleanDedupSortSynthesisResults old synthesisResults) old.synthesisResultsDict }

deleteInOutput old =
  let
    proximalInterpretations =
      ShapeWidgets.selectionsUniqueProximalEIdInterpretations
          old.inputExp
          old.slate
          old.widgets
          old.selectedFeatures
          old.selectedShapes
          old.selectedBlobs

    deleteEId eidToDelete program =
      case findExpByEId program eidToDelete of
        Just expToDelete ->
          let programWithDistalExpressionRemoved =
            case LangTools.findLetAndPatMatchingExpLoose (expEId expToDelete) program of
               Just (letExp, patBindingExpToDelete) ->
                 let identsToDelete = identifiersListInPat patBindingExpToDelete in
                 let scopeAreas = LangTools.findScopeAreas ((expEId letExp), 0) letExp "" in
                 let varUses = scopeAreas |> List.concatMap (LangTools.identifierSetUses (Set.fromList identsToDelete)) in
                 let deleteVarUses program =
                   varUses
                   |> List.map expEId
                   |> List.foldr deleteEId program
                   |> LangSimplify.simplifyAssignments
                 in
                 case CodeMotion.pluckByPId patBindingExpToDelete.val.pid program of -- TODO allow unsafe pluck out of as-pattern
                   Just (_, programWithoutBinding) -> deleteVarUses programWithoutBinding
                   Nothing                         -> deleteVarUses program

               Nothing ->
                 case parentByEId program (expEId <| LangTools.outerSameValueExp program expToDelete) of
                   (Just (Just parent)) ->
                     case (unwrapExp parent) of
                       EFun _ _ _ _ ->
                         deleteEId (expEId parent) program

                       EList ws1 heads ws2 maybeTail ws3 ->
                         case Utils.listValues heads |> Utils.findi (eidIs eidToDelete) of
                           Just iToDelete -> program |> replaceExpNodeE__ parent (EList ws1 (List.map ((,) space0) (Utils.removei iToDelete (Utils.listValues heads) |> imitateExpListWhitespace (Utils.listValues heads))) ws2 maybeTail ws3)
                           Nothing ->
                             if Maybe.map (eidIs eidToDelete) maybeTail == Just True
                             then program |> replaceExpNodeE__ parent (EList ws1 heads ws2 Nothing ws3)
                             else program

                       _ ->
                         let _ = Utils.log <| "can't remove from parent " ++ Syntax.unparser old.syntax parent in
                         program

                   _ ->
                     let _ = Utils.log <| "can't find parent to remove from" in
                     program
          in
          -- This seems to remove too much (e.g. will remove function if an application is deleted).
          -- let varEIdsPerhapsRemoved = LangTools.freeVars expToDelete |> List.map expEId |> Set.fromList in
          let varEIdsPerhapsRemoved =
            case LangTools.expToMaybeVar (expEffectiveExp expToDelete) of
               Just varExp -> Set.singleton ((expEId varExp))
               _           -> Set.empty
          in
          let pidsToMaybeRemove =
            program
            |> LangTools.allVarEIdsToBindingPId
            |> Dict.filter (\varEId _ -> Set.member varEId varEIdsPerhapsRemoved)
            |> Dict.values
            |> Utils.filterJusts -- Vars free in program are marked bound to "Nothing"
            |> Set.fromList
          in
          programWithDistalExpressionRemoved
          |> LangSimplify.removeUnusedLetPatsMatching (\pat -> Set.member pat.val.pid pidsToMaybeRemove)

        _ ->
          program


    deleteResults =
      proximalInterpretations
      |> List.take 1
      |> List.map (\eids -> eids |> List.foldl deleteEId old.inputExp)
  in
  case deleteResults of
    []              -> old
    deleteResult::_ -> upstateRun <| clearSelections { old | code = Syntax.unparser old.syntax deleteResult }

--------------------------------------------------------------------------------

msgSelectSynthesisResult: Exp -> Msg
msgSelectSynthesisResult newExp = Msg "Select Synthesis Result" <| doSelectSynthesisResult newExp

doSelectSynthesisResult: Exp -> Model -> Model
doSelectSynthesisResult newExp old =
  -- TODO unparse gets called twice, here and in runWith ...
  -- TODO decide whethert to typecheck newExp before or after SynthesisResult...
  let newCode = Syntax.unparser old.syntax newExp in
  let new =
    { old | code = newCode
          , lastParsedCode = newCode
          , lastRunCode = newCode
          , history = modelCommit newCode [] Nothing old.history
          } |> clearSynthesisResults
  in
  runWithErrorHandling new newExp (\reparsed newVal newEnv newWidgets newSlate newCode ->
    -- debugLog "new model" <|
      let newer =
      { new | inputExp             = reparsed -- newExp
            , inputVal             = newVal
            , inputEnv             = newEnv
            , valueEditorString    = LangUtils.valToString newVal
            , outputMode           = maybeUpdateOutputMode old newSlate
            , slate                = newSlate
            , slateCount           = 1 + old.slateCount
            , widgets              = newWidgets
            , synthesisResultsDict = Dict.singleton "Auto-Synthesis" (perhapsRunAutoSynthesis old reparsed)
      } |> clearSelections
      in
      { newer | liveSyncInfo = refreshLiveInfo newer
              , codeBoxInfo = updateCodeBoxInfo Types2.dummyAceTypeInfo newer
              , outputMode  = Graphics -- switch out of ValueText
              }
  )

maybeUpdateOutputMode: Model-> LangSvg.RootedIndexedTree -> OutputMode
maybeUpdateOutputMode old newSlate =  case old.outputMode of
  HtmlText k -> HtmlText (LangSvg.printHTML old.showGhosts newSlate)
  x -> x

clearSynthesisResults : Model -> Model
clearSynthesisResults old =
  { old
      | preview = Nothing
      , synthesisResultsDict = Dict.empty
      , updatedValue = Nothing
      }

msgClearSynthesisResults : Msg
msgClearSynthesisResults =
  Msg "Clear Synthesis Results" clearSynthesisResults

msgStartAutoSynthesis : Msg
msgStartAutoSynthesis =
  Msg "Start Auto Synthesis" <| \old ->
    { old | autoSynthesis = True }

msgStopAutoSynthesisAndClear : Msg
msgStopAutoSynthesisAndClear =
  Msg "Stop Auto Synthesis and Clear" <| \old ->
    clearSynthesisResults { old | autoSynthesis = False }

--------------------------------------------------------------------------------

msgGroupBlobs = Msg "Group Blobs" doGroup

doGroup =
  \old ->
    case Blobs.maybeSimpleProgram old.inputExp of
      Nothing -> old
      Just simple ->
        let maybeAnchorPoint = ETransform.anchorOfSelectedFeatures old.selectedFeatures in
        let multipleSelectedBlobs = Dict.size old.selectedBlobs > 1 in
        case (maybeAnchorPoint, multipleSelectedBlobs) of
          (Ok Nothing, False)   -> old
          (Ok Nothing, True)    -> upstateRun <| ETransform.groupSelectedBlobs old simple
          (Ok (Just anchor), _) -> upstateRun <| ETransform.groupSelectedBlobsAround old simple anchor
          (Err err, _)          -> let _ = Debug.log "bad anchor" err in old


-- Find a single expression that explains everything selected: duplicate it.
--
-- Could change to instead duplicate everything selected, inidividually.
msgDuplicate = Msg "Duplicate " doDuplicate


-- (def r 15)
--
-- (def circles
--   (map (\i
--       (let [cx cy r] [208 168 (+ (* i 18) r)]
--       (let color (if (= (mod i 2) 0) 499 0)
--         (rawCircle color 360 0 cx cy r))))
--     (reverse (zeroTo 8!{0-15}))))
--
-- (svg (concat [
--   circles
-- ]))

doDuplicate : Model -> Model
doDuplicate old =
  let
    singleExpressionInterpretations =
      ShapeWidgets.selectionsSingleEIdInterpretations
          old.inputExp
          old.slate
          old.widgets
          old.selectedFeatures
          old.selectedShapes
          old.selectedBlobs
          (always True)

  -- let _ = Utils.log <| LangUnparser.unparseWithIds old.inputExp in

    maybeNewProgram =
      singleExpressionInterpretations
      -- |> Debug.log "possible eids to duplicate"
      -- |> List.map (\eid -> let _ = Utils.log <| unparse <| LangTools.justFindExpByEId old.inputExp eid in eid)
      |> List.map (LangTools.outerSameValueExpByEId old.inputExp >> expEId)
      |> Utils.dedup
      |> List.map (LangTools.justFindExpByEId old.inputExp)
      |> List.filter (not << isVar << expEffectiveExp)
      |> List.sortBy LangTools.expToLocation -- Choose earliest single expression in program.
      |> List.head -- No multiple synthesis options for now.
      |> Maybe.map
          (\expToDuplicate ->
            let name = LangTools.expNameForExp old.inputExp expToDuplicate |> LangTools.removeTrailingDigits in
            -- Attempt 1: Try to add to output as a shape.
            let newProgram = Draw.addShape old name expToDuplicate (Set.size old.selectedShapes + Set.size old.selectedFeatures + Dict.size old.selectedBlobs) in
            if not <| LangUnparser.expsEquivalent newProgram old.inputExp then
              newProgram
            else
              -- Attempt 2: Simply duplicate the definition.
              -- TODO: Duplicate entire equation if selected.
              -- TODO: Lift free vars? Should be uncommon.
              let
                eidToInsertBefore =
                  case LangTools.findLetAndPatMatchingExpLoose (expEId expToDuplicate) old.inputExp of
                    Nothing          -> (expEId expToDuplicate)
                    Just (letExp, _) -> (expEId letExp)

                (_, newProgram) = EvalUpdate.newVariableVisibleTo -1 name 1 expToDuplicate [(expEId expToDuplicate)] old.inputExp
              in
              newProgram
          )
  in
  maybeNewProgram
  |> Maybe.map (\newProgram -> { old | code = Syntax.unparser old.syntax newProgram } |> clearSelections |> upstateRun)
  |> Maybe.withDefault old

msgMergeBlobs = Msg "Merge Blobs" <| \old ->
  if Dict.size old.selectedBlobs <= 1 then old
  else upstateRun <| ETransform.mergeSelectedBlobs old

msgAbstractBlobs = Msg "Abstract Blobs" <| \old ->
  upstateRun <| ETransform.abstractSelectedBlobs old

msgReplicateBlob option = Msg "Replicate Blob" <| \old ->
  case Blobs.maybeSimpleProgram old.inputExp of
    Nothing     -> old
    Just simple -> upstateRun <| ETransform.replicateSelectedBlob option old simple

--------------------------------------------------------------------------------

msgToggleCodeBox = Msg "Toggle Code Box" <| \old ->
  { old | basicCodeBox = not old.basicCodeBox }

msgSetOutputGraphics = Msg "Set Output Graphics" <| \old ->
  { old | outputMode = Graphics }

msgSetOutputHtmlText = Msg "Set Output Html Text" doSetOutputHtmlText

doSetOutputHtmlText : Model -> Model
doSetOutputHtmlText old =
  { old | outputMode = HtmlText (LangSvg.printHTML old.showGhosts old.slate), slateCount =  old.slateCount + 1 }

msgSetOutputValueText = Msg "Set Output Value Text" <| \old ->
  { old | outputMode = ValueText }

msgSetOutputPBE = Msg "Set Output PBE" <| \old ->
  { old | outputMode = PBE }

msgSetOutputStructuredEditor = Msg "Set Output Structured Editor" <| \old ->
  { old | outputMode = StructuredEditor }

updateHeuristics : Int -> Model -> Model
updateHeuristics heuristic old =
  let
    oldSyncOptions =
      old.syncOptions
    newSyncOptions =
      { oldSyncOptions | feelingLucky = heuristic }
  in
    case old.outputMode of
      Graphics ->
        case mkLive
               old.syntax
               newSyncOptions
               old.slideNumber
               old.movieNumber
               old.movieTime
               old.inputExp
               (old.inputVal, old.widgets) of
          Ok m ->
            { old | syncOptions = newSyncOptions, liveSyncInfo = m }
          Err s ->
            { old | syncOptions = newSyncOptions, errorBox = Just s }
      _ -> { old | syncOptions = newSyncOptions }

msgSetHeuristicsBiased =
  Msg "Set Heuristics Biased" (updateHeuristics Sync.heuristicsBiased)

msgSetHeuristicsNone =
  Msg "Set Heuristics None" (updateHeuristics Sync.heuristicsNone)

msgSetHeuristicsFair =
  Msg "Set Heuristics Fair" (updateHeuristics Sync.heuristicsFair)

--------------------------------------------------------------------------------

msgStartAnimation = Msg "Start Animation" <| \old ->
  upstate msgRedraw { old | movieTime = 0, runAnimation = True }

msgRedraw = Msg "Redraw" <| \old ->
  case LangSvg.fetchEverything old.syntax old.slideNumber old.movieNumber old.movieTime old.inputVal of
    Ok (newSlideCount, newMovieCount, newMovieDuration, newMovieContinue, newSlate) ->
      { old | slideCount    = newSlideCount
            , movieCount    = newMovieCount
            , movieDuration = newMovieDuration
            , movieContinue = newMovieContinue
            , slate         = newSlate }
    Err s -> { old | errorBox = Just s }

msgTickDelta deltaT = Msg ("Tick Delta " ++ toString deltaT) <| \old ->
  if old.movieTime < old.movieDuration then
    -- Prevent "jump" after slow first frame render.
    let adjustedDeltaT = if old.movieTime == 0.0 then clamp 0.0 50 deltaT else deltaT in
    let newMovieTime = clamp 0.0 old.movieDuration (old.movieTime + (adjustedDeltaT / 1000)) in
    upstate msgRedraw { old | movieTime = newMovieTime }
  else if old.movieContinue == True then
    upstate msgNextMovie old
  else
    { old | runAnimation = False }

msgNextSlide = Msg "Next Slide" <| \old ->
  if old.slideNumber >= old.slideCount then
    upstate msgStartAnimation
      { old | slideNumber = old.slideNumber
            , movieNumber = old.movieCount }
  else
    upstate msgStartAnimation
      { old | slideNumber = old.slideNumber + 1
            , movieNumber = 1 }

msgPreviousSlide = Msg "Previous Slide" <| \old ->
  if old.slideNumber <= 1 then
    upstate msgStartAnimation
      { old | slideNumber = 1, movieNumber = 1 }
  else
    let previousSlideNumber = old.slideNumber - 1 in
    let result =
      EvalUpdate.run old.syntax old.inputExp |>
      Result.andThen (\(previousVal, _) ->
        LangSvg.resolveToMovieCount old.syntax previousSlideNumber previousVal
        |> Result.map (\previousMovieCount ->
             upstate msgStartAnimation
               { old | slideNumber = previousSlideNumber
                     , movieNumber = previousMovieCount }
        )
      )
    in
    handleError old result

msgNextMovie = Msg "Next Movie" <| \old ->
  if old.movieNumber == old.movieCount && old.slideNumber < old.slideCount then
    upstate msgNextSlide old
  else if old.movieNumber < old.movieCount then
    upstate msgStartAnimation { old | movieNumber = old.movieNumber + 1 }
  else
    -- Last movie of slide show; skip to its end.
    upstate msgRedraw { old | movieTime    = old.movieDuration
                            , runAnimation = False }

msgPreviousMovie = Msg "Previous Movie" <| \old ->
  if old.movieNumber == 1 then
    upstate msgPreviousSlide old
  else
    upstate msgStartAnimation { old | movieNumber = old.movieNumber - 1 }

msgPauseResumeMovie = Msg "Pause/Resume Movie" <| \old ->
  { old | runAnimation = not old.runAnimation }


--------------------------------------------------------------------------------

msgUpdateValueEditor s = Msg "Update Value Editor" <| \m ->
  { m
      | valueEditorString =
          s
      , synthesisResultsDict =
          Dict.remove valueBackpropToolName m.synthesisResultsDict
      }

msgUpdateHTMLEditor s = Msg "Update HTML Editor" <| \m ->
  { m
      | htmlEditorString = Just s
      , synthesisResultsDict =
          Dict.remove valueBackpropToolName m.synthesisResultsDict
  }

msgCallUpdate = Msg "Call Update" doCallUpdate

doCallUpdate m =
  --let _ = Debug.log "I'll find the value" () in
  let updatedValResult =
    if htmlEditorNeedsCallUpdate m then
       Result.fromMaybe "No new HTML updated value available" m.htmlEditorString |>
         Result.andThen Update.buildUpdatedValueFromHtmlString
    else if domEditorNeedsCallUpdate m then
       Result.fromMaybe "No new updated value available" m.updatedValue |> Result.andThen (\i -> i)
    else
        m.valueEditorString
          |> Update.buildUpdatedValueFromEditorString m.syntax
  -- TODO updated value may be back to original, so may want to
  -- detect this and write a caption that says so.
  in
  --let _ = Debug.log "I'm here" () in
  let showSolutions results =
    -- when the only option is to revert, hovering over that option
    -- isn't correctly re-running and replacing the dirty slate.
    -- here's a temporary workaround: add a second dummy option.
    let results_ =
       results
{-
       case results of
         [_] ->
           let dummyResult =
             synthesisResult
                "HACK: Hover over this before selecting the following..."
                (eStr0 "HACK")
           in
           dummyResult :: results
         _   ->
           results
-}
    in
    { m
        | synthesisResultsDict =
            Dict.insert valueBackpropToolName results_ m.synthesisResultsDict
        }
  in
  backpropSynthesisResults m.inputExp m.inputEnv m.inputVal updatedValResult
  |> showSolutions


backpropSynthesisResults : Exp -> Env -> Val -> Result String Val -> List SynthesisResult
backpropSynthesisResults program inputEnv inputVal updatedValResult =
  --let _ = Debug.log "Let's do update" () in
  let updatedExpResults =
    EvalUpdate.doUpdate program inputEnv inputVal updatedValResult
  in
  let revertChanges caption =
    synthesisResult caption program
  in
  case updatedExpResults of
    Err msg ->
      let _ = Debug.log msg () in
      [revertChanges ("Error while updating: " ++ msg ++ ". Revert?")]

    Ok solutions ->
      let _ = Debug.log "Filtering solutions" () in
      let solutionsNotModifyingEnv =
         LazyList.filter
           (\(env, exp) -> List.isEmpty env.changes)
           solutions
      in
      let _ = Debug.log "Filtered solutions" () in
      case solutionsNotModifyingEnv of
        LazyList.Nil ->
          case solutions of
            LazyList.Nil ->
              [revertChanges "No solution found. Revert?"]

            LazyList.Cons (envModified, expModified) _ ->
              let _ = Debug.log (UpdateUtils.diffExp program expModified.val) "expModified" in
              let _ = Debug.log (EvalUpdate.preludeEnv |> List.take 5 |> List.map Tuple.first |> String.join " ") ("EnvNames original") in
              let _ = Debug.log (envModified.val |> List.take 5 |> List.map Tuple.first |> String.join " ") ("EnvNames modified") in
              let envModif = UpdateUtils.envDiffsToString EvalUpdate.preludeEnv envModified.val envModified.changes in
              let _ = Debug.log envModif ("EnvModified") in
              --let _ = Debug.log (UpdateUtils.diff (\(k, v) -> LangUtils.valToString v) (LangUtils.pruneEnv expModified envModified.val) (LangUtils.pruneEnv expModified EvalUpdate.preludeEnv)
              --     |> UpdateUtils.displayDiff (\(k, v) -> "\n" ++ k ++ " = " ++ LangUtils.valToString v )
              --     ) "envModified"
              --in

              [revertChanges <| "Only solutions modifying the library. Revert?\n\n"++envModif]

        LazyList.Cons _ _ ->
          let filteredResults =
            solutionsNotModifyingEnv
              --|> (\x -> let _ = Debug.log "Finished to obtain the list of solutions" () in x)
              |> LazyList.filter (\(_,newCodeExp) -> not <| Utils.maybeIsEmpty newCodeExp.changes)
              --|> (\x -> let _ = Debug.log "Finished to filter the list of solutions" () in x)
              |> LazyList.map (\(_,newCodeExp) ->
                   --synthesisResult ("Program Update " ++ toString i) newCodeExp
                   --let (diffResult, diffs) = ImpureGoodies.logTimedRun "UpdateUtils.diffExpWithPositions" <| \_ -> UpdateUtils.diffExpWithPositions program newCodeExp in
                   let (diffResult, diffs) = ImpureGoodies.logTimedRun "UpdateUtils.updatedExpToString" <| \_ -> UpdateStack.updatedExpToStringWithPositions program newCodeExp in -- TODO: Incorporate positions.
                   (String.trim diffResult, newCodeExp.val, diffs, newCodeExp.changes)
                   --synthesisResult diffResult newCodeExp.val diffs
                 )
              --|> (\x -> let _ = Debug.log "Finished to diff the solutions" () in x)
          in
          let aux: Bool -> Int -> LazyList (String, Exp, List Exp, Maybe EDiffs) -> nativeDict -> Maybe SynthesisResult
              aux stopIfDuplicate i ll prevSolutions = case ll of
            LazyList.Nil -> Nothing
            LazyList.Cons (diffResult, newCode, diffs, changes) lazyTail ->
              let thisSolution = Syntax.unparser Syntax.Leo newCode in
              if nativeDict.get thisSolution prevSolutions /= Nothing then -- remove duplicates only if the final Exp is the same
                if stopIfDuplicate then
                  Just <| synthesisResultDiffsLazy ("Solution #" ++ toString i ++ " is duplicate. Continue searching?") program [] <| \_ ->
                    aux False (i + 1) (Lazy.force lazyTail) prevSolutions
                else
                  aux False (i + 1) (Lazy.force lazyTail) prevSolutions
              else -- either the first solution or a different solution
                Just <| synthesisResultDiffsLazy diffResult newCode diffs <| \_ ->
                  aux True (i + 1) (Lazy.force lazyTail) (nativeDict.insert thisSolution 1 prevSolutions)
          in
          case aux False 1 filteredResults (nativeDict.empty ()) of
            Nothing ->
              [revertChanges "Only Solution is Original Program"]
            Just synthesisResult ->
              [synthesisResult, revertChanges "Revert to Original Program"]


decodeStyles : JSDecode.Decoder Val
decodeStyles =
  JSDecode.lazy <| \_ ->
    JSDecode.map vList <|
      JSDecode.list <|
        JSDecode.map2 (\key value -> vList [vStr key, vStr value])
          (JSDecode.index 0 JSDecode.string)
          (JSDecode.index 1 JSDecode.string)

decodeAttributes : JSDecode.Decoder Val
decodeAttributes =
  JSDecode.lazy <| \_ ->
  JSDecode.map vList <|
    JSDecode.list <|
        (JSDecode.index 0 JSDecode.string
         |> JSDecode.andThen (\key ->
           JSDecode.map2 (\vKey vValue -> vList [vKey, vValue])
             (JSDecode.succeed (vStr key))
             (if key /= "style" then
               (JSDecode.index 1 (JSDecode.map vStr JSDecode.string))
             else
               (JSDecode.index 1 decodeStyles))
           ))

decodeChildList: JSDecode.Decoder Val
decodeChildList =
  JSDecode.lazy <| \_ ->
  JSDecode.map vList
  (JSDecode.list <| JSDecode.lazy <| \_ -> decodeElemChild)

decodeElem: JSDecode.Decoder Val
decodeElem =
  JSDecode.lazy <| \_ ->
  JSDecode.map3 (\tag attrs children ->
     if tag == "comment" then
       case Vu.list Vu.identity attrs of
         Ok (head :: tail) ->
           case Vu.viewtuple2 Vu.string Vu.string head of
             Ok (("title", content)) ->
                let result = vList [vStr "COMMENT", vStr content] in
                let _ = Debug.log "comment resolved:" (LangUtils.valToString result) in
                result
             _ -> vList [vStr tag, attrs, children]
         _ -> vList [vStr tag, attrs, children]
     else
       vList [vStr tag, attrs, children])
     (JSDecode.index 0 JSDecode.string)
     (JSDecode.index 1 decodeAttributes)
     (JSDecode.index 2 decodeChildList)

decodeText: JSDecode.Decoder Val
decodeText = JSDecode.lazy <| \_ ->
  JSDecode.string |> JSDecode.map (\str -> vList [vStr "TEXT", vStr str])

decodeComment: JSDecode.Decoder Val
decodeComment = JSDecode.lazy <| \_ ->
  JSDecode.lazy <| \_ ->
    JSDecode.map2 (\_ content ->
       vList [vStr "COMMENT", vStr content])
       (JSDecode.index 0 JSDecode.string)
       (JSDecode.index 1 JSDecode.string)

decodeElemChild =
  JSDecode.lazy <| \_ ->
    JSDecode.oneOf [
      decodeElem, -- If 3 elements in the array
      decodeComment, -- If only 2 elements in the array
      decodeText -- If the element is some string.
    ]

integrateValue: List Int -> Val -> Val -> Result String Val
integrateValue path oldValue subValue =
  case path of
    [] -> Ok subValue
    i::itail -> case oldValue.v_ of
      VList elems ->
        let (elemsBefore, elemsAfter) = Utils.split i elems in
        case elemsAfter of
          changedElem::elemsRemaining ->
            integrateValue itail changedElem subValue |> Result.map (\newChangedElem ->
              replaceV_ oldValue <| VList <| elemsBefore ++ (newChangedElem :: elemsRemaining)
            )
          _ -> Err <| "Could not recover the change. The old value was " ++ LangUtils.valToString oldValue ++ " but we need to access index " ++ toString i
      _ -> Err <| "Expected to update a list, got " ++ LangUtils.valToString oldValue

msgValuePathUpdate: (Int, List Int, JSDecode.Value) -> Msg
msgValuePathUpdate (nodeAttrsOrChild, path, newEncodedValue) =
  Msg "Value update" <| \m ->
    case m.updatedValue of
      Just (Err msg) -> m
      _ ->
        let valueToUpdate = case m.updatedValue of
          Just (Ok u) -> u
          _ -> m.inputVal
        in
        let decoder = case nodeAttrsOrChild of
          0 -> decodeElemChild
          1 -> decodeAttributes
          _ -> decodeChildList
        in
        let newValueResult: Result String Val
            newValueResult =
             JSDecode.decodeValue decoder newEncodedValue |> --Result.map (\x -> let _ = Debug.log ("Decoded Value: " ++ LangUtils.valToString x) () in x) |>
                Result.andThen (\newSubValue ->
                 let _ = Debug.log (toString path ++ ":" ++ LangUtils.valToString newSubValue) () in
                 integrateValue path valueToUpdate newSubValue)
        in
        { m
             | updatedValue = Just newValueResult
        }

-- Computes the diff, and if it is not ambiguous, propagates the diff from the output.
msgAutoSync: Int -> Msg
msgAutoSync caretPosition =
  Msg "autoSync" (doAutoSync caretPosition)

doAutoSync: Int -> Model -> Model
doAutoSync caretPosition m =
    let newModel = doCallUpdate m in
    case Dict.get valueBackpropToolName newModel.synthesisResultsDict of
      Nothing -> newModel
      Just results -> -- If there are only two options (second is always revert to original program), and the first one is not a Hack, then we can apply it !
        case results of
          SynthesisResult {description, exp, diffs, isSafe}:: revert ->
            if List.length revert <= 1 then
            -- TODO: Once we have the diffs in the output, move the caretPosition
            --if String.startsWith "HACK: " description then newModel else
              let newerModel = doSelectSynthesisResult exp newModel in
              { newerModel
                  | addDummyDivAroundCanvas = Just True
                  , previewdiffs = Just diffs
                  , caretPosition = Just caretPosition
                  }
            else newModel
          _ -> newModel

msgClearPreviewDiff: Int -> Msg
msgClearPreviewDiff n = Msg "clearPreviewDiff" doClearPreviewDiff

doClearPreviewDiff: Model -> Model
doClearPreviewDiff m =
  { m | previewdiffs = Nothing }

msgSetExampleByName: String -> Msg
msgSetExampleByName name = msgNew name

--------------------------------------------------------------------------------

showCodePreview old code =
  case ImpureGoodies.logTimedRun "parsing time preview" <| \() -> Syntax.parser old.syntax code of
    Ok exp  -> showExpPreview old exp []
    Err err -> { old | preview = Just (code, [], Err (showError err)) }

showExpPreview old exp diffs =
  let code = Syntax.unparser old.syntax exp in
  case runAndResolve old exp of
    Ok (val, widgets, env, slate, _) -> { old | slateCount = old.slateCount + 1, preview = Just (code, diffs, Ok (val, widgets, slate)) }
    Err s                            -> { old | slateCount = old.slateCount + 1, preview = Just (code, diffs, Err s) }

{-
msgSelectOption (exp, val, slate, code) = Msg "Select Option..." <| \old ->
  { old | code          = code
        , inputExp      = exp
        , inputVal      = val
        , valueEditorString = LangUtils.valToString val
        , history       = modelCommit code [] Nothing old.history
        , slate         = slate
        , preview       = Nothing
        , synthesisResultsDict = Dict.empty
        , tool          = Cursor
        , liveSyncInfo  = Utils.fromOk "SelectOption mkLive" <|
                            mkLive old.syntax old.syncOptions old.slideNumber old.movieNumber old.movieTime exp
                              (val, []) -- TODO
        }
-}

msgHoverSynthesisResult: String -> List Int -> Msg
msgHoverSynthesisResult resultsKey pathByIndices = Msg "Hover SynthesisResult" <| doHoverSynthesisResult resultsKey pathByIndices

doHoverSynthesisResult: String -> List Int -> Model -> Model
doHoverSynthesisResult resultsKey pathByIndices old =
  let maybeFindResult path results =
    case path of
       []    -> Nothing
       [i]   -> Utils.maybeGeti0 i results
       i::is -> Utils.maybeGeti0 i results |> Maybe.andThen (\(SynthesisResult {children}) -> children |> Maybe.andThen (maybeFindResult is))
  in
  let setResultChildren path childResults oldResults =
    case path of
       []    -> oldResults
       [i]   -> oldResults |> Utils.getReplacei0 i (\(SynthesisResult attrs) ->
         SynthesisResult { attrs | children = Just childResults})
       i::is -> oldResults |> Utils.getReplacei0 i (\(SynthesisResult attrs) ->
         SynthesisResult { attrs | children = Just (setResultChildren is childResults (attrs.children |> Maybe.withDefault []))})
  in
  let insertResultSibling path newMbSynthesisResult oldResults =
    case path of
       [] -> oldResults
       [i] -> Utils.zipWithIndex oldResults |> List.concatMap (\((SynthesisResult attrs), j) ->
           if i == j
           then (if attrs.diffs == [] then [] else [SynthesisResult { attrs | nextLazy = Nothing}]) ++
                (newMbSynthesisResult |> Maybe.map (\x -> [x]) |> Maybe.withDefault [])
           else [SynthesisResult attrs]
         )
       i::is -> oldResults |> Utils.getReplacei0 i (\(SynthesisResult attrs) ->
          SynthesisResult { attrs | children = Just (insertResultSibling is newMbSynthesisResult (attrs.children |> Maybe.withDefault []))})
  in
  let oldResults = Utils.getWithDefault resultsKey [] old.synthesisResultsDict in
  case maybeFindResult pathByIndices oldResults of
    Just (SynthesisResult {description, exp, diffs, sortKey, children, nextLazy}) ->
      let old1 = case nextLazy of
        Just compute ->
           let newMbSynthesisResult = compute () in
           { old | synthesisResultsDict =
              Dict.insert resultsKey
                (insertResultSibling pathByIndices newMbSynthesisResult oldResults) old.synthesisResultsDict }
        Nothing ->
           old
      in
      let newModel = { old1 | hoveredSynthesisResultPathByIndices = pathByIndices } in
      let newModel2 =
        case (old.autoSynthesis, children) of
           (_, Just _)  -> newModel -- Children already computed.
           (False, _)   -> newModel -- Don't compute children if auto-synth off
           _            ->
             -- Compute child results.
             let childResults = cleanDedupSortSynthesisResults newModel (ETransform.passiveSynthesisSearch newModel exp) in
             let newTopLevelResults = Dict.insert resultsKey (setResultChildren pathByIndices childResults oldResults) old.synthesisResultsDict in
             { newModel | synthesisResultsDict = newTopLevelResults
                        , hoveredSynthesisResultPathByIndices = pathByIndices }
      in
      showExpPreview newModel2 exp diffs

    Nothing ->
      { old | preview = Nothing
            , previewdiffs = Nothing
            , hoveredSynthesisResultPathByIndices = [] }


msgPreview expOrCode = Msg "Preview" <| \old ->
  case expOrCode of
    Left exp            -> showExpPreview old exp []
    Right code          -> showCodePreview old code

msgClearPreview = Msg "Clear Preview" <| \old ->
  { old | preview = Nothing, previewdiffs = Nothing }

msgCancelSync = Msg "Cancel Sync" <| \old ->
  upstateRun
    { old | liveSyncInfo = refreshLiveInfo old }

msgActivateRenameInOutput pid = Msg ("Active Rename Box for PId " ++ toString pid) <| \old ->
  let oldName =
    findPatByPId old.inputExp pid
    |> Maybe.andThen LangTools.patToMaybeIdent
    |> Maybe.withDefault ""
  in
  { old | renamingInOutput = Just (pid, oldName) }

msgUpdateRenameInOutputTextBox newText = Msg ("Update Rename In Output: " ++ newText) <| \old ->
  case old.renamingInOutput of
    Just (pid, _) -> { old | renamingInOutput = Just (pid, newText) }
    Nothing       -> old

-- Shouldn't need pid b/c there should only be one rename box at a time, but I hate state.
msgDoRename pid = Msg ("Rename PId " ++ toString pid) <| \old ->
  case old.renamingInOutput of
    Just (renamingPId, newName) ->
      if renamingPId == pid then
        let newProgram =
          CodeMotion.renamePatByPId pid newName old.inputExp
          |> List.filter (extractSynthesisResultWith isResultSafe >> Utils.maybeToBool)
          |> List.head
          |> Maybe.andThen (extractSynthesisResultWith resultExp)
          |> Maybe.withDefault old.inputExp
        in
        { old | code             = Syntax.unparser old.syntax newProgram
              , renamingInOutput = Nothing
        } |> upstateRun
      else
        old

    Nothing ->
      old


msgAddArg funcBody = Msg "Add Arg to Function in Output" <| \old ->
  let maybeMaybeParent = parentByEId old.inputExp (expEId funcBody) in
  case Maybe.map (Maybe.map (\parent -> (parent, (unwrapExp parent)))) maybeMaybeParent of
    Just (Just (funcExp, EFun _ argPats funcBody _)) ->
      let targetPPId =
        ( ((expEId funcExp), 1)
        , [ 1 + List.length argPats ] -- By default, insert argument at the end
        )
      in
      let possibleArgEIds =
        let domain = flattenExpTree funcBody |> List.map expEId |> Set.fromList in
        let expFilter = expEId >> (flip Set.member) domain in
        -- let singleEIdInterps =
        --   ShapeWidgets.selectionsSingleEIdInterpretations
        --       old.inputExp
        --       old.slate
        --       old.widgets
        --       old.selectedFeatures
        --       old.selectedShapes
        --       old.selectedBlobs
        --       expFilter
        -- in
        -- let distalInterps =
        --   ShapeWidgets.selectionsDistalEIdInterpretations
        --       old.inputExp
        --       old.slate
        --       old.widgets
        --       old.selectedFeatures
        --       old.selectedShapes
        --       old.selectedBlobs
        --       expFilter
        --   |> List.concat
        -- in
        -- singleEIdInterps ++ distalInterps
        -- |> Utils.dedup
        ShapeWidgets.selectionsEIdsTouched
            old.inputExp
            old.slate
            old.widgets
            old.selectedFeatures
            old.selectedShapes
            old.selectedBlobs
            expFilter
      in
      let results =
        possibleArgEIds
        |> List.concatMap
            (\eid ->
              let (line, col) = LangTools.locationInProgram old.inputExp eid in
              CodeMotion.addArg old.syntax eid targetPPId old.inputExp
              |> List.map (mapSynthesisResult <| setResultSortKey [toFloat line, toFloat col])
            )
        |> synthesisResults
        |> List.sortBy (\(SynthesisResult result) -> (if result.isSafe then 0 else 1, result.sortKey))
      in
      case results of
        []             -> old
        [singleResult] -> upstateRun { old | code = Syntax.unparser old.syntax (resultExp singleResult) }
        _              -> { old | synthesisResultsDict = Dict.insert "Auto-Synthesis" results old.synthesisResultsDict } -- Commandere auto-synth results for now.

    _ ->
      let _ = Utils.log "could not find func to add argument to" in
      old


msgRemoveArg pid = Msg ("Remove Arg PId " ++ toString pid) <| \old ->
  case pidToPathedPatternId old.inputExp pid of
    Nothing              -> old
    Just pathedPatternId ->
      CodeMotion.removeArg old.syntax pathedPatternId old.inputExp
      |> synthesisResults
      |> List.head
      |> Maybe.map (\result -> upstateRun { old | code = Syntax.unparser old.syntax (resultExp result) })
      |> Maybe.withDefault old


--------------------------------------------------------------------------------

requireSaveAsker msg needsSave =
  case msg of
    Msg name _ ->
      if needsSave then
        Msg ("Ask " ++ name) <| (\old ->
          { old | pendingFileOperation = Just <| msg
                , fileOperationConfirmed = False })
            >> Model.openDialogBox AlertSave
      else
        msg

    _ ->
      Debug.crash "Controller.requireSaveAsker shouldn't get a solver response message!!"

--------------------------------------------------------------------------------
-- Dialog Box

msgOpenDialogBox db =
  Msg ("Open Dialog Box \"" ++ toString db ++ "\"") <| Model.openDialogBox db

msgCloseDialogBox db =
  Msg ("Close Dialog Box \"" ++ toString db ++ "\"") <| Model.closeDialogBox db

msgUpdateFilenameInput str = Msg "Update Filename Input" <| \old ->
  { old | filenameInput = str }

--------------------------------------------------------------------------------
-- File Handling API

confirmWrite : Filename -> Model -> Model
confirmWrite savedFilename old =
  if savedFilename == old.filename then
    { old | needsSave = False
          , lastSaveState = Just old.code }
  else
    old

confirmDelete : Filename -> Model -> Model
confirmDelete deletedFilename = identity

requestFile : Filename -> Model -> Model
requestFile requestedFilename old =
  { old | filename = requestedFilename }

readFile : File -> Bool -> Model -> Model
readFile file needsSave old =
  { old | filename = file.filename
        , code = file.contents
        , syntax = Syntax.fromFileExtension file.filename.extension
        , history = History.begin { code = file.contents, selectedDeuceWidgets = [], mbKeyboardFocusedWidget = Nothing }
        , lastSaveState = Just file.contents
        , needsSave = needsSave
        }

loadIcon : Env -> File -> Model -> Model
loadIcon env icon old =
  let
    (actualCode, syntax) =
      if icon.contents /= "" then
        ( icon.contents
        , Syntax.fromFileExtension icon.filename.extension
        )
      else
        case Dict.get icon.filename.name DefaultIconTheme.icons of
          Just c ->
            ( c
            , Syntax.Little
            )
          Nothing ->
            ( "(blobs [])"
            , Syntax.Little
            )
    oldIcons =
      old.icons
    iconHtml =
      iconify syntax env actualCode
    newIcons =
      Dict.insert icon.filename.name iconHtml oldIcons
  in
    { old | icons = newIcons }

-- for basic icons, env will be EvalUpdate.preludeEnv.
-- for LambdaTool icons, env will be from result of running main program.
iconify : Syntax -> Env -> String -> Html.Html Msg
iconify syntax env code =
  let
    exp =
      Utils.fromOkay "Error parsing icon"
        <| Syntax.parser syntax code
  in
  let valRes =
    Result.map (Tuple.first << Tuple.first) <| Eval.doEval Eval.withoutParentsProvenanceWidgets syntax env exp
  in
  case valRes of
    Err msg -> Debug.log msg (Svg.svg [] [])
    Ok val -> let
      slate =
        Utils.fromOkay "Error resolving index tree of icon"
          <| LangSvg.resolveToRootedIndexedTree syntax 1 1 0 val
      svgElements =
        -- Not using Canvas.buildHtml because can't have circular dependency between Controller and Canvas
        -- Various widgets in Canvas defined their model update functions inline to avoid this, but now we're
        -- invoking more complicated transforms from the output, so Canvas has to depend on Controller.
        --
        -- So icons are limited to SVG for now, which is probably fine.
        LangSvg.buildSvgSimple slate
        -- Canvas.buildHtml ({ initModel | showGhosts = False }, False) slate
      subPadding x =
        x - 10
    in
    Svg.svg
      [ Svg.Attributes.width <|
          (Layout.px << subPadding << .width) Layout.iconButton
      , Svg.Attributes.height <|
          (Layout.px << subPadding << .height) Layout.iconButton
      ]
      [ svgElements ]

loadLambdaToolIcons finalEnv old =
  let foo tool acc =
    let icon = lambdaToolIcon tool in
    if Dict.member icon.filename.name old.icons
       then acc
       else loadIcon finalEnv icon old
  in
  List.foldl foo old old.lambdaTools

updateFileIndex : FileIndex -> Model -> Model
updateFileIndex fileIndex old =
  { old | fileIndex = fileIndex }

-- TODO: clear state (e.g. selectedEIds) after read file

msgLoadIcon : File -> Msg
msgLoadIcon file =
  Msg "Load Icon" <|
    loadIcon EvalUpdate.preludeEnv file

fileMessageHandler : InternalFileMessage -> Msg
fileMessageHandler ifm =
  case ifm of
    ConfirmWrite filename ->
      Msg "Confirm Write" <|
        confirmWrite filename

    ConfirmDelete filename ->
      Msg "Confirm Delete" <|
        confirmDelete filename

    ReceiveFile file needsSave ->
      Msg "Read File" <|
        readFile file needsSave >> upstateRun

    ReceiveIcon file ->
      msgLoadIcon file

    ReceiveFileIndex fileIndex ->
      Msg "Update File Index" <|
        updateFileIndex fileIndex

fileMessageError : String -> Msg
fileMessageError err =
  Debug.log err msgNoop

--------------------------------------------------------------------------------
-- File Operations

msgNew template = Msg "New" (handleNew template)

handleNew template = (\old ->
  case loadTemplate template of
    Err msg -> let _ = Debug.log msg "" in old
    Ok f ->
  let fApplied = f () in
  let
     {e,v,ws,env} = case fApplied of
         Err msg -> {e=eStr "Example did not parse", v=(builtinVal "" (VBase (VString (msg)))), ws=[], env=[]}
         Ok ff -> ff
  in
  let
     (inputExp, holeEnv, aceTypeInfo, typeChecks) =
      maybeTypeCheck old.doTypeChecking e
  in
  let so = Sync.syncOptionsOf old.syncOptions inputExp in
  let outputMode =
     Utils.fromOk "SelectExample mkLive" <|
       mkLive old.syntax so old.slideNumber old.movieNumber old.movieTime inputExp (v,ws)
  in
  LangSvg.fetchEverything old.syntax old.slideNumber old.movieNumber old.movieTime v
  |> Result.map (\(slideCount, movieCount, movieDuration, movieContinue, slate) ->
     let code = Syntax.unparser old.syntax inputExp in
     { initModel | inputExp      = inputExp
                , holeEnv       = holeEnv
                , inputVal      = v
                , inputEnv      = env
                , caption       = (case fApplied of
                        Err msg -> Just (LangError msg)
                        _ -> Nothing
                      )
                , valueEditorString = LangUtils.valToString v
                , outputMode    = maybeUpdateOutputMode old slate
                , htmlEditorString = Nothing
                , code          = code
                , lastParsedCode  = code
                , lastRunCode   = code
                , history       = History.begin { code = code, selectedDeuceWidgets = [], mbKeyboardFocusedWidget = Nothing }
                , liveSyncInfo  = outputMode
                , syncOptions   = so
                , slideNumber   = 1
                , slideCount    = slideCount
                , movieCount    = movieCount
                , movieTime     = 0
                , movieDuration = movieDuration
                , movieContinue = movieContinue
                , runAnimation  = movieDuration > 0
                , slate         = slate
                , slateCount    = 1 + old.slateCount
                , widgets       = ws
                , codeBoxInfo   = updateCodeBoxInfo aceTypeInfo old
                , filename      = Model.bufferFilename old
                , syntax        = old.syntax
                , needsSave     = True
                , lastSaveState = Nothing
                , scopeGraph    = DependenceGraph.compute inputExp

                , lastSelectedTemplate = Just (template, code)

                , dimensions    = old.dimensions
                , localSaves    = old.localSaves
                , basicCodeBox  = old.basicCodeBox
                , randomColor   = old.randomColor
                , layoutOffsets = old.layoutOffsets
                , fileIndex     = old.fileIndex
                , icons         = old.icons

                , enableDeuceBoxSelection  = old.enableDeuceBoxSelection
                , enableDeuceTextSelection = old.enableDeuceTextSelection
                , codeToolsMenuMode        = old.codeToolsMenuMode
                , textSelectMode           = old.textSelectMode
                , enableTextEdits          = old.enableTextEdits
                , allowMultipleTargetPositions  = old.allowMultipleTargetPositions
                , mainResizerX             = old.mainResizerX
                , colorScheme              = old.colorScheme

                , syncMode = old.syncMode

                , doTypeChecking = old.doTypeChecking

                } |> showTypeInspectorIfNecessary typeChecks
                  |> resetDeuceState
  ) |> handleError old) >> closeDialogBox New

msgAskNew template = requireSaveAsker (msgNew template)

msgSaveAs =
  let
    switchFilenameToInput old =
      { old | filename = File.parseFilename old.filenameInput }
    closeDialogBoxIfNecessary old =
      if old.filename.name /= Model.bufferName then
        Model.closeDialogBox SaveAs old
      else
        old
  in
    Msg "Save As" (switchFilenameToInput >> closeDialogBoxIfNecessary)

msgSave = Msg "Save" <| \old ->
  if old.filename.name == Model.bufferName then
    Model.openDialogBox SaveAs old
  else
    old

msgOpen filename =
  Msg "Open" (requestFile filename >> closeDialogBox Open)

msgAskOpen filename = requireSaveAsker (msgOpen filename)

msgDelete filename =
  Msg "Delete" <| \old ->
    if filename == old.filename then
      { old | fileToDelete = filename
            , needsSave = True
            , lastSaveState = Nothing }
    else
      { old | fileToDelete = filename }

msgCancelFileOperation = Msg "Cancel File Operation" Model.cancelFileOperation

msgConfirmFileOperation = Msg "Confirm File Operation" <| (\old ->
  { old | fileOperationConfirmed = True })
    >> Model.closeDialogBox AlertSave

--------------------------------------------------------------------------------
-- Exporting

msgExportCode = Msg "Export Code" identity

msgExportHtml = Msg "Export HTML" identity

--------------------------------------------------------------------------------
-- Importing

msgImportCode = Msg "Import Code" <| closeDialogBox ImportCode

msgAskImportCode = requireSaveAsker msgImportCode


--------------------------------------------------------------------------------
-- Deuce Interactions

chooseDeuceExp : Model -> Exp -> Model
chooseDeuceExp old newRoot =
  let
    modifiedHistory =
      modelModify
        old.code
        old.deuceState.selectedWidgets
        old.deuceState.mbKeyboardFocusedWidget
        old.history
    modelWithCorrectHistory =
      case modifiedHistory of
        Just h ->
          { old | history = h }
        Nothing ->
          old
  in
  -- TODO version of tryRun/upstateRun starting with parsed expression
  upstateRun { modelWithCorrectHistory | code = Syntax.unparser old.syntax newRoot }
  |> \m -> { m | codeAtPbeSynthesis = Nothing }
  |> deuceRefresh newRoot
  |> resetDeuceKeyboardInfo
  |> resetDeuceState
  |> maybeTypeCheckAndUpdateModel

maybeChooseDeuceExp : Model -> Maybe Exp -> Model
maybeChooseDeuceExp m mbExp =
  case mbExp of
    Nothing  -> m
    Just exp -> chooseDeuceExp m exp

isDeuceWidgetValid : Exp -> Maybe DeuceWidget -> Bool
isDeuceWidgetValid root mbWidget =
  case mbWidget of
    Just (DeuceExp eId) ->
      findExpByEId root eId
      |> Utils.maybeToBool
    Just (DeucePat ppid) ->
      LangTools.pathedPatternIdToPId ppid root
      |> Utils.maybeToBool
    Just (DeuceLetBindingEquation (scopeEId, bn)) ->
      findExpByEId root scopeEId
      |> Maybe.andThen (flip findLetexpByBindingNumber bn)
      |> Utils.maybeToBool
    Just (DeuceType tId) ->
      -- TODO implement properly
      False
    _ ->
      False

resetDeuceState m =
  let layoutOffsets = m.layoutOffsets in
  resetDeuceKeyboardInfo <|
  { m | deuceState = emptyDeuceState
      , deuceToolsAndResults = DeuceTools.createToolCache initModel
      , deuceToolResultPreviews = Dict.empty
      , selectedDeuceTool = Nothing
      , preview = Nothing
      , previewdiffs = Nothing
      , layoutOffsets =
          { layoutOffsets |
              deuceToolBox =
                { pinned = layoutOffsets.deuceToolBox.pinned
                , offsets = if layoutOffsets.deuceToolBox.pinned
                            then layoutOffsets.deuceToolBox.offsets
                            else {dx=0, dy=0}
                }
          }
      , holeFillings = Nothing
      , unExpOutput =
          Result.map (Tuple.mapSecond <| \_ -> Just []) m.unExpOutput
      , selectedHoles = Set.empty
      , selectedUnExp = Nothing
      , holeExampleInputs = Dict.empty
      , backpropExampleInput = ""
      , unExpPreview = Nothing
      , codeAtPbeSynthesis = Nothing
      , code = m.codeAtPbeSynthesis |> Maybe.withDefault m.code
      }

msgMouseEnterCodeBox = Msg "Mouse Enter CodeBox" <| \m ->
  let codeBoxInfo = m.codeBoxInfo in
  { m | hoveringCodeBox = True}

msgMouseLeaveCodeBox = Msg "Mouse Leave CodeBox" <| \m ->
  let codeBoxInfo = m.codeBoxInfo in
  { m | hoveringCodeBox = False }

toggleDeuceWidget : DeuceWidget -> Model -> Model
toggleDeuceWidget widget model =
  if Model.modeActive model CEProgrammingByExample then
    case widget of
      DeuceExp eid ->
        case Lang.findExpByEId model.inputExp eid of
          Just exp ->
            case unwrapExp exp of
              EHole _ (EEmptyHole holeId) ->
                addSelectedHole holeId model

              _ ->
                model

          Nothing ->
            model

      _ ->
        model
  else
    let
      oldDeuceState =
        model.deuceState
      oldSelectedWidgets =
        oldDeuceState.selectedWidgets
      multipleTargetPositionsFilter =
        if model.allowMultipleTargetPositions then
          -- Do not filter anything
          identity
        else
          -- Only allow one target position
          List.filter (\w -> not (isTargetPosition w && isTargetPosition widget))
      newSelectedWidgets =
        if List.member widget oldSelectedWidgets then
          Utils.removeAsSet widget oldSelectedWidgets
        else
          if Model.allowOnlySingleSelection model then
            [ widget ]
          else
            oldSelectedWidgets
            |> List.filter (\w -> not (isSubWidget model.inputExp w widget || isSubWidget model.inputExp widget w)) -- Remove any overlapping widgets.
            |> multipleTargetPositionsFilter
            |> Utils.addAsSet widget
      newSelectedWidgetsEmpty =
        List.isEmpty newSelectedWidgets && oldDeuceState.mbKeyboardFocusedWidget == Nothing
      newDeuceState =
        { oldDeuceState
            | selectedWidgets =
                newSelectedWidgets
        }
      newDeuceRightClickMenuMode =
        if newSelectedWidgetsEmpty then
          Nothing
        else
          model.deuceRightClickMenuMode
      almostNewModel =
        { model
            | deuceState =
                newDeuceState
            , deuceRightClickMenuMode =
                newDeuceRightClickMenuMode
        }
    in
    resetDeuceCacheAndReselect almostNewModel DeuceTools.createToolCache

msgMouseClickDeuceWidget widget =
  let
    toggler old =
      toggleDeuceWidget widget old

    positionUpdater old =
      if old.codeEditorMode == CETypeInspector then
        deucePopupPanelPositionUpdater (Model.mousePosition old) old
      else
        old
  in
    Msg
      ("msgMouseClickDeuceWidget " ++ toString widget)
      (toggler >> positionUpdater)

msgMouseEnterDeuceWidget widget = Msg ("msgMouseEnterDeuceWidget " ++ toString widget) <| \old ->
  let
    oldDeuceState =
      old.deuceState

    newDeuceState =
      { oldDeuceState | hoveredWidgets = [widget] }

    newModel =
      { old | deuceState = newDeuceState }
  in
    if newModel.codeEditorMode == CETypeInspector then
      resetDeuceCacheAndReselect newModel DeuceTools.createToolCache
    else
      newModel

msgMouseLeaveDeuceWidget widget = Msg ("msgMouseLeaveDeuceWidget " ++ toString widget) <| \old ->
  let
    oldDeuceState =
      old.deuceState

    newDeuceState =
      { oldDeuceState | hoveredWidgets = [] }
  in
    { old | deuceState = newDeuceState }

msgChooseDeuceExp name exp = Msg ("Choose Deuce Exp \"" ++ name ++ "\"") <| \m ->
  chooseDeuceExp m exp

--------------------------------------------------------------------------------
-- Deuce Keyboard Interactions

resetDeuceKeyboardInfo : Model -> Model
resetDeuceKeyboardInfo old =
  { old | mbDeuceKeyboardInfo = Nothing }

deuceMove : DeuceWidget -> Model -> Model
deuceMove destWidget old =
  let
    oldDS = old.deuceState
    newDS = { oldDS | mbKeyboardFocusedWidget = Just destWidget }
  in
  { old | deuceState = newDS } |>
  flip resetDeuceCacheAndReselect DeuceTools.createToolCache

deuceChooserUI : Model -> String -> (String, String -> List TransformationResult) -> Model
deuceChooserUI old initText titleAndTextToTransformationResults =
  let
    (title, textToTransformationResults) = titleAndTextToTransformationResults
    oldReset = resetDeuceKeyboardInfo old
  in
  if List.isEmpty <| textToTransformationResults initText then
    old
  else
    { oldReset
      | mbDeuceKeyboardInfo =
          Just <|
          { title = title
          , text = initText
          , textToTransformationResults = textToTransformationResults
          , smartCompleteSelection = initText
          }
      , needsToFocusOn = Just deuceKeyboardPopupPanelTextBoxId
    }

moveSmartCompleteSelection old isDown =
  let
    oldDeuceKeyboardInfo =
      Utils.fromJust_ "No keyboard info" old.mbDeuceKeyboardInfo
    {text, textToTransformationResults, smartCompleteSelection} =
      oldDeuceKeyboardInfo
    textResults =
      textToTransformationResults text
      |> List.map transformationResultToString
      |> Utils.applyIf isDown List.reverse
    smartSelectionTail =
      Utils.dropWhile ((/=) smartCompleteSelection) textResults
    newSmartCompleteSelection =
      case smartSelectionTail of
        sel :: next :: _ ->
          -- common case: select the next child
          next
        _                ->
          -- if no such child exists, or we're at the last child, select the first
          List.head textResults |> Maybe.withDefault smartCompleteSelection
    newMbDeuceKeyboardInfo =
      Just <|
      { oldDeuceKeyboardInfo
        | smartCompleteSelection = newSmartCompleteSelection
      }
  in
  { old | mbDeuceKeyboardInfo = newMbDeuceKeyboardInfo }

handleDeuceMoveHorizontal_ old selected isLeftMove =
  let
    oldRoot = old.inputExp
    patMap = computePatMap oldRoot
    cosAfterSelected =
      E oldRoot |>
        flattenToCodeObjects |>
          Utils.applyIf isLeftMove List.reverse |>
            Utils.dropWhile (toDeuceWidget patMap >> (==) (Just selected) >> not) |>
              List.tail |>
                Maybe.withDefault []
  in
  Utils.mapFirstSuccess (\co -> if isTarget co then Nothing else toDeuceWidget patMap co) cosAfterSelected |>
    Maybe.map (flip deuceMove old) |>
      Maybe.withDefault old

handleDeuceLeft old selected = handleDeuceMoveHorizontal_ old selected True

handleDeuceRight old selected = handleDeuceMoveHorizontal_ old selected False

handleDeuceSpace old selected = toggleDeuceWidget selected old

handleDeuceHotKey : Model -> List Char.KeyCode -> DeuceWidget -> Model
handleDeuceHotKey oldModel keysDown selected =
  let
    old = resetDeuceKeyboardInfo oldModel
    -- TODO although difficult to avoid, this is way too hard-coded
    mbPrefixOfNumberOrOp keyCodes =
      if List.length keyCodes == 1
         && Utils.head_ keyCodes >= Keys.keyDigit 0
         && Utils.head_ keyCodes <= Keys.keyDigit 9 then
        Just <| toString <| Utils.head_ keyCodes - Keys.keyDigit 0
      else if keyCodes == [Keys.keyPeriod] then
        Just "."
      else if keyCodes == [Keys.keyShift, Keys.keyPlusEqual] then
        Just "+"
      else if keyCodes == [Keys.keyMinus] then
        Just "-"
      else if keyCodes == [Keys.keyShift, Keys.keyDigit 8] then
        Just "*"
      else if keyCodes == [Keys.keyForwardSlash] then
        Just "/"
      else if keyCodes == [Keys.keyShift, Keys.keyComma] then
        Just "<"
      else if keyCodes == [Keys.keyPlusEqual] then
        Just "="
      else if keyCodes == [Keys.keyShift, Keys.keyDigit 6] then
        Just "^"
      else
        Nothing
  in

  -- Movement

  if List.member keysDown [[Keys.keyLeft], [Keys.keyH]] then
    handleDeuceLeft old selected
  else if List.member keysDown [[Keys.keyRight], [Keys.keyL]] then
    handleDeuceRight old selected
  -- TODO revive with some alternative hotkey
  -- else if keysDown == [Keys.keySpace] then
  --   handleDeuceSpace old selected

  -- Deuce tools

  else if keysDown == [Keys.keyI] then
    DeuceTools.smartCompleteHoleViaHotkey old selected |> deuceChooserUI old ""
  else if mbPrefixOfNumberOrOp keysDown /= Nothing then
    DeuceTools.smartCompleteHoleViaHotkey old selected
    |> deuceChooserUI old (mbPrefixOfNumberOrOp keysDown |> Utils.fromJust_ "bad num/op")

  else if keysDown == [Keys.keyS] then
    DeuceTools.renameVariableViaHotkey old selected |> deuceChooserUI old ""
  else if List.member keysDown [[Keys.keyShift, Keys.keyDown], [Keys.keyShift, Keys.keyJ]] then
    DeuceTools.expandFormatViaHotkey old selected |> maybeChooseDeuceExp old
  else if keysDown == [Keys.keyShift, Keys.keyA] then
    DeuceTools.addToEndViaHotkey old selected |> maybeChooseDeuceExp old
  else if keysDown == [Keys.keyD] then
    DeuceTools.deleteViaHotkey old selected |> maybeChooseDeuceExp old

  else if keysDown == Keys.openParen then
    DeuceTools.replaceWithParens old selected |> maybeChooseDeuceExp old
  else if keysDown == Keys.openBrace then
    DeuceTools.replaceWithList old selected |> maybeChooseDeuceExp old
  else if keysDown == Keys.openCurly then
    DeuceTools.replaceWithRecord old selected |> maybeChooseDeuceExp old
  else if keysDown == [Keys.keyComma] then
    DeuceTools.replaceWithTuple old selected |> maybeChooseDeuceExp old
  else if keysDown == [Keys.keyShift, Keys.keySemicolon] then
    DeuceTools.replaceWithCons old selected |> maybeChooseDeuceExp old
  else if keysDown == Keys.backslash then
    DeuceTools.replaceWithLambda old selected |> maybeChooseDeuceExp old
  else if keysDown == [Keys.keySpace] then
    DeuceTools.replaceWithApp old selected |> maybeChooseDeuceExp old
  else if keysDown == [Keys.keyE] then
    DeuceTools.replaceWithCase old selected |> maybeChooseDeuceExp old
  else if keysDown == [Keys.keyT] then
    DeuceTools.replaceWithLet old selected |> maybeChooseDeuceExp old
  else if keysDown == [Keys.keyC] then
    DeuceTools.replaceWithCond old selected |> maybeChooseDeuceExp old

  else
    old

handleDeuceTextBoxCommand : Model -> List Char.KeyCode -> Model
handleDeuceTextBoxCommand old keysDown =
  let
    -- It'd be great if we could use any familiar, home-keys-accessible binding,
    -- but literally all of them are consumed by chrome:
    -- Ctrl+J, Ctrl+K, Ctrl+N, Ctrl+P, and Tab
    -- I've settled on Ctrl+m and Ctrl+; for now,
    -- but I don't what's really a good solution
    isUp =
      keysDown == [Keys.keyUp]
      || ( List.length keysDown == 2
           && (Utils.geti 1 keysDown |> Keys.isCommandKey)
           && (Utils.geti 2 keysDown == Keys.keySemicolon)
         )
    isDown =
      keysDown == [Keys.keyDown]
      || ( List.length keysDown == 2
           && (Utils.geti 1 keysDown |> Keys.isCommandKey)
           && (Utils.geti 2 keysDown == Keys.keyM)
         )
  in
  if isUp then
    moveSmartCompleteSelection old False
  else if isDown then
    moveSmartCompleteSelection old True
  else
    old

--------------------------------------------------------------------------------
-- DOT

msgReceiveDotImage s = Msg "Receive Image" <| \m ->
  { m | outputMode = Model.PrintScopeGraph (Just s) }

--------------------------------------------------------------------------------
-- Menu Handling

setMenuActive : Bool -> Model -> Model
setMenuActive isActive model =
  let
    oldViewState =
      model.viewState
    newViewState =
      { oldViewState | menuActive = isActive }
  in
    { model | viewState = newViewState }

-- msgShowMenu : Msg
-- msgShowMenu  =
--   Msg "Show Menu" <| setMenuActive True

msgHideMenu : Msg
msgHideMenu =
  Msg "Hide Menu" <| setMenuActive False

msgToggleMenu : Msg
msgToggleMenu =
  Msg "Toggle Menu" <| \old ->
    setMenuActive (not old.viewState.menuActive) old

--------------------------------------------------------------------------------
-- Fonts

msgUpdateFontSize : Int -> Msg
msgUpdateFontSize fontSize = Msg "Update Font Size" <| \old ->
  let
    oldCodeBoxInfo = old.codeBoxInfo
  in
    { old | codeBoxInfo =
      { oldCodeBoxInfo | fontSize = fontSize }
    }

--------------------------------------------------------------------------------
-- Tools

msgSetToolMode : ShapeToolKind -> Msg
msgSetToolMode mode =
  Msg "Set Tool Mode" <| \old ->
    let
      newTool =
        case old.tool of
          -- Tools with mode
          Line _ ->
            Line mode
          Rect _ ->
            Rect mode
          Oval _ ->
            Oval mode
          Poly _ ->
            Poly mode
          Path _ ->
            Path mode
          -- Tools without mode
          Cursor ->
            Cursor
          PointOrOffset ->
            PointOrOffset
          Text ->
            Text
          HelperLine ->
            HelperLine
          Lambda i ->
            Lambda i
          Function fName ->
            Function fName
    in
      { old | toolMode = mode
            , tool = newTool
      }

--------------------------------------------------------------------------------
-- Ghosts

msgSetGhostsShown : Bool -> Msg
msgSetGhostsShown shown =
  Msg "Set Ghosts Shown" <| \old ->
    let
      newMode =
        case old.outputMode of
          HtmlText _ ->
            HtmlText (LangSvg.printHTML shown old.slate)
          _ ->
            old.outputMode
    in
      { old | showGhosts = shown
            , outputMode = newMode
      }

--------------------------------------------------------------------------------
-- Deuce Tools

msgDeuceTextBoxSetFocus : Bool -> Msg
msgDeuceTextBoxSetFocus shouldFocus =
  Msg
    ((if shouldFocus then "Focus" else "Unfocus/Blur") ++ " Deuce Text Box")
    (\old -> { old | isDeuceTextBoxFocused = shouldFocus })

msgHoverDeuceResult : Bool -> SynthesisResult -> List Int -> Msg
msgHoverDeuceResult dontEval (SynthesisResult result) path =
  let evalIfNotCached m =
    case Dict.get path m.deuceToolResultPreviews of
      Just (preview, _) -> -- already run and cached
        { m | preview = preview }

      Nothing -> -- not already run and cached
        let
          (preview, class) =
            -- CSS classes from View leak out here. Oh, well.
            case (result.isSafe, runAndResolve m result.exp) of
              (True, Ok (val, widgets, env, slate, code)) ->
                (Just (code, [], Ok (val, widgets, slate)), "expected-safe")
              (True, Err err) ->
                let _ = Debug.log "not safe after all!" err in
                (Just (Syntax.unparser m.syntax result.exp, [], Err err), "unexpected-unsafe")
              (False, Ok (val, widgets, env, slate, code)) ->
                (Just (code, [], Ok (val, widgets, slate)), "unexpected-safe")
              (False, Err err) ->
                (Just (Syntax.unparser m.syntax result.exp, [], Err err), "expected-unsafe")

          deuceToolResultPreviews =
            Dict.insert path (preview, class) m.deuceToolResultPreviews
        in
        { m
        | preview = preview
        , deuceToolResultPreviews = deuceToolResultPreviews
        }
  in
  Msg
    ("Hover Deuce Result \"" ++ result.description ++ "\" " ++ toString path)
    (setHoveredMenuPath path >> Utils.applyIf (not dontEval) evalIfNotCached)

msgLeaveDeuceResult : SynthesisResult -> List Int -> Msg
msgLeaveDeuceResult (SynthesisResult result) path =
  let clearPreview m = { m | preview = Nothing } in
  Msg
    ("Leave Deuce Result \"" ++ result.description ++ "\" " ++ toString path)
    (clearHoveredMenuPath >> clearPreview)

msgUpdateRenameVarTextBox : String -> Msg
msgUpdateRenameVarTextBox text =
  Msg ("Update Rename Var Text Box: " ++ text) <| \model ->
    let
      oldDeuceState =
        model.deuceState
      almostNewModel =
        { model
            | deuceState =
                { oldDeuceState
                    | renameVarText =
                        Parser.sanitizeVariableName text
                }
        }
    in
    resetDeuceCacheAndReselect almostNewModel DeuceTools.updateInputSensitiveToolsInCache

msgUpdateSmartCompleteTextBox : String -> Msg
msgUpdateSmartCompleteTextBox text =
  Msg ("Update Smart-Complete Text Box: " ++ text) <| \model ->
    let
      oldDeuceState =
        model.deuceState
      almostNewModel =
        { model
            | deuceState =
                { oldDeuceState
                    | smartCompleteText = text
                }
        }
    in
    resetDeuceCacheAndReselect almostNewModel DeuceTools.updateInputSensitiveToolsInCache

msgUpdateDeuceKeyboardTextBox : String -> Msg
msgUpdateDeuceKeyboardTextBox text =
  Msg ("Update Deuce Keyboard Text Box: " ++ text) <| \old ->
    { old
        | mbDeuceKeyboardInfo =
            old.mbDeuceKeyboardInfo
            |> Maybe.map
              (\deuceKeyboardInfo -> { deuceKeyboardInfo | text = text })
    }

--------------------------------------------------------------------------------
-- Clear Drag

msgClearDrag : Msg
msgClearDrag =
  Msg "Clear Drag" <| \model ->
    { model | mouseMode = MouseNothing }

--------------------------------------------------------------------------------
-- Popup Panels

shift : (Int, Int) -> (Int, Int) -> (Int, Int)
shift (x, y) (dx, dy) =
  ( x + dx
  , y + dy
  )

deltaMouse : Mouse.Position -> Mouse.Position -> (Int, Int)
deltaMouse old new =
  ( new.x - old.x
  , new.y - old.y
  )

updatePopupPanelPosition
  :  (PopupPanelPositions -> (Int, Int))
  -> (PopupPanelPositions -> (Int, Int) -> PopupPanelPositions)
  -> Model -> Model
updatePopupPanelPosition get set model  =
  let
    updater oldPosition newPosition old =
      let
        -- Get old set
        oldPopupPanelPositions =
          old.popupPanelPositions
        -- Get old pos
        oldPopupPanelPosition =
          get old.popupPanelPositions
        -- Compute new pos
        newPopupPanelPosition =
          shift
            oldPopupPanelPosition
            (deltaMouse oldPosition newPosition)
        -- Update old set
        newPopupPanelPositions =
          set oldPopupPanelPositions newPopupPanelPosition
      in
        -- Update model
        { old | popupPanelPositions = newPopupPanelPositions }
  in
    { model | mouseMode = MouseDrag updater }

--------------------------------------------------------------------------------
-- Deuce Popup Panel

msgDragDeucePopupPanel : Msg
msgDragDeucePopupPanel =
  Msg "Drag Deuce Popup Panel" <|
    updatePopupPanelPosition
      .deuce
      ( \ppp pos ->
          { ppp | deuce = pos }
      )

--------------------------------------------------------------------------------
-- Auto Output Tools Popup Panel

msgDragAutoOutputToolsPopupPanel : Msg
msgDragAutoOutputToolsPopupPanel =
  Msg "Drag Auto Output Tools Popup Panel" <|
    updatePopupPanelPosition
      .autoOutputTools
      ( \ppp pos ->
          { ppp | autoOutputTools = pos }
      )

--------------------------------------------------------------------------------
-- Programming by Example Popup Panel

msgDragPbePopupPanel : Msg
msgDragPbePopupPanel =
  Msg "Drag Programming by Example Popup Panel" <|
    updatePopupPanelPosition
      .pbe
      ( \ppp pos ->
          { ppp | pbe = pos }
      )

--------------------------------------------------------------------------------
-- Edit Code Popup Panel

msgDragEditCodePopupPanel : Msg
msgDragEditCodePopupPanel =
  Msg "Drag Edit Code Popup Panel" <|
    updatePopupPanelPosition
      .editCode
      ( \ppp pos ->
          { ppp | editCode = pos }
      )

--------------------------------------------------------------------------------
-- Deuce Right Click Menu

msgDragDeuceRightClickMenu : Msg
msgDragDeuceRightClickMenu =
  Msg "Drag Deuce Right Click Menu" <|
    updatePopupPanelPosition
      .deuceRightClickMenu
      ( \ppp pos ->
          { ppp | deuceRightClickMenu = pos }
      )

--------------------------------------------------------------------------------
-- Text Select

textSelect : Bool -> Model -> Model
textSelect allowSingleSelection old =
  let
    patMap =
      computePatMap old.inputExp
  in
    case
      old
        |> Model.codeObjectFromSelection allowSingleSelection
        |> Maybe.andThen (toDeuceWidget patMap)
    of
      Just deuceWidget ->
        toggleDeuceWidget deuceWidget old
      Nothing ->
        old

msgTextSelect : Bool -> Msg
msgTextSelect allowSingleSelection =
  Msg "Text Select" <|
    textSelect allowSingleSelection

--------------------------------------------------------------------------------
-- Some Flags

msgSetEnableDeuceBoxSelection : Bool -> Msg
msgSetEnableDeuceBoxSelection bool =
  Msg "Set Enable Deuce Box Selection" <| \model ->
    { model
        | enableDeuceBoxSelection =
            bool
    }

msgSetEnableDeuceTextSelection : Bool -> Msg
msgSetEnableDeuceTextSelection bool =
  Msg "Set Enable Deuce Text Selection" <| \model ->
    { model
        | enableDeuceTextSelection =
            bool
    }

msgSetCodeToolsMenuMode : CodeToolsMenuMode -> Msg
msgSetCodeToolsMenuMode mode =
  Msg "Set Code Tools Menu Mode" <| \model ->
    { model
        | codeToolsMenuMode =
            mode
    }

msgSetTextSelectMode : TextSelectMode -> Msg
msgSetTextSelectMode textSelectMode =
  Msg "Set Text Select Mode" <| \model ->
    { model
        | textSelectMode =
            textSelectMode
    }

msgSetEnableTextEdits : Bool -> Msg
msgSetEnableTextEdits bool =
  Msg "Set Enable Text Edits" <| \model ->
    { model
        | enableTextEdits =
            Updatable.create bool
    }

msgSetAllowMultipleTargetPositions : Bool -> Msg
msgSetAllowMultipleTargetPositions bool =
  Msg "Set Allow Multiple Target Positions" <| \model ->
    { model
        | allowMultipleTargetPositions =
            bool
    } |> resetDeuceState

--------------------------------------------------------------------------------
-- Set the selected Deuce tool

msgSetSelectedDeuceTool : Bool -> CachedDeuceTool -> Msg
msgSetSelectedDeuceTool useTextSelect cachedDeuceTool =
  let
    selectedDeuceToolUpdater model =
      { model
          | selectedDeuceTool =
              Just cachedDeuceTool
          , deuceRightClickMenuMode =
              Nothing
      }
    maybeTextSelect =
      if useTextSelect then
        refreshInputExp >> textSelect False
      else
        identity
  in
    Msg ("Set Selected Deuce Tool " ++ toString (Utils.fst3 cachedDeuceTool |> .name)) <|
      selectedDeuceToolUpdater >> maybeTextSelect

--------------------------------------------------------------------------------
-- Deuce Right Click

msgDeuceRightClick : DeuceRightClickMenuMode -> Msg
msgDeuceRightClick menuMode =
  Msg "Deuce Right Click" <| \model ->
    if
      model.enableDeuceTextSelection &&
      (Model.noCodeWidgetsSelected model)
    then
      let
        modelAfterTextSelection =
          model
            |> refreshInputExp
            |> textSelect True
      in
        -- Make sure we selected at least one code object
        if Model.noCodeWidgetsSelected modelAfterTextSelection then
          model
        else
          showDeuceRightClickMenu
            deuceRightClickMenuMouseOffset.x
            deuceRightClickMenuMouseOffset.y
            menuMode
            modelAfterTextSelection
    else
      model

--------------------------------------------------------------------------------
-- Resizers

msgDragMainResizer : Msg
msgDragMainResizer =
  let
    updater oldPosition newPosition old =
      let
        leftBound =
          Layout.mainResizerLeftBound old
        rightBound =
          Layout.mainResizerRightBound old
        oldMainResizerX =
          (Layout.mainResizer old).x
        newMainResizerX =
          Utils.clamp leftBound rightBound <|
            oldMainResizerX +
              (Tuple.first <| deltaMouse oldPosition newPosition)
      in
        { old | mainResizerX = Just newMainResizerX }
  in
    Msg "Drag Main Resizer" <| \model ->
      { model | mouseMode = Model.MouseDrag updater }

msgResetInterfaceLayout : Msg
msgResetInterfaceLayout =
  Msg "Reset Interface Layout" <| \model ->
    { model
        | mainResizerX =
            Nothing
    }

--------------------------------------------------------------------------------
-- Deuce Popup Panel Info

msgReceiveDeucePopupPanelInfo : DeucePopupPanelInfo -> Msg
msgReceiveDeucePopupPanelInfo dppi =
  Msg "Receive Deuce Popup Panel Info" <| \old ->
    let
      (oldX, oldY) =
        old.popupPanelPositions.deuce
      oldDeucePopupPanelAbove =
        old.deucePopupPanelAbove
      newDeucePopupPanelAbove =
        oldY > dppi.height
    in
      -- Only update if going from False to True
      -- Also, reset to True if the popup panel is not even shown
      { old
          | deucePopupPanelAbove =
              if Model.deucePopupPanelShown old then
                oldDeucePopupPanelAbove && newDeucePopupPanelAbove
              else
                True
      }

--------------------------------------------------------------------------------
-- Color Scheme

msgSetColorScheme : ColorScheme -> Msg
msgSetColorScheme colorScheme =
  Msg "Set Color Scheme" <| \old ->
    { old | colorScheme = colorScheme }

--------------------------------------------------------------------------------
-- Syntax

msgSetSyntax : Syntax -> Msg
msgSetSyntax newSyntax =
  Msg ("Set Syntax to " ++ toString newSyntax) <| \old ->
    case Syntax.convertSyntax old.syntax newSyntax old.code of
      Ok newCode ->
        upstateRun
          { old
            | syntax = newSyntax
            , code = newCode
          }

      Err _ ->
        old

--------------------------------------------------------------------------------
-- Navigating Previous/Next Templates

msgAskNextTemplate =
  requireSaveAsker msgNextTemplate

msgAskPreviousTemplate =
  requireSaveAsker msgPreviousTemplate

msgNextTemplate =
  Msg "Next Template" (navigateTemplate ((+) 1))

msgPreviousTemplate =
  Msg "Previous Template" (navigateTemplate (\i -> i-1))

navigateTemplate offset m =
  case m.lastSelectedTemplate of
    Just (templateName, _) ->
      Utils.findSublistIndex [templateName] (List.map Tuple.first Examples.list)
        |> Utils.bindMaybe (\i -> Utils.maybeGeti0 (offset i) Examples.list)
        |> Maybe.map (\(newTemplate,_) -> handleNew newTemplate m)
        |> Maybe.withDefault m

    Nothing ->
      m

--------------------------------------------------------------------------------
-- Code Editor Mode
--------------------------------------------------------------------------------

msgSetCodeEditorMode : CodeEditorMode -> Msg
msgSetCodeEditorMode mode =
  Msg "Set Code Editor Mode" <| \model ->
    let refreshedModel = refreshInputExp model in
    { refreshedModel | codeEditorMode = mode }

--------------------------------------------------------------------------------
-- Collect and Solve
--------------------------------------------------------------------------------

-- Select which things to collect

addSelectedHole : HoleId -> Model -> Model
addSelectedHole holeId model =
  { model | selectedHoles = Set.insert holeId model.selectedHoles }

possiblyClearHoleFillings : Model -> Model
possiblyClearHoleFillings model =
  if
    model.holeExampleInputs == Dict.empty
      && model.backpropExampleInput == ""
  then
    { model | holeFillings = Nothing }
  else
    model

msgAddSelectedHole : HoleId -> Msg
msgAddSelectedHole holeId =
  Msg "Add Selected Hole" (addSelectedHole holeId)

msgRemoveSelectedHole : HoleId -> Msg
msgRemoveSelectedHole holeId =
  Msg "Remove Selected Hole" <| \model ->
    { model
        | selectedHoles =
            Set.remove holeId model.selectedHoles
        , holeExampleInputs =
            Dict.remove holeId model.holeExampleInputs
    } |> possiblyClearHoleFillings

msgSetSelectedUnExp : UnExp d -> Msg
msgSetSelectedUnExp u =
  Msg "Set Selected UnExp" <| \model ->
    { model | selectedUnExp = Just (U.mapData (\_ -> ()) u) }

msgUnsetSelectedUnExp : Msg
msgUnsetSelectedUnExp =
  Msg "Unset Selected UnExp" <| \model ->
    { model
        | selectedUnExp =
            Nothing
        , backpropExampleInput =
            ""
    } |> possiblyClearHoleFillings

-- Update textboxes

msgUpdateHoleExampleInput : HoleId -> Int -> U.Env -> String -> Msg
msgUpdateHoleExampleInput holeId index env input =
  Msg "Update Hole Example Input" <| \model ->
    let
      focusedExampleInputs =
        Dict.get holeId model.holeExampleInputs
          |> Maybe.withDefault Dict.empty

      newFocusedExampleInputs =
        Dict.insert index (env, input) focusedExampleInputs

      newExampleInputs =
        Dict.insert holeId newFocusedExampleInputs model.holeExampleInputs
    in
      { model | holeExampleInputs = newExampleInputs }

msgUpdateBackpropExampleInput : String -> Msg
msgUpdateBackpropExampleInput s =
  Msg "Update Backprop Example Input" <| \model ->
    { model | backpropExampleInput = s }

-- Collect and solve

msgCollectAndSolve : Msg
msgCollectAndSolve =
  Msg "Collect and Solve" <| \model ->
    let
      maybeCollectedConstraints : Maybe Constraints
      maybeCollectedConstraints =
        model.unExpOutput
          |> Result.toMaybe
          |> Maybe.andThen Tuple.second

      maybeBackpropExampleConstraints : Maybe Constraints
      maybeBackpropExampleConstraints =
        if model.backpropExampleInput == "" then
          Just []
        else
          flip Maybe.andThen model.selectedUnExp <| \uSelected ->
            model.backpropExampleInput
              |> U.parseExample
              |> Debug.log "parsed backprop example"
              |> Result.toMaybe
              |> Maybe.andThen (TriEval.backprop uSelected)

      maybeHoleExampleConstraints : Maybe Constraints
      maybeHoleExampleConstraints =
        let
          makeWorld : (U.Env, String) -> Maybe World
          makeWorld =
            Tuple.mapSecond
              ( U.parseExample
                  >> Debug.log "parsed hole example"
                  >> Result.toMaybe
              )
              >> Utils.liftMaybePair2
        in
          model.holeExampleInputs
            |> Dict.map (\_ -> Dict.values)
            |> Dict.toList
            |> List.map
                 ( Tuple.mapSecond
                       ( List.filter (\(_, input) -> input /= "")
                           >> List.map makeWorld
                           >> Utils.projJusts
                       )
                     >> Utils.liftMaybePair2
                     >> Maybe.map (\(holeId, worlds) -> List.map ((,) holeId) worlds)
                 )
            |> Utils.projJusts
            |> Maybe.map List.concat
    in
      case
        ( maybeCollectedConstraints
        , maybeBackpropExampleConstraints
        , maybeHoleExampleConstraints
        )
      of
        (Just k1, Just k2, Just k3) ->
          { model
              | holeFillings =
                  Just << NonDet.toList <|
                    PBESynthesis.solve
                      (Types2.getDataTypeDefs model.inputExp)
                      model.holeEnv
                      (k1 ++ k2 ++ k3)
              , codeAtPbeSynthesis =
                  Just model.code
          }

        _ ->
          { model
              | holeFillings = Nothing
              , codeAtPbeSynthesis = Nothing
          }

--------------------------------------------------------------------------------
-- UnExp Preview
--------------------------------------------------------------------------------

msgShowUnExpPreview : Exp -> Msg
msgShowUnExpPreview exp =
  Msg "Show UnExp Preview" <| \model ->
    { model
        | unExpPreview =
            Just
              ( exp
              , Result.map Tuple.first <| TriEval.eval exp
              )
    }

msgClearUnExpPreview : Msg
msgClearUnExpPreview =
  Msg "Clear UnExp Preview" <| \model ->
    { model | unExpPreview = Nothing }


--------------------------------------------------------------------------------
-- Tiny Structured Editors for Low, Low Prices!
--------------------------------------------------------------------------------

msgSelectTSEFLLPPath path =
  Msg ("Select TSEFLLP path " ++ toString path) <| \model ->
    { model | tinyStructuredEditorsForLowLowPricesState =
                  TinyStructuredEditorsForLowLowPrices.selectPath model.tinyStructuredEditorsForLowLowPricesState path
    }

msgDeselectTSEFLLPPath path =
  Msg ("Deselect TSEFLLP path " ++ toString path) <| \model ->
    { model | tinyStructuredEditorsForLowLowPricesState =
                  TinyStructuredEditorsForLowLowPrices.deselectPath model.tinyStructuredEditorsForLowLowPricesState path
    }

msgTSEFLLPApplyReplacement tsefllpReplacementVal path =
  Msg ("Apply replacement to TSEFLLP path " ++ toString path) <| \model ->
    let
      updatedValResult =
        TinyStructuredEditorsForLowLowPrices.newLangValResultViaReplacement model.tinyStructuredEditorsForLowLowPricesState tsefllpReplacementVal path

      _ =
        case updatedValResult of
          Ok _       -> ()
          Err errStr -> Utils.log errStr

      maybeBackpropResult =
        let originalCode = Syntax.unparser model.syntax model.inputExp in
        backpropSynthesisResults model.inputExp model.inputEnv model.inputVal updatedValResult
        |> Utils.findFirst (resultExp >> Syntax.unparser model.syntax >> (/=) originalCode)

    in
    case maybeBackpropResult of
      Just synthesisResult ->
        { model | tinyStructuredEditorsForLowLowPricesState =
                      TinyStructuredEditorsForLowLowPrices.deselectAll model.tinyStructuredEditorsForLowLowPricesState path
                , code = Syntax.unparser model.syntax (resultExp synthesisResult)
        } |> upstateRun

      Nothing ->
        let _ = Utils.log "No bidirectional backprop results!" in
        model
