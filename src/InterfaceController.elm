port module InterfaceController exposing
  ( update
  , msgNoop
  , msgWindowDimensions
  , msgVisibilityChange
  , msgCodeUpdate
  , msgKeyPress, msgKeyDown, msgKeyUp
  , msgMouseIsDown, msgMousePosition
  , msgRun, upstateRun, msgTryParseRun
  , msgCallUpdate
  , msgAceUpdate
  , msgUserHasTyped
  , msgOutputCanvasUpdate
  , msgUndo, msgRedo, msgCleanCode
  , msgHideWidgets
  , msgDigHole, msgMakeEqual, msgRelate, msgIndexedRelate, msgBuildAbstraction
  , msgSelectSynthesisResult, msgClearSynthesisResults
  , msgStartAutoSynthesis, msgStopAutoSynthesisAndClear
  , msgHoverSynthesisResult, msgPreview, msgClearPreview
  , msgSetEditingContext, msgClearEditingContext
  , msgActivateRenameInOutput, msgUpdateRenameInOutputTextBox, msgDoRename
  , msgAddArg, msgRemoveArg
  , msgGroupBlobs, msgDuplicate, msgMergeBlobs, msgAbstractBlobs
  , msgReplicateBlob
  , msgToggleCodeBox
  , msgSetOutputLive, msgSetOutputPrint, msgSetOutputShowValue
  , msgSetHeuristicsBiased, msgSetHeuristicsNone, msgSetHeuristicsFair
  , msgSetLiveSyncDelay
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
  , msgToggleAutosave
  , msgExportCode, msgExportSvg
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
  , msgHoverDeuceResult
  , msgLeaveDeuceResult
  , msgUpdateRenameVarTextBox
  , msgClearDrag
  , msgDragDeucePopupPanel
  , msgDragAutoOutputToolsPopupPanel
  , msgDragEditCodePopupPanel
  , msgDragDeuceRightClickMenu
  , msgTextSelect
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
  )

import Updatable exposing (Updatable)
import Lang exposing (..) --For access to what makes up the Vals
import Types
import Ace
import ParserUtils exposing (showError)
import FastParser exposing (freshen)
import Info
import LangTools
import LangSimplify
import ValueBasedTransform
import Blobs exposing (..)
import Draw
import ExpressionBasedTransform as ETransform
import Sync
import Eval
import Update
import Results
import Utils
import Keys
import InterfaceModel as Model exposing (..)
import SleekLayout exposing
  ( canvasPosition
  , clickToCanvasPoint
  , deucePopupPanelMouseOffset
  , deuceRightClickMenuMouseOffset
  )
import AceCodeBox
import OutputCanvas
import AnimationLoop
import FileHandler exposing (ExternalFileMessage(..), InternalFileMessage(..))
import File exposing (FileExtension, Filename, File, FileIndex)
import DeucePopupPanelInfo exposing (DeucePopupPanelInfo)
import ColorScheme
import SyntaxHighlight exposing (ExternalSyntaxHighlightMessage(..))
-- import InterfaceStorage exposing (installSaveState, removeDialog)
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
import ColorNum
import Syntax exposing (Syntax)
import ElmParser
import LangUnparser -- for comparing expressions for equivalence
import History exposing (History)
import SlowTypeInference

import ImpureGoodies

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

--------------------------------------------------------------------------------

debugLog = Config.debugLog Config.debugController

--------------------------------------------------------------------------------

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
      { initSubstPlus = FastParser.substPlusOf m.inputExp
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
    EApp _ e0 [e1,_,_] _ -> case e0.val.e__ of
      EVar _ "inferred"  -> e1.val.e__
      _                  -> e__
    EApp _ e0 [_,e1] _   -> case e0.val.e__ of
      EVar _ "flow"      -> e1.val.e__
      _                  -> e__
    EOp _ op [e1,e2] _   ->
      case (op.val, e2.val.e__) of
        (Plus, EConst _ 0 _ _) -> e1.val.e__
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
      |> Result.map (\(val, widgets, slate, code) -> onOk reparsed val widgets slate code)
    )
  in
  handleError model result

handleError : Model -> Result String Model -> Model
handleError oldModel result =
  case result of
    Ok newModel -> newModel
    Err s       -> { oldModel | errorBox = Just s }

updateCodeBoxInfo : Types.AceTypeInfo -> Model -> CodeBoxInfo
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
  case exp.val.e__ of
    EComment _ _ e       -> rewriteInnerMostExpToMain e
    EOption _ _ _ _ e    -> rewriteInnerMostExpToMain e
    ETyp _ _ _ e _       -> rewriteInnerMostExpToMain e
    ETypeAlias _ _ _ e _ -> rewriteInnerMostExpToMain e
    ELet ws1 lk rec p1 ws2 e1 ws3 e2 ws4 ->
      replaceE__ exp (ELet ws1 lk rec p1 ws2 e1 ws3 (rewriteInnerMostExpToMain e2) ws4)
    _ ->
      eLets [("main", exp)] (eVar "main")


--------------------------------------------------------------------------------
-- Mouse Events


maybeClickableToPointWithSnap : (Int, Int) -> Maybe Clickable -> PointWithSnap
maybeClickableToPointWithSnap (defaultX, defaultY) maybeClickable =
  case maybeClickable of
    Just (PointWithProvenance xVal yVal) ->
      ( (round (valToNum xVal), SnapVal xVal)
      , (round (valToNum yVal), SnapVal yVal)
      )
    Nothing ->
      ((defaultX, NoSnap), (defaultY, NoSnap))


onMouseClick clickPos old maybeClickable =
  let (isOnCanvas, (canvasX, canvasY) as pointOnCanvas) = clickToCanvasPoint old clickPos in
  case (old.tool, old.mouseMode) of

    -- Inactive zone
    (Cursor, MouseDragZone (i, k, z) Nothing) ->
      onClickPrimaryZone i k z { old | mouseMode = MouseNothing }

    -- Active zone but not dragged
    (Cursor, MouseDragZone (i, k, z) (Just (_, _, False))) ->
      onClickPrimaryZone i k z { old | mouseMode = MouseNothing }

    (Cursor, _) ->
      { old | mouseMode = MouseNothing }

    (Poly stk, MouseDrawNew polyPoints) ->
      let pointToAdd = maybeClickableToPointWithSnap (canvasX, canvasY) maybeClickable in
      case polyPoints of
        DrawJustStarted _ -> { old | mouseMode = MouseDrawNew (PolyPoints [pointToAdd]) } -- maybeClickable of DrawJustStarted will be same as the given maybeClickable
        PolyPoints [] -> Debug.crash "invalid state, PolyPoints should always be nonempty"
        PolyPoints points ->
          let ((initialX, _), (initialY, _)) = Utils.last_ points in
          let ((thisX, _), (thisY, _))       = pointToAdd in
          if Utils.distanceInt (thisX, thisY) (initialX, initialY) > Draw.drawDotSize then { old | mouseMode = MouseDrawNew (PolyPoints (pointToAdd :: points)) }
          else if List.length points == 2 then { old | mouseMode = MouseNothing }
          else if List.length points == 1 then switchToCursorTool old
          else upstateRun <| switchToCursorTool <| Draw.addPolygon stk old points
        _ -> Debug.crash "invalid state, points should be DrawJustStarted or PolyPoints for polygon tool"

    (Path stk, MouseDrawNew (DrawJustStarted _)) ->
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

    (_, MouseDrawNew (DrawJustStarted _)) -> switchToCursorTool old

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


onMouseDrag lastPosition newPosition old =
  let (mx0, my0) = (newPosition.x, newPosition.y) in
  let (isOnCanvas, (mx, my)) = clickToCanvasPoint old newPosition in
  let (_, (mxLast, myLast))  = clickToCanvasPoint old lastPosition in
  case old.mouseMode of

    MouseNothing -> old

    MouseDragLayoutWidget f ->
      f (mx0, my0) old

    MouseDrag f ->
      f lastPosition newPosition old

    MouseDragZone zoneKey Nothing ->
      old

    MouseDragZone zoneKey (Just (trigger, (mx0, my0), _)) ->
      case old.liveSyncDelay of
        False ->
          applyTrigger old.solutionsCache zoneKey trigger (mx0, my0) (mx, my) old
        True ->
          -- TODO: define and run a version of trigger that applies to the
          -- single value being manipulated, rather than the entire program.
          old

    MouseDragSelect initialPosition initialSelectedShapes initialSelectedFeatures initialSelectedBlobs ->
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
      case (old.tool, shapeBeingDrawn) of
        (Poly _, _) -> old -- handled by onMouseClick instead
        (Path _, _) -> old -- handled by onMouseClick instead

        (_, DrawJustStarted maybeClickable) ->
          let lastPointOnCanvas = maybeClickableToPointWithSnap (mxLast, myLast) maybeClickable in
          let pointOnCanvas = ((mx, NoSnap), (my, NoSnap)) in
          case old.tool of
            PointOrOffset -> { old | mouseMode = MouseDrawNew (Offset1D lastPointOnCanvas NoSnap (mx, my)) }
            _             -> { old | mouseMode = MouseDrawNew (TwoPoints lastPointOnCanvas pointOnCanvas) }

        (_, TwoPoints startingPoint _) ->
          -- Currently, only point widgets have the Vals on then to snap to.
          -- Everything else has dummy vals. Need to move features to Prelude rather than hard coding.
          -- So much work to do!
          let pointPerhapsWithSnap =
            old.widgets
            |> Utils.mapFirstSuccess
                (\widget ->
                  case widget of
                    WPoint _ xVal _ yVal _ ->
                      if Utils.distance (valToNum xVal, valToNum yVal) (toFloat mx, toFloat my) <= 7.0
                      then Just ((valToInt xVal, SnapVal xVal), (valToInt yVal, SnapVal yVal))
                      else Nothing
                    _ -> Nothing
                )
            |> Maybe.withDefault ((mx, NoSnap), (my, NoSnap))
          in
          { old | mouseMode = MouseDrawNew (TwoPoints startingPoint pointPerhapsWithSnap) }

        (_, Offset1D (((x1Int, _), (y1Int, _)) as basePoint) _ _) ->
          let ((effectiveMX, effectiveMY), amountSnap) =
            let (dxRaw, dyRaw) = (mx - x1Int, my - y1Int) in
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
                if      axis == X && sign == Positive then ((x1Int + snapToNumber, my), SnapVal val)
                else if axis == X && sign == Negative then ((x1Int - snapToNumber, my), SnapVal val)
                else if axis == Y && sign == Positive then ((mx, y1Int + snapToNumber), SnapVal val)
                else                                       ((mx, y1Int - snapToNumber), SnapVal val)
              Nothing ->
                let numberOfSnapsPassed = Utils.count (\((_, high), _, _) -> amount >= high) snapRanges in
                if      axis == X && sign == Positive then ((mx - numberOfSnapsPassed*pixelsPerSnap, my), NoSnap)
                else if axis == X && sign == Negative then ((mx + numberOfSnapsPassed*pixelsPerSnap, my), NoSnap)
                else if axis == Y && sign == Positive then ((mx, my - numberOfSnapsPassed*pixelsPerSnap), NoSnap)
                else                                       ((mx, my + numberOfSnapsPassed*pixelsPerSnap), NoSnap)
          in
          { old | mouseMode = MouseDrawNew (Offset1D basePoint amountSnap (effectiveMX, effectiveMY)) }

        _ -> old

    MouseDownInCodebox pos ->
      old

onMouseUp old =
  case (old.outputMode, old.mouseMode) of

    (Print _, _) -> old

    (PrintScopeGraph _, _) -> old

    (Live, MouseDragZone zoneKey (Just (trigger, (mx0, my0), _))) ->
      case old.liveSyncDelay of
        False ->
          finishTrigger zoneKey old
        True ->
          let (isOnCanvas, (mx, my)) = clickToCanvasPoint old (mousePosition old) in
          old
            |> applyTrigger old.solutionsCache zoneKey trigger (mx0, my0) (mx, my)
            |> finishTrigger zoneKey

    (_, MouseDrawNew points) ->
      let resetMouseMode model = { model | mouseMode = MouseNothing } in
      case (old.tool, points, old.keysDown == [Keys.keyShift]) of

        (Line _,     TwoPoints pt1 pt2, _) -> upstateRun <| resetMouseMode <| Draw.addLine old pt1 pt2
        (HelperLine, TwoPoints pt1 pt2, _) -> upstateRun <| resetMouseMode <| Draw.addLine old pt1 pt2

        -- (Rect Raw,      TwoPoints pt1 pt2, False) -> upstateRun <| resetMouseMode <| Draw.addRawRect old pt1 pt2
        -- (Rect Raw,      TwoPoints pt1 pt2, True)  -> upstateRun <| resetMouseMode <| Draw.addRawSquare old pt1 pt2
        -- (Rect Stretchy, TwoPoints pt1 pt2, False) -> upstateRun <| resetMouseMode <| Draw.addStretchyRect old pt1 pt2
        -- (Rect Stretchy, TwoPoints pt1 pt2, True)  -> upstateRun <| resetMouseMode <| Draw.addStretchySquare old pt1 pt2
        --
        -- (Oval Raw,      TwoPoints pt1 pt2, False) -> upstateRun <| resetMouseMode <| Draw.addRawOval old pt1 pt2
        -- (Oval Raw,      TwoPoints pt1 pt2, True)  -> upstateRun <| resetMouseMode <| Draw.addRawCircle old pt1 pt2
        -- (Oval Stretchy, TwoPoints pt1 pt2, False) -> upstateRun <| resetMouseMode <| Draw.addStretchyOval old pt1 pt2
        -- (Oval Stretchy, TwoPoints pt1 pt2, True)  -> upstateRun <| resetMouseMode <| Draw.addStretchyCircle old pt1 pt2

        -- (Lambda i,       TwoPoints pt1 pt2, _) -> upstateRun <| resetMouseMode <| Draw.addLambda i old pt1 pt2
        (Function fName, TwoPoints pt1 pt2, _) -> upstateRun <| resetMouseMode <| Draw.addFunction fName old pt1 pt2

        -- (Poly _, _, _) -> old
        -- (Path _, _, _) -> old
        --
        -- (Text, TwoPoints pt1 pt2, _) -> upstateRun <| resetMouseMode <| Draw.addTextBox old pt1 pt2

        (PointOrOffset, Offset1D pt1 snap pt2, _) -> upstateRun <| resetMouseMode <| Draw.addOffsetAndMaybePoint old pt1 snap pt2

        (_, (DrawJustStarted _), _)     -> switchToCursorTool old

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
  let dragInfo_ = (trigger, (mx0, my0), True) in

  evalFocusedExpression old.syntax old.editingContext newExp |> Result.andThen (\((newVal, newWidgets), _) ->
  LangSvg.resolveToRootedIndexedTree old.syntax old.slideNumber old.movieNumber old.movieTime newVal |> Result.map (\newSlate ->
    let newCode = Syntax.unparser old.syntax newExp in
    { old | code = newCode
          , lastRunCode = newCode
          , inputExp = newExp
          , inputVal = newVal
          , valueEditorString = Update.valToString newVal
          , slate = newSlate
          , widgets = newWidgets
          , codeBoxInfo = codeBoxInfo_
          , mouseMode = MouseDragZone zoneKey (Just dragInfo_)
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
  -- But this requires another Eval.run, which can be slow.
  -- Removing this workaround (which means the yellow highlights
  -- can be off after a live sync, until the next parse).
  --
  -- TODO: make a Sync.finishTrigger function that takes care of this.
  --
  let old_ = old in
  refreshHighlights zoneKey
    { old_ | mouseMode = MouseNothing, liveSyncInfo = refreshLiveInfo old_
           , history = modelCommit old.code [] old_.history
           , synthesisResultsDict = Dict.empty
           }

--------------------------------------------------------------------------------

evalFocusedExpression : Syntax -> Maybe (EId, Maybe EId) -> Exp -> Result String ((Val, Widgets), Env)
evalFocusedExpression syntax editingContext program =
  case editingContext of
    Just (focusedEId, maybeCallEId) ->
      let abortEId = maybeCallEId |> Maybe.withDefault focusedEId in
      Eval.doEvalEarlyAbort (\e -> e.val.eid == abortEId) syntax Eval.initEnv program

    Nothing ->
      -- want final environment of top-level definitions when evaluating e,
      -- for the purposes of running Little code to generate icons.
      -- but can't just use the output environment from eval directly.
      -- for example, if the last expression was a function call (either
      -- within the program or in Prelude), the final environment is from
      -- that function body. so instead, calling rewriteInnerMostExpToMain
      -- because the output environment from (let main eFinalBody main)
      -- will be the top-level definitions (and main).
      --
      let rewrittenE = rewriteInnerMostExpToMain program in
      Eval.doEval syntax Eval.initEnv rewrittenE


tryRun : Model -> Result (Model, String, Maybe Ace.Annotation) Model
tryRun old =
  let
    oldWithUpdatedHistory =
      let
        updatedHistory =
          modelCommit old.code [] old.history
      in
        { old | history = updatedHistory
              , lastRunCode = old.code
        }
  in
    case Syntax.parser old.syntax old.code of
      Err err ->
        Err (oldWithUpdatedHistory, showError err, Nothing)
      Ok e ->
        let resultThunk () =
          -- let aceTypeInfo = Types.typecheck e in

          evalFocusedExpression old.syntax old.editingContext e |>
          Result.andThen (\((newVal,ws),finalEnv) ->
            LangSvg.fetchEverything old.syntax old.slideNumber old.movieNumber 0.0 newVal
            |> Result.map (\(newSlideCount, newMovieCount, newMovieDuration, newMovieContinue, newSlate) ->
              let newCode = Syntax.unparser old.syntax e in -- unnecessary, if parse/unparse were inverses
              let new = loadLambdaAndFunctionToolIcons e finalEnv old in
              let new_ =
                { new | inputExp      = e
                      , inputVal      = newVal
                      , valueEditorString = Update.valToString newVal
                      , code          = newCode
                      , lastRunCode   = newCode
                      , slideCount    = newSlideCount
                      , movieCount    = newMovieCount
                      , movieTime     = 0
                      , movieDuration = newMovieDuration
                      , movieContinue = newMovieContinue
                      , runAnimation  = newMovieDuration > 0
                      , slate         = newSlate
                      , widgets       = ws
                      , typeGraph     = SlowTypeInference.typecheck e
                      , history       = modelCommit newCode [] old.history
                      , caption       = Nothing
                      , syncOptions   = Sync.syncOptionsOf old.syncOptions e
                      , errorBox      = Nothing
                      , preview       = Nothing
                      , synthesisResultsDict = Dict.singleton "Auto-Synthesis" (perhapsRunAutoSynthesis old e)
                }
              in
              let taskProgressAnnotation =
                case String.split "; The final program should look something like:\n" newCode |> List.map String.trimRight of
                  [regularCode, commentedOutTargetCode] ->
                    let normalize str =
                      Regex.replace Regex.All (Regex.regex "[\\s\\(\\)\\[\\]]+") (\_ -> "") str
                    in
                    let targetCode =
                      commentedOutTargetCode
                      |> Utils.stringReplace "\n;" "\n"
                      |> Utils.stringReplace ";\n" "\n"
                      |> normalize
                    in
                    let givenLines = String.split "\n" regularCode in
                    -- 1. For each line in code, try to find a match in targetCode.
                    let singleLineGoodness =
                      givenLines
                      |> List.map normalize
                      |> List.map (\givenLine -> String.contains givenLine targetCode)
                    in
                    -- 2. If line and its immediate neighbors are all good, compare the line and its neighbors together.
                    -- First and last line are missing some context for this step, so insert explicit beginning/end of code markers to ensure an error if the last line is incomplete.
                    let multiLineGoodness =
                      let targetCodeWithBeginEndMarkers = "BOC" ++ targetCode ++ "EOC" in
                      let lineTriples         = Utils.zip3 (["", "BOC"] ++ givenLines) (["BOC"] ++ givenLines ++ ["EOC"]) (givenLines ++ ["EOC", ""]) in
                      let lineGoodnessTriples = Utils.zip3 ([True, True] ++ singleLineGoodness) ([True] ++ singleLineGoodness ++ [True]) (singleLineGoodness ++ [True, True]) in
                      Utils.zip lineGoodnessTriples lineTriples
                      |> List.drop 1
                      |> Utils.dropLast 1
                      |> List.map
                          (\((priorLineGood, lineGood, nextLineGood), (priorLine, line, nextLine)) ->
                            if priorLineGood && lineGood && nextLineGood then
                              targetCodeWithBeginEndMarkers
                              |> String.contains (normalize (priorLine ++ line ++ nextLine))
                            else
                              True
                          )
                    in
                    let singleLineAnnotataions =
                      singleLineGoodness
                      |> Utils.zipi0
                      |> List.filter (not << Tuple.second)
                      |> List.map (\(row, _) -> { row = row, type_ = "error", text = "Does not match target code!" } )
                    in
                    let multiLineAnnotataions =
                      multiLineGoodness
                      |> Utils.zipi0
                      |> List.filter (not << Tuple.second)
                      |> List.map (\(row, _) -> { row = row, type_ = "error", text = "Missing code or ordering problem!" } )
                    in
                    { annotations = singleLineAnnotataions ++ multiLineAnnotataions
                    , highlights  = []
                    , tooltips    = []
                    }

                  _ ->
                    Types.dummyAceTypeInfo
              in
              let new__ =
                { new_ | liveSyncInfo = refreshLiveInfo new_
                       , codeBoxInfo = updateCodeBoxInfo taskProgressAnnotation new_
                       }
                |> resetDeuceState
              in
              case new__.editingContext of
                Just (eid, _) -> { new__ | deuceState = { emptyDeuceState | hoveredWidgets = [DeuceExp eid] } }
                Nothing       -> new__
            )
          )
        in
          case ImpureGoodies.crashToError resultThunk of
            Err s         -> Err (oldWithUpdatedHistory, s, Nothing)
            Ok (Err s)    -> Err (oldWithUpdatedHistory, s, Nothing)
            Ok (Ok model) -> Ok model


--------------------------------------------------------------------------------
-- Updating the Model

-- 1. Compute a "pure" update (only the newModel), and then
-- 2. Decide whether to issue a command based on simple predicates
--    (name of Msg, and simple comparison of oldModel and newModel).
--
update : Msg -> Model -> (Model, Cmd Msg)
update msg oldModel =
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
      let newModel = upstate msg oldModel in
      let cmd = issueCommand msg oldModel newModel in
      let (hookedModel, hookedCommands) = applyAllHooks oldModel newModel in
      ( Model.setAllUpdated hookedModel
      , Cmd.batch <|
          cmd :: updateCommands newModel ++ hookedCommands
      )

upstate : Msg -> Model -> Model
upstate msg old =
  case msg of
    Msg caption updateModel ->
      -- let _ = Debug.log "" (caption, old.userStudyTaskStartTime, old.userStudyTaskCurrentTime) in
      -- let _ = Debug.log "Msg" caption in
      -- let _ = if {-String.contains "Key" caption-} True then Debug.log caption (old.mouseMode, old.mouseState) else (old.mouseMode, old.mouseState) in
      let _ = debugLog "Msg" caption in
      updateModel old

    _ ->
      Debug.crash "InterfaceController.upstate: solver response messages should never hit here"

--------------------------------------------------------------------------------
-- Hooks to be run after every message

hooks : List (Model -> Model -> (Model, Cmd Msg))
hooks =
  [ handleSavedSelectionsHook
  , handleSyntaxHook
  , handleOutputSelectionChanges
  , focusJustShownRenameBox
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
{-
    let
      selectedEIdInterpretations = ShapeWidgets.selectionsProximalDistalEIdInterpretations newModel.inputExp newModel.slate newModel.widgets newModel.selectedFeatures newModel.selectedShapes newModel.selectedBlobs
      deuceWidgetInterpretations = selectedEIdInterpretations |> List.map (List.map DeuceExp)
      finalModel = { newModel | deuceToolsAndResults = DeuceTools.createToolCacheMultipleInterpretations newModel deuceWidgetInterpretations }
    in
    (finalModel, Cmd.none)
-}

port doFocusJustShownRenameBox : () -> Cmd msg

focusJustShownRenameBox : Model -> Model -> (Model, Cmd Msg)
focusJustShownRenameBox oldModel newModel =
  if oldModel.renamingInOutput == Nothing && newModel.renamingInOutput /= Nothing then
    (newModel, doFocusJustShownRenameBox ())
  else
    (newModel, Cmd.none)

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

issueCommand : Msg -> Model -> Model -> Cmd Msg
issueCommand msg oldModel newModel =
  case msg of
    Msg kind _ ->
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
          FileHandler.sendMessage <|
            Download
              (Model.prettyFilename WithoutExtension newModel ++ ".little")
              newModel.code

        "Export SVG" ->
          FileHandler.sendMessage <|
            Download
              (Model.prettyFilename WithoutExtension newModel ++ ".svg")
              (LangSvg.printSvg newModel.showGhosts newModel.slate)

        "Import Code" ->
          FileHandler.sendMessage <|
            RequestUploadedFile Model.importCodeFileInputId

        -- Do not send changes back to the editor, because this is the command where
        -- we receieve changes (if this is removed, an infinite feedback loop
        -- occurs).
        "Ace Update" ->
            if newModel.autosave && newModel.needsSave then
              FileHandler.sendMessage <|
                Write newModel.filename newModel.code
            else
              Cmd.none

        "Enable Text Edits" ->
          AceCodeBox.setReadOnly False

        "Disable Text Edits" ->
          AceCodeBox.setReadOnly True

        _ ->
          Cmd.batch
            [ if kind == "Update Font Size" then
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
                DependenceGraph.render (DependenceGraph.compute newModel.inputExp)
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

    _ ->
      Debug.crash "InterfaceController.issueCommand shouldn't get a solver response message!!"


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
      }
    almostNew =
      { ran
          | deuceState =
              newDeuceState
          , history =
              newHistory
      }
  in
    { almostNew
        | deuceToolsAndResults =
            DeuceTools.createToolCache almostNew
        , deuceToolResultPreviews =
            Dict.empty
    }
      |> DeuceTools.reselectDeuceTool
      |> resetDeucePopupPanelPosition

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
          { old | mouseState = (Nothing, pos_, Nothing) }
        (Just _, oldPos_, maybeClickable) ->
          onMouseDrag oldPos_ pos_ { old | mouseState = (Just True, pos_, maybeClickable) }

    deucePopupPanelPositionUpdater old =
      if noCodeWidgetsSelected old then
        let
          newDeucePopupPanelPosition =
            ( pos_.x + deucePopupPanelMouseOffset.x
            , pos_.y + deucePopupPanelMouseOffset.y
            )

          oldPopupPanelPositions =
            old.popupPanelPositions

          newPopupPanelPositions =
            { oldPopupPanelPositions | deuce = newDeucePopupPanelPosition }
        in
          { old | popupPanelPositions = newPopupPanelPositions }
      else
        old

    deHoverCallWidgets old =
      if isMouseDown old || old.renamingInOutput /= Nothing then
        old
      else
        let hoveredCallWidgets =
          let {x, y} = canvasPosition old (mousePosition old) in
          let extraMargin = 15 in
          old.hoveredCallWidgets
          |> List.filter
              (\((left, top, right, bot), _) ->
                round left  - extraMargin <= x &&
                round top   - extraMargin <= y &&
                round right + extraMargin >= x &&
                round bot   + extraMargin >= y
              )
        in
        { old | hoveredCallWidgets = hoveredCallWidgets }
  in
    Msg ("MousePosition " ++ toString pos_) <|
      mouseStateUpdater >> deucePopupPanelPositionUpdater >> deHoverCallWidgets

--------------------------------------------------------------------------------

refreshInputExp : Model -> Model
refreshInputExp old =
  let
    parseResult =
      Syntax.parser old.syntax old.code
    (newInputExp, codeClean) =
      case parseResult of
        Ok exp ->
          (exp, True)
        Err _ ->
          (old.inputExp, False)
  in
    { old
        | inputExp =
            newInputExp
        , codeClean =
            codeClean
    }

--------------------------------------------------------------------------------

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
      in
        if keyCode == Keys.keyEsc then
          if Model.anyDialogShown old then
            Model.closeAllDialogBoxes old
          else
            let
              new =
                { old | renamingInOutput = Nothing }
                  |> Model.hideDeuceRightClickMenu
                  |> resetDeuceState
                  |> \m -> { m | deucePopupPanelAbove = True }
            in
              case (old.tool, old.mouseMode) of
                (Cursor, _)         -> clearSelections new
                (_, MouseNothing)   -> { new | tool = Cursor }
                (_, MouseDrawNew _) -> { new | mouseMode = MouseNothing }
                _                   -> new

        -- TODO:
        --   make a better way than old.outputMode /= ShowValue to decide
        --   whether to enable output-tool-specific keyboard shortcuts
        else if old.outputMode /= ShowValue && keyCode == Keys.keyE && List.any Keys.isCommandKey old.keysDown && List.length old.keysDown == 1 then
          let newModel = doMakeEqual old in
          newModel.synthesisResultsDict
          |> Dict.get "Make Equal"
          |> Maybe.andThen (Utils.findFirst isResultSafe)
          |> Maybe.map (\synthesisResult -> { newModel | code = Syntax.unparser old.syntax  (resultExp synthesisResult) } |> clearSynthesisResults |> upstateRun )
          |> Maybe.withDefault old
        else if old.outputMode /= ShowValue && keyCode == Keys.keyBackspace then
          deleteInOutput old
        else if old.outputMode /= ShowValue && keyCode == Keys.keyD && List.any Keys.isCommandKey old.keysDown && List.length old.keysDown == 1 then
          doDuplicate old
        else if old.outputMode /= ShowValue && keyCode == Keys.keyG && List.any Keys.isCommandKey old.keysDown && List.length old.keysDown == 1 then
          doGroup old
        else if old.outputMode /= ShowValue && keyCode == Keys.keyZ && List.any Keys.isCommandKey old.keysDown && List.length old.keysDown == 1 then
          doUndo old
        else if old.outputMode /= ShowValue && keyCode == Keys.keyZ && List.any Keys.isCommandKey old.keysDown && List.any ((==) Keys.keyShift) old.keysDown && List.length old.keysDown == 2 then
          doRedo old

        else if keyCode == Keys.keyEnter && List.any Keys.isCommandKey old.keysDown && List.length old.keysDown == 1 then
          case old.outputMode of
            Live      -> upstateRun old
            ShowValue -> doCallUpdate old
            _         -> old

        else if old.outputMode == Live && keyCode == Keys.keyDown &&
                List.any Keys.isCommandKey old.keysDown && List.length old.keysDown == 1 then
          { old | outputMode = ShowValue }
        else if old.outputMode == ShowValue && keyCode == Keys.keyUp &&
                List.any Keys.isCommandKey old.keysDown && List.length old.keysDown == 1 then
          { old | outputMode = Live }

        else if
          not currentKeyDown &&
          keyCode == Keys.keyShift
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

cleanSynthesisResult (SynthesisResult {description, exp, isSafe, sortKey, children}) =
  SynthesisResult <|
    { description = description ++ " → Cleaned"
    , exp         = LangSimplify.cleanCode exp
    , isSafe      = isSafe
    , sortKey     = sortKey
    , children    = children
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

msgHideWidgets = Msg "Hide Widget(s)" hideWidgets


-- Should work for any selected widget, but only shows for offsets for now.
hideWidgets : Model -> Model
hideWidgets old =
  if Set.size old.selectedShapes == 0 && Dict.size old.selectedBlobs == 0 then
    let
      (offsetAmountEIds, offsetExpressionEIds) =
        old.selectedFeatures
        |> Set.toList
        |> List.filterMap
            (\feature ->
              case feature of
                ShapeFeature idAsShape (DFeat Offset) ->
                  case Utils.maybeGeti1 (-2 - idAsShape) old.widgets of
                    Just (WOffset1D baseXNumTr baseYNumTr axis sign amountNumTr amountVal endXVal endYVal) ->
                      Just
                        ( valEId amountVal
                        , valEId (if axis == X then endXVal else endYVal)
                        )
                    _ -> Nothing
                _ -> Nothing
            )
        |> List.unzip

      proximalInterpretation =
        ShapeWidgets.selectionsUniqueProximalEIdInterpretations
            old.inputExp
            old.slate
            old.widgets
            old.selectedFeatures
            old.selectedShapes
            old.selectedBlobs
        |> List.head
        |> Maybe.withDefault []

      hideWidgetsAroundEIds =
        Utils.removeAll proximalInterpretation offsetAmountEIds ++ offsetExpressionEIds

      newProgram =
        old.inputExp
        |> mapExp
            (\exp ->
              if List.member exp.val.eid hideWidgetsAroundEIds then
                eOp NoWidgets [replacePrecedingWhitespace " " exp] |> copyPrecedingWhitespace exp
              else
                exp
            )
    in
    upstateRun <| clearSelections { old | code = Syntax.unparser old.syntax newProgram }
  else
    old


msgDigHole = Msg "Dig Hole" <| \old ->
  let newExp =
    ValueBasedTransform.digHole old.inputExp old.selectedFeatures old.slate old.widgets old.syncOptions
  in
  runWithErrorHandling old newExp (\reparsed newVal newWidgets newSlate newCode ->
    debugLog "new model" <|
    clearSelections <|
      { old | code             = newCode
            , inputExp         = reparsed
            , inputVal         = newVal
            , valueEditorString = Update.valToString newVal
            , history          = modelCommit newCode [] old.history
            , slate            = newSlate
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
            case LangTools.findLetAndPatMatchingExpLoose expToDelete.val.eid program of
              Just (letExp, patBindingExpToDelete) ->
                let identsToDelete = LangTools.identifiersListInPat patBindingExpToDelete in
                let scopeAreas = LangTools.findScopeAreas (letExp.val.eid, 1) letExp in
                let varUses = scopeAreas |> List.concatMap (LangTools.identifierSetUses (Set.fromList identsToDelete)) in
                let deleteVarUses program =
                  varUses
                  |> List.map (.val >> .eid)
                  |> List.foldr deleteEId program
                  |> LangSimplify.simplifyAssignments
                in
                case CodeMotion.pluckByPId patBindingExpToDelete.val.pid program of -- TODO allow unsafe pluck out of as-pattern
                  Just (_, programWithoutBinding) -> deleteVarUses programWithoutBinding
                  Nothing                         -> deleteVarUses program

              Nothing ->
                case parentByEId program (LangTools.outerSameValueExp program expToDelete).val.eid of
                  (Just (Just parent)) ->
                    case parent.val.e__ of
                      EFun _ _ _ _ ->
                        deleteEId parent.val.eid program

                      EList ws1 heads ws2 maybeTail ws3 ->
                        case List.map Tuple.second heads |> Utils.findi (eidIs eidToDelete) of
                          Just iToDelete -> program |> replaceExpNodeE__ parent (EList ws1 (List.map ((,) space0) (Utils.removei iToDelete (List.map Tuple.second heads) |> imitateExpListWhitespace (List.map Tuple.second heads))) ws2 maybeTail ws3)
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
          -- let varEIdsPerhapsRemoved = LangTools.freeVars expToDelete |> List.map (.val >> .eid) |> Set.fromList in
          let varEIdsPerhapsRemoved =
            case LangTools.expToMaybeVar (expEffectiveExp expToDelete) of
              Just varExp -> Set.singleton (varExp.val.eid)
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

msgSelectSynthesisResult newExp = Msg "Select Synthesis Result" <| \old ->
  -- TODO unparse gets called twice, here and in runWith ...
  let newCode = Syntax.unparser old.syntax newExp in
  let new =
    { old | code = newCode
          , lastRunCode = newCode
          , synthesisResultsDict = Dict.empty
          , history = modelCommit newCode [] old.history
          }
  in
  runWithErrorHandling new newExp (\reparsed newVal newWidgets newSlate newCode ->
    -- debugLog "new model" <|
      let newer =
      { new | inputExp             = reparsed -- newExp
            , inputVal             = newVal
            , valueEditorString    = Update.valToString newVal
            , slate                = newSlate
            , widgets              = newWidgets
            , preview              = Nothing
            , synthesisResultsDict = Dict.singleton "Auto-Synthesis" (perhapsRunAutoSynthesis old reparsed)
      } |> clearSelections
      in
      { newer | liveSyncInfo = refreshLiveInfo newer
              , codeBoxInfo = updateCodeBoxInfo Types.dummyAceTypeInfo newer
              }
  )

clearSynthesisResults : Model -> Model
clearSynthesisResults old =
  { old | preview = Nothing, synthesisResultsDict = Dict.empty }

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
      |> List.map (LangTools.outerSameValueExpByEId old.inputExp >> .val >> .eid)
      |> Utils.dedup
      |> List.map (LangTools.justFindExpByEId old.inputExp)
      |> List.filter (not << isVar << expEffectiveExp)
      |> List.sortBy Info.parsedThingToLocation -- Choose earliest single expression in program.
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
                  case LangTools.findLetAndPatMatchingExpLoose expToDuplicate.val.eid old.inputExp of
                    Nothing          -> expToDuplicate.val.eid
                    Just (letExp, _) -> letExp.val.eid

                (_, newProgram) = LangTools.newVariableVisibleTo -1 name 1 expToDuplicate [expToDuplicate.val.eid] old.inputExp
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

msgSetOutputLive = Msg "Set Output Live" <| \old ->
  { old | outputMode = Live }

msgSetOutputPrint = Msg "Set Output Print" <| \old ->
  { old | outputMode = Print (LangSvg.printSvg old.showGhosts old.slate) }

msgSetOutputShowValue = Msg "Set Output Show Value" <| \old ->
  { old | outputMode = ShowValue }

updateHeuristics : Int -> Model -> Model
updateHeuristics heuristic old =
  let
    oldSyncOptions =
      old.syncOptions
    newSyncOptions =
      { oldSyncOptions | feelingLucky = heuristic }
  in
    case old.outputMode of
      Live ->
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

msgSetLiveSyncDelay b =
  Msg "Set Live" (\m -> { m | liveSyncDelay = b })

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
      evalFocusedExpression old.syntax old.editingContext old.inputExp |>
      Result.andThen (\((previousVal, _), _) ->
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

msgCallUpdate = Msg "Call Update" doCallUpdate

-- Doesn't yet work with focused editing contexts.
doCallUpdate m =
  let updatedExp = Syntax.parser m.syntax m.valueEditorString
    |> Result.mapError (\e -> toString e)
    |> Result.andThen (Eval.doEval m.syntax [])
    |> Result.map (\((v, _), _) -> Update.Raw v)
    |> Results.fromResult
    |> Results.andThen (\out -> Update.update Eval.initEnv m.inputExp m.inputVal out Results.LazyNil)
  in
  case updatedExp of
    Results.Errs msg -> Debug.log ("Could not update: " ++ msg) m
    Results.Oks solutions ->
      let firstValidSolution = Results.findFirst (
          \(env, exp) -> env == Eval.initEnv
          ) solutions
      in
      case firstValidSolution of
        Nothing -> let _ = Debug.log "No updates not modifying the environment." () in
          case solutions of
            Results.LazyNil -> let _ = Debug.log "More precisely, there was no solution" () in
              m
            Results.LazyCons (_, newCodeExp) _ -> let _ = Debug.log "There was at least one solution, but the environments would have differed" () in
              upstateRun { m | code = Syntax.unparser m.syntax newCodeExp }
        Just (_, newCodeExp) ->
          upstateRun { m | code = Syntax.unparser m.syntax newCodeExp }


--------------------------------------------------------------------------------

showCodePreview old code =
  case Syntax.parser old.syntax code of
    Ok exp  -> showExpPreview old exp
    Err err -> { old | preview = Just (code, Err (showError err)) }

showExpPreview old exp =
  let code = Syntax.unparser old.syntax exp in
  case runAndResolve old exp of
    Ok (val, widgets, slate, _) -> { old | preview = Just (code, Ok (val, widgets, slate)) }
    Err s                       -> { old | preview = Just (code, Err s) }

msgHoverSynthesisResult resultsKey pathByIndices = Msg "Hover SynthesisResult" <| \old ->
  let maybeFindResult path results =
    case path of
      []    -> Nothing
      [i]   -> Utils.maybeGeti0 i results
      i::is -> Utils.maybeGeti0 i results |> Maybe.andThen (\(SynthesisResult {children}) -> children |> Maybe.andThen (maybeFindResult is))
  in
  let setResultChildren path childResults oldResults =
    case path of
      []    -> oldResults
      [i]   -> oldResults |> Utils.getReplacei0 i (\(SynthesisResult attrs) -> SynthesisResult { attrs | children = Just childResults})
      i::is -> oldResults |> Utils.getReplacei0 i (\(SynthesisResult attrs) -> SynthesisResult { attrs | children = Just (setResultChildren is childResults (attrs.children |> Maybe.withDefault []))})
  in
  let oldResults = Utils.getWithDefault resultsKey [] old.synthesisResultsDict in
  case maybeFindResult pathByIndices oldResults of
    Just (SynthesisResult {description, exp, sortKey, children}) ->
      let newModel = { old | hoveredSynthesisResultPathByIndices = pathByIndices } in
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
      showExpPreview newModel2 exp

    Nothing ->
      { old | preview = Nothing
            , hoveredSynthesisResultPathByIndices = [] }


msgPreview expOrCode = Msg "Preview" <| \old ->
  case expOrCode of
    Left exp   -> showExpPreview old exp
    Right code -> showCodePreview old code

msgClearPreview = Msg "Clear Preview" <| \old ->
  { old | preview = Nothing }

-- msgCancelSync = Msg "Cancel Sync" <| \old ->
--   upstateRun
--     { old | liveSyncInfo = refreshLiveInfo old }

msgSetEditingContext focusedEId maybeExampleCall = Msg "Set Editing Context" <| \old ->
  let deuceState = old.deuceState in
  { old | editingContext = Just (focusedEId, maybeExampleCall) }
  |> clearSelections
  |> upstateRun

msgClearEditingContext = Msg "Clear Editing Context" <| \old ->
  { old | editingContext = Nothing }
  |> clearSelections
  |> upstateRun

msgActivateRenameInOutput pid = Msg ("Active Rename Box for PId " ++ toString pid) <| \old ->
  let oldPatStr =
    findPatByPId old.inputExp pid
    |> Maybe.map (Syntax.patternUnparser Syntax.Elm >> Utils.squish)
    |> Maybe.withDefault ""
  in
  { old | renamingInOutput = Just (pid, oldPatStr) }

msgUpdateRenameInOutputTextBox newText = Msg ("Update Rename In Output: " ++ newText) <| \old ->
  case old.renamingInOutput of
    Just (pid, _) -> { old | renamingInOutput = Just (pid, newText) }
    Nothing       -> old

-- Shouldn't need pid b/c there should only be one rename box at a time, but I hate state.
msgDoRename pid = Msg ("Rename PId " ++ toString pid) <| \old ->
  case old.renamingInOutput of
    Just (renamingPId, newPatStr) ->
      if renamingPId == pid then
        let
          pat = LangTools.justFindPatByPId old.inputExp pid
          identPatsInOrder =
            flattenPatTree pat
            |> List.filter (LangTools.patToMaybeIdent >> Utils.maybeToBool)
            |> List.sortBy Info.parsedThingToLocation
        in
        case ElmParser.parsePatternUnfresh newPatStr of
          Ok newPat ->
            let
              newIdentsInOrder =
                flattenPatTree newPat
                |> List.sortBy Info.parsedThingToLocation
                |> List.filterMap LangTools.patToMaybeIdent

              newProgram =
                Utils.zip identPatsInOrder newIdentsInOrder
                |> Utils.foldl
                    old.inputExp
                    (\(oldPat, newName) programSoFar ->
                      CodeMotion.renamePatByPId oldPat.val.pid newName programSoFar
                      |> List.filter isResultSafe
                      |> List.head
                      |> Maybe.map resultExp
                      |> Maybe.withDefault programSoFar
                    )
            in
            { old | code             = Syntax.unparser old.syntax newProgram
                  , renamingInOutput = Nothing
            } |> upstateRun

          Err _ ->
            let _ = Utils.log <| "Couldn't parse new pattern " ++ newPatStr in
            { old | renamingInOutput = Nothing }

      else
        old

    Nothing ->
      old


msgAddArg funcBody = Msg "Add Arg to Function in Output" <| \old ->
  let maybeMaybeParent = parentByEId old.inputExp funcBody.val.eid in
  case Maybe.map (Maybe.map (\parent -> (parent, parent.val.e__))) maybeMaybeParent of
    Just (Just (funcExp, EFun _ argPats funcBody _)) ->
      let targetPPId =
        ( (funcExp.val.eid, 1)
        , [ 1 + List.length argPats ] -- By default, insert argument at the end
        )
      in
      let possibleArgEIds =
        let domain = flattenExpTree funcBody |> List.map (.val >> .eid) |> Set.fromList in
        let expFilter = .val >> .eid >> (flip Set.member) domain in
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
              |> List.map (setResultSortKey [toFloat line, toFloat col])
            )
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
      Debug.crash "InterfaceController.requireSaveAsker shouldn't get a solver response message!!"

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
  { old | needsSave = False
        , lastSaveState = Just old.code }

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
        , history = History.begin { code = file.contents, selectedDeuceWidgets = [] }
        , lastSaveState = Just file.contents
        , needsSave = needsSave
        }

loadIconFromFile : Env -> File -> Model -> Model
loadIconFromFile env icon old =
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

-- for basic icons, env will be Eval.initEnv.
-- for LambdaTool icons, env will be from result of running main program.
iconify : Syntax -> Env -> String -> Html.Html Msg
iconify syntax env code =
  let
    exp =
      Utils.fromOkay "Error parsing icon"
        <| Syntax.parser syntax code
    svgElement =
      -- Not using Canvas.buildHtml because can't have circular dependency between InterfaceController and Canvas
      -- Various widgets in Canvas defined their model update functions inline to avoid this, but now we're
      -- invoking more complicated transforms from the output, so Canvas has to depend on InterfaceController.
      --
      -- So icons are limited to SVG for now, which is probably fine.
      Utils.fromOkay "Error creating icon"
        <| LangSvg.evalToSvg syntax env exp
      -- Canvas.buildHtml ({ initModel | showGhosts = False }, False) slate
  in
  wrapIconWithSvg False [ svgElement ]


wrapIconWithSvg computeViewBox svgElements =
  let subPadding x = x - 10 in
  let viewBoxAttrs =
    if computeViewBox then
      case LangSvg.estimatedBounds (Svg.svg [] svgElements) of
        Nothing     -> [Svg.Attributes.viewBox "0 0 50 50"]
        Just bounds ->
          [ bounds.left
          , bounds.top
          , bounds.right - bounds.left
          , bounds.bot   - bounds.top
          ] |> List.map toString |> String.join " " |> Svg.Attributes.viewBox |> List.singleton
    else
      []
  in
  Svg.svg
    ([ Svg.Attributes.width <|
        (SleekLayout.px << subPadding << .width) SleekLayout.iconButton
    , Svg.Attributes.height <|
        (SleekLayout.px << subPadding << .height) SleekLayout.iconButton
    ] ++ viewBoxAttrs)
    svgElements



loadLambdaAndFunctionToolIcons program finalEnv old =
  let modelWithLambdaIconsLoaded =
    let lambdaTools_ =
      Draw.lambdaToolOptionsOf old.syntax (splitExp program) finalEnv ++ initModel.lambdaTools
    in
    lambdaTools_
    |> Utils.foldl
        { old | lambdaTools = lambdaTools_ }
        (\tool model ->
          let icon = lambdaToolIcon tool in
          if Dict.member icon.filename.name model.icons
            then model
            else loadIconFromFile finalEnv icon model
        )
  in
  let modelWithFunctionIconsLoaded =
    Draw.getDrawableFunctions modelWithLambdaIconsLoaded
    |> Utils.foldl
        modelWithLambdaIconsLoaded
        (\(funcName, _, _) model ->
          if Dict.member funcName model.icons then
            model
          else
            let iconHtml = wrapIconWithSvg True (Draw.drawNewFunction funcName model ((10, NoSnap), (10, NoSnap)) ((40, NoSnap), (40, NoSnap))) in
            { model | icons = Dict.insert funcName iconHtml model.icons }
        )
  in
  modelWithFunctionIconsLoaded


updateFileIndex : FileIndex -> Model -> Model
updateFileIndex fileIndex old =
  { old | fileIndex = fileIndex }

-- TODO: clear state (e.g. selectedEIds) after read file

msgLoadIcon : File -> Msg
msgLoadIcon file =
  Msg "Load Icon" <|
    loadIconFromFile Eval.initEnv file

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
  case Utils.maybeFind template Examples.list of
    Nothing -> let _ = Debug.log "WARN: not found:" template in old
    Just (_, thunk) ->
      let {e,v,ws,ati} = thunk () in
      let so = Sync.syncOptionsOf old.syncOptions e in
      let outputMode =
        Utils.fromOk "SelectExample mkLive" <|
          mkLive old.syntax so old.slideNumber old.movieNumber old.movieTime e (v,ws)
      in
      LangSvg.fetchEverything old.syntax old.slideNumber old.movieNumber old.movieTime v
      |> Result.map (\(slideCount, movieCount, movieDuration, movieContinue, slate) ->
        let code = Syntax.unparser old.syntax e in
        { initModel | inputExp      = e
                    , inputVal      = v
                    , valueEditorString = Update.valToString v
                    , code          = code
                    , lastRunCode   = code
                    , history       = History.begin { code = code, selectedDeuceWidgets = [] }
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
                    , widgets       = ws
                    , codeBoxInfo   = updateCodeBoxInfo ati old
                    , filename      = Model.bufferFilename old
                    , syntax        = old.syntax
                    , needsSave     = True
                    , lastSaveState = Nothing

                    , lastSelectedTemplate = Just template

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
                    } |> resetDeuceState
      ) |> handleError old) >> closeDialogBox New >> upstateRun -- Run to load custom tool icons

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

msgToggleAutosave = Msg "Toggle Autosave" <| \old ->
  { old | autosave = not old.autosave }

--------------------------------------------------------------------------------
-- Exporting

msgExportCode = Msg "Export Code" identity

msgExportSvg = Msg "Export SVG" identity

--------------------------------------------------------------------------------
-- Importing

msgImportCode = Msg "Import Code" <| closeDialogBox ImportCode

msgAskImportCode = requireSaveAsker msgImportCode


--------------------------------------------------------------------------------
-- Deuce Interactions

resetDeuceState m =
  let layoutOffsets = m.layoutOffsets in
  { m | deuceState = emptyDeuceState
      , deuceToolsAndResults = DeuceTools.createToolCache initModel
      , deuceToolResultPreviews = Dict.empty
      , selectedDeuceTool = Nothing
      , preview = Nothing
      , layoutOffsets =
          { layoutOffsets |
              deuceToolBox =
                { pinned = layoutOffsets.deuceToolBox.pinned
                , offsets = if layoutOffsets.deuceToolBox.pinned
                            then layoutOffsets.deuceToolBox.offsets
                            else {dx=0, dy=0}
                }
          }
      }

msgMouseEnterCodeBox = Msg "Mouse Enter CodeBox" <| \m ->
  let codeBoxInfo = m.codeBoxInfo in
  { m | hoveringCodeBox = True}

msgMouseLeaveCodeBox = Msg "Mouse Leave CodeBox" <| \m ->
  let codeBoxInfo = m.codeBoxInfo in
  { m | hoveringCodeBox = False }

toggleDeuceWidget : DeuceWidget -> Model -> Model
toggleDeuceWidget widget model =
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
        oldSelectedWidgets
        |> List.filter (\w -> not (isSubWidget model.inputExp w widget || isSubWidget model.inputExp widget w)) -- Remove any overlapping widgets.
        |> multipleTargetPositionsFilter
        |> Utils.addAsSet widget
    newSelectedWidgetsEmpty =
      List.isEmpty newSelectedWidgets
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
    { almostNewModel
        | deuceToolsAndResults =
            DeuceTools.createToolCache almostNewModel
        , deuceToolResultPreviews =
            Dict.empty
    } |> DeuceTools.reselectDeuceTool

msgMouseClickDeuceWidget widget =
  Msg ("msgMouseClickDeuceWidget " ++ toString widget) <| \old ->
    toggleDeuceWidget widget old

msgMouseEnterDeuceWidget widget = Msg ("msgMouseEnterDeuceWidget " ++ toString widget) <| \old ->
  let deuceState = old.deuceState in
  { old | deuceState =
              { deuceState
              | hoveredWidgets = [widget] } }

msgMouseLeaveDeuceWidget widget = Msg ("msgMouseLeaveDeuceWidget " ++ toString widget) <| \old ->
  let deuceState = old.deuceState in
  { old | deuceState =
              { deuceState
              | hoveredWidgets = [] } }

msgChooseDeuceExp name exp = Msg ("Choose Deuce Exp \"" ++ name ++ "\"") <| \m ->
  let
    modifiedHistory =
      modelModify m.code m.deuceState.selectedWidgets m.history
    modelWithCorrectHistory =
      case modifiedHistory of
        Just h ->
          { m | history = h }

        Nothing ->
          m
  in
    -- TODO version of tryRun/upstateRun starting with parsed expression
    upstateRun ( { modelWithCorrectHistory | code = Syntax.unparser m.syntax exp })

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
          Print _ ->
            Print (LangSvg.printSvg shown old.slate)
          _ ->
            old.outputMode
    in
      { old | showGhosts = shown
            , outputMode = newMode
      }

--------------------------------------------------------------------------------
-- Deuce Tools

msgHoverDeuceResult : Bool -> SynthesisResult -> List Int -> Msg
msgHoverDeuceResult isRenamer (SynthesisResult result) path =
  let maybeRunToolAndCachePreview m =
    case (isRenamer, Dict.get path m.deuceToolResultPreviews) of
      (True, _) -> -- TODO make renaming dynamically appear in the code
        m

      (False, Just (preview, _)) -> -- already run and cached
        { m | preview = preview }

      (False, Nothing) -> -- not already run and cached
        let
          (preview, class) =
            -- CSS classes from SleekView leak out here. Oh, well.
            case (result.isSafe, runAndResolve m result.exp) of
              (True, Ok (val, widgets, slate, code)) ->
                (Just (code, Ok (val, widgets, slate)), "expected-safe")
              (True, Err err) ->
                let _ = Debug.log "not safe after all!" err in
                (Just (Syntax.unparser m.syntax result.exp, Err err), "unexpected-unsafe")
              (False, Ok (val, widgets, slate, code)) ->
                (Just (code, Ok (val, widgets, slate)), "unexpected-safe")
              (False, Err err) ->
                (Just (Syntax.unparser m.syntax result.exp, Err err), "expected-unsafe")

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
    (setHoveredMenuPath path >> maybeRunToolAndCachePreview)

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
                    | renameVarTextBox =
                        FastParser.sanitizeVariableName text
                }
        }
    in
      { almostNewModel
          | deuceToolsAndResults =
              DeuceTools.updateRenameToolsInCache almostNewModel
          , deuceToolResultPreviews =
              Dict.empty
      } |> DeuceTools.reselectDeuceTool

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
          SleekLayout.mainResizerLeftBound old
        rightBound =
          SleekLayout.mainResizerRightBound old
        oldMainResizerX =
          (SleekLayout.mainResizer old).x
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
