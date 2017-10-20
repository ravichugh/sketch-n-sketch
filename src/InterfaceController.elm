port module InterfaceController exposing
  ( update
  , msgNoop
  , msgWindowDimensions
  , msgVisibilityChange
  , msgCodeUpdate
  , msgKeyPress, msgKeyDown, msgKeyUp
  , msgMouseIsDown, msgMousePosition
  , msgRun, upstateRun, msgTryParseRun
  , msgAceUpdate
  , msgUserHasTyped
  , msgUndo, msgRedo, msgCleanCode
  , msgDigHole, msgMakeEqual, msgRelate, msgIndexedRelate, msgBuildAbstraction
  , msgSelectSynthesisResult, msgClearSynthesisResults
  , msgStartAutoSynthesis, msgStopAutoSynthesisAndClear
  , msgHoverSynthesisResult, msgPreview, msgClearPreview
  , msgActivateRenameInOutput, msgUpdateRenameInOutputTextBox, msgDoRename
  , msgGroupBlobs, msgDuplicate, msgMergeBlobs, msgAbstractBlobs
  , msgReplicateBlob
  , msgToggleCodeBox, msgToggleOutput
  , msgSetOutputLive, msgSetOutputPrint
  , msgSetHeuristicsBiased, msgSetHeuristicsNone, msgSetHeuristicsFair
  , msgStartAnimation, msgRedraw, msgTickDelta
  , msgNextSlide, msgPreviousSlide
  , msgNextMovie, msgPreviousMovie
  , msgPauseResumeMovie
  , msgOpenDialogBox, msgCloseDialogBox
  , msgUpdateFilenameInput
  , msgConfirmWrite, msgConfirmDelete
  , msgReadFile, msgReadFileFromInput, msgUpdateFileIndex
  , msgLoadIcon
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
  )

import Updatable exposing (Updatable)
import Lang exposing (..) --For access to what makes up the Vals
import Types
import Ace
import FastParser exposing (parseE, freshen, showError)
import LangUnparser exposing (unparse)
import LangTools
import LangSimplify
import ValueBasedTransform
import Blobs exposing (..)
import Draw
import ExpressionBasedTransform as ETransform
import Sync
import Eval
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
import AnimationLoop
import FileHandler
import DeucePopupPanelInfo exposing (DeucePopupPanelInfo)
import ColorScheme
-- import InterfaceStorage exposing (installSaveState, removeDialog)
import LangSvg
import ShapeWidgets exposing (Feature(..), FeatureNum(..), RealZone(..), PointFeature(..), OtherFeature(..))
import ExamplesGenerated as Examples
import Config exposing (params)
import Either exposing (Either(..))
import DefaultIconTheme
import DependenceGraph exposing (lookupIdent)
import CodeMotion
import DeuceWidgets exposing (..) -- TODO
import DeuceTools
import ColorNum

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

refreshMode model e =
  case mkLive_ model.syncOptions model.slideNumber model.movieNumber model.movieTime e of
    Ok liveInfo ->
      liveInfo

    Err s ->
      let _ = Debug.log "refreshMode Error" (toString s) in
      Live { initSubstPlus = FastParser.substPlusOf e
           , triggers = Dict.empty
           }


refreshMode_ model = refreshMode model model.inputExp

-- TODO refresh type highlights, too
refreshHighlights zoneKey model =
  let codeBoxInfo = model.codeBoxInfo in
  let hi = liveInfoToHighlights zoneKey model in
  { model | codeBoxInfo = { codeBoxInfo | highlights = hi } }

-- may want to eventually have a maximum history length
addToHistory currentCode h =
  let (past, _) = h in
  case past of
    [] ->
      (currentCode::past, [])

    last::older ->
      -- trimRight to tolerate differences in newlines at the end
      if String.trimRight currentCode == String.trimRight last
      then h
      else (currentCode::past, [])


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
    let reparsedResult = unparse exp |> parseE |> (Result.mapError showError) in
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
    ELet ws1 lk rec p1 e1 e2 ws2 ->
      replaceE__ exp (ELet ws1 lk rec p1 e1 (rewriteInnerMostExpToMain e2) ws2)
    _ ->
      eLets [("main", exp)] (eVar "main")


--------------------------------------------------------------------------------
-- Mouse Events

onMouseClick click old maybeClickable =
  let (isOnCanvas, (canvasX, canvasY) as pointOnCanvas) = clickToCanvasPoint old click in
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
      let pointToAdd =
        case maybeClickable of
          -- TODO revisit with better provenance smarts
          Just (PointWithProvenance (snapX, snapXTr) xVal (snapY, snapYTr) yVal) ->
            let (Provenance snapXEnv snapXExp snapXBasedOn) = xVal.provenance in
            let (Provenance snapYEnv snapYExp snapYBasedOn) = yVal.provenance in
            -- TODO static check
            if snapXExp.val.eid > 0 && snapYExp.val.eid > 0 then
              ((round snapX, SnapEId snapXExp.val.eid), (round snapY, SnapEId snapYExp.val.eid))
            else
              ((canvasX, NoSnap), (canvasY, NoSnap))
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
        _ -> Debug.crash "invalid state, points shoudl be NoPointsYet or PolyPoints for polygon tool"

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


onClickPrimaryZone : LangSvg.NodeId -> LangSvg.ShapeKind -> ShapeWidgets.ZoneName -> Model -> Model
onClickPrimaryZone i k z old =
  let realZone = ShapeWidgets.parseZone z in
  let hoveredCrosshairs_ =
    case ShapeWidgets.zoneToCrosshair k realZone of
      Just (xFeature, yFeature) ->
        Set.insert (i, xFeature, yFeature) old.hoveredCrosshairs
      _ ->
        old.hoveredCrosshairs
  in
  let (selectedFeatures_, selectedShapes_, selectedBlobs_) =
    if i < -2 then -- Clicked a widget
      if z == "Offset1D" then
        let update = if Set.member (i,"offset") old.selectedFeatures then Set.remove else Set.insert in
        (update (i,"offset") old.selectedFeatures, old.selectedShapes, old.selectedBlobs)
      else
        (old.selectedFeatures, old.selectedShapes, old.selectedBlobs)
    else
      let selectThisShape () =
        if old.keysDown == Keys.shift
        then Utils.toggleSet i old.selectedShapes
        else Set.singleton i
      in
      let selectBlob blobId =
        if old.keysDown == Keys.shift then
          if Dict.member blobId old.selectedBlobs
          then Dict.remove blobId old.selectedBlobs
          else Dict.insert blobId i old.selectedBlobs
        else
          Dict.singleton blobId i
      in
      let maybeBlobId =
        case Dict.get i (Tuple.second old.slate) |> Maybe.map .interpreted of
          Just (LangSvg.SvgNode _ l _) -> LangSvg.maybeFindBlobId l
          _                            -> Debug.crash "onClickPrimaryZone"
      in
      case (k, realZone, maybeBlobId) of
        ("line", ZLineEdge, Just blobId) -> (old.selectedFeatures, selectThisShape (), selectBlob blobId)
        (_,      ZInterior, Just blobId) -> (old.selectedFeatures, selectThisShape (), selectBlob blobId)
        ("line", ZLineEdge, Nothing)     -> (old.selectedFeatures, selectThisShape (), old.selectedBlobs)
        (_,      ZInterior, Nothing)     -> (old.selectedFeatures, selectThisShape (), old.selectedBlobs)
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
      let dx = if old.keysDown == Keys.y then 0 else (mx - mx0) in
      let dy = if old.keysDown == Keys.x then 0 else (my - my0) in

      let (newExp, highlights) = trigger (mx0, my0) (dx, dy) in

      let codeBoxInfo_ =
        let codeBoxInfo = old.codeBoxInfo in
        { codeBoxInfo | highlights = highlights }
      in
      let dragInfo_ = (trigger, (mx0, my0), True) in

      Eval.run newExp |> Result.andThen (\(newVal, newWidgets) ->
      LangSvg.resolveToIndexedTree old.slideNumber old.movieNumber old.movieTime newVal |> Result.map (\newSlate ->
        let newCode = unparse newExp in
        { old | code = newCode
              , lastRunCode = newCode
              , inputExp = newExp
              , inputVal = newVal
              , slate = newSlate
              , widgets = newWidgets
              , codeBoxInfo = codeBoxInfo_
              , mouseMode = MouseDragZone zoneKey (Just dragInfo_)
              }
      )) |> handleError old

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
                  ShapeWidgets.featuresOfShape shapeKind shapeAttrs
                  |> List.filterMap
                      (\feature ->
                        case feature of
                          PointFeature pf -> Just (ShapeWidgets.featureNumsOfFeature feature |> List.map (ShapeWidgets.unparseFeatureNum (Just shapeKind)), ShapeWidgets.getPointEquations shapeKind shapeAttrs pf)
                          _               -> Nothing
                      )
                  |> List.concatMap
                      (\(shapeFeatureStrs, (xEqn, yEqn)) ->
                        case (ShapeWidgets.evaluateFeatureEquation xEqn, ShapeWidgets.evaluateFeatureEquation yEqn) of
                          (Just x, Just y) -> shapeFeatureStrs |> List.map (\shapeFeatureStr -> ((nodeId, shapeFeatureStr), (x, y)))
                          _                -> []
                      )
            )
      in
      let selectableWidgetFeaturesAndPositions =
        old.widgets
        |> Utils.zipi1
        |> List.concatMap
            (\(widgetI, widget) ->
              let idAsShape = -2 - widgetI in
              case widget of
                WIntSlider _ _ _ _ _ _ _ -> []
                WNumSlider _ _ _ _ _ _ _ -> []
                WPoint (x, xTr) _ (y, yTr) _ ->
                  [ ((idAsShape, ShapeWidgets.unparseFeatureNum (Just "point") (XFeat LonePoint)), (x, y))
                  , ((idAsShape, ShapeWidgets.unparseFeatureNum (Just "point") (YFeat LonePoint)), (x, y))
                  ]
                WOffset1D (baseX, baseXTr) (baseY, baseYTr) axis sign (amount, amountTr) _ _ _ ->
                  let (effectiveAmount, ((endX, endXTr), (endY, endYTr))) =
                    offsetWidget1DEffectiveAmountAndEndPoint ((baseX, baseXTr), (baseY, baseYTr)) axis sign (amount, amountTr)
                  in
                  [ ((idAsShape, ShapeWidgets.unparseFeatureNum (Just "offset") (XFeat EndPoint)), (endX, endY))
                  , ((idAsShape, ShapeWidgets.unparseFeatureNum (Just "offset") (YFeat EndPoint)), (endX, endY))
                  ]
                WCall _ _ _ _ -> []
            )
      in
      let shapesAndBounds =
        selectableShapeFeaturesAndPositions
        |> Utils.groupBy (\((nodeId, shapeFeatureStr), (x, y)) -> nodeId)
        |> Dict.toList
        |> List.map
            (\(nodeId, featuresAndPositions) ->
              let xs = featuresAndPositions |> List.map (\(_, (x, y)) -> x) in
              let ys = featuresAndPositions |> List.map (\(_, (x, y)) -> y) in
              let (left, right) = (List.minimum xs |> Maybe.withDefault -100000, List.maximum xs |> Maybe.withDefault -100000) in
              let (top, bot)    = (List.minimum ys |> Maybe.withDefault -100000, List.maximum ys |> Maybe.withDefault -100000) in
              (nodeId, (top, left, bot, right))
            )
      in
      let blobsAndBounds = [] in -- Ignore for now. Blobs are going to go bye-bye.
      let blobsToSelect = [] in  -- Ignore for now. Blobs are going to go bye-bye.
      let shapesToSelect =
        shapesAndBounds
        |> List.filter (\(nodeId, (top, left, bot, right)) -> selectLeft <= left && right <= selectRight && selectTop <= top && bot <= selectBot)
        |> List.map    (\(nodeId, _) -> nodeId)
      in
      let featuresToSelect =
        selectableShapeFeaturesAndPositions ++ selectableWidgetFeaturesAndPositions
        |> List.filter (\((nodeId, shapeFeatureStr), (x, y)) -> not (List.member nodeId shapesToSelect) && selectLeft <= round x && round x <= selectRight && selectTop <= round y && round y <= selectBot)
        |> List.map    (\(selectableShapeFeature,    (x, y)) -> selectableShapeFeature)
      in
      if old.keysDown == Keys.shift then
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

        (_, NoPointsYet) ->
          let pointOnCanvas     = (old.keysDown, (mx, my)) in
          let lastPointOnCanvas = (old.keysDown, (mxLast, myLast)) in
          { old | mouseMode = MouseDrawNew (TwoPoints pointOnCanvas lastPointOnCanvas) }

        (_, TwoPoints _ point1) ->
          let pointOnCanvas = (old.keysDown, (mx, my)) in
          { old | mouseMode = MouseDrawNew (TwoPoints pointOnCanvas point1) }

        (_, Offset1DFromExisting _ _ basePoint) ->
          let ((effectiveMX, effectiveMY), snap) =
            let ((x0,_), (y0,_)) = basePoint in
            let (dxRaw, dyRaw) = (mx - round x0, my - round y0) in
            -- Hmm, shoudn't assume this here. Yolo.
            let (axis, sign, amount) = Draw.horizontalVerticalSnap (0, 0) (dxRaw, dyRaw) in
            let possibleSnaps =
              flattenExpTree old.inputExp
              |> List.filterMap (\e -> LangTools.expToMaybeNum e |> Maybe.map (\n -> (round n, e.val.eid)))
              |> List.filter (\(n,_) -> n >= 2)
              |> List.sort
            in
            let pixelsPerSnap = 20 in
            let snapRanges =
              possibleSnaps
              |> Utils.mapi0
                  (\(i, (numberToSnapTo, eid))->
                    ((numberToSnapTo + pixelsPerSnap * i, numberToSnapTo + pixelsPerSnap * (i+1)), numberToSnapTo, eid)
                  )
              |> Debug.log "snapRanges"
            in
            let maybeInSnapRange = Utils.findFirst (\((low, high), _, _) -> amount >= low && amount < high) snapRanges in
            case maybeInSnapRange of
              Just (_, snapToNumber, eid) ->
                if      axis == X && sign == Positive then ((round x0 + snapToNumber, my), SnapEId eid)
                else if axis == X && sign == Negative then ((round x0 - snapToNumber, my), SnapEId eid)
                else if axis == Y && sign == Positive then ((mx, round y0 + snapToNumber), SnapEId eid)
                else                                       ((mx, round y0 - snapToNumber), SnapEId eid)
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
      old

onMouseUp old =
  case (old.mode, old.mouseMode) of

    (Print _, _) -> old

    (PrintScopeGraph _, _) -> old

    (_, MouseDragZone zoneKey (Just _)) ->
      let e = Utils.fromOkay "onMouseUp" <| parseE old.code in
      let old_ = { old | inputExp = e } in
      refreshHighlights zoneKey
        { old_ | mouseMode = MouseNothing, mode = refreshMode_ old_
               , history = addToHistory old.code old_.history }

    (_, MouseDrawNew points) ->
      let resetMouseMode model = { model | mouseMode = MouseNothing } in
      case (old.tool, points, old.keysDown == Keys.shift) of

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

        (Lambda i, TwoPoints pt2 pt1, _) -> upstateRun <| resetMouseMode <| Draw.addLambda i old pt2 pt1

        (Poly _, _, _) -> old
        (Path _, _, _) -> old

        (Text, TwoPoints pt2 pt1, _) -> upstateRun <| resetMouseMode <| Draw.addTextBox old pt2 pt1

        (PointOrOffset, TwoPoints (_, pt2) (_, (x1,y1)), _)         -> upstateRun <| resetMouseMode <| Draw.addOffsetAndMaybePoint old NoSnap ((toFloat x1, dummyTrace), (toFloat y1, dummyTrace)) pt2
        (PointOrOffset, Offset1DFromExisting pt2 snap basePoint, _) -> upstateRun <| resetMouseMode <| Draw.addOffsetAndMaybePoint old snap basePoint pt2

        (_, NoPointsYet, _)     -> switchToCursorTool old

        _              -> resetMouseMode old

    _ -> { old | mouseMode = MouseNothing, mode = refreshMode_ old }


tryRun : Model -> Result (Model, String, Maybe Ace.Annotation) Model
tryRun old =
  let
    oldWithUpdatedHistory =
      let
        updatedHistory =
          addToHistory old.code old.history
      in
        { old | history = updatedHistory }
  in
    case parseE old.code of
      Err err ->
        Err (oldWithUpdatedHistory, showError err, Nothing)
      Ok e ->
        let resultThunk () =
          -- let aceTypeInfo = Types.typecheck e in

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

          Eval.doEval Eval.initEnv rewrittenE |>
          Result.andThen (\((newVal,ws),finalEnv) ->
            LangSvg.fetchEverything old.slideNumber old.movieNumber 0.0 newVal
            |> Result.map (\(newSlideCount, newMovieCount, newMovieDuration, newMovieContinue, newSlate) ->
              let newCode = unparse e in -- unnecessary, if parse/unparse were inverses
              let lambdaTools_ =
                -- TODO should put program into Model
                -- TODO actually, ideally not. caching introduces bugs
                let program = splitExp e in
                Draw.lambdaToolOptionsOf program ++ initModel.lambdaTools
              in
              let new =
                loadLambdaToolIcons finalEnv { old | lambdaTools = lambdaTools_ }
              in
              let new_ =
                { new | inputExp      = e
                      , inputVal      = newVal
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
                      , history       = addToHistory newCode old.history
                      , caption       = Nothing
                      , syncOptions   = Sync.syncOptionsOf old.syncOptions e
                      , lambdaTools   = lambdaTools_
                      , errorBox      = Nothing
                      , scopeGraph    = DependenceGraph.compute e
                      , preview       = Nothing
                      , synthesisResults = maybeRunAutoSynthesis old e
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
              resetDeuceState <|
              { new_ | mode = refreshMode_ new_
                     , codeBoxInfo = updateCodeBoxInfo taskProgressAnnotation new_
                     }
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
upstate (Msg caption updateModel) old =
  let _ = Debug.log caption (old.mouseMode, old.mouseState) in
  let _ = debugLog "Msg" caption in
  updateModel old

--------------------------------------------------------------------------------
-- Hooks to be run after every message

hooks : List (Model -> Model -> (Model, Cmd Msg))
hooks =
  [ handleSavedSelectionsHook
  -- , handleOutputSelectionChanges
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

-- This function is not used.
handleOutputSelectionChanges : Model -> Model -> (Model, Cmd Msg)
handleOutputSelectionChanges oldModel newModel =
  if oldModel.selectedFeatures /= newModel.selectedFeatures || oldModel.selectedShapes /= newModel.selectedShapes || oldModel.selectedBlobs /= newModel.selectedBlobs then
    let
      selectedEIdInterpretations = ShapeWidgets.selectionsProximalDistalEIdInterpretations newModel.inputExp newModel.slate newModel.widgets newModel.selectedFeatures newModel.selectedShapes newModel.selectedBlobs
      deuceWidgetInterpretations = selectedEIdInterpretations |> List.map (List.map DeuceExp)
      finalModel = { newModel | deuceToolsAndResults = DeuceTools.createToolCacheMultipleInterpretations newModel deuceWidgetInterpretations }
    in
    (finalModel, Cmd.none)
  else
    (newModel, Cmd.none)

port doFocusJustShownRenameBox : () -> Cmd msg

focusJustShownRenameBox : Model -> Model -> (Model, Cmd Msg)
focusJustShownRenameBox oldModel newModel =
  if oldModel.renamingInOutput == Nothing && newModel.renamingInOutput /= Nothing then
    (newModel, doFocusJustShownRenameBox ())
  else
    (newModel, Cmd.none)

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
issueCommand (Msg kind _) oldModel newModel =
  case kind of
    "Toggle Code Box" ->
      if newModel.basicCodeBox
        then Cmd.none
        else AceCodeBox.initializeAndDisplay newModel
          -- TODO crash: "Uncaught Error: ace.edit can't find div #editor"

    "Save As" ->
      if newModel.filename /= Model.bufferName then
        FileHandler.write <| getFile newModel
      else
        Cmd.none

    "Save" ->
      if newModel.filename /= Model.bufferName then
        FileHandler.write <| getFile newModel
      else
        FileHandler.requestFileIndex ()

    "Confirm Write" ->
      Cmd.batch <| iconCommand newModel.filename

    "Open" ->
      FileHandler.requestFile newModel.filename

    "Delete" ->
      FileHandler.delete newModel.fileToDelete

    "Confirm Delete" ->
      Cmd.batch <| iconCommand newModel.fileToDelete

    "Export Code" ->
      FileHandler.download
        { filename = (Model.prettyFilename newModel) ++ ".little"
        , text = newModel.code
        }

    "Export SVG" ->
      FileHandler.download
        { filename = (Model.prettyFilename newModel) ++ ".svg"
        , text = LangSvg.printSvg newModel.showGhosts newModel.slate
        }

    "Import Code" ->
      FileHandler.requestFileFromInput Model.importCodeFileInputId

    -- Do not send changes back to the editor, because this is the command where
    -- we receieve changes (if this is removed, an infinite feedback loop
    -- occurs).
    "Ace Update" ->
        if newModel.autosave && newModel.needsSave then
          FileHandler.write <| getFile newModel
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
            kind == "Mouse Leave CodeBox"
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
          else if kind == "Toggle Output" && newModel.mode == PrintScopeGraph Nothing then
            DependenceGraph.render newModel.scopeGraph
          else if newModel.runAnimation then
            AnimationLoop.requestFrame ()
          else
            Cmd.none
        , if String.startsWith "New" kind then
            AceCodeBox.resetScroll newModel
          else
            Cmd.none
        , if String.startsWith "msgMouseClickDeuceWidget" kind then
            DeucePopupPanelInfo.requestDeucePopupPanelInfo ()
          else
            Cmd.none
        , if String.startsWith "Open Dialog Box" kind then
            FileHandler.requestFileIndex ()
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
      String.dropLeft 6 filename -- __ui__
  in
    if List.member potentialIconName Model.iconNames then
      [ FileHandler.requestIcon potentialIconName ]
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

msgUndo = Msg "Undo" <| \old ->
  case old.history of
    ([], _) ->
      old
    ([firstRun], _) ->
      old
    (lastRun::secondToLast::older, future) ->
      let
        new =
          { old
              | history =
                  (secondToLast::older, lastRun::future)
              , code =
                  secondToLast
          }
            |> Model.hideDeuceRightClickMenu
            |> resetDeuceState
      in
        upstateRun new

msgRedo = Msg "Redo" <| \old ->
  case old.history of
    (_, []) ->
      old
    (past, next::future) ->
      let
        new =
          { old
              | history =
                  (next::past, future)
              , code =
                  next
          }
            |> Model.hideDeuceRightClickMenu
            |> resetDeuceState
      in
        upstateRun new

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
  in
    Msg ("MousePosition " ++ toString pos_) <|
      mouseStateUpdater >> deucePopupPanelPositionUpdater

--------------------------------------------------------------------------------

refreshInputExp : Model -> Model
refreshInputExp old =
  let
    parseResult =
      parseE old.code
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
        else if keyCode == Keys.keyBackspace then
          deleteInOutput old
        else if keyCode == Keys.keyD && List.any Keys.isCommandKey old.keysDown then
          doDuplicate old
        else if List.any Keys.isCommandKey old.keysDown then
          old
        {-
        else if List.member Keys.keyMeta old.keysDown then
          -- when keyMeta is down and another key k is downed,
          -- there will not be a key up event for k. so, not putting
          -- k in keysDown. if want to handle keyMeta + k, keep track
          -- of this another way.
          old
        -}
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
  { old | keysDown = Utils.removeFirst keyCode old.keysDown }

--------------------------------------------------------------------------------

cleanSynthesisResult (SynthesisResult {description, exp, isSafe, sortKey, children}) =
  SynthesisResult <|
    { description = description ++ " → Cleaned"
    , exp         = LangSimplify.cleanCode exp
    , isSafe      = isSafe
    , sortKey     = sortKey
    , children    = children
    }

cleanDedupSortSynthesisResults synthesisResults =
  synthesisResults
  -- |> List.map cleanSynthesisResult
  |> Utils.dedupBy (\(SynthesisResult {description, exp, sortKey, children}) -> unparse exp)
  |> List.sortBy (\(SynthesisResult {description, exp, sortKey, children}) -> (LangTools.nodeCount exp, sortKey, description))

maybeRunAutoSynthesis m e =
  if m.autoSynthesis
    then cleanDedupSortSynthesisResults (ETransform.passiveSynthesisSearch e)
    else []

msgCleanCode = Msg "Clean Code" <| \old ->
  case parseE old.code of
    Err err ->
      { old | caption = Just (LangError (showError err)) }
    Ok reparsed ->
      let cleanedExp = LangSimplify.cleanCode reparsed in
      let code_ = unparse cleanedExp in
      if old.code == code_ then old
      else
        let _ = debugLog "Cleaned: " code_ in
        upstateRun { old | inputExp = cleanedExp, code = code_ }

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
            , history          = addToHistory newCode old.history
            , slate            = newSlate
            , widgets          = newWidgets
            , preview          = Nothing
              -- we already ran it successfully once so it shouldn't crash the second time
            , mode             = Utils.fromOk "DigHole MkLive" <|
                                   mkLive old.syncOptions
                                     old.slideNumber old.movieNumber old.movieTime reparsed
                                     (newVal, newWidgets)
      }
  )

msgMakeEqual = Msg "Make Equal" <| \old ->
  let synthesisResults =
    ValueBasedTransform.makeEqual
        old.inputExp
        old.selectedFeatures
        old.slideNumber
        old.movieNumber
        old.movieTime
        old.syncOptions
  in
  { old | synthesisResults = cleanDedupSortSynthesisResults synthesisResults }

msgRelate = Msg "Relate" <| \old ->
  let synthesisResults =
    ValueBasedTransform.relate
        old.inputExp
        old.selectedFeatures
        old.slideNumber
        old.movieNumber
        old.movieTime
        old.syncOptions
  in
  { old | synthesisResults = cleanDedupSortSynthesisResults synthesisResults }

msgIndexedRelate = Msg "Indexed Relate" <| \old ->
  let synthesisResults =
    ValueBasedTransform.indexedRelate
        old.inputExp
        old.selectedFeatures
        old.selectedShapes
        old.slideNumber
        old.movieNumber
        old.movieTime
        old.syncOptions
  in
  { old | synthesisResults = cleanDedupSortSynthesisResults synthesisResults }

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
        old.inputExp
        old.selectedFeatures
        old.selectedShapes
        old.selectedBlobs
        old.slideNumber
        old.movieNumber
        old.movieTime
        old.syncOptions
  in
  { old | synthesisResults = cleanDedupSortSynthesisResults synthesisResults }

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
                        case heads |> Utils.findi (eidIs eidToDelete) of
                          Just iToDelete -> program |> replaceExpNodeE__ parent (EList ws1 (Utils.removei iToDelete heads |> imitateExpListWhitespace heads) ws2 maybeTail ws3)
                          Nothing ->
                            if Maybe.map (eidIs eidToDelete) maybeTail == Just True
                            then program |> replaceExpNodeE__ parent (EList ws1 heads ws2 Nothing ws3)
                            else program

                      _ ->
                        let _ = Utils.log <| "can't remove from parent " ++ LangUnparser.unparse parent in
                        program

                  _ ->
                    let _ = Utils.log <| "can't find parent to remove from" in
                    program
          in
          -- This seems to remove too much (e.g. will remove function if an application is deleted).
          -- let varEIdsPerhapsRemoved = LangTools.freeVars expToDelete |> List.map (.val >> .eid) |> Set.fromList in
          let varEIdsPerhapsRemoved =
            case LangTools.expToMaybeVar (LangTools.expValueExp expToDelete) of
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
    deleteResult::_ -> upstateRun <| clearSelections { old | code = unparse deleteResult }

--------------------------------------------------------------------------------

msgSelectSynthesisResult newExp = Msg "Select Synthesis Result" <| \old ->
  -- TODO unparse gets called twice, here and in runWith ...
  let newCode = unparse newExp in
  let new =
    { old | code = newCode
          , lastRunCode = newCode
          , history = addToHistory newCode old.history
          , synthesisResults = []
          }
  in
  runWithErrorHandling new newExp (\reparsed newVal newWidgets newSlate newCode ->
    -- debugLog "new model" <|
      let newer =
      { new | inputExp         = reparsed -- newExp
            , inputVal         = newVal
            , slate            = newSlate
            , widgets          = newWidgets
            , preview          = Nothing
            , synthesisResults = maybeRunAutoSynthesis old reparsed
      } |> clearSelections
      in
      { newer | mode = refreshMode_ newer
              , codeBoxInfo = updateCodeBoxInfo Types.dummyAceTypeInfo newer
              }
  )

clearSynthesisResults : Model -> Model
clearSynthesisResults old =
  { old | preview = Nothing, synthesisResults = [] }

setAutoSynthesis : Bool -> Model -> Model
setAutoSynthesis shouldUseAutoSynthesis old =
  { old | autoSynthesis = shouldUseAutoSynthesis }

msgClearSynthesisResults : Msg
msgClearSynthesisResults =
  Msg "Clear Synthesis Results" clearSynthesisResults

msgStartAutoSynthesis : Msg
msgStartAutoSynthesis =
  Msg "Start Auto Synthesis" (setAutoSynthesis True)

msgStopAutoSynthesisAndClear : Msg
msgStopAutoSynthesisAndClear =
  Msg "Stop Auto Synthesis and Clear" <|
    clearSynthesisResults << (setAutoSynthesis False)

--------------------------------------------------------------------------------

msgGroupBlobs = Msg "Group Blobs" <| \old ->
    case Blobs.isSimpleProgram old.inputExp of
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
          (not << isVar << LangTools.expValueExp) -- Don't simply duplicate a variable usage.

    maybeNewProgram =
      singleExpressionInterpretations
      |> List.concat
      |> List.map (LangTools.outerSameValueExpByEId old.inputExp >> .val >> .eid)
      |> Utils.dedup
      |> Debug.log "possible eids to duplicate"
      |> List.map (LangTools.justFindExpByEId old.inputExp)
      |> List.sortBy LangTools.expToLocation -- Choose earliest single expression in program.
      |> List.head -- No multiple synthesis options for now.
      |> Maybe.map
          (\expToDuplicate ->
            let name = LangTools.expNameForExp old.inputExp expToDuplicate |> LangTools.removeTrailingDigits in
            -- Attempt 1: Try to add to output as a shape.
            let newProgram = Draw.addShape old name expToDuplicate in
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
  |> Maybe.map (\newProgram -> { old | code = unparse newProgram } |> clearSelections |> upstateRun)
  |> Maybe.withDefault old

msgMergeBlobs = Msg "Merge Blobs" <| \old ->
  if Dict.size old.selectedBlobs <= 1 then old
  else upstateRun <| ETransform.mergeSelectedBlobs old

msgAbstractBlobs = Msg "Abstract Blobs" <| \old ->
  upstateRun <| ETransform.abstractSelectedBlobs old

msgReplicateBlob option = Msg "Replicate Blob" <| \old ->
  case Blobs.isSimpleProgram old.inputExp of
    Nothing     -> old
    Just simple -> upstateRun <| ETransform.replicateSelectedBlob option old simple

--------------------------------------------------------------------------------

msgToggleCodeBox = Msg "Toggle Code Box" <| \old ->
  { old | basicCodeBox = not old.basicCodeBox }

msgSetOutputLive = Msg "Set Output Live" <| \old ->
  { old | mode = refreshMode_ old }

msgSetOutputPrint = Msg "Set Output Print" <| \old ->
  { old | mode = Print (LangSvg.printSvg old.showGhosts old.slate) }

msgToggleOutput = Msg "Toggle Output" <| \old ->
  let m = case old.mode of
    Live _  -> Print (LangSvg.printSvg old.showGhosts old.slate)
    Print _ -> let showScopeGraph = False in
               if showScopeGraph
                 then PrintScopeGraph Nothing
                 else refreshMode_ old
    PrintScopeGraph _ -> refreshMode_ old
  in
  { old | mode = m }

updateHeuristics : Int -> Model -> Model
updateHeuristics heuristic old =
  let
    oldSyncOptions =
      old.syncOptions
    newSyncOptions =
      { oldSyncOptions | feelingLucky = heuristic }
  in
    case old.mode of
      Live _ ->
        case mkLive_
               newSyncOptions
               old.slideNumber
               old.movieNumber
               old.movieTime
               old.inputExp of
          Ok m ->
            { old | syncOptions = newSyncOptions, mode = m }
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
  case LangSvg.fetchEverything old.slideNumber old.movieNumber old.movieTime old.inputVal of
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
      Eval.run old.inputExp |>
      Result.andThen (\(previousVal, _) ->
        LangSvg.resolveToMovieCount previousSlideNumber previousVal
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

showExpPreview old exp =
  let previewCode = unparse exp in
  case runAndResolve old exp of
    Ok (val, widgets, slate, _) ->
      { old | preview = Just (previewCode, Ok (val, widgets, slate)) }

    Err s ->
      { old | preview = Just (previewCode, Err s) }

msgSelectOption (exp, val, slate, code) = Msg "Select Option..." <| \old ->
  { old | code          = code
        , inputExp      = exp
        , inputVal      = val
        , history       = addToHistory code old.history
        , slate         = slate
        , preview       = Nothing
        , synthesisResults = []
        , tool          = Cursor
        , mode          = Utils.fromOk "SelectOption mkLive" <|
                            mkLive old.syncOptions old.slideNumber old.movieNumber old.movieTime exp
                              (val, []) -- TODO
        }

msgHoverSynthesisResult pathByIndices = Msg "Hover SynthesisResult" <| \old ->
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
  case maybeFindResult pathByIndices old.synthesisResults of
    Just (SynthesisResult {description, exp, sortKey, children}) ->
      let newModel = { old | hoveredSynthesisResultPathByIndices = pathByIndices } in
      let newModel2 =
        case (old.autoSynthesis, children) of
          (_, Just _)  -> newModel -- Children already computed.
          (False, _)   -> newModel -- Don't compute children if auto-synth off
          _            ->
            -- Compute child results.
            let childResults = cleanDedupSortSynthesisResults (ETransform.passiveSynthesisSearch exp) in
            let newTopLevelResults = setResultChildren pathByIndices childResults old.synthesisResults in
            { newModel | synthesisResults = newTopLevelResults
                       , hoveredSynthesisResultPathByIndices = pathByIndices }
      in
      showExpPreview newModel2 exp

    Nothing ->
      { old | preview = Nothing
            , hoveredSynthesisResultPathByIndices = [] }


msgPreview expOrCode = Msg "Preview" <| \old ->
  let previewExp =
    case expOrCode of
      Left exp   -> exp
      Right code -> Utils.fromOkay "msgPreview" (parseE code)
  in
  showExpPreview old previewExp

msgClearPreview = Msg "Clear Preview" <| \old ->
  { old | preview = Nothing }

msgCancelSync = Msg "Cancel Sync" <| \old ->
  upstateRun
    { old | mode = refreshMode_ old }

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
          |> List.filter isResultSafe
          |> List.head
          |> Maybe.map resultExp
          |> Maybe.withDefault old.inputExp
        in
        { old | code             = unparse newProgram
              , renamingInOutput = Nothing
        } |> upstateRun
      else
        old

    Nothing ->
      old

--------------------------------------------------------------------------------

requireSaveAsker ((Msg name _) as msg) needsSave =
  if needsSave then
    Msg ("Ask " ++ name) <| (\old ->
      { old | pendingFileOperation = Just <| msg
            , fileOperationConfirmed = False })
        >> Model.openDialogBox AlertSave
  else
    msg

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

confirmWrite savedFilename old =
  { old | needsSave = False
        , lastSaveState = Just old.code }

confirmDelete deletedFilename = identity

requestFile requestedFilename old =
  { old | filename = requestedFilename }

readFile file old =
  { old | filename = file.filename
        , code = file.code
        , history = ([file.code], [])
        , lastSaveState = Just file.code
        , needsSave = False }

loadIcon env icon old =
  let
    actualCode =
      if icon.code /= "" then
        icon.code
      else
        case Dict.get icon.iconName DefaultIconTheme.icons of
          Just c ->
            c
          Nothing ->
            "(blobs [])"
    oldIcons =
      old.icons
    iconHtml =
      iconify env actualCode
    newIcons =
      Dict.insert icon.iconName iconHtml oldIcons
  in
    { old | icons = newIcons }

-- for basic icons, env will be Eval.initEnv.
-- for LambdaTool icons, env will be from result of running main program.
iconify env code =
  let
    exp =
      Utils.fromOkay "Error parsing icon"
        <| parseE code
    ((val, _), _) =
      Utils.fromOkay "Error evaluating icon"
        <| Eval.doEval env exp
    slate =
      Utils.fromOkay "Error resolving index tree of icon"
        <| LangSvg.resolveToIndexedTree 1 1 0 val
    svgElements =
      LangSvg.buildSvgSimple slate
    subPadding x =
      x - 10
  in
    Svg.svg
      [ Svg.Attributes.width <|
          (SleekLayout.px << subPadding << .width) SleekLayout.iconButton
      , Svg.Attributes.height <|
          (SleekLayout.px << subPadding << .height) SleekLayout.iconButton
      ]
      [ svgElements ]


loadLambdaToolIcons finalEnv old =
  let foo tool acc =
    let icon = lambdaToolIcon tool in
    if Dict.member icon.iconName old.icons
      then acc
      else loadIcon finalEnv icon old
  in
  List.foldl foo old old.lambdaTools

readFileFromInput file old =
  { old | filename = file.filename
        , code = file.code
        , history = ([file.code], [])
        , lastSaveState = Nothing
        , needsSave = True }

updateFileIndex fileIndex old =
  { old | fileIndex = fileIndex }

-- Subscription Handlers

msgConfirmWrite savedFilename =
  Msg "Confirm Write" <| confirmWrite savedFilename

msgConfirmDelete deletedFilename =
  Msg "Confirm Delete" <| confirmDelete deletedFilename

-- TODO: clear state (e.g. selectedEIds) after read file

msgReadFile file =
  Msg "Read File" <| readFile file >> upstateRun

msgLoadIcon file =
  Msg "Load Icon" <| loadIcon Eval.initEnv file

msgReadFileFromInput file =
  Msg "Read File From Input" <| readFileFromInput file >> upstateRun

msgUpdateFileIndex fileIndex =
  Msg "Update File Index" <| updateFileIndex fileIndex

--------------------------------------------------------------------------------
-- File Operations

msgNew template = Msg "New" (handleNew template)

handleNew template = (\old ->
  case Utils.maybeFind template Examples.list of
    Nothing -> let _ = Debug.log "WARN: not found:" template in old
    Just (_, thunk) ->
      let {e,v,ws,ati} = thunk () in
      let so = Sync.syncOptionsOf old.syncOptions e in
      let m =
        Utils.fromOk "SelectExample mkLive_" <|
          mkLive so old.slideNumber old.movieNumber old.movieTime e (v,ws)
      in
      LangSvg.fetchEverything old.slideNumber old.movieNumber old.movieTime v
      |> Result.map (\(slideCount, movieCount, movieDuration, movieContinue, slate) ->
        let code = unparse e in
        { initModel | inputExp      = e
                    , inputVal      = v
                    , code          = code
                    , lastRunCode   = code
                    , history       = ([code],[])
                    , mode          = m
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
                    , filename      = Model.bufferName
                    , needsSave     = True
                    , lastSaveState = Nothing
                    , scopeGraph    = DependenceGraph.compute e

                    , lastSelectedTemplate = Just template

                    , dimensions    = old.dimensions
                    , syncOptions   = old.syncOptions
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
      ) |> handleError old) >> closeDialogBox New

msgAskNew template = requireSaveAsker (msgNew template)

msgSaveAs =
  let
    switchFilenameToInput old =
      { old | filename = old.filenameInput }
    closeDialogBoxIfNecessary old =
      if old.filename /= Model.bufferName then
        Model.closeDialogBox SaveAs old
      else
        old
  in
    Msg "Save As" (switchFilenameToInput >> closeDialogBoxIfNecessary)

msgSave = Msg "Save" <| \old ->
  if old.filename == Model.bufferName then
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
      , deuceToolsAndResults =
          DeuceTools.createToolCache initModel
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
    deuceToolsAndResults =
      DeuceTools.createToolCache almostNewModel
  in
    { almostNewModel
        | deuceToolsAndResults =
            deuceToolsAndResults
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
  -- TODO version of tryRun/upstateRun starting with parsed expression
  upstateRun (resetDeuceState { m | code = unparse exp })

--------------------------------------------------------------------------------
-- DOT

msgReceiveDotImage s = Msg "Receive Image" <| \m ->
  { m | mode = Model.PrintScopeGraph (Just s) }

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
        case old.mode of
          Print _ ->
            Print (LangSvg.printSvg shown old.slate)
          _ ->
            old.mode
    in
      { old | showGhosts = shown
            , mode = newMode
      }

--------------------------------------------------------------------------------
-- Deuce Tools

msgHoverDeuceResult : String -> List Int -> Maybe Preview -> Msg
msgHoverDeuceResult name path maybePreview =
  Msg ("Hover Deuce Result \"" ++ name ++ "\" " ++ toString path) <|
    setHoveredMenuPath path >>
      case maybePreview of
        Just preview ->
          \m -> { m | preview = preview }
        Nothing ->
          identity

msgLeaveDeuceResult : String -> List Int -> Maybe Preview -> Msg
msgLeaveDeuceResult name path maybePreview =
  Msg ("Leave Deuce Result \"" ++ name ++ "\" " ++ toString path) <|
    clearHoveredMenuPath >>
      case maybePreview of
        Just preview ->
          \m -> { m | preview = Nothing }
        Nothing ->
          identity

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
      deuceToolsAndResults =
        DeuceTools.updateRenameToolsInCache almostNewModel
    in
      { almostNewModel
          | deuceToolsAndResults =
              deuceToolsAndResults
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
