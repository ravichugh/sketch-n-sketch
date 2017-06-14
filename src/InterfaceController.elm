module InterfaceController exposing
  ( update
  , msgNoop
  , msgWindowDimensions
  , msgCodeUpdate
  , msgKeyPress, msgKeyDown, msgKeyUp
  , msgMouseIsDown, msgMousePosition
  , msgRun, upstateRun, msgTryParseRun
  , msgAceUpdate
  , msgUndo, msgRedo, msgCleanCode
  , msgDigHole, msgMakeEqual, msgRelate, msgIndexedRelate
  , msgSelectSynthesisResult, msgClearSynthesisResults
  , msgHoverSynthesisResult, msgPreview, msgClearPreview
  , msgGroupBlobs, msgDuplicateBlobs, msgMergeBlobs, msgAbstractBlobs
  , msgReplicateBlob
  , msgToggleCodeBox, msgToggleOutput
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
  , msgMouseClickCodeBox
  , msgReceiveDotImage
  , msgMoveExp
  , msgMouseClickDeuceWidget
  , msgMouseEnterDeuceWidget, msgMouseLeaveDeuceWidget
  , contextSensitiveDeuceTools, msgChooseDeuceExp
  )

import Lang exposing (..) --For access to what makes up the Vals
import Types
import Ace
import FastParser exposing (parseE, freshen)
import LangUnparser exposing (unparse)
import LangTools
import LangSimplify
import ValueBasedTransform
import Blobs exposing (..)
import Draw
import ExpressionBasedTransform as ETransform
import Sync
import Eval
import Utils exposing (maybePluralize)
import Keys
import InterfaceModel as Model exposing (..)
import Layout exposing (clickToCanvasPoint)
import AceCodeBox
import AnimationLoop
import FileHandler
-- import InterfaceStorage exposing (installSaveState, removeDialog)
import LangSvg
import ShapeWidgets exposing (RealZone(..), PointFeature(..), OtherFeature(..))
import ExamplesGenerated as Examples
import Config exposing (params)
import Either exposing (Either(..))
import Canvas
import DefaultIconTheme
import DependenceGraph exposing (lookupIdent)
import CodeMotion
import DeuceWidgets exposing (..) -- TODO
import ColorNum

import VirtualDom

--Core Libraries
import List
import Dict exposing (Dict)
import Set
import String
import Char

--Html Libraries
import Html
import Html.Attributes as Attr
import Html.Events as Events

--Svg Libraries
import Svg
import Svg.Attributes
import Svg.Events
import Svg.Lazy

--Error Checking Libraries
import Debug

--------------------------------------------------------------------------------

debugLog = Config.debugLog Config.debugController

--------------------------------------------------------------------------------

refreshMode model e =
  Utils.fromOk "refreshMode" <|
    mkLive_ model.syncOptions model.slideNumber model.movieNumber model.movieTime e

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
      if currentCode == last
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

-- We may want to revisit our error handling so we don't have different error types floating around.
discardErrorAnnotations : Result (String, Ace.Annotation) a -> Result String a
discardErrorAnnotations result =
  result |> Result.mapError (\(string, annot) -> string)

runWithErrorHandling model exp onOk =
  let result =
    -- runWithErrorHandling is called after synthesis. Recompute line numbers.
    -- TODO PARSER_ERROR_HANDLING
    let reparsedResult = unparse exp |> parseE |> (Result.mapError toString) in
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
    ELet ws1 lk rec p1 e1 e2 ws2 ->
      replaceE__ exp (ELet ws1 lk rec p1 e1 (rewriteInnerMostExpToMain e2) ws2)
    _ ->
      eLets [("main", exp)] (eVar "main")


--------------------------------------------------------------------------------
-- Mouse Events

onMouseClick click old =
  case (old.tool, old.mouseMode) of

    -- Inactive zone
    (Cursor, MouseDragZone (i, k, z) Nothing) ->
      onClickPrimaryZone i k z { old | mouseMode = MouseNothing }

    -- Active zone but not dragged
    (Cursor, MouseDragZone (i, k, z) (Just (_, _, False))) ->
      onClickPrimaryZone i k z { old | mouseMode = MouseNothing }

    (Poly stk, MouseDrawNew points) ->
      let pointOnCanvas = clickToCanvasPoint old click in
      let add () =
        let points_ = (old.keysDown, pointOnCanvas) :: points in
        { old | mouseMode = MouseDrawNew points_ }
      in
      if points == [] then add ()
      else
        let (_,initialPoint) = Utils.last_ points in
        if Utils.distanceInt pointOnCanvas initialPoint > Draw.drawDotSize then add ()
        else if List.length points == 2 then { old | mouseMode = MouseNothing }
        else if List.length points == 1 then switchToCursorTool old
        else upstateRun <| Draw.addPolygon stk old points

    (Path stk, MouseDrawNew points) ->
      let pointOnCanvas = clickToCanvasPoint old click in
      let add new =
        let points_ = (old.keysDown, new) :: points in
        (points_, { old | mouseMode = MouseDrawNew points_ })
      in
      case points of
        [] -> Tuple.second (add pointOnCanvas)
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
              upstateRun <| Draw.addPath stk old_ points_
            else
              Tuple.second (add pointOnCanvas)

    (HelperDot, MouseDrawNew []) ->
      let pointOnCanvas = (old.keysDown, clickToCanvasPoint old click) in
      { old | mouseMode = MouseDrawNew [pointOnCanvas] }

    (_, MouseDrawNew []) -> switchToCursorTool old

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
        Set.insert i <|
          if old.keysDown == Keys.shift
          then old.selectedShapes
          else Set.empty
      in
      let selectBlob blobId =
        Dict.insert blobId i <|
          if old.keysDown == Keys.shift
          then old.selectedBlobs
          else Dict.empty
      in
      let maybeBlobId =
        case Dict.get i (Tuple.second old.slate) of
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

onMouseMove newPosition old =
  let (mx0, my0) = (newPosition.x, newPosition.y) in
  let (mx, my) = clickToCanvasPoint old newPosition in
  case old.mouseMode of

    MouseNothing -> old

    MouseDragLayoutWidget f ->
      f (mx0, my0) old

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

    MouseDrawNew points ->
      case (old.tool, points) of
        (Poly _, _) -> old -- handled by onMouseClick instead
        (Path _, _) -> old -- handled by onMouseClick instead
        (_, []) ->
          let pointOnCanvas = (old.keysDown, (mx, my)) in
          { old | mouseMode = MouseDrawNew [pointOnCanvas, pointOnCanvas] }
        (_, (_::points)) ->
          let pointOnCanvas = (old.keysDown, (mx, my)) in
          { old | mouseMode = MouseDrawNew (pointOnCanvas::points) }

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
      case (old.tool, points, old.keysDown == Keys.shift) of

        (Line _,     [pt2, pt1], _) -> upstateRun <| Draw.addLine old pt2 pt1
        (HelperLine, [pt2, pt1], _) -> upstateRun <| Draw.addLine old pt2 pt1

        (Rect Raw,      [pt2, pt1], False) -> upstateRun <| Draw.addRawRect old pt2 pt1
        (Rect Raw,      [pt2, pt1], True)  -> upstateRun <| Draw.addRawSquare old pt2 pt1
        (Rect Stretchy, [pt2, pt1], False) -> upstateRun <| Draw.addStretchyRect old pt2 pt1
        (Rect Stretchy, [pt2, pt1], True)  -> upstateRun <| Draw.addStretchySquare old pt2 pt1

        (Oval Raw,      [pt2, pt1], False) -> upstateRun <| Draw.addRawOval old pt2 pt1
        (Oval Raw,      [pt2, pt1], True)  -> upstateRun <| Draw.addRawCircle old pt2 pt1
        (Oval Stretchy, [pt2, pt1], False) -> upstateRun <| Draw.addStretchyOval old pt2 pt1
        (Oval Stretchy, [pt2, pt1], True)  -> upstateRun <| Draw.addStretchyCircle old pt2 pt1

        (HelperDot, [pt], _) -> upstateRun <| Draw.addHelperDot old pt

        (Lambda i, [pt2, pt1], _) -> upstateRun <| Draw.addLambda i old pt2 pt1

        (Poly _, _, _) -> old
        (Path _, _, _) -> old

        (Text, [pt2, pt1], _) -> upstateRun <| Draw.addTextBox old pt2 pt1

        (_, [], _)     -> switchToCursorTool old

        _              -> old

    (_, MouseDownInCodebox downPos) ->
      let oldPos = pixelToRowColPosition downPos old in
      let newPos = pixelToRowColPosition (Tuple.second old.mouseState) old in
      onMouseDrag (dragSource oldPos old) (dragTarget newPos old)
        { old | mouseMode = MouseNothing }

    _ -> { old | mouseMode = MouseNothing, mode = refreshMode_ old }

dragSource pixelPos m =
  -- TODO: Allow dragging and scrolling (store touched item in MouseDownInCodebox rather than initial position)
  -- TODO: Allow selection of ECase patterns
  let maybePathedPatId = getClickedPat (findPats m.inputExp) pixelPos m in
  let maybeEId   = getClickedEId (computeExpRanges m.inputExp) pixelPos in
  -- case Debug.log "source maybeEId, source maybePathedPatId" (maybeEId, maybePathedPatId) of
  case (maybeEId, maybePathedPatId) of
    (Nothing, Just ppid) -> Just (Left ppid)
    (Just eid, _)        -> Just (Right eid)
    _                    -> Nothing

dragTarget pixelPos m =
  let expTarget = getClickedExpTarget (computeExpTargets m.inputExp) pixelPos in
  let patTarget = getClickedPatTarget (findPatTargets m.inputExp) pixelPos m in
  let target = case List.head expTarget of
                Nothing       -> case List.head patTarget of
                                    Nothing         -> Nothing
                                    Just firstPat   -> Just (PatTargetPosition firstPat)
                Just etarget  -> Just (ExpTargetPosition etarget) in
  target

tryRun : Model -> Result (String, Maybe Ace.Annotation) Model
tryRun old =
  case parseE old.code of
    -- TODO PARSER_ERROR_HANDLING
    Err err -> Err (toString err, Nothing)
    Ok e ->
      let result =
        -- let aceTypeInfo = Types.typecheck e in
        let aceTypeInfo = Types.dummyAceTypeInfo in

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
            resetDeuceState <|
            { new_ | mode = refreshMode_ new_
                   , codeBoxInfo = updateCodeBoxInfo aceTypeInfo new_
                   }
          )
        )
      in
      case result of
        Err s    -> Err (s, Nothing)
        Ok model -> Ok model


--------------------------------------------------------------------------------
-- Updating the Model

-- 1. Compute a "pure" update (only the newModel), and then
-- 2. Decide whether to issue a command based on simple predicates
--    (name of Msg, and simple comparison of oldModel and newModel).
--
update : Msg -> Model -> (Model, Cmd Msg)
update msg oldModel =
  case (oldModel.pendingFileOperation, oldModel.fileOperationConfirmed) of
    (Just msg2, True) ->
      update
        msg2
        { oldModel | pendingFileOperation = Nothing
                   , fileOperationConfirmed = False }
    _ ->
      let newModel = upstate msg oldModel in
      let cmd = issueCommand msg oldModel newModel in
      (newModel, cmd)


upstate : Msg -> Model -> Model
upstate (Msg caption updateModel) old =
  let _ = debugLog "Msg" caption in
  updateModel old


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

    "Open Dialog Box" ->
      FileHandler.requestFileIndex ()

    _ ->
      if kind == "Update Font Size" then
        AceCodeBox.updateFontSize newModel
      else
      if newModel.code /= oldModel.code ||
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

msgRun = Msg "Run" <| \old -> upstateRun old

msgAceUpdate aceCodeBoxInfo = Msg "Ace Update" <| \old ->
    if old.preview /= Nothing then
      old
    else
      let isSame = old.lastSaveState == (Just aceCodeBoxInfo.code) in
      { old | code = aceCodeBoxInfo.code
            , codeBoxInfo = aceCodeBoxInfo.codeBoxInfo
            , needsSave = not isSame }

upstateRun old =
  case tryRun old of
    Err (err, Just annot) ->
      { old | errorBox = Just err
            , codeBoxInfo = updateCodeBoxWithParseError annot old.codeBoxInfo }
    Err (err, Nothing) ->
      { old | errorBox = Just err }
    Ok newModel -> newModel

msgTryParseRun newModel = Msg "Try Parse Run" <| \old ->
  case tryRun newModel of
    Err (err, Just annot) ->
      { old | caption = Just (LangError err)
            , codeBoxInfo = updateCodeBoxWithParseError annot old.codeBoxInfo }
    Err (err, Nothing) ->
      { old | caption = Just (LangError err) }
    Ok modelAfterRun ->
      modelAfterRun

--------------------------------------------------------------------------------

msgUndo = Msg "Undo" <| \old ->
  case old.history of
    ([],_)         -> old
    ([firstRun],_) -> old
    (lastRun::secondToLast::older, future) ->
      let new = { old | history = (secondToLast::older, lastRun::future)
                      , code    = secondToLast } in
      upstateRun new

msgRedo = Msg "Redo" <| \old ->
  case old.history of
    (_,[]) -> old
    (past, next::future) ->
      let new = { old | history = (next::past, future)
                      , code    = next } in
      upstateRun new

--------------------------------------------------------------------------------

msgMouseIsDown b = Msg ("MouseIsDown " ++ toString b) <| \old ->
  let new =
    let {x,y} = Tuple.second old.mouseState in
    let lightestColor = 470 in
    { old | randomColor = (old.randomColor + x + y) % lightestColor }
  in
  case (b, new.mouseState) of

    (True, (Nothing, pos)) -> -- mouse down
      let _ = debugLog "mouse down" () in
      if old.hoveringCodeBox
      -- TODO disabling MouseDownInCodeBox because onMouseDrag is disabled
{-
      then { new | mouseState = (Just False, pos),
                   mouseMode = MouseDownInCodebox pos }
-}
      then { new | mouseState = (Just False, pos) }
      else { new | mouseState = (Just False, pos) }

    (False, (Just False, pos)) -> -- click (mouse up after not being dragged)
      let _ = debugLog "mouse click" () in
      onMouseClick pos { new | mouseState = (Nothing, pos) }

    (False, (Just True, pos)) -> -- mouse up (after being dragged)
      let _ = debugLog "mouse up" () in
      onMouseUp { new | mouseState = (Nothing, pos) }

    (False, (Nothing, _)) ->
      let _ = debugLog "mouse down was preempted by a handler in View" () in
      new

    -- (True, (Just _, _)) -> Debug.crash "upstate MouseIsDown: impossible"
    (True, (Just _, _)) ->
      let _ = Debug.log "upstate MouseIsDown: impossible" () in
      new

msgMousePosition pos_ = Msg ("MousePosition " ++ toString pos_) <| \old ->
  case old.mouseState of
    (Nothing, _)    -> { old | mouseState = (Nothing, pos_) }
    (Just False, _) -> onMouseMove pos_ { old | mouseState = (Just True, pos_) }
    (Just True, _)  -> onMouseMove pos_ { old | mouseState = (Just True, pos_) }

--------------------------------------------------------------------------------

msgKeyPress keyCode = Msg ("Key Press " ++ toString keyCode) <| \old ->
  old

msgKeyDown keyCode = Msg ("Key Down " ++ toString keyCode) <| \old ->
  -- let _ = Debug.log "Key Down" (keyCode, old.keysDown) in
  if [keyCode] == Keys.escape then
    let new = resetDeuceState old in
    case (old.tool, old.mouseMode) of
      (Cursor, _) ->
        { new | selectedFeatures = Set.empty
              , selectedShapes = Set.empty
              , selectedBlobs = Dict.empty
              }
      (_, MouseNothing)   -> { new | tool = Cursor }
      (_, MouseDrawNew _) -> { new | mouseMode = MouseNothing }
      _                   -> new
  else if keyCode == Keys.keyMeta then
    old
    -- for now, no need to ever put keyMeta in keysDown
    -- TODO need to put keyMeta in model, so know not to put the next key in model
{-
  else if List.member Keys.keyMeta old.keysDown then
    -- when keyMeta is down and another key k is downed,
    -- there will not be a key up event for k. so, not putting
    -- k in keysDown. if want to handle keyMeta + k, keep track
    -- of this another way.
    old
-}
  else if not (List.member keyCode old.keysDown) then
    { old | keysDown = keyCode :: old.keysDown }
  else
    old

msgKeyUp keyCode = Msg ("Key Up " ++ toString keyCode) <| \old ->
  -- let _ = Debug.log "Key Up" (keyCode, old.keysDown) in
  { old | keysDown = Utils.removeFirst keyCode old.keysDown }

--------------------------------------------------------------------------------

cleanSynthesisResult (SynthesisResult {description, exp, isSafe, sortKey, children}) =
  SynthesisResult <|
    { description = description ++ " -> Cleaned"
    , exp         = LangSimplify.cleanCode exp
    , isSafe      = isSafe
    , sortKey     = sortKey
    , children    = children
    }

cleanDedupSortSynthesisResults synthesisResults =
  synthesisResults
  |> List.map cleanSynthesisResult
  |> Utils.dedupBy (\(SynthesisResult {description, exp, sortKey, children}) -> unparse exp)
  |> List.sortBy (\(SynthesisResult {description, exp, sortKey, children}) -> (LangTools.nodeCount exp, sortKey, description))

maybeRunAutoSynthesis m e =
  if m.autoSynthesis
    then cleanDedupSortSynthesisResults (ETransform.passiveSynthesisSearch e)
    else []

msgCleanCode = Msg "Clean Code" <| \old ->
  case parseE old.code of
    -- TODO PARSER_ERROR_HANDLING
    Err err ->
      { old | caption = Just (LangError ("PARSE ERROR!\n" ++ toString err)) }
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
            , selectedFeatures = Set.empty
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
            , selectedFeatures = Set.empty
            , synthesisResults = maybeRunAutoSynthesis old reparsed
      }
      in
      { newer | mode = refreshMode_ newer
              , codeBoxInfo = updateCodeBoxInfo Types.dummyAceTypeInfo newer
              }
  )

msgClearSynthesisResults = Msg "Clear Synthesis Results" <| \old ->
  { old | preview = Nothing, synthesisResults = [] }

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

msgDuplicateBlobs = Msg "Duplicate Blobs" <| \old ->
  upstateRun <| ETransform.duplicateSelectedBlobs old

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
      [i]   -> Utils.maybeGet0 i results
      i::is -> Utils.maybeGet0 i results |> Maybe.andThen (\(SynthesisResult {children}) -> children |> Maybe.andThen (maybeFindResult is))
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
    { old | mode = Utils.fromOk "CancelSync mkLive_" <|
              mkLive_ old.syncOptions old.slideNumber old.movieNumber old.movieTime old.inputExp }

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
  Msg "Open Dialog Box" <| Model.openDialogBox db

msgCloseDialogBox db =
  Msg "Close Dialog Box" <| Model.closeDialogBox db

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
    iconNameLower =
      String.toLower icon.iconName
    actualCode =
      if icon.code /= "" then
        icon.code
      else
        case Dict.get iconNameLower DefaultIconTheme.icons of
          Just c ->
            c
          Nothing ->
            "(blobs [])"
    oldIcons =
      old.icons
    iconHtml =
      Canvas.iconify env actualCode
    newIcons =
      Dict.insert icon.iconName iconHtml oldIcons
  in
    { old | icons = newIcons }

loadLambdaToolIcons finalEnv old =
  let foo tool acc =
    let icon = lambdaToolIcon tool in
    if Dict.member (String.toLower icon.iconName) old.icons
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

msgNew template = Msg "New" <| (\old ->
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
                    , dimensions    = old.dimensions
                    , syncOptions   = old.syncOptions
                    , localSaves    = old.localSaves
                    , basicCodeBox  = old.basicCodeBox
                    , randomColor   = old.randomColor
                    , layoutOffsets = old.layoutOffsets
                    , fileIndex     = old.fileIndex
                    , icons         = old.icons
                    , scopeGraph    = DependenceGraph.compute e
                    , deuceState    = DeuceWidgets.emptyDeuceState
                    }
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

msgCancelFileOperation = Msg "Cancel File Operation" <| (\old ->
  { old | pendingFileOperation = Nothing
        , fileOperationConfirmed = False })
    >> Model.closeDialogBox AlertSave

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

msgMouseEnterCodeBox = Msg "Mouse Enter CodeBox" <| \m ->
  let codeBoxInfo = m.codeBoxInfo in
  { m | hoveringCodeBox = True}

msgMouseLeaveCodeBox = Msg "Mouse Leave CodeBox" <| \m ->
  let codeBoxInfo = m.codeBoxInfo in
  { m | hoveringCodeBox = False }

msgMouseClickCodeBox = Msg "Mouse Click CodeBox" <| \m -> m
  --let _ = Debug.log "selectedEIds" m.selectedEIds in
  --let _ = Debug.log "selectedPats" m.selectedPats in
  --let _ = Debug.log "selectedExpTargets" m.selectedExpTargets in
  --let _ = Debug.log "selectedPatTargets" m.selectedPatTargets in

  --if showDeuceWidgets m
  --then
  --  let downPos = case m.mouseMode of
  --                  MouseDownInCodebox downPos -> downPos
  --                  _                          -> { x = 0 , y = 0} in
  --  let pos = case m.mouseState of
  --              (Nothing, _) -> downPos
  --              (_, p)       -> p  in
  --  let codeBoxInfo = m.codeBoxInfo in
  --  let mousePos = case m.mouseState of
  --                  (b, pos) -> pos in
  --  let pixelPos = pixelToRowColPosition mousePos m in
  --  let selectedEIds =
  --    case getClickedEId (computeExpRanges m.inputExp) pixelPos of
  --      Nothing  -> m.selectedEIds
  --      Just eid -> if Set.member eid m.selectedEIds
  --                  then Set.remove eid m.selectedEIds
  --                  else Set.insert eid m.selectedEIds
  --  in
  --  let selectedExpTargets =
  --    case getClickedExpTarget (computeExpTargets m.inputExp) pixelPos of
  --      [] -> m.selectedExpTargets
  --      ls -> getSetMembers ls m.selectedExpTargets
  --  in
  --  let selectedPats =
  --    case getClickedPat (findPats m.inputExp) pixelPos m of
  --      Nothing  -> m.selectedPats
  --      Just s -> if Set.member s m.selectedPats
  --                  then Set.remove s m.selectedPats
  --                  else Set.insert s m.selectedPats
  --  in
  --  let selectedPatTargets =
  --    case getClickedPatTarget (findPatTargets m.inputExp) pixelPos m of
  --      [] -> m.selectedPatTargets
  --      ls -> getSetMembers ls m.selectedPatTargets
  --  in
  --  let new = { m | --selectedEIds = selectedEIds
  --                --selectedPats = selectedPats
  --                selectedPatTargets = selectedPatTargets
  --                , selectedExpTargets = selectedExpTargets }
  --  in
  --  { new | --expSelectionBoxes = expRangeSelections new
  --        expTargetSelections = expTargetsToSelect new
  --        --, patSelectionBoxes = patRangeSelections new
  --        , patTargetSelections = patTargetsToSelect new
  --        }
  --else
  --  m

msgMouseClickDeuceWidget id = Msg ("msgMouseClickDeuceWidget " ++ toString id) <| \m ->
  if showDeuceWidgets m
  then
    let selectedWidgets =
        if List.member id m.deuceState.selectedWidgets
          then Utils.removeAsSet id m.deuceState.selectedWidgets
          else Utils.addAsSet id m.deuceState.selectedWidgets
    in
    let deuceState = m.deuceState in
    { m | deuceState =
            { deuceState
            | selectedWidgets = selectedWidgets } }
  else
    m

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

getSetMembers ls s =
  case ls of
    [] -> s
    first::rest -> if Set.member first s
                  then Set.remove first (getSetMembers rest s)
                  else Set.insert first (getSetMembers rest s)

getClickedEId ls pixelPos =
  let selected =
    List.filter (\(exp,eid,start,end,selectStart,selectEnd) -> betweenPos selectStart pixelPos selectEnd) ls
  in
  case selected of
    []                    -> Nothing
    [(exp,eid,_,_,_,_)]   -> Just eid
    _                     -> let _ = Debug.log "WARN: getClickedEId: multiple eids" () in
                              Nothing

getClickedExpTarget ls pixelPos =
  let selected =
    List.filter (\(expTarget,selectStart,selectEnd) -> betweenPos selectStart pixelPos selectEnd) ls
  in
    List.map (\(expTarget,start,end) -> expTarget) selected

getClickedPat ls pixelPos m =
  let selected =
      List.filter (\(pat,ppid,start,end,selectEnd) -> betweenPos start pixelPos selectEnd) ls
  in
  case selected of
    []                     -> Nothing
    [(pat,ppid,s,e,se)]    -> Just ppid
    _                      -> let _ = Debug.log "WARN: getClickedPat: multiple pats" () in
                                Nothing

getClickedPatTarget ls pixelPos m =
  let selected =
    List.filter (\(tid,start,end) -> betweenPos start pixelPos end) ls
  in
    List.map (\(tid,start,end) -> tid) selected

msgReceiveDotImage s = Msg "Receive Image" <| \m ->
  { m | mode = Model.PrintScopeGraph (Just s) }

onMouseDrag
    : Maybe (Either PathedPatternId EId)
   -> Maybe TargetPosition
   -> Model -> Model
onMouseDrag dragSource dragTarget m =
  if showDeuceWidgets m
  then
    let _ = Debug.log "ignoring drag" (dragSource, dragTarget) in
    m
{-
    let new = resetDeuceState m in
    case Debug.log "source, target" (dragSource, dragTarget) of
      (Just (Left sourcePathedPatId), Just (ExpTargetPosition (Before, targetEId))) ->
        movePatBeforeEId sourcePathedPatId targetEId new
      (Just (Left sourcePathedPatId), Just (PatTargetPosition patTargetPosition)) ->
        movePatToPat sourcePathedPatId (patTargetPositionToTargetPathedPatId patTargetPosition) new
      _ ->
        new
-}
  else
    m


msgMoveExp = Msg "Move Exp" <| \m -> m
  --let selections =
  --  -- Debug.log "selections" <|
  --    { exps = Set.toList m.deuceState.selectedEIds
  --    , pats = Set.toList m.deuceState.selectedPats
  --    , patTargets = Set.toList m.deuceState.selectedPatTargets
  --    , expTargets = Set.toList m.deuceState.selectedExpTargets
  --    } in

  --let new = resetDeuceState m in
  --let bad () =
  --  let _ = Debug.log "bad selections" (selections) in
  --  new
  --in

  --let {pats, patTargets, expTargets} = selections in
  --case (pats, patTargets, expTargets) of

  --  ([sourcePat], [], [(0, targetId)]) ->
  --    movePatBeforeEId sourcePat targetId new

  --  ([sourcePat], targetPat :: targetPats, []) ->
  --    movePatToPat_ bad sourcePat targetPat targetPats new

  --  _ ->
  --    bad ()


-- movePatBeforeEId sourcePathedPatId targetEId model =
--   let (safeResults, unsafeResults) =
--     CodeMotion.moveDefinitionBeforeEId sourcePathedPatId targetEId model.inputExp
--   in
--   updateWithMoveExpResults model (safeResults, unsafeResults)
--
--
-- movePatToPat sourcePathedPatId targetPathedPatId model =
--   let (safeResults, unsafeResults) =
--     CodeMotion.moveDefinitionPat sourcePathedPatId targetPathedPatId model.inputExp
--   in
--   updateWithMoveExpResults model (safeResults, unsafeResults)


-- updateWithMoveExpResults model (safeResults, unsafeResults) =
--   -- If exactly one result and it is safe, apply the update to the code immediately.
--   -- Otherwise, place results in the synthesis results box.
--   case (safeResults, unsafeResults) of
--     ([SynthesisResult safeResult], []) ->
--       -- TODO version of upstateRun to avoid unparse then re-parse
--       let newCode = unparse safeResult.exp in
--       upstateRun { model | code = newCode, synthesisResults = [] }
--
--     (safeResults, unsafeResults) ->
--       { model | synthesisResults = safeResults ++ unsafeResults }


--------------------------------------------------------------------------------

-- TODO if/when drawing extra widgets for tools,
-- this all should move elsewhere (View, DeuceWidgets, ...)

contextSensitiveDeuceTools m =

  let {selectedWidgets} = m.deuceState in
  let nums = selectedNums m in
  let baseVals = selectedBaseVals m in
  let exps = selectedExps selectedWidgets in
  let pathedPatIds = selectedPats selectedWidgets in
  let letBindingEquations = selectedEquations selectedWidgets in
  let expTargets = selectedExpTargets selectedWidgets in
  let patTargets = selectedPatTargets selectedWidgets in
  let selections =
    (nums, baseVals, exps, pathedPatIds, letBindingEquations, expTargets, patTargets) in

  List.concat <|
    [ addToolMakeEqual m selections
    , addToolFlipBoolean m selections
    , addToolRenamePat m selections
    , addToolRenameVar m selections
    , addToolSwapNames m selections
    , addToolInlineDefintion m selections
    , addToolTwiddleShapes m selections
    , addToolIntroduceVar m selections
    , CodeMotion.addToolCompareSubExpressions m selections
    , addToolMoveDefinition m selections
    , addToolDuplicateDefinition m selections
    , addToolOneOrMoreNumsOnly thawOrFreezeTool m selections
    , addToolOneOrMoreNumsOnly showOrHideRangeTool m selections
    , addToolOneOrMoreNumsOnly addOrRemoveRangeTool m selections
    , addToolRewriteOffset m selections
    , addToolConvertColorString m selections
    , addToolAbstract m selections
    , addToolMerge m selections
    , addToolAddArg m selections
    , addToolRemoveArg m selections
    , addToolReorderFunctionArgs m selections
    , CodeMotion.addToolReorderEList m selections
    , addToolMakeSingleLine m selections
    , addToolMakeMultiLine m selections
    , addToolAlign m selections
    ]



msgChooseDeuceExp exp = Msg "Choose Deuce Exp" <| \m ->
  -- TODO version of tryRun/upstateRun starting with parsed expression
  upstateRun (resetDeuceState { m | code = unparse exp })

dummyDeuceTool m = \() ->
  [synthesisResult "not yet implemented" m.inputExp |> setResultSafe False]


--------------------------------------------------------------------------------

selectedNums : Model -> List (LocId, (WS, Num, Loc, WidgetDecl))
selectedNums = selectedNumsAndBaseVals >> Tuple.first

selectedBaseVals : Model -> List (EId, (WS, EBaseVal))
selectedBaseVals = selectedNumsAndBaseVals >> Tuple.second

selectedNumsAndBaseVals
    : Model
   -> ( List (LocId, (WS, Num, Loc, WidgetDecl))
      , List (EId, (WS, EBaseVal))
      )
selectedNumsAndBaseVals m =
  let noMatches = ([], []) in
  -- TODO may want to distinguish between different kinds of selected
  -- items earlier
  m.deuceState.selectedWidgets
  |> List.map (\deuceWidget ->
       case deuceWidget of
         DeuceExp eid ->
           case findExpByEId m.inputExp eid of
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

selectedExps deuceWidgets = flip List.concatMap deuceWidgets <| \deuceWidget ->
  case deuceWidget of
    DeuceExp x -> [x]
    _ -> []

selectedPats deuceWidgets = flip List.concatMap deuceWidgets <| \deuceWidget ->
  case deuceWidget of
    DeucePat x -> [x]
    _ -> []

selectedEquations deuceWidgets = flip List.concatMap deuceWidgets <| \deuceWidget ->
  case deuceWidget of
    DeuceLetBindingEquation x -> [x]
    _ -> []

selectedExpTargets deuceWidgets = flip List.concatMap deuceWidgets <| \deuceWidget ->
  case deuceWidget of
    DeuceExpTarget x -> [x]
    _ -> []

selectedPatTargets deuceWidgets = flip List.concatMap deuceWidgets <| \deuceWidget ->
  case deuceWidget of
    DeucePatTarget x -> [x]
    _ -> []


--------------------------------------------------------------------------------

addToolOneOrMoreNumsOnly func m selections = case selections of
  (nums, [], exps, [], [], [], []) ->
    if List.length nums >= 1 &&
       List.length nums == List.length exps
    then func m nums
    else []

  _ -> []


--------------------------------------------------------------------------------

addToolMakeEqual m selections = case selections of
  (nums, baseVals, exps, [], [], [], []) ->
    let literals = List.map Left nums ++ List.map Right baseVals in
    if List.length literals >= 2 &&
       List.length literals == List.length exps
    then CodeMotion.makeMakeEqualTool m literals
    else []

  _ -> []


--------------------------------------------------------------------------------

addToolAbstract m selections = case selections of
  ([], [], [], [pathedPatId], [], [], []) ->
    case LangTools.findScopeExpAndPat pathedPatId m.inputExp |> Maybe.map (\(e,p) -> (e.val.e__, p.val.p__)) of
      Just (ELet _ _ _ _ _ _ _, PVar _ ident _) ->
        [ ("Abstract", \() ->
            CodeMotion.abstractPVar pathedPatId m.inputExp
          ) ]

      _ -> []

  (_, _, [eid], [], [], [], []) ->
    let maybeExpToAbstract = findExpByEId m.inputExp eid in
    let expToAbstractParts = maybeExpToAbstract |> Maybe.map flattenExpTree |> Maybe.withDefault [] in
    let parameterCount =
      Utils.count (\e -> CodeMotion.shouldBeParameterIsConstant e m.inputExp) expToAbstractParts +
      Utils.count (\e -> CodeMotion.shouldBeParameterIsNamedUnfrozenConstant e m.inputExp) expToAbstractParts
    in
    let expSize = maybeExpToAbstract |> Maybe.map LangTools.nodeCount |> Maybe.withDefault 0 in
    if parameterCount > 0 && expSize >= 3 then
      [ ("Extract Function", \() -> -- Martin Fowler's name is "Extract Method" but this is FP
          CodeMotion.abstractExp eid m.inputExp
        ) ]
    else
      []

  ([], [], [], [], [letEId], [], []) ->
    case LangTools.justFindExpByEId m.inputExp letEId |> LangTools.expToMaybeLetPat |> Maybe.map (.val >> .p__) of
      Just (PVar _ _ _) ->
        [ ("Abstract", \() ->
            let pathedPatId = ((letEId, 1), []) in
            CodeMotion.abstractPVar pathedPatId m.inputExp
          ) ]

      _ -> []

  _ -> []


addToolMerge m selections =
  case selections of
    (_, _, eid1::eid2::restEIds, [], [], [], []) ->
      let eids = eid1::eid2::restEIds in
      let mergeResults =
        let candidateExpFilter e = List.member e.val.eid eids in
        let minCloneCount = List.length eids in
        ETransform.cloneEliminationSythesisResults candidateExpFilter minCloneCount 2 m.inputExp
      in
      if mergeResults /= []
      then [ ("Merge", always mergeResults) ]
      else []

    _ -> []


--------------------------------------------------------------------------------

-- Map a selected argument at a call site to the corresponding pathedPatId in the called function.
eidToMaybeCorrespondingArgumentPathedPatId : Exp -> EId -> Maybe PathedPatternId
eidToMaybeCorrespondingArgumentPathedPatId program targetEId =
  -- This should be more efficient than running the massive predicate over every expression in the program
  findWithAncestorsByEId program targetEId
  |> Maybe.withDefault []
  |> Utils.mapFirstSuccess
      (\exp ->
        case exp.val.e__ of
          EApp _ appFuncExp argExps _ ->
            case appFuncExp.val.e__ of
              EVar _ funcName ->
                case LangTools.resolveIdentifierToExp funcName appFuncExp.val.eid program of -- This is probably slow.
                  Just (LangTools.Bound funcExp) ->
                    case funcExp.val.e__ of
                      EFun _ fpats _ _ ->
                        -- Allow partial application
                        LangTools.tryMatchExpsPatsToPathsAtFunctionCall fpats argExps
                        |> Utils.mapFirstSuccess
                            (\(path, correspondingExp) ->
                              if correspondingExp.val.eid == targetEId
                              then Just ((funcExp.val.eid, 1), path)
                              else Nothing
                            )

                      _ -> Nothing

                  _ -> Nothing

              _ -> Nothing

          _ -> Nothing
      )


addToolAddArg m selections = case selections of
  -- (_, _, [], [], _, _, _) -> []

  (_, _, firstEId::restEIds, [], [], [], [patTarget]) ->
    let eids = firstEId::restEIds in
    let targetPathedPatId = patTargetPositionToTargetPathedPatId patTarget in
    let maybeScopeExp = findExpByEId m.inputExp (pathedPatIdToScopeEId targetPathedPatId) in
    case maybeScopeExp |> Maybe.map (.val >> .e__) of
      Just (EFun _ _ fbody _) ->
        let isAllSelectedExpsInFBody =
          eids |> List.all (\eid -> findExpByEId fbody eid /= Nothing)
        in
        if isAllSelectedExpsInFBody && List.length eids == 1 then
          [ ("Add Argument", \() -> -- Fowler calls this "Add Parameter"
              CodeMotion.addArg (Utils.head_ eids) targetPathedPatId m.inputExp
            ) ]
        else if isAllSelectedExpsInFBody then
          [ ("Add Arguments", \() -> -- Fowler calls this "Add Parameter"
              CodeMotion.addArgs eids targetPathedPatId m.inputExp
            ) ]
        else
          []

      _ -> []

  (_, _, [], firstArgSourcePathedPatId::restArgSourcePathedPatId, [], [], [patTarget]) ->
    let argSourcePathedPatIds = firstArgSourcePathedPatId::restArgSourcePathedPatId in
    let targetPathedPatId = patTargetPositionToTargetPathedPatId patTarget in
    let maybeScopeExp = findExpByEId m.inputExp (pathedPatIdToScopeEId targetPathedPatId) in
    case maybeScopeExp |> Maybe.map (.val >> .e__) of
      Just (EFun _ _ fbody _) ->
        let isAllSelectedPatsInFBody =
          argSourcePathedPatIds |> List.all (\argSourcePathedPatId -> LangTools.findScopeExpAndPat argSourcePathedPatId fbody /= Nothing)
        in
        if isAllSelectedPatsInFBody && List.length argSourcePathedPatIds == 1 then
          [ ("Add Argument", \() -> -- Fowler calls this "Add Parameter"
              CodeMotion.addArgFromPat (Utils.head_ argSourcePathedPatIds) targetPathedPatId m.inputExp
            ) ]
        else if isAllSelectedPatsInFBody then
          [ ("Add Arguments", \() -> -- Fowler calls this "Add Parameter"
              CodeMotion.addArgsFromPats argSourcePathedPatIds targetPathedPatId m.inputExp
            ) ]
        else
          []

      _ -> []

  _ -> []


addToolReorderFunctionArgs m selections = case selections of
  (_, _, [], [], _, _, _) -> []

  ([], [], [], pathedPatIds, [], [], [patTarget]) ->
    let targetPathedPatId = patTargetPositionToTargetPathedPatId patTarget in
    let allScopesSame = List.map pathedPatIdToScopeId (targetPathedPatId::pathedPatIds) |> Utils.allSame in
    case (allScopesSame, findExpByEId m.inputExp (pathedPatIdToScopeEId targetPathedPatId) |> Maybe.map (.val >> .e__)) of
      (True, Just (EFun _ _ _ _)) ->
        [ ("Reorder Arguments", \() ->
            CodeMotion.reorderFunctionArgs
                (pathedPatIdToScopeEId targetPathedPatId)
                (List.map pathedPatIdToPath pathedPatIds)
                (pathedPatIdToPath targetPathedPatId)
                m.inputExp
          ) ]

      _ -> []

  (_, _, eids, [], [], [(beforeAfter, eid)], []) ->
    case (eid::eids) |> List.map (eidToMaybeCorrespondingArgumentPathedPatId m.inputExp) |> Utils.projJusts of
      Just (targetReferencePathedPatId::pathedPatIds) ->
        let targetPathedPatId = patTargetPositionToTargetPathedPatId (beforeAfter, targetReferencePathedPatId) in
        let allScopesSame = List.map pathedPatIdToScopeId (targetPathedPatId::pathedPatIds) |> Utils.allSame in
        case (allScopesSame, findExpByEId m.inputExp (pathedPatIdToScopeEId targetPathedPatId) |> Maybe.map (.val >> .e__)) of
          (True, Just (EFun _ _ _ _)) ->
            [ ("Reorder Function Arguments", \() ->
                CodeMotion.reorderFunctionArgs
                    (pathedPatIdToScopeEId targetPathedPatId)
                    (List.map pathedPatIdToPath pathedPatIds)
                    (pathedPatIdToPath targetPathedPatId)
                    m.inputExp
              ) ]

          _ -> []

      _ -> []

  _ -> []


addToolRemoveArg m selections = case selections of
  (_, _, [], [], _, _, _) -> []

  ([], [], [], pathedPatIds, [], [], []) ->
    let isAllArgumentSelected =
      pathedPatIds
      |> List.all
          (\pathedPatId ->
            let scopeExp = findExpByEId m.inputExp (pathedPatIdToScopeEId pathedPatId) in
            case scopeExp |> Maybe.map (.val >> .e__) of
              Just (EFun _ _ _ _) -> True
              _                   -> False
          )
    in
    if isAllArgumentSelected && List.length pathedPatIds == 1 then
      [ ("Remove Argument", \() ->
          CodeMotion.removeArg (Utils.head_ pathedPatIds) m.inputExp
        ) ]
    else if isAllArgumentSelected then
      [ ("Remove Arguments", \() ->
          CodeMotion.removeArgs pathedPatIds m.inputExp
        ) ]
    else
      []

  (_, _, eids, [], [], [], []) ->
    case eids |> List.map (eidToMaybeCorrespondingArgumentPathedPatId m.inputExp) |> Utils.projJusts of
      Just [argPathedPatId] ->
        [ ("Remove Argument", \() ->
            CodeMotion.removeArg argPathedPatId m.inputExp
          ) ]

      Just argPathedPatIds->
        [ ("Remove Arguments", \() ->
            CodeMotion.removeArgs argPathedPatIds m.inputExp
          ) ]

      _ ->
        []

  _ -> []

--------------------------------------------------------------------------------

-- TODO: the following three functions
-- (thawOrFreezeTool, showOrHideRangeTool, addOrRemoveRangeTool)
-- could be factored...

thawOrFreezeTool m nums =

  let thawOrFreeze m nums =
    let freezeAnnotations = List.map (\(_,(_,_,(_,frzn,_),_)) -> frzn) nums in
    case Utils.dedup freezeAnnotations of

      [frzn] ->
        if m.syncOptions.thawedByDefault then
          if frzn == unann then Just ("Freeze", frozen)
          else if frzn == frozen then Just ("Thaw", unann)
          else if frzn == thawed then Just ("Freeze", frozen)
          else Nothing
        else
          if frzn == unann then Just ("Thaw", thawed)
          else if frzn == frozen then Just ("Thaw", thawed)
          else if frzn == thawed then Just ("Freeze", unann)
          else Nothing

      _ ->
        if m.syncOptions.thawedByDefault
        then Just ("Freeze", frozen)
        else Just ("Thaw", thawed)
  in

  case thawOrFreeze m nums of
    Nothing -> []
    Just (toolName, newAnnotation) ->
      [ (toolName, \() ->
          let eSubst =
             List.foldl
               (\(eId,(ws,n,(locid,_,x),wd)) acc ->
                 Dict.insert eId (EConst ws n (locid, newAnnotation, x) wd) acc
               )
               Dict.empty nums
          in
          [synthesisResult toolName (applyESubst eSubst m.inputExp)]
        )
      ]

showOrHideRangeTool m nums =
  let showOrHide m nums =
    let freezeAnnotations = flip List.map nums <| \(_,(_,_,_,wd)) ->
      case wd.val of
        IntSlider _ _ _ _ b -> Just b
        NumSlider _ _ _ _ b -> Just b
        _                   -> Nothing
    in
    case Utils.dedup freezeAnnotations of
      [Just b] -> Just b
      _        -> Nothing
  in
  case showOrHide m nums of
    Nothing -> []
    Just hidden ->
      let toolName =
        case (hidden, List.length nums) of
          (True, 1)  -> "Show Slider"
          (True, _)  -> "Show Sliders"
          (False, 1) -> "Hide Slider"
          (False, _) -> "Hide Sliders"
      in
      [ (toolName, \() ->
          let eSubst =
             List.foldl
               (\(eId,(ws,n,loc,wd)) acc ->
                 let wd_ =
                   case wd.val of
                     IntSlider a b c d _ -> IntSlider a b c d (not hidden)
                     NumSlider a b c d _ -> NumSlider a b c d (not hidden)
                     _                   -> wd.val
                 in
                 Dict.insert eId (EConst ws n loc { wd | val = wd_ }) acc
               )
               Dict.empty nums
          in
          [synthesisResult toolName (applyESubst eSubst m.inputExp)]
        )
      ]

addOrRemoveRangeTool m nums =
  let addOrRemove m nums =
    let freezeAnnotations = flip List.map nums <| \(_,(_,_,_,wd)) ->
      case wd.val of
        NoWidgetDecl -> True
        _            -> False
    in
    case Utils.dedup freezeAnnotations of
      [b] -> Just b
      _   -> Nothing
  in
  case addOrRemove m nums of
    Nothing -> []
    Just noRanges ->
      let toolName =
        case (noRanges, List.length nums) of
          (True, 1)  -> "Add Slider"
          (True, _)  -> "Add Sliders"
          (False, 1) -> "Remove Slider"
          (False, _) -> "Remove Sliders"
      in
      [ (toolName, \() ->
          let eSubst =
             List.foldl
               (\(eId,(ws,n,loc,_)) acc ->
                 let wd =
                   if noRanges then
                     if Utils.between n (0.001, 1) then
                       numSlider 0.001 1
                     else
                       let (i, j) = rangeAround n in
                       if toFloat (round n) == n then
                         intSlider (max 0 (round i)) (round j)
                       else
                         numSlider (max 0 i) j
                   else
                     withDummyRange NoWidgetDecl
                 in
                 Dict.insert eId (EConst ws n loc wd) acc
               )
               Dict.empty nums
          in
          [synthesisResult toolName (applyESubst eSubst m.inputExp)]
        )
      ]

rangeAround n =
  let i = n - 3 * n in
  let j = n + 3 * n in
  (i, j)


--------------------------------------------------------------------------------

addToolRenamePat m selections = case selections of
  ([], [], [], [pathedPatId], [], [], []) ->
    case LangTools.findPat pathedPatId m.inputExp |> Maybe.andThen LangTools.patToMaybeIdent of
      Just ident ->
        [ ("Rename " ++ ident, \() ->
            let newName = m.deuceState.renameVarTextBox in
            renamePat pathedPatId newName m.inputExp
          ) ]

      _ -> []

  _ -> []


composeTransformations : String -> List (Exp -> List SynthesisResult) -> Exp -> List SynthesisResult
composeTransformations finalCaption transformations originalProgram =
  transformations
  |> List.foldl
      (\transformation results ->
        results
        |> List.concatMap
            (\(SynthesisResult result) ->
              transformation (freshen result.exp) |> List.map (mapResultSafe ((&&) result.isSafe))
            )
      )
      [ synthesisResult "Original" originalProgram ]
  |> List.map (setResultDescription finalCaption)


addToolSwapNames m selections = case selections of
  ([], [], [], pathedPatId1::pathedPatId2::[], [], [], []) ->
    case [LangTools.findPat pathedPatId1 m.inputExp, LangTools.findPat pathedPatId2 m.inputExp] |> List.map (Maybe.andThen LangTools.patToMaybeIdent) of
      [Just name1, Just name2] ->
        [ ("Swap Names and Usages", \() ->
            m.inputExp
            |> composeTransformations ("Swap names " ++ name1 ++ " and " ++ name2)
                [ renamePat pathedPatId1 "IMPROBABLE_TEMPORARY_NAME_FOR_SAFETY_CHECK!!!"
                , renamePat pathedPatId2 name1
                , renamePat pathedPatId1 name2
                ]
          ),
          ("Swap Usages", \() ->
            swapUsages pathedPatId1 pathedPatId2 m.inputExp
          )
        ]

      _ -> []

  _ -> []


renamePat : PathedPatternId -> String -> Exp -> List SynthesisResult
renamePat (scopeId, path) newName program =
  case LangTools.findScopeExpAndPat (scopeId, path) program of
    Just (scopeExp, pat) ->
      case LangTools.patToMaybeIdent pat of
        Just oldName ->
          let scopeAreas = LangTools.findScopeAreas scopeId scopeExp in
          let oldUseEIds = List.concatMap (LangTools.identifierUses oldName) scopeAreas |> List.map (.val >> .eid) in
          let newScopeAreas = List.map (LangTools.renameVarUntilBound oldName newName) scopeAreas in
          let newUseEIds = List.concatMap (LangTools.identifierUses newName) newScopeAreas |> List.map (.val >> .eid) in
          let isSafe =
            oldUseEIds == newUseEIds && not (List.member newName (LangTools.identifiersListInPat pat))
          in
          let newScopeExp =
            let scopeAreasReplaced =
              newScopeAreas
              |> List.foldl
                  (\newScopeArea scopeExp -> replaceExpNode newScopeArea.val.eid newScopeArea scopeExp)
                  scopeExp
            in
            LangTools.setPatName (scopeId, path) newName scopeAreasReplaced
          in
          let newProgram = replaceExpNode newScopeExp.val.eid newScopeExp program in
          let result =
            synthesisResult ("Rename " ++ oldName ++ " to " ++ newName) newProgram |> setResultSafe isSafe
          in
          [result]

        Nothing ->
          []

    Nothing ->
      []


swapUsages : PathedPatternId -> PathedPatternId -> Exp -> List SynthesisResult
swapUsages (scopeId1, path1) (scopeId2, path2) originalProgram =
  case (LangTools.findScopeExpAndPat (scopeId1, path1) originalProgram, LangTools.findScopeExpAndPat (scopeId2, path2) originalProgram) of
    (Just (scopeExp1, pat1), Just (scopeExp2, pat2)) ->
      case (LangTools.patToMaybeIdent pat1, LangTools.patToMaybeIdent pat2) of
        (Just name1, Just name2) ->
          let eidToBindingPId =
            LangTools.allVarEIdsToBindingPId originalProgram
          in
          let newProgram =
            originalProgram
            |> mapExp
                (\exp ->
                  case (Dict.get exp.val.eid eidToBindingPId, exp.val.e__) of
                    (Just (Just pid), EVar ws _) ->
                      if pid == pat1.val.pid then
                        replaceE__ exp (EVar ws name2)
                      else if pid == pat2.val.pid then
                        replaceE__ exp (EVar ws name1)
                      else
                        exp
                    _ ->
                      exp
                )
          in
          let isSafe =
            let expectedVarEIdsToBindingPId =
              eidToBindingPId
              |> Dict.map
                  (\eid maybePId ->
                    if maybePId == Just pat1.val.pid then
                      Just pat2.val.pid
                    else if maybePId == Just pat2.val.pid then
                      Just pat1.val.pid
                    else
                      maybePId
                  )
            in
            LangTools.allVarEIdsToBindingPId newProgram == expectedVarEIdsToBindingPId
          in
          let result =
            synthesisResult ("Swap usages of " ++ name1 ++ " and " ++ name2) newProgram |> setResultSafe isSafe
          in
          [result]

        _ -> []

    _ -> []


--------------------------------------------------------------------------------

addToolRenameVar m selections = case selections of
  ([], [], [eId], [], [], [], []) ->
    case findExpByEId m.inputExp eId of
      Just ePlucked ->
        case ePlucked.val.e__ of
          EVar _ ident ->
            let newName = m.deuceState.renameVarTextBox in
            [("Rename All " ++ ident, \() -> renameVar eId newName m.inputExp)]
          _ -> []
      _ -> []
  _ -> []

renameVar : EId -> String -> Exp -> List SynthesisResult
renameVar varEId newName program =
  let varExp = LangTools.justFindExpByEId program varEId in
  let oldName = LangTools.expToIdent varExp in
  case LangTools.bindingPathedPatternIdFor varExp program of
    Just pathedPatternId ->
      renamePat pathedPatternId newName program

    Nothing ->
      let _ = Debug.log (oldName ++ " is free at this location in the program") () in
      []


--------------------------------------------------------------------------------

addToolInlineDefintion m selections =
  case selections of
    (_, _, _, [], [], _, _) -> []

    ([], [], [], pathedPatIds, [], [], []) ->
      [ (maybePluralize "Inline Definition" pathedPatIds, \() ->
          CodeMotion.inlineDefinitions pathedPatIds m.inputExp
        ) ]

    ([], [], [], [], letEIds, [], []) ->
      [ (maybePluralize "Inline Definition" letEIds, \() ->
          CodeMotion.inlineDefinitions (letEIds |> List.map (\letEId -> ((letEId, 1), []))) m.inputExp
        ) ]

    _ -> []


--------------------------------------------------------------------------------

addToolFlipBoolean m selections = case selections of
  ([], [_], [eId], [], [], [], []) ->
    case findExpByEId m.inputExp eId of
      Just ePlucked ->
        case ePlucked.val.e__ of
          EBase ws (EBool bool) ->
            let flipped = withDummyExpInfo (EBase ws (EBool (not bool))) in
            let newExp = replaceExpNode eId flipped m.inputExp in
            [("Flip Boolean", \() -> oneSafeResult newExp)]
          _ -> []
      _ -> []
  _ -> []


--------------------------------------------------------------------------------

addToolConvertColorString m selections = case selections of
  (_, [], _, _, _, _, _) -> []

  ([], literals, exps, [], [], [], []) ->
    if List.length exps /= List.length literals then []
    else
      let maybeStrings =
        List.map (\(eid, (_, baseVal)) ->
                   case baseVal of
                     EString _ string -> Just (eid, string)
                     _                -> Nothing
                 ) literals
      in
      bindMaybesToList maybeStrings <| \idsAndStrings ->
      let maybeConverted = List.map convertStringToRgbAndHue idsAndStrings in
      bindMaybesToList maybeConverted <| \converted ->
      let (newExp1, newExp2) =
         List.foldl
           (\(eid,(r,g,b),hue) (acc1,acc2) ->
             let replaceString = replaceExpNodePreservingPrecedingWhitespace eid in
             let eRgba = eList (listOfNums [r,g,b,1.0]) Nothing in
             let eColorNum = eConst hue dummyLoc in
             (replaceString eRgba acc1, replaceString eColorNum acc2)
           )
           (m.inputExp, m.inputExp) converted
      in
      [ (maybePluralize "Convert Color String" literals, \() ->
          [ newExp1 |> synthesisResult "RGBA"
          , newExp2 |> synthesisResult "Color Number (Hue Only)" |> setResultSafe False
          ])
      ]

  _ -> []

convertStringToRgbAndHue (eid, string) =
  let colorName = String.toLower string in
  let values = Utils.maybeFind colorName ColorNum.htmlColorNames in
  values |> Utils.mapMaybe (\((r,g,b), (h,_,_)) ->
    let colorNum =
      if colorName == "black" then 360
      else if colorName == "white" then 499
      else if String.contains "gray" colorName ||
              String.contains "grey" colorName then 450
                -- not dealing with different grays individually
      else h
    in
    (eid, (r,g,b), colorNum)
  )

bindMaybesToList : List (Maybe a) -> (List a -> List b) -> List b
bindMaybesToList list f =
  case Utils.projJusts list of
    Nothing -> Utils.nothing
    Just xs -> f xs


--------------------------------------------------------------------------------

addToolTwiddleShapes m selections = case selections of
  ([], [], [eId], [], [], [], []) ->
    case findExpByEId m.inputExp eId of
      Just ePlucked ->
        let tools = Draw.makeTwiddleTools m eId ePlucked in
        case tools of
          _::_ -> tools
          [] -> []
      _ -> []
  _ -> []


--------------------------------------------------------------------------------

addToolRewriteOffset m selections = case selections of
  ([], _, _, _, _, _, _) -> []

  (nums, [], exps, [ppid], [], [], []) ->
    if List.length nums == List.length exps
    then CodeMotion.rewriteOffsetTool m ppid nums
    else []

  _ -> []


--------------------------------------------------------------------------------

-- could do the following inside CodeMotion...

addToolIntroduceVar m selections = case selections of
  (_, _, [], _, _, _, _) -> []
  (_, _, exps, [], [], [], [patTarget]) ->
    CodeMotion.makeIntroduceVarTool m exps (PatTargetPosition patTarget)
  (_, _, exps, [], [], [expTarget], []) ->
    CodeMotion.makeIntroduceVarTool m exps (ExpTargetPosition expTarget)
  _ -> []

addToolMoveDefinition m selections = case selections of
  (_, _, _, [], [], _, _) -> []
  ([], [], [], pathedPatIds, [], [(Before, eId)], []) ->
    [ (maybePluralize "Move Definition" pathedPatIds, \() ->
        CodeMotion.moveDefinitionsBeforeEId pathedPatIds eId m.inputExp
      ) ]
  ([], [], [], pathedPatIds, [], [], [patTarget]) ->
    let targetPathedPatId = patTargetPositionToTargetPathedPatId patTarget in
    case findExpByEId m.inputExp (pathedPatIdToScopeEId targetPathedPatId) |> Maybe.map (.val >> .e__) of
      Just (ELet _ _ _ _ _ _ _) ->
        [ (maybePluralize "Move Definition" pathedPatIds, \() ->
            CodeMotion.moveDefinitionsPat pathedPatIds targetPathedPatId m.inputExp
          ) ]

      _ -> []

  ([], [], [], [], [letEId], [(Before, eId)], []) ->
    [ ("Move Definition", \() ->
        -- Better result names if we hand the singular case directly to moveDefinitionsBeforeEId.
        CodeMotion.moveDefinitionsBeforeEId [((letEId, 1), [])] eId m.inputExp
      ) ]

  ([], [], [], [], letEIds, [(Before, eId)], []) ->
    [ ("Move Definitions", \() ->
        CodeMotion.moveEquationsBeforeEId letEIds eId m.inputExp
      ) ]
  _ -> []

addToolDuplicateDefinition m selections = case selections of
  (_, _, _, [], _, _, _) -> []
  ([], [], [], pathedPatIds, [], [(Before, eId)], []) ->
    [ (maybePluralize "Duplicate Definition" pathedPatIds, \() ->
        CodeMotion.duplicateDefinitionsBeforeEId pathedPatIds eId m.inputExp
      ) ]
  ([], [], [], pathedPatIds, [], [], [patTarget]) ->
    let targetPathedPatId = patTargetPositionToTargetPathedPatId patTarget in
    case findExpByEId m.inputExp (pathedPatIdToScopeEId targetPathedPatId) |> Maybe.map (.val >> .e__) of
      Just (ELet _ _ _ _ _ _ _) ->
        [ (maybePluralize "Duplicate Definition" pathedPatIds, \() ->
            CodeMotion.duplicateDefinitionsPat pathedPatIds targetPathedPatId m.inputExp
          ) ]

      _ -> []

  _ -> []


addToolMakeSingleLine m selections =
  let maybeEIdToDeLineAndWhetherToPreservePrecedingWhitespace =
    case selections of
      (_, _, [eid], [], [], [], [])      -> Just (eid, True)
      ([], [], [], [], [letEId], [], []) -> findExpByEId m.inputExp letEId |> Maybe.andThen LangTools.expToMaybeLetBoundExp |> Maybe.map (\letBoundExp -> (letBoundExp.val.eid, False))
      _                                  -> Nothing
  in
  case maybeEIdToDeLineAndWhetherToPreservePrecedingWhitespace of
    Nothing  -> []
    Just (eid, shouldPreservePrecedingWhitespace) ->
      let perhapsLeftTrimmer = if shouldPreservePrecedingWhitespace then String.trimLeft else identity in
      if LangUnparser.unparse (LangTools.justFindExpByEId m.inputExp eid) |> perhapsLeftTrimmer |> String.contains "\n" then
        [ ("Make Single Line", \() ->
            let deLine ws =
              if String.contains "\n" ws
              then " "
              else ws
            in
            let deLineP__ p__ =
              case p__ of
                PVar   ws ident wd         -> PVar   (deLine ws) ident wd
                PConst ws n                -> PConst (deLine ws) n
                PBase  ws v                -> PBase  (deLine ws) v
                PList  ws1 ps ws2 rest ws3 -> PList  (deLine ws1) (setPatListWhitespace "" " " ps) (deLine ws2) rest ""
                PAs    ws1 ident ws2 p     -> PAs    (deLine ws1) ident " " p
            in
            let deLinePat p = mapPatTopDown (mapNodeP__ deLineP__) p in
            let deLineE__ e__ =
              case e__ of
                EBase      ws v                     -> EBase      (deLine ws) v
                EConst     ws n l wd                -> EConst     (deLine ws) n l wd
                EVar       ws x                     -> EVar       (deLine ws) x
                EFun       ws1 ps e1 ws2            -> EFun       (deLine ws1) (ps |> List.map deLinePat |> setPatListWhitespace "" " ") e1 ""
                EApp       ws1 e1 es ws2            -> EApp       (deLine ws1) (replacePrecedingWhitespace "" e1) es ""
                EList      ws1 es ws2 rest ws3      -> EList      (deLine ws1) (setExpListWhitespace "" " " es) (deLine ws2) rest ""
                EOp        ws1 op es ws2            -> EOp        (deLine ws1) op es ""
                EIf        ws1 e1 e2 e3 ws2         -> EIf        (deLine ws1) e1 e2 e3 ""
                ELet       ws1 kind rec p e1 e2 ws2 -> ELet       (deLine ws1) kind rec p e1 e2 ""
                ECase      ws1 e1 bs ws2            -> ECase      (deLine ws1) e1 bs ""
                ETypeCase  ws1 pat bs ws2           -> ETypeCase  (deLine ws1) pat bs ""
                EComment   ws s e1                  -> EComment   ws s e1
                EOption    ws1 s1 ws2 s2 e1         -> EOption    ws1 s1 " " s2 e1
                ETyp       ws1 pat tipe e ws2       -> ETyp       (deLine ws1) pat tipe e ""
                EColonType ws1 e ws2 tipe ws3       -> EColonType (deLine ws1) e (deLine ws2) tipe ""
                ETypeAlias ws1 pat tipe e ws2       -> ETypeAlias (deLine ws1) pat tipe e ""
            in
            let deLineExp e = mapExp (mapNodeE__ deLineE__) e in
            m.inputExp
            |> mapExpNode eid (\e -> e |> deLineExp |> (if shouldPreservePrecedingWhitespace then copyPrecedingWhitespace e else replacePrecedingWhitespace " "))
            |> synthesisResult "Make Single Line"
            |> Utils.singleton
          ) ]
      else
        []

-- For Tuples and function calls
addToolMakeMultiLine m selections =
  case selections of
    (_, _, [eid], [], [], [], []) ->
      let exp = LangTools.justFindExpByEId m.inputExp eid in
      case exp.val.e__ of
        EList ws1 es ws2 Nothing ws3 ->
          if es |> List.all (precedingWhitespace >> String.contains "\n") then
            []
          else
            [ ("Make Multi-line", \() ->
              let indentation = indentationAt eid m.inputExp in
              m.inputExp
              |> replaceExpNodeE__ByEId eid (EList ws1 (setExpListWhitespace ("\n" ++ indentation ++ "  ") ("\n" ++ indentation ++ "  ") es) ws2 Nothing ("\n" ++ indentation))
              |> synthesisResult "Make Multi-line"
              |> Utils.singleton
            ) ]

        EApp ws1 e es ws2 ->
          if es |> List.all (precedingWhitespace >> String.contains "\n") then
            []
          else
            [ ("Make Multi-line", \() ->
              let indentation = String.repeat (e.end.col) " " in
              m.inputExp
              |> replaceExpNodeE__ByEId eid (EApp ws1 e (setExpListWhitespace " " ("\n" ++ indentation) es) "")
              |> synthesisResult "Make Multi-line"
              |> Utils.singleton
            ) ]

        _ -> []
    _ -> []


-- Align multiple expressions all on different lines
addToolAlign m selections =
  case selections of
    (_, _, eid1::eid2::restEIds, [], [], [], []) ->
      let eids = eid1::eid2::restEIds in
      let exps = eids |> List.map (LangTools.justFindExpByEId m.inputExp) in
      let lineNums = exps |> List.map (.start >> .line) in
      if lineNums /= Utils.dedup lineNums then
        []
      else
        [ ("Align Expressions", \() ->
          let maxCol = exps |> List.map (.start >> .col) |> List.maximum |> Utils.fromJust_ "addToolAlign maxCol" in
          m.inputExp
          |> mapExp
              (\e ->
                if List.member e.val.eid eids then
                  let wsDelta = maxCol - e.start.col in
                  e |> pushRight (String.repeat wsDelta " ")
                else
                  e
              )
          |> synthesisResult "Align Expressions"
          |> Utils.singleton
        ) ]

    _ -> []

