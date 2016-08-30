module InterfaceController (upstate) where

import Lang exposing (..) --For access to what makes up the Vals
import Types
import LangParser2 exposing (parseE, freshen)
import LangUnparser exposing (unparse, precedingWhitespace, addPrecedingWhitespace)
import LangTransform
import ValueBasedTransform
import Blobs exposing (..)
import Draw
import ExpressionBasedTransform as ETransform
import Sync
import Eval
import Utils
import Keys
import InterfaceModel exposing (..)
import InterfaceStorage exposing (installSaveState, removeDialog)
import LangSvg exposing (toNum, toNumTr, toPoints)
import ShapeWidgets
import ExamplesGenerated as Examples
import Config exposing (params)

import VirtualDom

--Core Libraries
import List
import Dict exposing (Dict)
import Set
import String
import Char
import Graphics.Element as GE
import Graphics.Collage as GC

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

slateToVal : LangSvg.RootedIndexedTree -> Val
slateToVal (rootId, tree) =
  let foo n =
    case n of
      LangSvg.TextNode s -> vList [vBase (VString "TEXT"), vBase (VString s)]
      LangSvg.SvgNode kind l1 l2 ->
        let vs1 = List.map LangSvg.valOfAttr l1 in
        let vs2 = List.map (foo << flip Utils.justGet tree) l2 in
        vList [vBase (VString kind), vList vs1, vList vs2]
          -- NOTE: if relate needs the expression that led to this
          --  SvgNode, need to store it in IndexedTree
  in
  foo (Utils.justGet rootId tree)

upslate : LangSvg.NodeId -> (String, LangSvg.AVal) -> LangSvg.IndexedTree -> LangSvg.IndexedTree
upslate id newattr nodes = case Dict.get id nodes of
    Nothing   -> Debug.crash "upslate"
    Just node -> case node of
        LangSvg.TextNode x -> nodes
        LangSvg.SvgNode shape attrs children ->
            let newnode = LangSvg.SvgNode shape (Utils.update newattr attrs) children
            in Dict.insert id newnode nodes

refreshMode model e =
  case model.mode of
    Live _  -> Utils.fromOk "refreshMode" <| mkLive_ model.syncOptions model.slideNumber model.movieNumber model.movieTime e
    Print _ -> Utils.fromOk "refreshMode" <| mkLive_ model.syncOptions model.slideNumber model.movieNumber model.movieTime e
    m       -> m

refreshMode_ model = refreshMode model model.inputExp

-- TODO refresh type highlights, too
refreshHighlights id zone model =
  let codeBoxInfo = model.codeBoxInfo in
  let hi = liveInfoToHighlights id zone model in
  { model | codeBoxInfo = { codeBoxInfo | highlights = hi } }

switchOrient m = case m of
  Vertical -> Horizontal
  Horizontal -> Vertical

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


between1 i (j,k) = i `Utils.between` (j+1, k+1)

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


-- this is a bit redundant with View.turnOn...
maybeStuff id shape zone m =
  case m.mode of
    Live info ->
      flip Utils.bindMaybe (Dict.get id info.assignments) <| \d ->
      flip Utils.bindMaybe (Dict.get zone d) <| \(yellowLocs,_) ->
        Just (info.initSubst, yellowLocs)
    _ ->
      Nothing

highlightChanges mStuff changes codeBoxInfo =
  case mStuff of
    Nothing -> codeBoxInfo
    Just (initSubstPlus, locs) ->

      let (hi,stringOffsets) =
        -- hi : List Highlight, stringOffsets : List (Pos, Int)
        --   where Pos is start pos of a highlight to offset by Int chars
        let f loc (acc1,acc2) =
          let (locid,_,_) = loc in
          let highlight c = makeHighlight initSubstPlus c loc in
          case (Dict.get locid initSubstPlus, Dict.get locid changes) of
            (Nothing, _)             -> Debug.crash "Controller.highlightChanges"
            (Just n, Nothing)        -> (highlight yellow :: acc1, acc2)
            (Just n, Just Nothing)   -> (highlight red :: acc1, acc2)
            (Just n, Just (Just n')) ->
              if n' == n.val then
                (highlight yellow :: acc1, acc2)
              else
                let (s, s') = (strNum n.val, strNum n') in
                let x = (acePos n.start, String.length s' - String.length s) in
                (highlight green :: acc1, x :: acc2)
        in
        List.foldl f ([],[]) (Set.toList locs)
      in

      let hi' =
        let g (startPos,extraChars) (old,new) =
          let bump pos = { pos | column = pos.column + extraChars } in
          let ret new' = (old, new') in
          ret <|
            if startPos.row    /= old.start.row         then new
            else if startPos.column >  old.start.column then new
            else if startPos.column == old.start.column then { start = new.start, end = bump new.end }
            else if startPos.column <  old.start.column then { start = bump new.start, end = bump new.end }
            else
              Debug.crash "highlightChanges"
        in
        -- hi has <= 4 elements, so not worrying about the redundant processing
        flip List.map hi <| \{color,range} ->
          let (_,range') = List.foldl g (range,range) stringOffsets in
          { color = color, range = range' }
      in

      { codeBoxInfo | highlights = hi' }

addSlateAndCode old (exp, val) =
  slateAndCode old (exp, val)
  |> Result.map (\(slate, code) -> (exp, val, slate, code))

slateAndCode old (exp, val) =
  LangSvg.resolveToIndexedTree old.slideNumber old.movieNumber old.movieTime val
  |> Result.map (\slate -> (slate, unparse exp))

runWithErrorHandling model exp onOk =
  let result =
    Eval.run exp `Result.andThen` (\(val, widgets) ->
      slateAndCode model (exp, val)
      |> Result.map (\(slate, code) -> onOk val widgets slate code)
    )
  in
  handleError model result

handleError : Model -> Result String Model -> Model
handleError oldModel result =
  case result of
    Ok newModel -> newModel
    Err s       -> { oldModel | errorBox = Just s }

updateCodeBoxWithTypes : Types.AceTypeInfo -> CodeBoxInfo -> CodeBoxInfo
updateCodeBoxWithTypes ati codeBoxInfo =
  { codeBoxInfo | annotations = ati.annotations
                , highlights = ati.highlights
                , tooltips = ati.tooltips }

updateCodeBoxWithParseError annot codeBoxInfo =
  { codeBoxInfo | annotations = [annot] , highlights = [] , tooltips = [] }

switchToCursorTool old =
  { old | mouseMode = MouseNothing , tool = Cursor }

--------------------------------------------------------------------------------

clickToCanvasPoint old (mx, my) =
  let (xOrigin, yOrigin) = case old.orient of
    Vertical   -> canvasOriginVertical old
    Horizontal -> canvasOriginHorizontal old
  in
  (mx - xOrigin, my - yOrigin)

-- the computations of the top-left corner of the canvas
-- are based on copying the computations from View
-- TODO: refactor these

canvasOriginVertical old =
  let
    sideGut = params.topSection.h
    wGut    = params.mainSection.vertical.wGut
    wMiddle = params.mainSection.widgets.wBtn
    wCode_  = (fst old.dimensions - sideGut - sideGut - wMiddle - wGut - wGut) // 2
    wCode   = if old.hideCode then 0
              else if old.hideCanvas then (fst old.dimensions - sideGut - sideGut - wMiddle - wGut - wGut)
              else wCode_ + old.midOffsetX
  in
    ( sideGut + wCode + 2*wGut + wMiddle
    , params.topSection.h
    )

canvasOriginHorizontal old =
  -- TODO the y-position in horizontal mode is off by a few pixels
  -- TODO in View, the height of codebox isn't the same as the canvas.
  --   hMid is calculated weirdly in View...
  let
    hTop    = params.topSection.h
    hBot    = params.botSection.h
    hGut    = params.mainSection.horizontal.hGut
    hCode_  = (snd old.dimensions - hTop - hBot - hGut) // 2
    hCode   = hCode_ + old.midOffsetY
    -- TODO consider hideCode and hideCanvas
    wTools  = params.mainSection.widgets.wBtn + 2 * params.mainSection.vertical.wGut
  in
    ( wTools
    , params.topSection.h + hCode + hGut
    )


--------------------------------------------------------------------------------
-- Mouse Events

onMouseClick click old =
  case (old.tool, old.mouseMode) of

    -- Inactive zone
    (Cursor, MouseObject i k z Nothing) ->
      onClickPrimaryZone i k z { old | mouseMode = MouseNothing }

    -- Active zone but not dragged
    (Cursor, MouseObject i k z (Just (_, _, False))) ->
      onClickPrimaryZone i k z { old | mouseMode = MouseNothing }

    (Poly stk, MouseDrawNew points) ->
      let pointOnCanvas = clickToCanvasPoint old click in
      let add () =
        let points' = (old.keysDown, pointOnCanvas) :: points in
        { old | mouseMode = MouseDrawNew points' }
      in
      if points == [] then add ()
      else
        let (_,initialPoint) = Utils.last_ points in
        if Utils.distanceInt pointOnCanvas initialPoint > Draw.drawNewPolygonDotSize then add ()
        else if List.length points == 2 then { old | mouseMode = MouseNothing }
        else if List.length points == 1 then switchToCursorTool old
        else upstate Run <| Draw.addPolygon stk old points

    (Path stk, MouseDrawNew points) ->
      let pointOnCanvas = clickToCanvasPoint old click in
      let add new =
        let points' = (old.keysDown, new) :: points in
        (points', { old | mouseMode = MouseDrawNew points' })
      in
      case points of
        [] -> snd (add pointOnCanvas)
        (_,firstClick) :: [] ->
          if Utils.distanceInt pointOnCanvas firstClick < Draw.drawNewPolygonDotSize
          then switchToCursorTool old
          else snd (add pointOnCanvas)
        (_,lastClick) :: _ ->
          if Utils.distanceInt pointOnCanvas lastClick < Draw.drawNewPolygonDotSize
          then upstate Run <| Draw.addPath stk old points
          else
            let (_,firstClick) = Utils.last_ points in
            if Utils.distanceInt pointOnCanvas firstClick < Draw.drawNewPolygonDotSize
            then
              let (points',old') = add firstClick in
              upstate Run <| Draw.addPath stk old' points'
            else
              snd (add pointOnCanvas)

    (HelperDot, MouseDrawNew []) ->
      let pointOnCanvas = (old.keysDown, clickToCanvasPoint old click) in
      { old | mouseMode = MouseDrawNew [pointOnCanvas] }

    (_, MouseDrawNew []) -> switchToCursorTool old

    _ -> old

onClickPrimaryZone i k z old =
  let hoveredCrosshairs' =
    case ShapeWidgets.zoneToCrosshair k z of
      Just (xFeature, yFeature) ->
        Set.insert (i, xFeature, yFeature) old.hoveredCrosshairs
      _ ->
        old.hoveredCrosshairs
  in
  let (selectedShapes', selectedBlobs') =
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
      case Dict.get i (snd old.slate) of
        Just (LangSvg.SvgNode _ l _) -> LangSvg.maybeFindBlobId l
        _                            -> Debug.crash "onClickPrimaryZone"
    in
    case (k, z, maybeBlobId) of
      ("line", "Edge",     Just blobId) -> (selectThisShape (), selectBlob blobId)
      (_,      "Interior", Just blobId) -> (selectThisShape (), selectBlob blobId)
      ("line", "Edge",     Nothing)     -> (selectThisShape (), old.selectedBlobs)
      (_,      "Interior", Nothing)     -> (selectThisShape (), old.selectedBlobs)
      _                                 -> (old.selectedShapes, old.selectedBlobs)
  in
  { old | hoveredCrosshairs = hoveredCrosshairs'
        , selectedShapes = selectedShapes'
        , selectedBlobs = selectedBlobs'
        }

onMouseMove (mx0, my0) old =
  let (mx, my) = clickToCanvasPoint old (mx0, my0) in
  case old.mouseMode of

    MouseNothing -> old

    MouseResizeMid Nothing ->
      let f =
        case old.orient of
          Vertical   -> \(mx1,_) -> (old.midOffsetX + mx1 - mx0, old.midOffsetY)
          Horizontal -> \(_,my1) -> (old.midOffsetY, old.midOffsetY + my1 - my0)
      in
      { old | mouseMode = MouseResizeMid (Just f) }

    MouseResizeMid (Just f) ->
      let (x,y) = f (mx0, my0) in
      { old | midOffsetX = x , midOffsetY = y }

    MouseObject id kind zone Nothing ->
      old

    MouseObject id kind zone (Just (mStuff, (mx0, my0), _)) ->
      let (dx, dy) = (mx - mx0, my - my0) in
      applyTrigger id kind zone old mx0 my0 dx dy
      |> Result.map (\(newE,newV,changes,newSlate,newWidgets) ->
        { old | code = unparse newE
              , inputExp = newE
              , inputVal = newV
              , slate = newSlate
              , widgets = newWidgets
              , codeBoxInfo = highlightChanges mStuff changes old.codeBoxInfo
              , mouseMode =
                  MouseObject id kind zone (Just (mStuff, (mx0, my0), True))
              }
      ) |> handleError old

    MouseSlider widget Nothing ->
      let onNewPos = createMousePosCallbackSlider mx my widget old in
      { old | mouseMode = MouseSlider widget (Just onNewPos) }

    MouseSlider widget (Just onNewPos) ->
      onNewPos (mx, my)
      |> Result.map (\(newE,newV,newSlate,newWidgets) ->
        { old | code = unparse newE
              , inputExp = newE
              , inputVal = newV
              , slate = newSlate
              , widgets = newWidgets
              }
      ) |> handleError old

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

onMouseUp old =
  case (old.mode, old.mouseMode) of

    (Print _, _) -> old
    (_, MouseObject i k z (Just _)) ->
      -- 8/10: re-parsing to get new position info after live sync-ing
      -- TODO: could update positions within highlightChanges
      -- TODO: update inputVal?
      let e = Utils.fromOkay "onMouseUp" <| parseE old.code in
      let old' = { old | inputExp = e } in
      refreshHighlights i z
        { old' | mouseMode = MouseNothing, mode = refreshMode_ old'
               , history = addToHistory old.code old'.history }

    (_, MouseSlider _ (Just _)) ->
      let e = Utils.fromOkay "onMouseUp" <| parseE old.code in
      let old' = { old | inputExp = e } in
        { old' | mouseMode = MouseNothing, mode = refreshMode_ old'
               , history = addToHistory old.code old'.history }

    (_, MouseDrawNew points) ->
      case (old.tool, points, old.keysDown == Keys.shift) of

        (Line _,     [pt2, pt1], _) -> upstate Run <| Draw.addLine old pt2 pt1
        (HelperLine, [pt2, pt1], _) -> upstate Run <| Draw.addLine old pt2 pt1

        (Rect Raw,      [pt2, pt1], False) -> upstate Run <| Draw.addRawRect old pt2 pt1
        (Rect Raw,      [pt2, pt1], True)  -> upstate Run <| Draw.addRawSquare old pt2 pt1
        (Rect Stretchy, [pt2, pt1], False) -> upstate Run <| Draw.addStretchyRect old pt2 pt1
        (Rect Stretchy, [pt2, pt1], True)  -> upstate Run <| Draw.addStretchySquare old pt2 pt1

        (Oval Raw,      [pt2, pt1], False) -> upstate Run <| Draw.addRawOval old pt2 pt1
        (Oval Raw,      [pt2, pt1], True)  -> upstate Run <| Draw.addRawCircle old pt2 pt1
        (Oval Stretchy, [pt2, pt1], False) -> upstate Run <| Draw.addStretchyOval old pt2 pt1
        (Oval Stretchy, [pt2, pt1], True)  -> upstate Run <| Draw.addStretchyCircle old pt2 pt1

        (HelperDot, [pt], _) -> upstate Run <| Draw.addHelperDot old pt

        (Lambda, [pt2, pt1], _) -> upstate Run <| Draw.addLambda old pt2 pt1

        (Poly _, _, _) -> old
        (Path _, _, _) -> old

        (Text, [pt2, pt1], _) -> upstate Run <| Draw.addTextBox old pt2 pt1

        (_, [], _)     -> switchToCursorTool old

        _              -> old

    _ -> { old | mouseMode = MouseNothing, mode = refreshMode_ old }


--------------------------------------------------------------------------------
-- Updating the Model

upstate : Event -> Model -> Model
upstate evt old = case debugLog "Event" evt of

    Noop -> old

    WindowDimensions wh -> { old | dimensions = wh }

    Run ->
      -- TODO move upstateRun to a function definition elsewhere,
      -- to reduce the number of cases that have a dependency on upstate
      case parseE old.code of
        Err (err, annot) ->
          -- TODO maybe get rid of (computing and) displaying err in caption area
          { old | errorBox = Just err
                , codeBoxInfo = updateCodeBoxWithParseError annot old.codeBoxInfo }
        Ok e ->
          let result =
            let aceTypeInfo = Types.typecheck e in
            Eval.run e
            `Result.andThen` (\(newVal,ws) ->
              LangSvg.fetchEverything old.slideNumber old.movieNumber 0.0 newVal
              |> Result.map (\(newSlideCount, newMovieCount, newMovieDuration, newMovieContinue, newSlate) ->
                let newCode = unparse e in
                let lambdaTools' =
                  -- TODO should put program into Model
                  let program = splitExp e in
                  let options = Draw.lambdaToolOptionsOf program ++ snd sampleModel.lambdaTools in
                  let selectedIdx = min (fst old.lambdaTools) (List.length options) in
                  (selectedIdx, options)
                in
                let new =
                  { old | inputExp      = e
                        , inputVal      = newVal
                        , code          = newCode
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
                        , lambdaTools   = lambdaTools'
                        , codeBoxInfo   = updateCodeBoxWithTypes aceTypeInfo old.codeBoxInfo
                  }
                in
                { new | mode = refreshMode_ new
                      , errorBox = Nothing }
              )
            )
          in
          handleError old result

    StartAnimation -> upstate Redraw { old | movieTime = 0
                                           , runAnimation = True }

    Redraw ->
      case LangSvg.fetchEverything old.slideNumber old.movieNumber old.movieTime old.inputVal of
        Ok (newSlideCount, newMovieCount, newMovieDuration, newMovieContinue, newSlate) ->
          { old | slideCount    = newSlideCount
                , movieCount    = newMovieCount
                , movieDuration = newMovieDuration
                , movieContinue = newMovieContinue
                , slate         = newSlate }
        Err s -> { old | errorBox = Just s }

    ToggleOutput ->
      let m = case old.mode of
        Print _ -> refreshMode_ old
        _       -> Print (LangSvg.printSvg old.showGhosts old.slate)
      in
      { old | mode = m }

    StartResizingMid ->
      if old.hideCode then old
      else if old.hideCanvas then old
      else { old | mouseMode = MouseResizeMid Nothing }

    SelectObject id kind zone ->
      let mStuff = maybeStuff id kind zone old in
      case mStuff of

        Nothing -> -- Inactive zone
          { old | mouseMode = MouseObject id kind zone Nothing }

        Just _  -> -- Active zone
          let (mx, my) = clickToCanvasPoint old (snd old.mouseState) in
          let blah = Just (mStuff, (mx, my), False) in
          { old | mouseMode = MouseObject id kind zone blah }

    MouseClickCanvas ->
      case (old.tool, old.mouseMode) of
        (Cursor, MouseObject _ _ _ _) -> old
        (Cursor, _) ->
          { old | selectedShapes = Set.empty, selectedBlobs = Dict.empty }

        (_ , MouseNothing) ->
          { old | mouseMode = MouseDrawNew []
                , selectedShapes = Set.empty, selectedBlobs = Dict.empty }

        _ -> old

    MousePosition pos' ->
      case old.mouseState of
        (Nothing, _)    -> { old | mouseState = (Nothing, pos') }
        (Just False, _) -> onMouseMove pos' { old | mouseState = (Just True, pos') }
        (Just True, _)  -> onMouseMove pos' { old | mouseState = (Just True, pos') }

    MouseIsDown b ->
      let old =
        let (x,y) = snd old.mouseState in
        let lightestColor = 470 in
        { old | randomColor = (old.randomColor + x + y) % lightestColor }
      in
      case (b, old.mouseState) of

        (True, (Nothing, pos)) -> -- mouse down
          let _ = debugLog "mouse down" () in
          { old | mouseState = (Just False, pos) }

        (False, (Just False, pos)) -> -- click (mouse up after not being dragged)
          let _ = debugLog "mouse click" () in
          onMouseClick pos { old | mouseState = (Nothing, pos) }

        (False, (Just True, pos)) -> -- mouse up (after being dragged)
          let _ = debugLog "mouse up" () in
          onMouseUp { old | mouseState = (Nothing, pos) }

        (False, (Nothing, _)) ->
          let _ = debugLog "mouse down was preempted by a handler in View" () in
          old

        -- (True, (Just _, _)) -> Debug.crash "upstate MouseIsDown: impossible"
        (True, (Just _, _)) ->
          let _ = Debug.log "upstate MouseIsDown: impossible" () in
          old

    TickDelta deltaT ->
      case old.mode of
        SyncSelect _ ->
          -- Prevent "jump" after slow first frame render.
          let adjustedDeltaT = if old.syncSelectTime == 0.0 then clamp 0.0 50 deltaT else deltaT in
          upstate Redraw { old | syncSelectTime = old.syncSelectTime + (adjustedDeltaT / 1000) }
        _ ->
          if old.movieTime < old.movieDuration then
            -- Prevent "jump" after slow first frame render.
            let adjustedDeltaT = if old.movieTime == 0.0 then clamp 0.0 50 deltaT else deltaT in
            let newMovieTime = clamp 0.0 old.movieDuration (old.movieTime + (adjustedDeltaT / 1000)) in
            upstate Redraw { old | movieTime = newMovieTime }
          else if old.movieContinue == True then
            upstate NextMovie old
          else
            { old | runAnimation = False }

    DigHole ->
      let newExp =
        ValueBasedTransform.digHole old.inputExp old.selectedFeatures old.slate old.syncOptions
      in
      runWithErrorHandling old newExp (\newVal newWidgets newSlate newCode ->
        debugLog "new model" <|
          { old | code             = newCode
                , inputExp         = newExp
                , inputVal         = newVal
                , history          = addToHistory old.code old.history
                , slate            = newSlate
                , widgets          = newWidgets
                , previewCode      = Nothing
                , mode             = Utils.fromOk "DigHole MkLive" <| mkLive old.syncOptions old.slideNumber old.movieNumber old.movieTime newExp newVal -- we already ran it successfully once so it shouldn't crash the second time
                , selectedFeatures = Set.empty
          }
      )

    MakeEqual ->
      let newExp =
        ValueBasedTransform.makeEqual
            old.inputExp
            old.selectedFeatures
            old.slideNumber
            old.movieNumber
            old.movieTime
            old.syncOptions
      in
      runWithErrorHandling old newExp (\newVal newWidgets newSlate newCode ->
        upstate CleanCode <|
        debugLog "new model" <|
          { old | code             = newCode
                , inputExp         = newExp
                , inputVal         = newVal
                , history          = addToHistory old.code old.history
                , slate            = newSlate
                , widgets          = newWidgets
                , previewCode      = Nothing
                , mode             = Utils.fromOk "MakeEqual MkLive" <| mkLive old.syncOptions old.slideNumber old.movieNumber old.movieTime newExp newVal -- we already ran it successfully once so it shouldn't crash the second time
                , selectedFeatures = Set.empty
          }
      )

    MakeEquidistant ->
      let newExp =
        ValueBasedTransform.makeEquidistant
            old.inputExp
            old.selectedFeatures
            old.slideNumber
            old.movieNumber
            old.movieTime
            old.slate
            old.syncOptions
      in
      runWithErrorHandling old newExp (\newVal newWidgets newSlate newCode ->
        debugLog "new model" <|
          { old | code             = newCode
                , inputExp         = newExp
                , inputVal         = newVal
                , history          = addToHistory old.code old.history
                , slate            = newSlate
                , widgets          = newWidgets
                , previewCode      = Nothing
                , mode             = Utils.fromOk "MakeEquidistant MkLive" <| mkLive old.syncOptions old.slideNumber old.movieNumber old.movieTime newExp newVal -- we already ran it successfully once so it shouldn't crash the second time
                , selectedFeatures = Set.empty
          }
      )

    GroupBlobs ->
      let (defs,me) = splitExp old.inputExp in
      case me of
        SvgConcat _ _ -> old
        OtherExp _    -> old
        Blobs blobs f ->
          let maybeAnchorPoint = ETransform.anchorOfSelectedFeatures old.selectedFeatures in
          let multipleSelectedBlobs = Dict.size old.selectedBlobs > 1 in
          case (maybeAnchorPoint, multipleSelectedBlobs) of
            (Ok Nothing, False)   -> old
            (Ok Nothing, True)    -> upstate Run <| ETransform.groupSelectedBlobs old defs blobs f
            (Ok (Just anchor), _) -> upstate Run <| ETransform.groupSelectedBlobsAround old anchor defs blobs f
            (Err err, _)          -> let _ = Debug.log "bad anchor" err in old

    DuplicateBlobs ->
      upstate Run <| ETransform.duplicateSelectedBlobs old

    MergeBlobs ->
      if Dict.size old.selectedBlobs <= 1 then old
      else upstate Run <| ETransform.mergeSelectedBlobs old

    AbstractBlobs ->
      upstate Run <| ETransform.abstractSelectedBlobs old

{-
    -- TODO AdHoc/Sync not used at the moment
    Sync ->
      case (old.mode, old.inputExp) of
        (AdHoc, ip) ->
          let
            -- If stuff breaks, try re-adding this.
            -- We forgot why it was here.
            -- inputval   = fst <| Eval.run ip
            -- inputSlate = LangSvg.resolveToIndexedTree old.slideNumber old.movieNumber old.movieTime inputval
            -- inputval'  = slateToVal inputSlate
            newval     = slateToVal old.slate
            local      = Sync.inferLocalUpdates old.syncOptions ip old.inputVal newval
            struct     = Sync.inferStructuralUpdates ip old.inputVal newval
            delete     = Sync.inferDeleteUpdates ip old.inputVal newval
            relatedG   = Sync.inferNewRelationships ip old.inputVal newval
            relatedV   = Sync.relateSelectedAttrs old.genSymCount ip old.inputVal newval
          in
          let addSlateAndCodeToAll list = List.filterMap (Result.toMaybe << addSlateAndCode old) list in
            case (local, relatedV) of
              (Ok [], (_, [])) -> { old | mode = Utils.fromOk "Sync mkLive_" <| mkLive_ old.syncOptions old.slideNumber old.movieNumber old.movieTime ip }
              (Ok [], (nextK, changes)) ->
                let _ = debugLog ("no live updates, only related var") () in
                let m = SyncSelect (addSlateAndCodeToAll changes) in
                { old | mode = m, genSymCount = nextK, runAnimation = True, syncSelectTime = 0.0 }
              (Ok live, _) ->
                let n = debugLog "# of live updates" (List.length live) in
                let changes = live ++ delete ++ relatedG ++ struct in
                let m = SyncSelect (addSlateAndCodeToAll changes) in
                { old | mode = m, runAnimation = True, syncSelectTime = 0.0 }
              (Err e, _) ->
                let _ = debugLog ("no live updates: " ++ e) () in
                let changes = delete ++ relatedG ++ struct in
                let m = SyncSelect (addSlateAndCodeToAll changes) in
                { old | mode = m, runAnimation = True, syncSelectTime = 0.0 }
        _ -> Debug.crash "upstate Sync"
-}

    SelectOption (exp, val, slate, code) ->
        { old | code          = code
              , inputExp      = exp
              , inputVal      = val
              , history       = addToHistory old.code old.history
              , slate         = slate
              , previewCode   = Nothing
              , tool          = Cursor
              , mode          = Utils.fromOk "SelectOption mkLive" <| mkLive old.syncOptions old.slideNumber old.movieNumber old.movieTime exp val }

    PreviewCode maybeCode ->
      { old | previewCode = maybeCode }

    CancelSync ->
      upstate Run { old | mode = Utils.fromOk "CancelSync mkLive_" <| mkLive_ old.syncOptions old.slideNumber old.movieNumber old.movieTime old.inputExp }

    SelectExample name thunk ->
      if name == Examples.scratchName then
        upstate Run { old | exName = name, code = old.scratchCode, history = ([],[]) }
      else

      let {e,v,ws,ati} = thunk () in
      let (so, m) =
        case old.mode of
          Live _  -> let so = Sync.syncOptionsOf old.syncOptions e in (so, Utils.fromOk "SelectExample mkLive_" <| mkLive so old.slideNumber old.movieNumber old.movieTime e v)
          Print _ -> let so = Sync.syncOptionsOf old.syncOptions e in (so, Utils.fromOk "SelectExample mkLive_" <| mkLive so old.slideNumber old.movieNumber old.movieTime e v)
          _      -> (old.syncOptions, old.mode)
      in
      let scratchCode' =
        if old.exName == Examples.scratchName then old.code else old.scratchCode
      in
      LangSvg.fetchEverything old.slideNumber old.movieNumber old.movieTime v
      |> Result.map (\(slideCount, movieCount, movieDuration, movieContinue, slate) ->
        let code = unparse e in
        { old | scratchCode   = scratchCode'
              , exName        = name
              , inputExp      = e
              , inputVal      = v
              , code          = code
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
              , codeBoxInfo   = updateCodeBoxWithTypes ati old.codeBoxInfo
              }
      ) |> handleError old

    SwitchMode m -> { old | mode = m }

    SwitchOrient -> { old | orient = switchOrient old.orient }

    Undo ->
      case old.history of
        ([],_)         -> old
        ([firstRun],_) -> old
        (lastRun::secondToLast::older, future) ->
          let new = { old | history = (secondToLast::older, lastRun::future)
                          , code    = secondToLast } in
          upstate Run new

    Redo ->
      case old.history of
        (_,[]) -> old
        (past, next::future) ->
          let new = { old | history = (next::past, future)
                          , code    = next } in
          upstate Run new

    NextSlide ->
      if old.slideNumber >= old.slideCount then
        upstate StartAnimation { old | slideNumber = old.slideNumber
                                     , movieNumber = old.movieCount }
      else
        upstate StartAnimation { old | slideNumber = old.slideNumber + 1
                                     , movieNumber = 1 }

    PreviousSlide ->
      if old.slideNumber <= 1 then
        upstate StartAnimation { old | slideNumber = 1
                                     , movieNumber = 1 }
      else
        let previousSlideNumber = old.slideNumber - 1 in
        let result =
          Eval.run old.inputExp
          `Result.andThen` (\(previousVal, _) ->
            LangSvg.resolveToMovieCount previousSlideNumber previousVal
            |> Result.map (\previousMovieCount ->
              upstate StartAnimation { old | slideNumber = previousSlideNumber
                                           , movieNumber = previousMovieCount }
            )
          )
        in
        handleError old result

    NextMovie ->
      if old.movieNumber == old.movieCount && old.slideNumber < old.slideCount then
        upstate NextSlide old
      else if old.movieNumber < old.movieCount then
        upstate StartAnimation { old | movieNumber = old.movieNumber + 1 }
      else
        -- Last movie of slide show; skip to its end.
        upstate Redraw { old | movieTime    = old.movieDuration
                             , runAnimation = False }

    PreviousMovie ->
      if old.movieNumber == 1 then
        upstate PreviousSlide old
      else
        upstate StartAnimation { old | movieNumber = old.movieNumber - 1 }

    KeysDown l ->
      let _ = debugLog "keys" (toString l) in
      let new = { old | keysDown = l } in

      if l == Keys.escape then
        case (new.tool, new.mouseMode) of
          (Cursor, _) ->
            { new | selectedFeatures = Set.empty
                  , selectedShapes = Set.empty
                  , selectedBlobs = Dict.empty
                  }
          (_, MouseNothing)   -> { new | tool = Cursor }
          (_, MouseDrawNew _) -> { new | mouseMode = MouseNothing }
          _                   -> new

      else if l == Keys.delete then
         upstate Run <| ETransform.deleteSelectedBlobs new
      -- else if l == Keys.backspace || l == Keys.delete then
      --   deleteSelectedBlobs new
      -- TODO
      -- else if l == Keys.metaPlus Keys.d then
      -- else if l == Keys.metaPlus Keys.d || l == Keys.commandPlus Keys.d then
      -- else if l == Keys.d then
      --   duplicateSelectedBlobs new
      else
        new

{-      case old.mode of
          SaveDialog _ -> old
          _ -> case editingMode old of
            True -> if
              | l == keysMetaShift -> upstate Run old
              | otherwise -> old
            False -> if
              | l == keysE -> upstate Edit old
              | l == keysZ -> upstate Undo old
              -- | l == keysShiftZ -> upstate Redo old
              | l == keysY -> upstate Redo old
              | l == keysG || l == keysH -> -- for right- or left-handers
                  upstate ToggleZones old
              | l == keysO -> upstate ToggleOutput old
              | l == keysP -> upstate SwitchOrient old
              | l == keysS ->
                  let _ = Debug.log "TODO Save" () in
                  upstate Noop old
              | l == keysShiftS ->
                  let _ = Debug.log "TODO Save As" () in
                  upstate Noop old
              | l == keysRight -> adjustMidOffsetX old 25
              | l == keysLeft  -> adjustMidOffsetX old (-25)
              | l == keysUp    -> adjustMidOffsetY old (-25)
              | l == keysDown  -> adjustMidOffsetY old 25
              | otherwise -> old
-}

{-
      let fire evt = upstate evt old in

      case editingMode old of

        True ->
          if l == keysEscShift then fire Run
          else                      fire Noop

        False ->

          -- events for any non-editing mode
          if      l == keysO          then fire ToggleOutput
          else if l == keysP          then fire SwitchOrient
          else if l == keysShiftRight then adjustMidOffsetX old 25
          else if l == keysShiftLeft  then adjustMidOffsetX old (-25)
          else if l == keysShiftUp    then adjustMidOffsetY old (-25)
          else if l == keysShiftDown  then adjustMidOffsetY old 25

          -- events for specific non-editing mode
          else case old.mode of

              Live _ ->
                if      l == keysE        then fire Edit
                else if l == keysZ        then fire Undo
                else if l == keysY        then fire Redo
                else if l == keysT        then fire (SwitchMode AdHoc)
                else if l == keysS        then fire Noop -- placeholder for Save
                else if l == keysShiftS   then fire Noop -- placeholder for Save As
                else                           fire Noop

              AdHoc ->
                if      l == keysZ        then fire Undo
                else if l == keysY        then fire Redo
                else if l == keysT        then fire Sync
                else                           fire Noop

              _                       -> fire Noop
-}

    CleanCode ->
      case parseE old.code of
        Err (err, _) ->
          { old | caption = Just (LangError ("PARSE ERROR!\n" ++ err)) }
        Ok reparsed ->
          let cleanedExp =
            reparsed
            -- |> cleanExp
            |> LangTransform.simplify
            |> LangTransform.removeExtraPostfixes ["_orig", "'"]
            |> freshen
          in
          let code' = unparse cleanedExp in
          if old.code == code' then old
          else
            let _ = debugLog "Cleaned: " code' in
            upstate Run { old | inputExp = cleanedExp, code = code' }
{-
          let history' =
            if old.code == code'
              then old.history
              else addToHistory code' old.history
          in
          let _ = debugLog "Cleaned: " code' in
          let newModel = { old | inputExp = cleanedExp, code = code', history = history' } in
          newModel
-}

    -- Elm does not have function equivalence/pattern matching, so we need to
    -- thread these events through upstate in order to catch them to rerender
    -- appropriately (see CodeBox.elm)
    InstallSaveState -> installSaveState old
    RemoveDialog makeSave saveName -> removeDialog makeSave saveName old
    ToggleBasicCodeBox -> { old | basicCodeBox = not old.basicCodeBox }
    UpdateFieldContents fieldContents -> { old | fieldContents = fieldContents }

    UpdateModel f -> f old

    -- Lets multiple events be executed in sequence (useful for CodeBox.elm)
    MultiEvent evts -> case evts of
      [] -> old
      e1 :: es -> upstate e1 old |> upstate (MultiEvent es)

    WaitRun -> old
    WaitSave saveName -> { old | exName = saveName }
    WaitClean -> old
    WaitCodeBox -> old


adjustMidOffsetX old dx =
  case old.orient of
    Vertical   -> { old | midOffsetX = old.midOffsetX + dx }
    Horizontal -> upstate SwitchOrient old

adjustMidOffsetY old dy =
  case old.orient of
    Horizontal -> { old | midOffsetY = old.midOffsetY + dy }
    Vertical   -> upstate SwitchOrient old



--------------------------------------------------------------------------------
-- Mouse Callbacks for Zones

applyTrigger objid kind zone old mx0 my0 dx_ dy_ =
  let dx = if old.keysDown == Keys.y then 0 else dx_ in
  let dy = if old.keysDown == Keys.x then 0 else dy_ in
  case old.mode of
    AdHoc ->
      Ok (old.inputExp, old.inputVal, Dict.empty, old.slate, old.widgets)
    Live info ->
      case Utils.justGet_ "#4" zone (Utils.justGet_ "#5" objid info.triggers) of
        Nothing -> Debug.crash "shouldn't happen due to upstate SelectObject"
        Just trigger ->
          let (newE,changes) = trigger (mx0, my0) (dx, dy) in
          Eval.run newE
          |> Result.map (\(newVal,newWidgets) ->
              (newE, newVal, changes, LangSvg.valToIndexedTree newVal, newWidgets)
            )
    _ -> Debug.crash "applyTrigger"


--------------------------------------------------------------------------------
-- Mouse Callbacks for UI Widgets

wSlider = params.mainSection.uiWidgets.wSlider

pickLocId t =
  case Set.toList (Sync.locsOfTrace Sync.defaultOptions t) of
    [(k,_,_)]    -> k
    (k,_,_) :: _ -> k -- picking arbitrarily...
    []           -> Debug.crash "InterfaceController.pickLocId"

createMousePosCallbackSlider mx my widget old =

  let create maybeUpdateX maybeUpdateY = \(mx',my') ->
    let xSubst =
      case maybeUpdateX of
        Just (xLoc, xFoo) -> Dict.singleton xLoc (xFoo (toFloat (mx' - mx)))
        Nothing           -> Dict.empty
    in
    let ySubst =
      case maybeUpdateY of
        Just (yLoc, yFoo) -> Dict.singleton yLoc (yFoo (toFloat (my' - my)))
        Nothing           -> Dict.empty
    in
    -- unlike the live triggers via Sync,
    -- this substitution only binds the location to change
    let newE = applyLocSubst (Dict.union xSubst ySubst) old.inputExp in
    Eval.run newE
    `Result.andThen` (\(newVal,newWidgets) ->
      -- Can't manipulate slideCount/movieCount/movieDuration/movieContinue via sliders at the moment.
      LangSvg.resolveToIndexedTree old.slideNumber old.movieNumber old.movieTime newVal
      |> Result.map (\newSlate -> (newE, newVal, newSlate, newWidgets))
    ) in

  case widget of

    WNumSlider minVal maxVal _ curVal (locid,_,_) ->
      let updateX dx =
        curVal + (dx / toFloat wSlider) * (maxVal - minVal)
          |> clamp minVal maxVal
      in
      create (Just (locid, updateX)) Nothing

    WIntSlider a b _ c (locid,_,_) ->
      let (minVal, maxVal, curVal) = (toFloat a, toFloat b, toFloat c) in
      let updateX dx =
        curVal + (dx / toFloat wSlider) * (maxVal - minVal)
          |> clamp minVal maxVal
          |> round
          |> toFloat
      in
      create (Just (locid, updateX)) Nothing

    WPointSlider (xCur, xTrace) (yCur, yTrace) ->
      create
        (Just (pickLocId xTrace, \dx -> xCur + dx))
        (Just (pickLocId yTrace, \dy -> yCur + dy))
