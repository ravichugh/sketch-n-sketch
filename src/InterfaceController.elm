module InterfaceController (upstate) where

import Lang exposing (..) --For access to what makes up the Vals
import LangParser2 exposing (parseE, freshen)
import LangUnparser exposing (unparse, preceedingWhitespace, addPreceedingWhitespace)
import Sync
import Eval
import Utils
import InterfaceModel exposing (..)
import InterfaceView2 as View
import InterfaceStorage exposing (installSaveState, removeDialog)
import LangSvg exposing (toNum, toNumTr, toPoints, addi)
import ExamplesGenerated as Examples
import Config exposing (params)

import VirtualDom

--Core Libraries
import List
import Dict
import Set
import String
import Char
import Graphics.Element as GE
import Graphics.Collage as GC
import Regex as Re

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
      LangSvg.TextNode s -> vList [vBase (String "TEXT"), vBase (String s)]
      LangSvg.SvgNode kind l1 l2 ->
        let vs1 = List.map LangSvg.valOfAttr l1 in
        let vs2 = List.map (foo << flip Utils.justGet tree) l2 in
        vList [vBase (String kind), vList vs1, vList vs2]
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
    Live _  -> mkLive_ model.syncOptions model.slideNumber model.movieNumber model.movieTime e
    Print _ -> mkLive_ model.syncOptions model.slideNumber model.movieNumber model.movieTime e
    m       -> m

refreshMode_ model = refreshMode model model.inputExp

refreshHighlights id zone model =
  let codeBoxInfo = model.codeBoxInfo in
  let hi = liveInfoToHighlights id zone model in
  { model | codeBoxInfo = { codeBoxInfo | highlights = hi } }

switchOrient m = case m of
  Vertical -> Horizontal
  Horizontal -> Vertical

toggleShowZones x = (1 + x) % showZonesModeCount
{- -- TODO turning off rotation zones for now
toggleShowZones x =
  let i = (1 + x) % showZonesModeCount in
  if | i == showZonesRot -> toggleShowZones i
     | otherwise         -> i
-}

-- if delete mode is not applicable but set, use oldMode instead
maybeLeaveDeleteMode newModel oldShowZones =
  case (newModel.mode, newModel.showZones == showZonesDel) of
    (Live _, True) -> { newModel | showZones = oldShowZones }
    _              -> newModel

-- may want to eventually have a maximum history length
addToHistory s h = (s :: fst h, [])

between1 i (j,k) = i `Utils.between` (j+1, k+1)

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
  let (slate, code) = slateAndCode old (exp, val) in
  (exp, val, slate, code)

slateAndCode old (exp, val) =
  let slate =
    LangSvg.resolveToIndexedTree old.slideNumber old.movieNumber old.movieTime val
  in
  (slate, unparse exp)

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
    hGut    = params.mainSection.horizontal.hGut
    hCode_  = (snd old.dimensions - hMid - 2*hGut) // 2 + hMid
    hCode   = hCode_ + old.midOffsetY
    -- TODO consider hideCode and hideCanvas
    hMid    = params.mainSection.widgets.hBtn
  in
    ( params.wGut
    , params.topSection.h + hCode + hMid
    )

strCall = Sync.strCall
gray    = "420"

addLineToCodeAndRun old (x2,y2) (x1,y1) =
  addToCodeAndRun old <|
    strCall "line" ("'gray'":: "5" :: List.map toString [x1, y1, x2, y2])

addRectToCodeAndRun old (x2,y2) (x1,y1) =
  let
    (xa, xb)     = (min x1 x2, max x1 x2)
    (ya, yb)     = (min y1 y2, max y1 y2)
    (x, y, w, h) = (xa, ya, xb - xa, yb - ya)
  in
  addToCodeAndRun old <|
    strCall "rect" (gray :: List.map toString [x, y, w, h])

addEllipseToCodeAndRun old (x2,y2) (x1,y1) =
  let
    (xa, xb) = (min x1 x2, max x1 x2)
    (ya, yb) = (min y1 y2, max y1 y2)
    (rx, ry) = ((xb-xa)//2, (yb-ya)//2)
    (cx, cy) = (xa + rx, ya + ry)
  in
  addToCodeAndRun old <|
    strCall "ellipse" (gray :: List.map toString [cx, cy, rx, ry])

addPolygonToCodeAndRun old points =
  let sPoints =
    Utils.bracks <| Utils.spaces <|
      List.map (\(x,y) -> Utils.bracks (Utils.spaces (List.map toString [x,y])))
               (List.reverse points)
  in
  addToCodeAndRun old <|
    strCall "polygon" [gray, "'black'", "2", sPoints]

addToCodeAndRun old newShape =
  -- the updated code can be made cleaner if top-level declarations are kept
  -- separate from the "main" expression
  let code =
    let oldCode = Re.replace Re.All (Re.regex "\n") (always "\n  ") old.code in
    let tmp = "previous" ++ toString old.genSymCount in
    "(let " ++ tmp ++ "\n" ++
    "  " ++ oldCode ++ "\n" ++
    strCall "addShapeToCanvas" [tmp, newShape] ++ ")"
  in
  upstate Run
    { old | code = code
          , history = addToHistory old.code old.history
          , genSymCount = old.genSymCount + 1
          , mouseMode = MouseNothing }

switchToCursorTool old =
  { old | mouseMode = MouseNothing , toolType = Cursor }


nodeIdAndAttrNameToVal (nodeId, attrName) tree =
  case Dict.get nodeId tree of
    Just (LangSvg.SvgNode _ attrs _) ->
      case Utils.maybeFind attrName attrs of
        Just aval -> Just (LangSvg.valOfAVal aval)
        Nothing   -> Debug.crash <| "nodeIdAndAttrNameToVal " ++ (toString nodeId) ++ " " ++ (toString attrName) ++ " " ++ (toString tree)
    Just (LangSvg.TextNode _) -> Nothing
    Nothing                   -> Debug.crash <| "nodeIdAndAttrNameToVal " ++ (toString nodeId) ++ " " ++ (toString tree)

pluckSelectedVals selectedAttrs slate =
  let (_, tree) = slate in
  let foo nodeIdAndAttrName acc =
    case nodeIdAndAttrNameToVal nodeIdAndAttrName tree of
      Just val -> val :: acc
      Nothing  -> acc
  in
  Set.foldl foo [] selectedAttrs


--------------------------------------------------------------------------------
-- Updating the Model

upstate : Event -> Model -> Model
upstate evt old = case debugLog "Event" evt of

    Noop -> old

    WindowDimensions wh -> { old | dimensions = wh }

    Edit -> { old | editingMode = Just old.code }

    Run ->
      case parseE old.code of
        Ok e ->
         let h = case old.editingMode of
           Nothing -> old.history
           Just "" -> old.history -- "" from InterfaceStorage
           Just s  -> addToHistory s old.history
         in
         let (newVal,ws) = (Eval.run e) in
         let (newSlideCount, newMovieCount, newMovieDuration, newMovieContinue, newSlate) = LangSvg.fetchEverything old.slideNumber old.movieNumber 0.0 newVal in
         let new =
           { old | inputExp      = e
                 , inputVal      = newVal
                 , code          = unparse e
                 , slideCount    = newSlideCount
                 , movieCount    = newMovieCount
                 , movieTime     = 0
                 , movieDuration = newMovieDuration
                 , movieContinue = newMovieContinue
                 , runAnimation  = newMovieDuration > 0
                 , slate         = newSlate
                 , widgets       = ws
                 , history       = h
                 , editingMode   = Nothing
                 , caption       = Nothing
                 , syncOptions   = Sync.syncOptionsOf old.syncOptions e }
          in
          { new | mode = refreshMode_ new
                , errorBox = Nothing }
        Err err ->
          { old | caption = Just (LangError ("PARSE ERROR!\n" ++ err)) }

    StartAnimation -> upstate Redraw { old | movieTime = 0
                                           , runAnimation = True }

    Redraw ->
      case old.inputVal of
        val ->
          let (newSlideCount, newMovieCount, newMovieDuration, newMovieContinue, newSlate) = LangSvg.fetchEverything old.slideNumber old.movieNumber old.movieTime val in
          { old | slideCount    = newSlideCount
                , movieCount    = newMovieCount
                , movieDuration = newMovieDuration
                , movieContinue = newMovieContinue
                , slate         = newSlate }

    ToggleOutput ->
      let m = case old.mode of
        Print _ -> refreshMode_ old
        _       -> Print (LangSvg.printSvg old.showWidgets old.slate)
      in
      { old | mode = m }

    CodeUpdate newcode -> { old | code = newcode }

    StartResizingMid ->
      if old.hideCode then old
      else if old.hideCanvas then old
      else { old | mouseMode = MouseResizeMid Nothing }

    MouseClickCanvas ->
      case (old.mouseMode, old.toolType) of
        (MouseNothing, Line) -> { old | mouseMode = MouseDrawNew "line" [] }
        (MouseNothing, Rect) -> { old | mouseMode = MouseDrawNew "rect" [] }
        (MouseNothing, Oval) -> { old | mouseMode = MouseDrawNew "ellipse" [] }
        (MouseNothing, Poly) -> { old | mouseMode = MouseDrawNew "polygon" [] }
        _                    ->   old

    MouseClick click ->
      case old.mouseMode of
        MouseDrawNew "polygon" points ->
          let pointOnCanvas = clickToCanvasPoint old click in
          let add () =
            let points' = pointOnCanvas :: points in
            { old | mouseMode = MouseDrawNew "polygon" points' }
          in
          if points == [] then add ()
          else
            let initialPoint = Utils.last_ points in
            if Utils.distanceInt pointOnCanvas initialPoint > View.drawNewPolygonDotSize then add ()
            else if List.length points == 2 then { old | mouseMode = MouseNothing }
            else if List.length points == 1 then switchToCursorTool old
            else addPolygonToCodeAndRun old points
        _ ->
          old

    MousePos (mx0, my0) ->
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

        MouseObject objid kind zone Nothing ->
          let onNewPos = createMousePosCallback mx my objid kind zone old in
          let mStuff = maybeStuff objid kind zone old in
          let blah = Just (old.code, mStuff, onNewPos) in
          { old | mouseMode = MouseObject objid kind zone blah }

        MouseObject _ _ _ (Just (_, mStuff, onNewPos)) ->
          let (newE,newV,changes,newSlate,newWidgets) = onNewPos (mx, my) in
          { old | code = unparse newE
                , inputExp = newE
                , inputVal = newV
                , slate = newSlate
                , widgets = newWidgets
                , codeBoxInfo = highlightChanges mStuff changes old.codeBoxInfo
                }

        MouseSlider widget Nothing ->
          let onNewPos = createMousePosCallbackSlider mx my widget old in
          { old | mouseMode = MouseSlider widget (Just (old.code, onNewPos)) }

        MouseSlider widget (Just (_, onNewPos)) ->
          let (newE,newV,newSlate,newWidgets) = onNewPos (mx, my) in
          { old | code = unparse newE
                , inputExp = newE
                , inputVal = newV
                , slate = newSlate
                , widgets = newWidgets
                }

        MouseDrawNew "polygon" _ -> old -- handled by MouseClick instead

        MouseDrawNew k [] ->
          let pointOnCanvas = (mx, my) in
          { old | mouseMode = MouseDrawNew k [pointOnCanvas, pointOnCanvas] }

        MouseDrawNew k (_::points) ->
          let pointOnCanvas = (mx, my) in
          { old | mouseMode = MouseDrawNew k (pointOnCanvas::points) }

    SelectObject id kind zone ->
      case old.mode of
        AdHoc       -> { old | mouseMode = MouseObject id kind zone Nothing }
        Live info ->
          case Dict.get id info.triggers of
            Nothing -> { old | mouseMode = MouseNothing }
            Just dZones ->
              case Dict.get zone dZones of
                Just (Just _) -> { old | mouseMode = MouseObject id kind zone Nothing }
                _             -> { old | mouseMode = MouseNothing }
        _ -> old

    MouseUp ->
      case (old.mode, old.mouseMode) of

        (Print _, _) -> old

        (_, MouseObject i k z (Just (s, _, _))) ->
          -- 8/10: re-parsing to get new position info after live sync-ing
          -- TODO: could update positions within highlightChanges
          -- TODO: update inputVal?
          let e = Utils.fromOk_ <| parseE old.code in
          let old' = { old | inputExp = e } in
          refreshHighlights i z
            { old' | mouseMode = MouseNothing, mode = refreshMode_ old'
                   , history = addToHistory s old'.history }

        (_, MouseSlider _ (Just (s, _))) ->
          let e = Utils.fromOk_ <| parseE old.code in
          let old' = { old | inputExp = e } in
            { old' | mouseMode = MouseNothing, mode = refreshMode_ old'
                   , history = addToHistory s old'.history }

        (_, MouseDrawNew _ [])                 -> switchToCursorTool old
        (_, MouseDrawNew "line" [pt2, pt1])    -> addLineToCodeAndRun old pt2 pt1
        (_, MouseDrawNew "rect" [pt2, pt1])    -> addRectToCodeAndRun old pt2 pt1
        (_, MouseDrawNew "ellipse" [pt2, pt1]) -> addEllipseToCodeAndRun old pt2 pt1
        (_, MouseDrawNew "polygon" points)     -> old

        _ -> { old | mouseMode = MouseNothing, mode = refreshMode_ old }

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


    RelateAttrs ->
      let selectedVals = debugLog "selectedVals" <| pluckSelectedVals old.selectedAttrs old.slate in
      let revert = (old.inputExp, old.inputVal) in
      let (nextK, l) = Sync.relate old.genSymCount old.inputExp selectedVals in
      let possibleChanges = List.map (addSlateAndCode old) l in
        { old | mode = SyncSelect possibleChanges
              , genSymCount = nextK
              , selectedAttrs = Set.empty -- TODO
              , runAnimation = True
              , syncSelectTime = 0.0
              }

    DigHole ->
      let selectedVals =
        debugLog "selectedVals" <|
          pluckSelectedVals old.selectedAttrs old.slate
      in
      let traces =
        List.map valToTrace selectedVals
      in
      let tracesLocsets =
        List.map (Sync.locsOfTrace old.syncOptions) traces
      in
      let locset =
        List.foldl Set.union Set.empty tracesLocsets
      in
      let locsetList =
        Set.toList locset
      in
      let isLocsetNode exp =
        case exp.val.e__ of
          EConst ws n loc wd -> Set.member loc locset
          _                  -> False
      in
      let locToNumber =
        let accumulateLocToNumbers exp__ dict =
          case exp__ of
            EConst ws n loc wd ->
              if Set.member loc locset then
                Dict.insert loc n dict
              else
                dict
            _ -> dict
        in
        foldExpViaE__
            accumulateLocToNumbers
            Dict.empty
            old.inputExp
      in
      let locsAncestors = debugLog "locsAncestors" <|
        findAllWithAncestors isLocsetNode old.inputExp
      in
      -- isScope needs to see the node's parent...because case statements
      -- produce many scopes out of one expression
      -- The below adds a maybe parent to each node, so we get List (List
      -- (Maybe Exp, Exp))
      let locsAncestorsWithParents = debugLog "locsAncestorsWithParents" <|
        List.map
            (\locAncestors ->
              Utils.zip (Nothing :: (List.map Just locAncestors)) locAncestors
            )
            locsAncestors
      in
      let locsAncestorScopesWithParents = debugLog "locsAncestorScopesWithParents" <|
        List.map
            (List.filter (\(parent, node) -> isScope parent node))
            locsAncestorsWithParents
      in
      let deepestCommonScopeWithParent = debugLog "deepestCommonAncestorWithParent" <|
        -- If no common scope, we will wrap the root node.
        let commonPrefix = debugLog "commonPrefix" <|
          [(Nothing, old.inputExp)] ++
          Utils.commonPrefix locsAncestorScopesWithParents
        in
        Utils.last_ commonPrefix
      in
      let (deepestCommonScopeParent, deepestCommonScope) =
        deepestCommonScopeWithParent
      in
      -- Avoid name collisions here
      let locIdNameOrigNamePrime =
        List.map
            (\(locId, frozen, ident) -> (locId, "k"++(toString locId)++ident++"Orig", "k"++(toString locId)++ident++"Prime"))
            locsetList
      in
      let locIdToNewName = debugLog "locIdToNewName" <|
        Dict.fromList
          <| List.map (\(locId, nameOrig, namePrime) -> (locId, namePrime))
          <| locIdNameOrigNamePrime
      in
      let replaceConstsWithVars exp__ =
        case exp__ of
          EConst ws n (locId, frozen, ident) wd ->
            case Dict.get locId locIdToNewName of
              Just newName -> EVar ws newName
              Nothing      -> exp__
          _ -> exp__
      in
      let commonScopeReplaced =
        mapExpViaExp__ replaceConstsWithVars deepestCommonScope
      in
      let newlyWrappedCommonScope =
        let origNames  = List.map Utils.snd3 locIdNameOrigNamePrime in
        let primeNames = List.map Utils.thd3 locIdNameOrigNamePrime in
        let valueStrs =
          List.map
              (\loc ->
                toString (Utils.justGet loc locToNumber)
              )
              locsetList
        in
        let oldPreceedingWhitespace = preceedingWhitespace commonScopeReplaced in
        let extraWhitespace =
          if String.contains "\n" oldPreceedingWhitespace then "" else "\n"
        in
        let templateStr =
          let variableOrigNamesStr  = String.join " " origNames in
          let variablePrimeNamesStr = String.join " " primeNames in
          let variableValuesStr     = String.join " " valueStrs in
          oldPreceedingWhitespace ++
          "(let ["++variableOrigNamesStr++"] ["++variableValuesStr++"]" ++
          extraWhitespace ++ oldPreceedingWhitespace ++
          "(let ["++variablePrimeNamesStr++"] ["++variableOrigNamesStr++"] 'dummy body'))"
        in
        let template =
          case parseE templateStr of
            Ok templateExp -> templateExp
            Err err        -> Debug.crash <| "Dig template err: " ++ err
        in
        -- Now replace the dummy body:
        let newLet =
          mapExpViaExp__
              (\e__ ->
                case e__ of
                  EBase _ (String "dummy body") -> (addPreceedingWhitespace extraWhitespace commonScopeReplaced).val.e__
                  _                             -> e__
              )
              template
        in
        newLet
      in
      -- Debug only:
      let newSubtreeStr = debugLog "newlyWrappedCommonScope" <| unparse newlyWrappedCommonScope in
      let newExp =
        freshen <|
        replaceExpNode deepestCommonScope newlyWrappedCommonScope old.inputExp
      in
      let (newVal, newWidgets) = Eval.run newExp in
      let (newSlate, newCode)  = slateAndCode old (newExp, newVal) in
      debugLog "new model" <|
        { old | code          = newCode
              , inputExp      = newExp
              , inputVal      = newVal
              , history       = addToHistory old.code old.history
              , slate         = newSlate
              , widgets       = newWidgets
              , previewCode   = Nothing
              , mode          = mkLive old.syncOptions old.slideNumber old.movieNumber old.movieTime newExp newVal }


    RelateShapes ->
      let newval = slateToVal old.slate in
      let l = Sync.inferNewRelationships old.inputExp old.inputVal newval in
      let possibleChanges = List.map (addSlateAndCode old) l in
        { old | mode = SyncSelect possibleChanges, runAnimation = True, syncSelectTime = 0.0 }

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
          let addSlateAndCodeToAll list = List.map (addSlateAndCode old) list in
            case (local, relatedV) of
              (Ok [], (_, [])) -> { old | mode = mkLive_ old.syncOptions old.slideNumber old.movieNumber old.movieTime ip }
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

    SelectOption (exp, val, slate, code) ->
      maybeLeaveDeleteMode
        { old | code          = code
              , inputExp      = exp
              , inputVal      = val
              , history       = addToHistory old.code old.history
              , slate         = slate
              , previewCode   = Nothing
              , toolType      = Cursor
              , mode          = mkLive old.syncOptions old.slideNumber old.movieNumber old.movieTime exp val }
        showZonesNone


    PreviewCode maybeCode ->
      { old | previewCode = maybeCode }

    CancelSync ->
      upstate Run { old | mode = mkLive_ old.syncOptions old.slideNumber old.movieNumber old.movieTime old.inputExp }

    SelectExample name thunk ->
      if name == Examples.scratchName then
        upstate Run { old | exName = name, code = old.scratchCode, history = ([],[]) }
      else

      let {e,v,ws} = thunk () in
      let (so, m) =
        case old.mode of
          Live _  -> let so = Sync.syncOptionsOf old.syncOptions e in (so, mkLive so old.slideNumber old.movieNumber old.movieTime e v)
          Print _ -> let so = Sync.syncOptionsOf old.syncOptions e in (so, mkLive so old.slideNumber old.movieNumber old.movieTime e v)
          _      -> (old.syncOptions, old.mode)
      in
      let scratchCode' =
        if old.exName == Examples.scratchName then old.code else old.scratchCode
      in
      let (slideCount, movieCount, movieDuration, movieContinue, slate) = LangSvg.fetchEverything old.slideNumber old.movieNumber old.movieTime v in
      { old | scratchCode   = scratchCode'
            , exName        = name
            , inputExp      = e
            , inputVal      = v
            , code          = unparse e
            , history       = ([],[])
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
            }

    SwitchMode m -> { old | mode = m }

    SwitchOrient -> { old | orient = switchOrient old.orient }

    SelectZonesMode i ->
      maybeLeaveDeleteMode { old | showZones = i } old.showZones

    Undo ->
      case (old.code, old.history) of
        (_, ([],_)) -> old                -- because of keyboard shortcuts
        (current, (s::past, future)) ->
          let new = { old | history = (past, current::future) } in
          upstate Run (upstate (CodeUpdate s) new)

    Redo ->
      case (old.code, old.history) of
        (_, (_,[])) -> old                -- because of keyboard shorcuts
        (current, (past, s::future)) ->
          let new = { old | history = (current::past, future) } in
          upstate Run (upstate (CodeUpdate s) new)

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
        let previousSlideNumber    = old.slideNumber - 1 in
        case old.inputExp of
          exp ->
            let previousVal = fst <| Eval.run exp in
            let previousMovieCount = LangSvg.resolveToMovieCount previousSlideNumber previousVal in
            upstate StartAnimation { old | slideNumber = previousSlideNumber
                                         , movieNumber = previousMovieCount }

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
      -- let _ = Debug.log "keys" (toString l) in
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

    CleanCode ->
      let s' = unparse (cleanExp old.inputExp) in
      let h' =
        if old.code == s'
          then old.history
          else addToHistory old.code old.history
      in
      upstate Run { old | code = s', history = h' }

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
-- Key Combinations

keysMetaShift           = List.sort [keyMeta, keyShift]
keysEscShift            = List.sort [keyEsc, keyShift]
keysEnter               = List.sort [keyEnter]
keysE                   = List.sort [Char.toCode 'E']
keysZ                   = List.sort [Char.toCode 'Z']
keysY                   = List.sort [Char.toCode 'Y']
-- keysShiftZ              = List.sort [keyShift, Char.toCode 'Z']
keysG                   = List.sort [Char.toCode 'G']
keysH                   = List.sort [Char.toCode 'H']
keysO                   = List.sort [Char.toCode 'O']
keysP                   = List.sort [Char.toCode 'P']
keysT                   = List.sort [Char.toCode 'T']
keysS                   = List.sort [Char.toCode 'S']
keysShiftS              = List.sort [keyShift, Char.toCode 'S']
keysLeft                = List.sort [keyLeft]
keysRight               = List.sort [keyRight]
keysUp                  = List.sort [keyUp]
keysDown                = List.sort [keyDown]
keysShiftLeft           = List.sort [keyShift, keyLeft]
keysShiftRight          = List.sort [keyShift, keyRight]
keysShiftUp             = List.sort [keyShift, keyUp]
keysShiftDown           = List.sort [keyShift, keyDown]

keyEnter                = 13
keyEsc                  = 27
keyMeta                 = 91
keyCtrl                 = 17
keyShift                = 16
keyLeft                 = 37
keyUp                   = 38
keyRight                = 39
keyDown                 = 40


--------------------------------------------------------------------------------
-- Mouse Callbacks for Zones

type alias OnMouse =
  { posX : Num -> Num , posY : Num -> Num
  , negX : Num -> Num , negY : Num -> Num
  -- , posXposY : Num -> Num
  }

createMousePosCallback mx my objid kind zone old =
 case Utils.justGet_ "#3" objid (snd old.slate) of
 LangSvg.TextNode _ -> Debug.crash "createMousePosCallback TextNode"
 LangSvg.SvgNode _ attrs _ ->
  let numAttr = toNum << Utils.find_ attrs in
  let mapNumAttr f a =
    let av = Utils.find_ attrs a in
    let (n,trace) = toNumTr av in
    (a, LangSvg.AVal (LangSvg.ANum (f n, trace)) av.vtrace) in
      -- preserve existing VTrace

  \(mx',my') ->

    let scaledPosX scale n = n + scale * (toFloat mx' - toFloat mx) in

    let posX n = n - toFloat mx + toFloat mx' in
    let posY n = n - toFloat my + toFloat my' in
    let negX n = n + toFloat mx - toFloat mx' in
    let negY n = n + toFloat my - toFloat my' in

    -- let posXposY n =
    --   let dx = toFloat mx - toFloat mx' in
    --   let dy = toFloat my - toFloat my' in
    --   if | abs dx >= abs dy  -> n - dx
    --      | otherwise         -> n - dy in

    let onMouse =
      { posX = posX, posY = posY, negX = negX, negY = negY } in

    -- let posX' (n,tr) = (posX n, tr) in
    -- let posY' (n,tr) = (posY n, tr) in
    -- let negX' (n,tr) = (negX n, tr) in
    -- let negY' (n,tr) = (negY n, tr) in

    let fx  = mapNumAttr posX in
    let fy  = mapNumAttr posY in
    let fx_ = mapNumAttr negX in
    let fy_ = mapNumAttr negY in

    let fxColorBall =
      mapNumAttr (LangSvg.clampColorNum << scaledPosX View.scaleColorBall) in

    let ret l = (l, l) in

    let (newRealAttrs,newFakeAttrs) =
      case (kind, zone) of

        -- first match zones that can be attached to different shape kinds...

        (_, "FillBall")   -> ret [fxColorBall "fill"]
        (_, "RotateBall") -> createCallbackRotate (toFloat mx) (toFloat my)
                                                  (toFloat mx') (toFloat my')
                                                  kind objid old

        -- ... and then match each kind of shape separately

        ("rect", "Interior")       -> ret [fx "x", fy "y"]
        ("rect", "RightEdge")      -> ret [fx "width"]
        ("rect", "BotRightCorner") -> ret [fx "width", fy "height"]
        ("rect", "BotEdge")        -> ret [fy "height"]
        ("rect", "BotLeftCorner")  -> ret [fx "x", fx_ "width", fy "height"]
        ("rect", "LeftEdge")       -> ret [fx "x", fx_ "width"]
        ("rect", "TopLeftCorner")  -> ret [fx "x", fy "y", fx_ "width", fy_ "height"]
        ("rect", "TopEdge")        -> ret [fy "y", fy_ "height"]
        ("rect", "TopRightCorner") -> ret [fy "y", fx "width", fy_ "height"]

        ("circle", "Interior") -> ret [fx "cx", fy "cy"]
        ("circle", "Edge") ->
          let (cx,cy) = Utils.unwrap2 <| List.map numAttr ["cx", "cy"] in
          let dx = if toFloat mx >= cx then mx' - mx else mx - mx' in
          let dy = if toFloat my >= cy then my' - my else my - my' in
          ret [ (mapNumAttr (\r -> r + toFloat (max dx dy)) "r") ]

        ("ellipse", "Interior") -> ret [fx "cx", fy "cy"]
        ("ellipse", "Edge")     ->
          let (cx,cy) = Utils.unwrap2 <| List.map numAttr ["cx", "cy"] in
          let dx = if toFloat mx >= cx then fx else fx_ in
          let dy = if toFloat my >= cy then fy else fy_ in
          ret [dx "rx", dy "ry"]

        ("line", "Edge") -> ret [fx "x1", fx "x2", fy "y1", fy "y2"]
        ("line", _) ->
          case LangSvg.realZoneOf zone of
            LangSvg.ZPoint i -> ret [fx (addi "x" i), fy (addi "y" i)]
            _                -> Debug.crash "createMousePosCallback line"

        ("polygon", _)  -> createCallbackPoly zone kind objid old onMouse
        ("polyline", _) -> createCallbackPoly zone kind objid old onMouse

        ("path", _) -> createCallbackPath zone kind objid old onMouse

        _ -> Debug.crash "createMousePosCallback"

    in
    let newTree = List.foldr (upslate objid) (snd old.slate) newRealAttrs in
      case old.mode of
        AdHoc -> (old.inputExp, old.inputVal, Dict.empty, (fst old.slate, newTree), old.widgets)
        Live info ->
          case Utils.justGet_ "#4" zone (Utils.justGet_ "#5" objid info.triggers) of
            -- Nothing -> (Utils.fromJust old.inputExp, newSlate)
            Nothing -> Debug.crash "shouldn't happen due to upstate SelectObject"
            Just trigger ->
              -- let (newE,otherChanges) = trigger (List.map (Utils.mapSnd toNum) newFakeAttrs) in
              let (newE,changes) = trigger (List.map (Utils.mapSnd toNum) newFakeAttrs) in
              if not Sync.tryToBeSmart then
                let (newV,newWidgets) = Eval.run newE in
                (newE, newV, changes, LangSvg.resolveToIndexedTree old.slideNumber old.movieNumber old.movieTime newV, newWidgets)
              else
                Debug.crash "Controller tryToBeSmart"
              {-
              let newSlate' =
                Dict.foldl (\j dj acc1 ->
                  let _ = Debug.crash "TODO: dummyTrace is probably a problem..." in
                  Dict.foldl
                    (\a n acc2 -> upslate j (a, LangSvg.ANum (n, dummyTrace)) acc2) acc1 dj
                  ) newSlate otherChanges
              in
              (newE, newSlate')
              -}
        _ -> Debug.crash "createMousePosCallback"

-- Callbacks for Polygons/Polylines

createCallbackPoly zone shape =
  let _ = Utils.assert "createCallbackPoly" (shape == "polygon" || shape == "polyline") in
  case LangSvg.realZoneOf zone of
    LangSvg.Z "Interior" -> polyInterior shape
    LangSvg.ZPoint i     -> polyPoint i shape
    LangSvg.ZEdge i      -> polyEdge i shape
    _                    -> Debug.crash "createCallbackPoly"

-- TODO:
--  - differentiate between "polygon" and "polyline" for interior
--  - rethink/refactor point/edge zones

lift : (Num -> Num) -> (NumTr -> NumTr)
lift f (n,t) = (f n, t)

-- TODO everywhere aNum, aTransform, etc is called, preserve vtrace

polyInterior shape objid old onMouse =
  case Dict.get objid (snd old.slate) of
    Just (LangSvg.SvgNode _ nodeAttrs _) ->
      let pts = toPoints <| Utils.find_ nodeAttrs "points" in
      let accs =
        let foo (j,(xj,yj)) (acc1,acc2) =
          let (xj',yj') = (lift onMouse.posX xj, lift onMouse.posY yj) in
          let acc2' = (addi "x"j, LangSvg.aNum xj') :: (addi "y"j, LangSvg.aNum yj') :: acc2 in
          ((xj',yj')::acc1, acc2')
        in
        Utils.foldli foo ([],[]) pts
      in
      let (acc1,acc2) = Utils.reverse2 accs in
      ([("points", LangSvg.aPoints acc1)], acc2)
    _ ->
      Debug.crash "polyInterior"

polyPoint i shape objid old onMouse =
  case Dict.get objid (snd old.slate) of
    Just (LangSvg.SvgNode _ nodeAttrs _) ->
      let pts = toPoints <| Utils.find_ nodeAttrs "points" in
      let accs =
        let foo (j,(xj,yj)) (acc1,acc2) =
          if i /= j
            then ((xj,yj)::acc1, acc2)
            else let (xj',yj') = (lift onMouse.posX xj, lift onMouse.posY yj) in
                 let acc2' = (addi "x"i, LangSvg.aNum xj')
                             :: (addi "y"i, LangSvg.aNum yj')
                             :: acc2 in
                 ((xj',yj')::acc1, acc2')
        in
        Utils.foldli foo ([],[]) pts
      in
      let (acc1,acc2) = Utils.reverse2 accs in
      ([("points", LangSvg.aPoints acc1)], acc2)
    _ ->
      Debug.crash "polyPoint"

polyEdge i shape objid old onMouse =
  case Dict.get objid (snd old.slate) of
    Just (LangSvg.SvgNode _ nodeAttrs _) ->
      let pts = toPoints <| Utils.find_ nodeAttrs "points" in
      let n = List.length pts in
      let accs =
        let foo (j,(xj,yj)) (acc1,acc2) =
          if i == j || (i == n && j == 1) || (i < n && j == i+1) then
            let (xj',yj') = (lift onMouse.posX xj, lift onMouse.posY yj) in
            let acc2' = (addi "x"j, LangSvg.aNum xj')
                        :: (addi "y"j, LangSvg.aNum yj')
                        :: acc2 in
            ((xj',yj')::acc1, acc2')
          else
            ((xj,yj)::acc1, acc2)
        in
        Utils.foldli foo ([],[]) pts
      in
      let (acc1,acc2) = Utils.reverse2 accs in
      ([("points", LangSvg.aPoints acc1)], acc2)
    _ ->
      Debug.crash "polyEdge"

-- Callbacks for Paths

createCallbackPath zone shape =
  let _ = Utils.assert "createCallbackPath" (shape == "path") in
  case LangSvg.realZoneOf zone of
    LangSvg.ZPoint i -> pathPoint i
    _                -> Debug.crash "createCallbackPath"

pathPoint i objid old onMouse =

  let updatePt (mj,(x,y)) =
    if mj == Just i
      then (mj, (lift onMouse.posX x, lift onMouse.posY y))
      else (mj, (x, y)) in
  let addFakePts =
    List.foldl <| \(mj,(x,y)) acc ->
      if mj == Just i
        then (addi "x"i, LangSvg.aNum x) :: (addi "y"i, LangSvg.aNum y) :: acc
        else acc in

  case Dict.get objid (snd old.slate) of
    Just (LangSvg.SvgNode _ nodeAttrs _) ->
      let (cmds,counts) = LangSvg.toPath <| Utils.find_ nodeAttrs "d" in
      let accs =
        let foo c (acc1,acc2) =
          let (c',acc2') = case c of
            LangSvg.CmdZ s ->
              (LangSvg.CmdZ s, acc2)
            LangSvg.CmdMLT s pt ->
              let pt' = updatePt pt in
              (LangSvg.CmdMLT s pt', addFakePts acc2 [pt'])
            LangSvg.CmdHV s n ->
              (LangSvg.CmdHV s n, acc2)
            LangSvg.CmdC s pt1 pt2 pt3 ->
              let (pt1',pt2',pt3') = Utils.unwrap3 <| List.map updatePt [pt1,pt2,pt3] in
              (LangSvg.CmdC s pt1' pt2' pt3', addFakePts acc2 [pt1',pt2',pt3'])
            LangSvg.CmdSQ s pt1 pt2 ->
              let (pt1',pt2') = Utils.unwrap2 <| List.map updatePt [pt1,pt2] in
              (LangSvg.CmdSQ s pt1' pt2' , addFakePts acc2 [pt1',pt2'])
            LangSvg.CmdA s a b c d e pt ->
              let pt' = updatePt pt in
              (LangSvg.CmdA s a b c d e pt', addFakePts acc2 [pt'])
          in
          (c' :: acc1, acc2')
        in
        List.foldr foo ([],[]) cmds
      in
      let (acc1,acc2) = Utils.reverse2 accs in
      ([("d", LangSvg.aPath2 (acc1, counts))], acc2)

    _ ->
      Debug.crash "pathPoint"

-- Callbacks for Rotate zones

createCallbackRotate mx0 my0 mx1 my1 shape objid old =
  case Dict.get objid (snd old.slate) of
    Just (LangSvg.SvgNode _ nodeAttrs _) ->
      let (rot,cx,cy) = LangSvg.toTransformRot <| Utils.find_ nodeAttrs "transform" in
      let rot' =
        let a0 = Utils.radiansToDegrees <| atan2 (fst cy - my0) (mx0 - fst cx) in
        let a1 = Utils.radiansToDegrees <| atan2 (fst cy - my1) (mx1 - fst cx) in
        (fst rot + (a0 - a1), snd rot) in
      let real = [("transform", LangSvg.aTransform [LangSvg.Rot rot' cx cy])] in
      let fake = [("transformRot", LangSvg.aNum rot')] in
      (real, fake)
    _ -> Debug.crash "createCallbackRotate"


--------------------------------------------------------------------------------
-- Mouse Callbacks for UI Widgets

wSlider = params.mainSection.uiWidgets.wSlider

createMousePosCallbackSlider mx my widget old =

  let (maybeRound, minVal, maxVal, curVal, locid) =
    case widget of
      WIntSlider a b _ curVal (locid,_,_) ->
        (toFloat << round, toFloat a, toFloat b, toFloat curVal, locid)
      WNumSlider a b _ curVal (locid,_,_) ->
        (identity, a, b, curVal, locid)
  in
  let range = maxVal - minVal in

  \(mx',my') ->
    let newVal =
      curVal + (toFloat (mx' - mx) / toFloat wSlider) * range
        |> clamp minVal maxVal
        |> maybeRound
    in
    -- unlike the live triggers via Sync,
    -- this substitution only binds the location to change
    let subst = Dict.singleton locid newVal in
    let newE = applyLocSubst subst old.inputExp in
    let (newVal,newWidgets) = Eval.run newE in
    -- Can't manipulate slideCount/movieCount/movieDuration/movieContinue via sliders at the moment.
    let newSlate = LangSvg.resolveToIndexedTree old.slideNumber old.movieNumber old.movieTime newVal in
    (newE, newVal, newSlate, newWidgets)
