module InterfaceController (upstate) where

import Lang exposing (..) --For access to what makes up the Vals
import LangParser2 exposing (parseE, parseV)
import LangUnparser exposing (unparseE)
import Sync
import Eval exposing (run)
import Utils
import MicroTests
import InterfaceModel exposing (..)
import InterfaceView2 exposing (..)
import InterfaceStorage exposing (installSaveState, removeDialog)
import LangSvg exposing (toNum, toNumTr, toPoints, addi)
import ExamplesGenerated as Examples
import Config

import VirtualDom

--Core Libraries
import List 
import Dict
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
    Live _  -> mkLive_ model.syncOptions e
    Print _ -> mkLive_ model.syncOptions e
    m       -> m

refreshMode_ model = refreshMode model (Utils.fromJust model.inputExp)

refreshHighlights id zone model =
  let codeBoxInfo = model.codeBoxInfo in
  let hi = liveInfoToHighlights id zone model in
  { model | codeBoxInfo <- { codeBoxInfo | highlights <- hi } }

switchOrient m = case m of
  Vertical -> Horizontal
  Horizontal -> Vertical

toggleShowZones x = (1 + x) % showZonesModes
{- -- TODO turning off rotation zones for now
toggleShowZones x =
  let i = (1 + x) % showZonesModes in
  if | i == showZonesRot -> toggleShowZones i
     | otherwise         -> i
-}

maybeAdjustShowZones m =
  case (m.mode, m.showZones == showZonesDel) of
    (Live _, True) -> { m | showZones <- toggleShowZones m.showZones }
    _              -> m

-- may want to eventually have a maximum history length
addToHistory s h = (s :: fst h, [])

between1 i (j,k) = i `Utils.between` (j+1, k+1)

cleanExp =
  mapExp <| \e__ -> case e__ of
    EApp e0 [e1,_,_]  -> case e0.val.e__ of
      EVar "inferred" -> e1.val.e__
      _               -> e__
    EApp e0 [_,e1]    -> case e0.val.e__ of
      EVar "flow"     -> e1.val.e__
      _               -> e__
      _               -> e__
    EOp op [e1,e2]    ->
      case (op.val, e2.val.e__) of
        (Plus, EConst 0 _) -> e1.val.e__
        _                  -> e__
    _                 -> e__

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
              if | n' == n.val       -> (highlight yellow :: acc1, acc2)
                 | otherwise         ->
                     let (s, s') = (strNum n.val, strNum n') in
                     let x = (acePos n.start, String.length s' - String.length s) in
                     (highlight green :: acc1, x :: acc2)
        in
        List.foldl f ([],[]) (Set.toList locs)
      in

      let hi' =
        let g (startPos,extraChars) (old,new) =
          let bump pos = { pos | column <- pos.column + extraChars } in
          let ret new' = (old, new') in
          ret <| if
             | startPos.row    /= old.start.row    -> new
             | startPos.column >  old.start.column -> new
             | startPos.column == old.start.column -> { start = new.start, end = bump new.end }
             | startPos.column <  old.start.column -> { start = bump new.start, end = bump new.end }
        in
        -- hi has <= 4 elements, so not worrying about the redundant processing
        flip List.map hi <| \{color,range} ->
          let (_,range') = List.foldl g (range,range) stringOffsets in
          { color = color, range = range' }
      in

      { codeBoxInfo | highlights <- hi' }


--------------------------------------------------------------------------------
-- Updating the Model

upstate : Event -> Model -> Model
upstate evt old = case debugLog "Event" evt of

    Noop -> old

    Edit -> { old | editingMode <- Just old.code }

    Run ->
      case parseE old.code of
        Ok e ->
         let h = case old.editingMode of
           Nothing -> old.history
           Just "" -> old.history -- "" from InterfaceStorage
           Just s  -> addToHistory s old.history
         in
         let new =
          { old | inputExp <- Just e
                , code <- unparseE e
                , slate <- LangSvg.valToIndexedTree (Eval.run e)
                , history <- h
                , editingMode <- Nothing
                , caption <- Nothing
                , syncOptions <- Sync.syncOptionsOf e }
          in
          { new | mode <- refreshMode_ new 
                , errorBox <- Nothing }
        Err err ->
          { old | caption <- Just (LangError ("PARSE ERROR!\n" ++ err)) }

    ToggleOutput ->
      let m = case old.mode of
        Print _ -> refreshMode_ old
        _       -> Print (LangSvg.printSvg old.slate)
      in
      { old | mode <- m }

    CodeUpdate newcode -> { old | code <- newcode }

    StartResizingMid -> { old | mouseMode <- MouseResizeMid Nothing }

    MousePos (mx, my) ->
      case old.mouseMode of
        MouseNothing -> old
        MouseResizeMid Nothing ->
          let f =
            case old.orient of
              Vertical   -> \(mx',_) -> (old.midOffsetX + mx' - mx, old.midOffsetY)
              Horizontal -> \(_,my') -> (old.midOffsetY, old.midOffsetY + my' - my)
          in
          { old | mouseMode <- MouseResizeMid (Just f) }
        MouseResizeMid (Just f) ->
          let (x,y) = f (mx, my) in
          { old | midOffsetX <- x , midOffsetY <- y }
        MouseObject objid kind zone Nothing ->
          let onNewPos = createMousePosCallback mx my objid kind zone old in
          let mStuff = maybeStuff objid kind zone old in
          let blah = Just (old.code, mStuff, onNewPos) in
          { old | mouseMode <- MouseObject objid kind zone blah  }
        MouseObject _ _ _ (Just (_, mStuff, onNewPos)) ->
          let (newE,changes,newSlate) = onNewPos (mx, my) in
          { old | code <- unparseE newE
                , inputExp <- Just newE
                , slate <- newSlate
                , codeBoxInfo <- highlightChanges mStuff changes old.codeBoxInfo
                }

    SelectObject id kind zone ->
      case old.mode of
        AdHoc       -> { old | mouseMode <- MouseObject id kind zone Nothing }
        Live info ->
          case Dict.get id info.triggers of
            Nothing -> { old | mouseMode <- MouseNothing }
            Just dZones ->
              case Dict.get zone dZones of
                Just (Just _) -> { old | mouseMode <- MouseObject id kind zone Nothing }
                _             -> { old | mouseMode <- MouseNothing }
        SyncSelect _ _ _ -> old

    MouseUp ->
      case (old.mode, old.mouseMode) of
        (Print _, _) -> old
        (_, MouseObject i k z (Just (s, _, _))) ->
          -- 8/10: re-parsing to get new position info after live sync-ing
          -- TODO: could update positions within highlightChanges
          let (Ok e) = parseE old.code in
          let old' = { old | inputExp <- Just e } in
          refreshHighlights i z
            { old' | mouseMode <- MouseNothing, mode <- refreshMode_ old'
                   , history <- addToHistory s old'.history }
        _ ->
          { old | mouseMode <- MouseNothing, mode <- refreshMode_ old }

    Sync -> 
      case (old.mode, old.inputExp) of
        (Live _, _) -> Debug.crash "upstate Sync: shouldn't happen anymore"
        (AdHoc, Just ip) ->
          let
            inputval  = Eval.run ip
            inputval' = inputval |> LangSvg.valToIndexedTree
                                 |> slateToVal
            newval    = slateToVal old.slate
            local     = Sync.inferLocalUpdates old.syncOptions ip inputval' newval
            struct    = Sync.inferStructuralUpdate ip inputval' newval
            delete    = Sync.inferDeleteUpdate ip inputval' newval
            relatedG  = Sync.inferNewRelationships ip inputval' newval
            relatedV  = Sync.relateSelectedAttrs old.genSymCount ip inputval' newval
            revert    = (ip, inputval)
          in
          let mkOptions l1 l2 revert =
            ( (List.length l1, l1)
            , (List.length l2, l2)
            , revert)
          in
            case (local, relatedV) of
              (Ok [], Nothing) -> { old | mode <- mkLive_ old.syncOptions ip  }
              (Ok [], Just (nextK, eNew, vNew)) ->
                let _ = debugLog ("no live updates, only related var") () in
                let l2 = [(eNew,vNew)] in
                let m = SyncSelect (old.code, old.slate) 0 (mkOptions [] l2 revert) in
                upstate (TraverseOption 1) { old | mode <- m, genSymCount <- nextK }
              (Ok l, _) ->
                let n = debugLog "# of live updates" (List.length l) in
                let l1 = List.map fst l in
                let l2 = delete ++ relatedG ++ [struct] in
                let m = SyncSelect (old.code, old.slate) 0 (mkOptions l1 l2 revert) in
                upstate (TraverseOption 1) { old | mode <- m }
              (Err e, _) ->
                let _ = debugLog ("no live updates: " ++ e) () in
                let l2 = delete ++ relatedG ++ [struct] in
                let m = SyncSelect (old.code, old.slate) 0 (mkOptions [] l2 revert) in
                upstate (TraverseOption 1) { old | mode <- m }

    SelectOption ->
      let (SyncSelect (prevCode,_) i options) = old.mode in
      let ((n1,l1),(n2,l2),revert) = options in
      let ((ei,vi),h) =
        if | i `between1` ( 0, n1   ) -> (Utils.geti i l1, addToHistory prevCode old.history)
           | i `between1` (n1, n1+n2) -> (Utils.geti (i-n1) l2, addToHistory prevCode old.history)
           | otherwise                -> (revert, old.history)
      in
      maybeAdjustShowZones
      { old | code <- unparseE ei
            , inputExp <- Just ei
            , history <- h
            , slate <- LangSvg.valToIndexedTree vi
            , mode <- mkLive old.syncOptions ei vi }

    TraverseOption offset ->
      let (SyncSelect prev i options) = old.mode in
      let ((n1,l1),(n2,l2),revert) = options in
      let j = i + offset in
      let (ei,vi) =
        if | j `between1` ( 0, n1   ) -> Utils.geti j l1
           | j `between1` (n1, n1+n2) -> Utils.geti (j-n1) l2
           | otherwise                -> revert
      in
      { old | code <- unparseE ei
            , inputExp <- Just ei
            , slate <- LangSvg.valToIndexedTree vi
            , mode <- SyncSelect prev j options }

    SelectExample name thunk ->
      if name == Examples.scratchName then
        upstate Run { old | exName <- name, code <- old.scratchCode, history <- ([],[]) }
      else

      let {e,v} = thunk () in
      let (so, m) =
        case old.mode of
          Live _ -> let so = Sync.syncOptionsOf e in (so, mkLive so e v)
          _      -> (old.syncOptions, old.mode)
      in
      let scratchCode' =
        if old.exName == Examples.scratchName then old.code else old.scratchCode
      in
      { old | scratchCode <- scratchCode'
            , exName <- name
            , inputExp <- Just e
            , code <- unparseE e
            , history <- ([],[])
            , mode <- m
            , syncOptions <- so
            , slate <- LangSvg.valToIndexedTree v }

    SwitchMode m -> { old | mode <- m }

    SwitchOrient -> { old | orient <- switchOrient old.orient }

    ToggleZones ->
      maybeAdjustShowZones { old | showZones <- toggleShowZones old.showZones }

    Undo ->
      case (old.code, old.history) of
        (_, ([],_)) -> old                -- because of keyboard shortcuts
        (current, (s::past, future)) ->
          let new = { old | history <- (past, current::future) } in
          upstate Run (upstate (CodeUpdate s) new)

    Redo ->
      case (old.code, old.history) of
        (_, (_,[])) -> old                -- because of keyboard shorcuts
        (current, (past, s::future)) ->
          let new = { old | history <- (current::past, future) } in
          upstate Run (upstate (CodeUpdate s) new)

    KeysDown l ->
      -- let _ = Debug.log "keys" (toString l) in
      let fire evt = upstate evt old in

      case editingMode old of

        True -> if
          | l == keysEscShift   -> fire Run
          | otherwise           -> fire Noop

        False -> if

          -- events for any non-editing mode
          | l == keysO          -> fire ToggleOutput
          | l == keysP          -> fire SwitchOrient
          | l == keysShiftRight -> adjustMidOffsetX old 25
          | l == keysShiftLeft  -> adjustMidOffsetX old (-25)
          | l == keysShiftUp    -> adjustMidOffsetY old (-25)
          | l == keysShiftDown  -> adjustMidOffsetY old 25

          -- events for specific non-editing mode
          | otherwise -> case old.mode of

              Live _ -> if
                | l == keysE          -> fire Edit
                | l == keysZ          -> fire Undo
                | l == keysY          -> fire Redo
                | l == keysG          -> fire ToggleZones  -- for righties
                | l == keysH          -> fire ToggleZones  -- for lefties
                | l == keysT          -> fire (SwitchMode AdHoc)
                | l == keysS          -> fire Noop -- placeholder for Save
                | l == keysShiftS     -> fire Noop -- placeholder for Save As
                | otherwise           -> fire Noop

              AdHoc -> if
                | l == keysG          -> fire ToggleZones  -- for righties
                | l == keysH          -> fire ToggleZones  -- for lefties
                | l == keysZ          -> fire Undo
                | l == keysY          -> fire Redo
                | l == keysT          -> fire Sync
                | otherwise           -> fire Noop

              SyncSelect _ i opts -> if
                | l == keysLeft && prevButtonEnabled i       -> fire (TraverseOption (-1))
                | l == keysRight && nextButtonEnabled i opts -> fire (TraverseOption 1)
                | l == keysEnter      -> fire SelectOption
                | otherwise           -> fire Noop

              _                       -> fire Noop

    CleanCode ->
      let s' = unparseE (cleanExp (Utils.fromJust old.inputExp)) in
      let h' =
        if | old.code == s' -> old.history
           | otherwise      -> addToHistory old.code old.history
      in
      upstate Run { old | code <- s', history <- h' }

    -- Elm does not have function equivalence/pattern matching, so we need to
    -- thread these events through upstate in order to catch them to rerender
    -- appropriately (see CodeBox.elm)
    InstallSaveState -> installSaveState old
    RemoveDialog makeSave saveName -> removeDialog makeSave saveName old
    SetBasicCodeBox set -> { old | basicCodeBox <- set }

    UpdateModel f -> f old

    _ -> Debug.crash ("upstate, unhandled evt: " ++ toString evt)

adjustMidOffsetX old dx =
  case old.orient of
    Vertical   -> { old | midOffsetX <- old.midOffsetX + dx }
    Horizontal -> upstate SwitchOrient old

adjustMidOffsetY old dy =
  case old.orient of
    Horizontal -> { old | midOffsetY <- old.midOffsetY + dy }
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

  let (LangSvg.SvgNode _ attrs _) = Utils.justGet_ "#3" objid (snd old.slate) in
  let numAttr = toNum << Utils.find_ attrs in
  let mapNumAttr f a =
    let (n,trace) = toNumTr (Utils.find_ attrs a) in
    (a, LangSvg.ANum (f n, trace)) in

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
      mapNumAttr (LangSvg.clampColorNum << scaledPosX scaleColorBall) in

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
          let [cx,cy] = List.map numAttr ["cx", "cy"] in
          let dx = if toFloat mx >= cx then mx' - mx else mx - mx' in
          let dy = if toFloat my >= cy then my' - my else my - my' in
          ret [ (mapNumAttr (\r -> r + toFloat (max dx dy)) "r") ]

        ("ellipse", "Interior") -> ret [fx "cx", fy "cy"]
        ("ellipse", "Edge")     ->
          let [cx,cy] = List.map numAttr ["cx", "cy"] in
          let dx = if toFloat mx >= cx then fx else fx_ in
          let dy = if toFloat my >= cy then fy else fy_ in
          ret [dx "rx", dy "ry"]

        ("line", "Edge") -> ret [fx "x1", fx "x2", fy "y1", fy "y2"]
        ("line", _) ->
          case LangSvg.realZoneOf zone of
            LangSvg.ZPoint i -> ret [fx (addi "x" i), fy (addi "y" i)]

        ("polygon", _)  -> createCallbackPoly zone kind objid old onMouse
        ("polyline", _) -> createCallbackPoly zone kind objid old onMouse

        ("path", _) -> createCallbackPath zone kind objid old onMouse

    in
    let newTree = List.foldr (upslate objid) (snd old.slate) newRealAttrs in
      case old.mode of
        AdHoc -> (Utils.fromJust old.inputExp, Dict.empty, (fst old.slate, newTree))
        Live info ->
          case Utils.justGet_ "#4" zone (Utils.justGet_ "#5" objid info.triggers) of
            -- Nothing -> (Utils.fromJust old.inputExp, newSlate)
            Nothing -> Debug.crash "shouldn't happen due to upstate SelectObject"
            Just trigger ->
              -- let (newE,otherChanges) = trigger (List.map (Utils.mapSnd toNum) newFakeAttrs) in
              let (newE,changes) = trigger (List.map (Utils.mapSnd toNum) newFakeAttrs) in
              if not Sync.tryToBeSmart then
                (newE, changes, LangSvg.valToIndexedTree <| Eval.run newE) else
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

-- Callbacks for Polygons/Polylines

createCallbackPoly zone shape =
  let _ = Utils.assert "createCallbackPoly" (shape == "polygon" || shape == "polyline") in
  case LangSvg.realZoneOf zone of
    LangSvg.Z "Interior" -> polyInterior shape
    LangSvg.ZPoint i     -> polyPoint i shape
    LangSvg.ZEdge i      -> polyEdge i shape

-- TODO:
--  - differentiate between "polygon" and "polyline" for interior
--  - rethink/refactor point/edge zones

lift : (Num -> Num) -> (NumTr -> NumTr)
lift f (n,t) = (f n, t)

polyInterior shape objid old onMouse =
  let (Just (LangSvg.SvgNode _ nodeAttrs _)) = Dict.get objid (snd old.slate) in
  let pts = toPoints <| Utils.find_ nodeAttrs "points" in
  let accs =
    let foo (j,(xj,yj)) (acc1,acc2) =
      let (xj',yj') = (lift onMouse.posX xj, lift onMouse.posY yj) in
      let acc2' = (addi "x"j, LangSvg.ANum xj') :: (addi "y"j, LangSvg.ANum yj') :: acc2 in
      ((xj',yj')::acc1, acc2')
    in
    Utils.foldli foo ([],[]) pts
  in
  let (acc1,acc2) = Utils.reverse2 accs in
  ([("points", LangSvg.APoints acc1)], acc2)

polyPoint i shape objid old onMouse =
  let (Just (LangSvg.SvgNode _ nodeAttrs _)) = Dict.get objid (snd old.slate) in
  let pts = toPoints <| Utils.find_ nodeAttrs "points" in
  let accs =
    let foo (j,(xj,yj)) (acc1,acc2) =
      if | i /= j -> ((xj,yj)::acc1, acc2)
         | otherwise ->
             let (xj',yj') = (lift onMouse.posX xj, lift onMouse.posY yj) in
             let acc2' = (addi "x"i, LangSvg.ANum xj')
                         :: (addi "y"i, LangSvg.ANum yj')
                         :: acc2 in
             ((xj',yj')::acc1, acc2')
    in
    Utils.foldli foo ([],[]) pts
  in
  let (acc1,acc2) = Utils.reverse2 accs in
  ([("points", LangSvg.APoints acc1)], acc2)

polyEdge i shape objid old onMouse =
  let (Just (LangSvg.SvgNode _ nodeAttrs _)) = Dict.get objid (snd old.slate) in
  let pts = toPoints <| Utils.find_ nodeAttrs "points" in
  let n = List.length pts in
  let accs =
    let foo (j,(xj,yj)) (acc1,acc2) =
      if | i == j || (i == n && j == 1) || (i < n && j == i+1) ->
             let (xj',yj') = (lift onMouse.posX xj, lift onMouse.posY yj) in
             let acc2' = (addi "x"j, LangSvg.ANum xj')
                         :: (addi "y"j, LangSvg.ANum yj')
                         :: acc2 in
             ((xj',yj')::acc1, acc2')
         | otherwise ->
             ((xj,yj)::acc1, acc2)
    in
    Utils.foldli foo ([],[]) pts
  in
  let (acc1,acc2) = Utils.reverse2 accs in
  ([("points", LangSvg.APoints acc1)], acc2)

-- Callbacks for Paths

createCallbackPath zone shape =
  let _ = Utils.assert "createCallbackPath" (shape == "path") in
  case LangSvg.realZoneOf zone of
    LangSvg.ZPoint i -> pathPoint i

pathPoint i objid old onMouse =

  let updatePt (mj,(x,y)) =
    if | mj == Just i -> (mj, (lift onMouse.posX x, lift onMouse.posY y))
       | otherwise    -> (mj, (x, y)) in
  let addFakePts =
    List.foldl <| \(mj,(x,y)) acc ->
      if | mj == Just i -> (addi "x"i, LangSvg.ANum x)
                           :: (addi "y"i, LangSvg.ANum y)
                           :: acc
         | otherwise    -> acc in

  let (Just (LangSvg.SvgNode _ nodeAttrs _)) = Dict.get objid (snd old.slate) in
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
          let [pt1',pt2',pt3'] = List.map updatePt [pt1,pt2,pt3] in
          (LangSvg.CmdC s pt1' pt2' pt3', addFakePts acc2 [pt1',pt2',pt3'])
        LangSvg.CmdSQ s pt1 pt2 ->
          let [pt1',pt2'] = List.map updatePt [pt1,pt2] in
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
  ([("d", LangSvg.APath2 (acc1, counts))], acc2)

-- Callbacks for Rotate zones

createCallbackRotate mx0 my0 mx1 my1 shape objid old =
  let (Just (LangSvg.SvgNode _ nodeAttrs _)) = Dict.get objid (snd old.slate) in
  let (rot,cx,cy) = LangSvg.toTransformRot <| Utils.find_ nodeAttrs "transform" in
  let rot' =
    let a0 = Utils.radiansToDegrees <| atan ((mx0 - fst cx) / (fst cy - my0)) in
    let a1 = Utils.radiansToDegrees <| atan ((fst cy - my1) / (mx1 - fst cx)) in
    (fst rot + (90 - a0 - a1), snd rot) in
  let real = [("transform", LangSvg.ATransform [LangSvg.Rot rot' cx cy])] in
  let fake = [("transformRot", LangSvg.ANum rot')] in
  (real, fake)

