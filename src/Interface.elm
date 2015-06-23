-- Interface.elm
-- This defines and renders an interactive interface for editing the
-- program and output of the language as defined in int-trees.

--Import the little language and its parsing utilities
import Lang exposing (..) --For access to what makes up the Vals
import LangParser exposing (parseE, parseV)
import Sync
import Eval exposing (run)
import Utils
import MicroTests
import InterfaceUtils exposing (..)
import InterfaceView2 exposing (..)
import LangSvg exposing (IndexedTree, NodeId, ShapeKind, toNum, toNumTr, toPoints, addi)
import Examples

import VirtualDom

--Core Libraries
import List 
import Dict
import String 
import Graphics.Element as GE 
import Graphics.Collage as GC

--Signaling Libraries
import Mouse 
import Window 

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


sampleModel =
  let
    (name,f) = Utils.head_ Examples.list
    {e,v} = f ()
    (rootId,slate) = LangSvg.valToIndexedTree v
  in
    { scratchCode  = Examples.blank
    , exName       = name
    , code         = sExp e
    , inputExp     = Just e
    , rootId       = rootId
    , workingSlate = slate
    , mode         = mkLive Sync.defaultOptions e v
    , mouseMode    = MouseNothing
    , orient       = Vertical
    , midOffsetX   = 0
    , midOffsetY   = -100
    , showZones    = False
    , syncOptions  = Sync.defaultOptions
    , editingMode  = False
    }

refreshMode model e =
  case model.mode of
    Live _ -> mkLive_ model.syncOptions e
    Print  -> mkLive_ model.syncOptions e
    m      -> m

refreshMode_ model = refreshMode model (Utils.fromJust model.inputExp)

upstate : Event -> Model -> Model
upstate evt old = case Debug.log "Event" evt of

    Noop -> old

    Edit -> { old | editingMode <- True }

    Run ->
      case parseE old.code of
        Ok e ->
          let v = Eval.run e in
          let (rootId,slate) = LangSvg.valToIndexedTree v in
          { old | inputExp <- Just e
                , code <- sExp e
                , rootId <- rootId
                , workingSlate <- slate
                , editingMode <- False
                , mode <- refreshMode old e }
        Err err ->
          { old | code <- "PARSE ERROR!\n\n" ++ err }

    ToggleOutput ->
      let m = case old.mode of
        Print -> refreshMode_ old
        _     -> Print
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
        MouseObject (objid, kind, zone, Nothing) ->
          let onNewPos = createMousePosCallback mx my objid kind zone old in
          { old | mouseMode <- MouseObject (objid, kind, zone, Just onNewPos) }
        MouseObject (_, _, _, Just onNewPos) ->
          let (newE,newSlate) = onNewPos (mx, my) in
          { old | code <- sExp newE
                , inputExp <- Just newE
                , workingSlate <- newSlate }

    SelectObject id kind zone ->
      case old.mode of
        AdHoc       -> { old | mouseMode <- MouseObject (id, kind, zone, Nothing) }
        Live triggers ->
          case Utils.justGet zone (Utils.justGet id triggers) of
            Nothing -> { old | mouseMode <- MouseNothing }
            Just _  -> { old | mouseMode <- MouseObject (id, kind, zone, Nothing) }
        SyncSelect _ _ -> old

    DeselectObject ->
      case old.mode of
        Print -> old
        _     -> { old | mouseMode <- MouseNothing, mode <- refreshMode_ old }

    Sync -> 
      case (old.mode, old.inputExp) of
        (Live _, _) -> Debug.crash "upstate Sync: shouldn't happen anymore"
        (AdHoc, Just ip) ->
          let
            inputval  = Eval.run ip
            inputval' = inputval |> LangSvg.valToIndexedTree
                                 |> snd
                                 |> indexedTreeToVal old.rootId
            newval    = indexedTreeToVal old.rootId old.workingSlate
            dummyE    = Utils.fromOk_ (parseE "; TODO infer structural update
                                               (svg [])")
            struct    = (dummyE, Eval.run dummyE)
            revert    = (ip, inputval)
          in
            case Sync.inferLocalUpdates old.syncOptions ip inputval' newval of
              Ok [] -> { old | mode <- mkLive_ old.syncOptions ip  }
              Ok ls ->
                let n = Debug.log "# of sync options" (List.length ls) in
                let ls' = List.map fst ls in
                let m = SyncSelect 0 (n, ls' ++ [struct, revert]) in
                upstate (TraverseOption 1) { old | mode <- m }
              Err e ->
                let _ = Debug.log ("bad sync: ++ " ++ e) () in
                let m = SyncSelect 0 (0, [struct, revert]) in
                upstate (TraverseOption 1) { old | mode <- m }

    SelectOption ->
      let (SyncSelect i options) = old.mode in
      let (_,l) = options in
      let (ei,vi) = Utils.geti i l in
      let (rootId,tree) = LangSvg.valToIndexedTree vi in
      { old | code <- sExp ei
            , inputExp <- Just ei
            , rootId <- rootId
            , workingSlate <- tree
            , mode <- mkLive old.syncOptions ei vi }

    TraverseOption offset ->
      let (SyncSelect i options) = old.mode in
      let (_,l) = options in
      let j = i + offset in
      let (ei,vi) = Utils.geti j l in
      let (rootId,tree) = LangSvg.valToIndexedTree vi in
      { old | code <- sExp ei
            , inputExp <- Just ei
            , rootId <- rootId
            , workingSlate <- tree
            , mode <- SyncSelect j options }

    SelectExample "Scratch" _ ->
      upstate Run { old | exName <- "Scratch", code <- old.scratchCode }

    SelectExample name thunk ->
      let {e,v} = thunk () in
      let (rootId,tree) = LangSvg.valToIndexedTree v in
      let m =
        case old.mode of
          Live _ -> mkLive old.syncOptions e v
          _      -> old.mode
      in
      { old | scratchCode <- if | old.exName == "Scratch" -> old.code
                                | otherwise               -> old.scratchCode
            , exName <- name
            , inputExp <- Just e
            , code <- sExp e
            , mode <- m
            , rootId <- rootId
            , workingSlate <- tree }

    SwitchMode m -> { old | mode <- m }

    SwitchOrient -> { old | orient <- switchOrient old.orient }

    ToggleZones -> { old | showZones <- not old.showZones }

    ToggleThawed ->
      let so = old.syncOptions in
      let so' = { so | thawedByDefault <- not so.thawedByDefault } in
      let model' = { old | syncOptions <- so' } in
      { model' | mode <- refreshMode_ model' }

    _ -> Debug.crash ("upstate, unhandled evt: " ++ toString evt)

type alias OnMouse =
  { posX : Num -> Num , posY : Num -> Num
  , negX : Num -> Num , negY : Num -> Num
  -- , posXposY : Num -> Num
  }

createMousePosCallback mx my objid kind zone old =

  let (LangSvg.SvgNode _ attrs _) = Utils.justGet objid old.workingSlate in
  let numAttr = toNum << Utils.find_ attrs in
  let mapNumAttr f a =
    let (n,trace) = toNumTr (Utils.find_ attrs a) in
    (a, LangSvg.ANum (f n, trace)) in

  \(mx',my') ->

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

    let ret l = (l, l) in

    let (newRealAttrs,newFakeAttrs) =
      case (kind, zone) of

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

        -- ("polygon", _)  -> createCallbackPoly zone kind objid old posX' posY'
        -- ("polyline", _) -> createCallbackPoly zone kind objid old posX' posY'
        ("polygon", _)  -> createCallbackPoly zone kind objid old onMouse
        ("polyline", _) -> createCallbackPoly zone kind objid old onMouse

    in
    let newSlate = List.foldr (upslate objid) old.workingSlate newRealAttrs in
      case old.mode of
        AdHoc -> (Utils.fromJust old.inputExp, newSlate)
        Live triggers ->
          case Utils.justGet zone (Utils.justGet objid triggers) of
            -- Nothing -> (Utils.fromJust old.inputExp, newSlate)
            Nothing -> Debug.crash "shouldn't happen due to upstate SelectObject"
            Just trigger ->
              let (newE,otherChanges) = trigger (List.map (Utils.mapSnd toNum) newFakeAttrs) in
              if not Sync.tryToBeSmart then
                (newE, snd <| LangSvg.valToIndexedTree <| Eval.run newE) else
              let newSlate' =
                Dict.foldl (\j dj acc1 ->
                  let _ = Debug.crash "TODO: dummyTrace is probably a problem..." in
                  Dict.foldl
                    (\a n acc2 -> upslate j (a, LangSvg.ANum (n, dummyTrace)) acc2) acc1 dj
                  ) newSlate otherChanges
              in
              (newE, newSlate')

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
  let (Just (LangSvg.SvgNode _ nodeAttrs _)) = Dict.get objid old.workingSlate in
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
  let (Just (LangSvg.SvgNode _ nodeAttrs _)) = Dict.get objid old.workingSlate in
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
  let (Just (LangSvg.SvgNode _ nodeAttrs _)) = Dict.get objid old.workingSlate in
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

main : Signal GE.Element
main = let sigModel = Signal.foldp upstate sampleModel
                        <| Signal.mergeMany
                            [ events.signal
                            , Signal.map2 (,) Mouse.isDown Mouse.position
                                |> Signal.filter (\(x,y) -> x) (False, (0,0))
                                |> Signal.map (\(x,y) -> y)
                                |> Signal.map2 adjustCoords Window.dimensions
                                |> Signal.map MousePos
                            ]
       in Signal.map2 view Window.dimensions sigModel
