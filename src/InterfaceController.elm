module InterfaceController (upstate) where

import Lang exposing (..) --For access to what makes up the Vals
import LangParser exposing (parseE, parseV)
import Sync
import Eval exposing (run)
import Utils
import MicroTests
import InterfaceModel exposing (..)
import InterfaceView2 exposing (..)
import LangSvg exposing (toNum, toNumTr, toPoints, addi)
import ExamplesGenerated as Examples

import VirtualDom

--Core Libraries
import List 
import Dict
import String 
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

slateToVal : LangSvg.RootedIndexedTree -> Val
slateToVal (rootId, tree) =
  let foo n =
    case n of
      LangSvg.TextNode s -> VList [VBase (String "TEXT"), VBase (String s)]
      LangSvg.SvgNode kind l1 l2 ->
        let vs1 = List.map LangSvg.valOfAttr l1 in
        let vs2 = List.map (foo << flip Utils.justGet tree) l2 in
        VList [VBase (String kind), VList vs1, VList vs2]
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

switchOrient m = case m of
  Vertical -> Horizontal
  Horizontal -> Vertical


--------------------------------------------------------------------------------
-- Updating the Model

upstate : Event -> Model -> Model
upstate evt old = case Debug.log "Event" evt of

    Noop -> old

    Edit -> { old | editingMode <- True }

    Run ->
      case parseE old.code of
        Ok e ->
          { old | inputExp <- Just e
                , code <- sExp e
                , slate <- LangSvg.valToIndexedTree (Eval.run e)
                , editingMode <- False
                , caption <- Nothing
                , mode <- refreshMode old e }
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
        MouseObject (objid, kind, zone, Nothing) ->
          let onNewPos = createMousePosCallback mx my objid kind zone old in
          { old | mouseMode <- MouseObject (objid, kind, zone, Just onNewPos) }
        MouseObject (_, _, _, Just onNewPos) ->
          let (newE,newSlate) = onNewPos (mx, my) in
          { old | code <- sExp newE
                , inputExp <- Just newE
                , slate <- newSlate }

    SelectObject id kind zone ->
      case old.mode of
        AdHoc       -> { old | mouseMode <- MouseObject (id, kind, zone, Nothing) }
        Live info ->
          let dZones = Utils.justGet_ "#1" id info.triggers in
          case Dict.get zone dZones of
            Just (Just _) -> { old | mouseMode <- MouseObject (id, kind, zone, Nothing) }
            _             -> { old | mouseMode <- MouseNothing }
        SyncSelect _ _ -> old

    MouseUp ->
      case old.mode of
        Print _ -> old
        _       -> { old | mouseMode <- MouseNothing, mode <- refreshMode_ old }

    Sync -> 
      case (old.mode, old.inputExp) of
        (Live _, _) -> Debug.crash "upstate Sync: shouldn't happen anymore"
        (AdHoc, Just ip) ->
          let
            inputval  = Eval.run ip
            inputval' = inputval |> LangSvg.valToIndexedTree
                                 |> slateToVal
            newval    = slateToVal old.slate
            struct    = Sync.inferStructuralUpdate ip inputval' newval
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
      { old | code <- sExp ei
            , inputExp <- Just ei
            , slate <- LangSvg.valToIndexedTree vi
            , mode <- mkLive old.syncOptions ei vi }

    TraverseOption offset ->
      let (SyncSelect i options) = old.mode in
      let (_,l) = options in
      let j = i + offset in
      let (ei,vi) = Utils.geti j l in
      { old | code <- sExp ei
            , inputExp <- Just ei
            , slate <- LangSvg.valToIndexedTree vi
            , mode <- SyncSelect j options }

    SelectExample name thunk ->
      if name == Examples.scratchName then
        upstate Run { old | exName <- name, code <- old.scratchCode }
      else

      let {e,v} = thunk () in
      let m =
        case old.mode of
          Live _ -> mkLive old.syncOptions e v
          _      -> old.mode
      in
      let scratchCode' =
        if old.exName == Examples.scratchName then old.code else old.scratchCode
      in
      { old | scratchCode <- scratchCode'
            , exName <- name
            , inputExp <- Just e
            , code <- sExp e
            , mode <- m
            , slate <- LangSvg.valToIndexedTree v }

    SwitchMode m -> { old | mode <- m }

    SwitchOrient -> { old | orient <- switchOrient old.orient }

    ToggleZones -> { old | showZones <- not old.showZones }

    ToggleThawed ->
      let so = old.syncOptions in
      let so' = { so | thawedByDefault <- not so.thawedByDefault } in
      let model' = { old | syncOptions <- so' } in
      { model' | mode <- refreshMode_ model' }

    UpdateModel f -> f old

    _ -> Debug.crash ("upstate, unhandled evt: " ++ toString evt)


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

        ("path", _) -> createCallbackPath zone kind objid old onMouse

    in
    let newTree = List.foldr (upslate objid) (snd old.slate) newRealAttrs in
      case old.mode of
        AdHoc -> (Utils.fromJust old.inputExp, (fst old.slate, newTree))
        Live info ->
          case Utils.justGet_ "#4" zone (Utils.justGet_ "#5" objid info.triggers) of
            -- Nothing -> (Utils.fromJust old.inputExp, newSlate)
            Nothing -> Debug.crash "shouldn't happen due to upstate SelectObject"
            Just trigger ->
              let (newE,otherChanges) = trigger (List.map (Utils.mapSnd toNum) newFakeAttrs) in
              if not Sync.tryToBeSmart then
                (newE, LangSvg.valToIndexedTree <| Eval.run newE) else
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

