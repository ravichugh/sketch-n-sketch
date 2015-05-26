-- Interface.elm
-- This defines and renders an interactive interface for editing the
-- program and output of the language as defined in int-trees.

--Import the little language and its parsing utilities
import Lang exposing (..) --For access to what makes up the Vals
import LangParser exposing (parseE, parseV)
import Sync
import Eval exposing (run)
import MainSvg
import Utils
import MicroTests
import InterfaceUtils exposing (..)
import InterfaceView exposing (..)
import LangSvg exposing (IndexedTree, NodeId, ShapeKind, toNum, toNumTr, toPoints, addi)
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


tempTest = MicroTests.test42 ()
sampleModel =
  let (rootId,slate) = LangSvg.valToIndexedTree tempTest.v in
    { code         = sExp tempTest.e
    , inputExp     = Just tempTest.e
    , movingObj    = Nothing
    , rootId       = rootId
    , workingSlate = slate
    , mode         = Live <| Sync.prepareLiveUpdates tempTest.e tempTest.v
    , ui           = {orient = Vertical}
    }

upstate : Event -> Model -> Model
upstate evt old = case Debug.log "Event" evt of

    Render ->
      let e = parseE old.code in
      let v = Eval.run e in
      let (rootId,slate) = LangSvg.valToIndexedTree v in
      { old | inputExp <- Just e, rootId <- rootId , workingSlate <- slate }

    CodeUpdate newcode -> { old | code <- newcode }

    MousePos (mx, my) ->
      case (old.mode, old.movingObj) of

        (NoDirectMan, _) -> old

        (_, Nothing) -> old

        (_, Just (objid, kind, zone, Nothing)) ->

          let (_,attrs) =
            buildSvg False (objid, Utils.justGet objid old.workingSlate) in
          let numAttr = toNum << Utils.find_ attrs in
          let mapNumAttr f a =
            let (n,trace) = toNumTr (Utils.find_ attrs a) in
            (a, LangSvg.ANum (f n, trace)) in

          let onNewPos (mx',my') =

            let posX n = n - toFloat mx + toFloat mx' in
            let posY n = n - toFloat my + toFloat my' in
            let negX n = n + toFloat mx - toFloat mx' in
            let negY n = n + toFloat my - toFloat my' in

            let posX' (n,tr) = (posX n, tr) in
            let posY' (n,tr) = (posY n, tr) in
            let negX' (n,tr) = (negX n, tr) in
            let negY' (n,tr) = (negY n, tr) in

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

                -- TODO rethink/refactor polygon zones

                ("polygon", "Interior") ->
                  let (Just (LangSvg.SvgNode _ nodeAttrs _)) = Dict.get objid old.workingSlate in
                  let pts = toPoints <| Utils.find_ nodeAttrs "points" in
                  let accs =
                    let foo (j,(xj,yj)) (acc1,acc2) =
                      let (xj',yj') = (posX' xj, posY' yj) in
                      let acc2' = (addi "x"j, LangSvg.ANum xj') :: (addi "y"j, LangSvg.ANum yj') :: acc2 in
                      ((xj',yj')::acc1, acc2')
                    in
                    Utils.foldli foo ([],[]) pts
                  in
                  let (acc1,acc2) = Utils.reverse2 accs in
                  ([("points", LangSvg.APoints acc1)], acc2)

                ("polygon", _) ->
                  case LangSvg.realZoneOf zone of
                    LangSvg.ZPoint i ->
                      let (Just (LangSvg.SvgNode _ nodeAttrs _)) = Dict.get objid old.workingSlate in
                      let pts = toPoints <| Utils.find_ nodeAttrs "points" in
                      let accs =
                        let foo (j,(xj,yj)) (acc1,acc2) =
                          if | i /= j -> ((xj,yj)::acc1, acc2)
                             | otherwise ->
                                 let (xj',yj') = (posX' xj, posY' yj) in
                                 let acc2' = (addi "x"i, LangSvg.ANum xj') :: (addi "y"i, LangSvg.ANum yj') :: acc2 in
                                 ((xj',yj')::acc1, acc2')
                        in
                        Utils.foldli foo ([],[]) pts
                      in
                      let (acc1,acc2) = Utils.reverse2 accs in
                      ([("points", LangSvg.APoints acc1)], acc2)
                    LangSvg.ZEdge i ->
                      let (Just (LangSvg.SvgNode _ nodeAttrs _)) = Dict.get objid old.workingSlate in
                      let pts = toPoints <| Utils.find_ nodeAttrs "points" in
                      let n = List.length pts in
                      let accs =
                        let foo (j,(xj,yj)) (acc1,acc2) =
                          if | i == j || (i == n && j == 1) || (i < n && j == i+1) ->
                                 let (xj',yj') = (posX' xj, posY' yj) in
                                 let acc2' = (addi "x"j, LangSvg.ANum xj') :: (addi "y"j, LangSvg.ANum yj') :: acc2 in
                                 ((xj',yj')::acc1, acc2')
                             | otherwise ->
                                 ((xj,yj)::acc1, acc2)
                        in
                        Utils.foldli foo ([],[]) pts
                      in
                      let (acc1,acc2) = Utils.reverse2 accs in
                      ([("points", LangSvg.APoints acc1)], acc2)

                ("polyline", _) -> Debug.crash "TODO polyline zone callbacks"

            in
            let newSlate = List.foldr (upslate objid) old.workingSlate newRealAttrs in
              case old.mode of
                AdHoc -> (Utils.fromJust old.inputExp, newSlate)
                Live triggers ->
                  case Utils.justGet zone (Utils.justGet objid triggers) of
                    Nothing -> (Utils.fromJust old.inputExp, newSlate)
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
          in
          { old | movingObj <- Just (objid, kind, zone, Just onNewPos) }

        (_, Just (objid, kind, zone, Just onNewPos)) ->
          let (newE,newSlate) = onNewPos (mx, my) in
          { old | code <- sExp newE
                , inputExp <- Just newE
                , workingSlate <- newSlate }

    SelectObject id kind zone ->
      case old.mode of
        NoDirectMan -> Debug.crash "SelectObject shouldn't be triggered in NoDirectMan mode"
        _           -> { old | movingObj <- Just (id, kind, zone, Nothing) }

    DeselectObject ->
      let mode' =
        case old.mode of
          Live _ -> let e = Utils.fromJust old.inputExp in
                    Live <| Sync.prepareLiveUpdates e (Eval.run e)
          m      -> m   -- since top-level SVG may have triggered this event,
                        -- preserve the current mode
      in
      { old | movingObj <- Nothing , mode <- mode' }

    Sync -> 
        case (old.mode, old.inputExp) of
            (Live _, _) -> Debug.crash "upstate Sync: shouldn't happen anymore"
            (AdHoc, Just ip) ->
                let inputval  = Eval.run ip
                    inputval' = inputval |> LangSvg.valToIndexedTree
                                         |> snd
                                         |> indexedTreeToVal old.rootId
                    newval    = indexedTreeToVal old.rootId old.workingSlate
                in
                  case Sync.sync ip inputval' newval of
                    Ok [] -> old
                    Ok ls -> let _ = Debug.log "# of sync options" (List.length ls) in
                             { old | mode <- SyncSelect ls }
                    Err e -> Debug.crash ("upstate Sync: ++ " ++ e)

    SelectOption ((e,v), f) -> { old | inputExp <- Just e, mode <- AdHoc }

    SelectTest i ->
      let {e,v} = (Utils.geti (i - 14) MainSvg.tests') () in
      let (rootId,tree) = LangSvg.valToIndexedTree v in
      { old | inputExp <- Just e
            , code <- sExp e
            , rootId <- rootId
            , workingSlate <- tree }

    SwitchMode m -> { old | mode <- m }

    UIupdate u -> { old | ui <- u }

    _ -> Debug.crash ("upstate, unhandled evt: " ++ toString evt)


main : Signal Html.Html
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
