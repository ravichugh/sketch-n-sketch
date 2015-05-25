-- Interface.elm
-- This defines and renders an interactive interface for editing the
-- program and output of the language as defined in int-trees.

--Import the little language and its parsing utilities
import Lang exposing (..) --For access to what makes up the Vals
import LangParser exposing (parseE, parseV)
import Sync exposing (sync, Triggers)
import Eval exposing (run)
import MainSvg
import Utils
import MicroTests
import InterfaceUtils exposing (..)
import InterfaceView exposing (..)
import LangSvg exposing (IndexedTree, NodeId, ShapeKind, toNum, toPoints, addi)
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


-- tempTest = MicroTests.test42 ()
-- tempTest = MicroTests.test20 ()
-- tempTest = MicroTests.test31 ()
tempTest = MicroTests.test41 ()
-- tempTest = MicroTests.test43 ()

sampleModel = { code      = sExp tempTest.e
              , inputExp  = Just tempTest.e
              , movingObj = Nothing
              , workingSlate = LangSvg.valToIndexedTree tempTest.v
              , mode  = Live <| Sync.prepareLiveUpdates tempTest.e tempTest.v
              }

upstate : Event -> Model -> Model
upstate evt old = case Debug.log "Event" evt of

    Render ->
      let e = parseE old.code in
      let v = Eval.run e in
      { old | inputExp <- Just e
            , workingSlate <- LangSvg.valToIndexedTree v }

    CodeUpdate newcode -> { old | code <- newcode }

    MousePos (mx, my) ->
      case old.movingObj of

        Nothing -> old

        Just (objid, kind, zone, Nothing) ->
          let (_,attrs) = buildSvg (objid, Utils.justGet objid old.workingSlate) in
          let numAttr   = toNum << Utils.find_ attrs in
          let onNewPos (mx',my') =
            let posX n = n - toFloat mx + toFloat mx' in
            let posY n = n - toFloat my + toFloat my' in
            let negX n = n + toFloat mx - toFloat mx' in
            let negY n = n + toFloat my - toFloat my' in
            let fx   a = (a, aNum <| posX (numAttr a)) in
            let fy   a = (a, aNum <| posY (numAttr a)) in
            let fx_  a = (a, aNum <| negX (numAttr a)) in
            let fy_  a = (a, aNum <| negY (numAttr a)) in
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
                  ret [ ("r", aNum <| numAttr "r" + toFloat (max dx dy)) ]

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
                      let (xj',yj') = (posX xj, posY yj) in
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
                                 let (xj',yj') = (posX xj, posY yj) in
                                 let acc2' = (addi "x"i, LangSvg.ANum xj') :: (addi "y"i, LangSvg.ANum yj') :: acc2 in
                                 ((xj',yj')::acc1, acc2')
                        in
                        Utils.foldli foo ([],[]) pts
                      in
                      let (acc1,acc2) = Utils.reverse2 accs in
                      ([("points", LangSvg.APoints acc1)], acc2)

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
                        (newE, LangSvg.valToIndexedTree <| Eval.run newE) else
                      let newSlate' =
                        Dict.foldl (\j dj acc1 ->
                          Dict.foldl
                            (\a n acc2 -> upslate j (a, aNum n) acc2) acc1 dj
                          ) newSlate otherChanges
                      in
                      (newE, newSlate')
          in
          { old | movingObj <- Just (objid, kind, zone, Just onNewPos) }

        Just (objid, kind, zone, Just onNewPos) ->
          let (newE,newSlate) = onNewPos (mx, my) in
          { old | code <- sExp newE
                , inputExp <- Just newE
                , workingSlate <- newSlate }

    SelectObject id kind zone -> { old | movingObj <- Just (id, kind, zone, Nothing) }

    DeselectObject x ->
      let e = Utils.fromJust old.inputExp in
      { old | movingObj <- Nothing
            , mode      <- Live <| Sync.prepareLiveUpdates e (Eval.run e) }

    Sync -> 
        case (old.mode, old.inputExp) of
            (Live _, _) -> Debug.crash "upstate Sync: shouldn't happen anymore"
            (AdHoc, Just ip) ->
                let inputval = Eval.run ip
                    inputval' = indexedTreeToVal (LangSvg.valToIndexedTree inputval)
                    newval = indexedTreeToVal old.workingSlate
                in
                  case sync ip inputval' newval of
                    Ok [] -> old
                    Ok ls -> { old | mode <- SyncSelect ls }
                    Err e -> Debug.crash ("upstate Sync: ++ " ++ e)

    SelectOption ((e,v), f) -> { old | inputExp <- Just e, mode <- AdHoc }

    SwitchMode m -> { old | mode <- m }

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
