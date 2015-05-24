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
import LangSvg exposing (IndexedTree, NodeId, ShapeKind)
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

sampleModel = { code      = sExp tempTest.e
              , inputExp  = Just tempTest.e
              , movingObj = Nothing
              , workingSlate = LangSvg.valToIndexedTree tempTest.v
              , mode  = Live <| Sync.prepareLiveUpdates tempTest.e tempTest.v
              , ui = {orient = Vertical}
              }

upstate : Event -> Model -> Model
upstate evt old = case Debug.log "Event" evt of

    Render m ->
      let e = parseE old.code in
      let v = Eval.run e in
      case m of
        Nothing -> { old | inputExp <- Just e
                   , workingSlate <- LangSvg.valToIndexedTree v }
        Just x -> { old | inputExp <- Just x
                   , workingSlate <- 
                      LangSvg.valToIndexedTree (Eval.run x)
                 }

    CodeUpdate newcode -> { old | code <- newcode }

    MousePos (mx, my) ->
      case old.movingObj of

        Nothing -> old

        Just (objid, kind, zone, Nothing) ->
          let (_,attrs) = buildSvg (objid, Utils.justGet objid old.workingSlate) in
          let numAttr   = toFloat_ << Utils.find_ attrs in
          let onNewPos (mx',my') =
            let fx x  = (x, aNum <| numAttr x - toFloat mx + toFloat mx') in
            let fy y  = (y, aNum <| numAttr y - toFloat my + toFloat my') in
            let fx_ x = (x, aNum <| numAttr x + toFloat mx - toFloat mx') in
            let fy_ y = (y, aNum <| numAttr y + toFloat my - toFloat my') in
            let newAttrs =
              case (kind, zone) of

                ("rect", "Interior")       -> [fx "x", fy "y"]
                ("rect", "RightEdge")      -> [fx "width"]
                ("rect", "BotRightCorner") -> [fx "width", fy "height"]
                ("rect", "BotEdge")        -> [fy "height"]
                ("rect", "BotLeftCorner")  -> [fx "x", fx_ "width", fy "height"]
                ("rect", "LeftEdge")       -> [fx "x", fx_ "width"]
                ("rect", "TopLeftCorner")  -> [fx "x", fy "y", fx_ "width", fy_ "height"]
                ("rect", "TopEdge")        -> [fy "y", fy_ "height"]
                ("rect", "TopRightCorner") -> [fy "y", fx "width", fy_ "height"]

                ("circle", "Interior") -> [fx "cx", fy "cy"]
                ("circle", "Edge") ->
                  -- TODO to make stretching more intuitive, take orientation
                  -- of init click w.r.t center into account
                  let (dx,dy) = (mx' - mx, my' - my) in
                  [ ("r", aNum <| numAttr "r" + toFloat (max dx dy)) ]

                ("ellipse", "Interior") -> [fx "cx", fy "cy"]
                ("ellipse", "Edge")     -> [fx "rx", fy "ry"]
            in
            let newSlate = List.foldr (upslate objid) old.workingSlate newAttrs in
              case old.mode of
                AdHoc -> (Utils.fromJust old.inputExp, newSlate)
                Live triggers ->
                  let trigger = Utils.justGet zone (Utils.justGet objid triggers) in
                  let newAttrs' = List.map (Utils.mapSnd toFloat_) newAttrs in
                  let (newE,otherChanges) = trigger newAttrs' in
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
