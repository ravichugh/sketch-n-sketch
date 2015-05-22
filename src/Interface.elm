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

--A Sample model for working with a given microtest
sampleModel = { code      = sExp tempTest.e
              , inputExp  = Just tempTest.e
              , objects   = buildVisual <| LangSvg.valToIndexedTree tempTest.v 
              , movingObj = Nothing
              , workingSlate = LangSvg.valToIndexedTree tempTest.v
              , possibleChanges = []
              , mode  = Live
              , triggers = Sync.prepareLiveUpdates tempTest.e tempTest.v
              }

-- Update --
upstate : Event -> Model -> Model
upstate evt old = case Debug.log "Event" evt of
    --make a val from code and display it
    Render ->
      let res = Debug.log "run" <| Eval.run <| parseE old.code in
      { old | objects      <- buildVisual <| LangSvg.valToIndexedTree res
            , workingSlate <- LangSvg.valToIndexedTree res }

    --Replace old code with new code
    CodeUpdate newcode -> { old | code <- newcode }

    --check if a mouse position is within an object when a mouse down
    --event occured.
    MousePos (mx, my) ->
      case old.movingObj of

        Nothing -> old

        Just (objid, kind, zone, Nothing) ->
          let (_,attrs) = buildSvg (objid, get_ objid old.workingSlate) in
          let numAttr   = toFloat_ << Utils.find_ attrs in
          let onNewPos (mx',my') =
            let fx x  = (x, numAttr x - toFloat mx + toFloat mx') in
            let fy y  = (y, numAttr y - toFloat my + toFloat my') in
            let fx_ x = (x, numAttr x + toFloat mx - toFloat mx') in
            let fy_ y = (y, numAttr y + toFloat my - toFloat my') in
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
                  [ ("r", numAttr "r" + toFloat (max dx dy)) ]

                ("ellipse", "Interior") -> [fx "cx", fy "cy"]
                ("ellipse", "Edge")     -> [fx "rx", fy "ry"]
            in
            let trigger = get_ zone (get_ objid old.triggers) in
            let (newE,otherChanges) = trigger newAttrs in
            let slate =
              case old.mode of
                AdHoc -> old.workingSlate
                Live ->
                  Dict.foldl (\j dj acc1 ->
                    Dict.foldl
                      (\a n acc2 -> upslate j (a, toString n) acc2) acc1 dj
                    ) old.workingSlate otherChanges in
            let newAttrs' = List.map (Utils.mapSnd toString) newAttrs in
            let newSlate  = List.foldr (upslate objid) slate newAttrs' in
            (newE, newSlate)
          in
          { old | movingObj <- Just (objid, kind, zone, Just onNewPos) }

        Just (objid, kind, zone, Just onNewPos) ->
          let (newE,newSlate) = onNewPos (mx, my) in
          let (code',inputExp') =
            case old.mode of
              Live -> (sExp newE, Just newE)
              _    -> (old.code, old.inputExp)
          in
          { old | code <- code'
                , inputExp <- inputExp'
                , workingSlate <- newSlate
                , objects <- buildVisual newSlate }

        -- -- rkc: what does "dist"ance refer to?
        -- Just (objid, zone, xdist, ydist) -> if
        --     | xdist == dummyInit || ydist == dummyInit -> 
        --         let (svg,attrs) = buildSvg (objid, get_ objid old.workingSlate)
        --             xpos = toFloat_ <| Utils.find_ attrs "x"
        --             ypos = toFloat_ <| Utils.find_ attrs "y"
        --             obj' = Just (objid , zone , xpos - Basics.toFloat mx , ypos - Basics.toFloat my) 
        --         in { old | movingObj <- obj' }
        --     | otherwise -> 
        --         let newpos = [ ("x", toString <| Basics.toFloat mx + xdist)
        --                      , ("y", toString <| Basics.toFloat my + ydist) ]
        --             newSlate = List.foldr (updateSlate objid) old.workingSlate newpos
        --             newobjs = buildVisual newSlate
        --         in  { old | objects <- newobjs, workingSlate <- newSlate }

    --Selecting a given zone within an object
    SelectObject id kind zone -> { old | movingObj <- Just (id, kind, zone, Nothing) }

    --wipes out selection of an object
    DeselectObject x ->
      let e = Utils.fromJust old.inputExp in
      { old | movingObj <- Nothing
            , triggers  <- Sync.prepareLiveUpdates e (Eval.run e) }

    --run sync function and then populate the list of possibleChanges
    Sync -> 
        case (old.mode, old.inputExp) of
            (Live, _) -> { old | mode <- AdHoc } -- shouldn't happen anymore
            (AdHoc, Just ip) ->
                let inputval = Eval.run ip
                    inputval' = indexedTreeToVal (LangSvg.valToIndexedTree inputval)
                    newval = indexedTreeToVal old.workingSlate
                in
                  -- let _ = if inputval' == newval then Debug.crash "bad" else 0 in
                  case sync ip inputval' newval of
                    Err e -> Debug.crash ("upstate Sync: ++ " ++ e)
                    Ok ls -> { old | possibleChanges <- ls , mode <- SyncSelect }

    --Given possible changes, an option is selected. Vals are correspondingly
    --updated and mode is turned off.
    SelectOption ((e,v), f) -> { old | possibleChanges <- []
                                     , inputExp <- Just e
                                     , mode <- AdHoc
                               }

    SwitchMode m -> { old | mode <- m }

    --catch all
    _ -> old


-- Main --
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
