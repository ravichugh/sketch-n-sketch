--A set of functions to assist with viewing in
--Interface.elm
module InterfaceView where
--imports copied from Interface.elm
--TODO: some of these could probably be cleaned up

--Import the little language and its parsing utilities
import Lang exposing (..) --For access to what makes up the Vals
import LangParser exposing (parseE, parseV)
import Sync exposing (sync, Triggers)
import Eval exposing (run)
import MainSvg
import Utils
import MicroTests
import InterfaceUtils exposing (..)
import LangSvg exposing (IndexedTree, NodeId, ShapeKind, Attr, toNum, toNumTr, addi)
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

-- View --
codeBox : String -> Bool -> Html.Html
codeBox code switch =
    let
        --depending on switch, toggle manipulatability
        event = case switch of
            True -> []
            False ->  [(Events.on "input" Events.targetValue
                (Signal.message events.address << CodeUpdate))]
    in
        --build text area
        Html.textarea
            ([ Attr.id "codeBox"
            , Attr.style
                [ ("height", "100%")
                , ("width",  "100%")
                , ("resize", "none")
                , ("overflow", "scroll")
                ]
            , Attr.value code
            ]
            ++
            --add event, if it exsists
            event)
            []

--Build viusal pane and populate with the model's objects
visualsBox : List Object -> Html.Html
visualsBox objects =
  Svg.svg 
    [ onMouseUp DeselectObject
    , Attr.style 
      [ ("width", "100%")
      , ("height", "100%")
      , ("border", "2px solid black")
      ]
    ]
    (List.map fst objects)

--Umbrella function for taking and indexed tree and calling buildSvg over it
buildVisual : Bool -> LangSvg.IndexedTree -> List (Svg.Svg, List Attr)
buildVisual showZones valDict =
  List.map (buildSvg showZones) (Dict.toList valDict)

--Function for handling attributes and children of an indexed tree and building them
--into Svgs with attr lists to be updated as necessary
buildSvg : Bool -> (LangSvg.NodeId, LangSvg.IndexedTreeNode) -> (Svg.Svg, List Attr)
buildSvg showZones (nodeID, node) = case node of
    LangSvg.TextNode text ->
      let str = LangSvg.AString in
      -- rkc TODO: is the "shape" attribute needed for anything?
      -- (VirtualDom.text text, [("shape", str "TEXT"), ("text", str text)])
      (VirtualDom.text text, [("text", str text)])
    LangSvg.SvgNode shape attrs childrenids ->
      -- TODO: figure out: (LangSvg.attr "draggable" "false")
      let zones =
        if | showZones -> makeZones shape nodeID attrs
           | otherwise -> [] in
      let mainshape = (LangSvg.svg shape) (LangSvg.compileAttrs attrs) [] in
      (Svg.svg [] (mainshape :: zones), attrs)

-- compileAttr will throw away the trace anyway
attrNum k n    = LangSvg.compileAttr k (LangSvg.ANum (n, dummyTrace))
attrNumTr k nt = LangSvg.compileAttr k (LangSvg.ANum nt)

onMouseDown = Svg.Events.onMouseDown << Signal.message events.address
onMouseUp   = Svg.Events.onMouseUp   << Signal.message events.address

zoneEvents id shape zone =
  [ onMouseDown (SelectObject id shape zone)
  , onMouseUp DeselectObject ]

zone svgFunc id shape zone l =
  svgFunc (zoneEvents id shape zone ++ l) []

cursorStyle s = LangSvg.attr "cursor" s

cursorOfZone zone = if
  -- rect zones
  | zone == "Interior"       -> cursorStyle "move"
  | zone == "RightEdge"      -> cursorStyle "ew-resize"
  | zone == "BotRightCorner" -> cursorStyle "nwse-resize"
  | zone == "BotEdge"        -> cursorStyle "ns-resize"
  | zone == "BotLeftCorner"  -> cursorStyle "nesw-resize"
  | zone == "LeftEdge"       -> cursorStyle "ew-resize"
  | zone == "TopLeftCorner"  -> cursorStyle "nwse-resize"
  | zone == "TopEdge"        -> cursorStyle "ns-resize"
  | zone == "TopRightCorner" -> cursorStyle "nesw-resize"
  -- circle/ellipse zones
  | zone == "Edge"           -> cursorStyle "pointer"
  -- default
  | otherwise                -> cursorStyle "default"

-- TODO use zone
zoneBorder svgFunc id shape zone flag =
  flip svgFunc [] <<
  (++) (zoneEvents id shape zone) <<
  (++) [ LangSvg.attr "stroke" "rgba(255,0,0,0.5)"
       , LangSvg.attr "strokeWidth" (if flag then "5" else "0")
       , LangSvg.attr "fill" "rgba(0,0,0,0)"
       , cursorOfZone zone
       ]

zonePoint id shape zone =
  flip Svg.circle [] <<
  (++) (zoneEvents id shape zone) <<
  (++) [ LangSvg.attr "r" "6"
       , LangSvg.attr "fill" "rgba(255,0,0,0.5)"
       , cursorStyle "pointer"
       ]

zonePoints id shape pts =
  flip Utils.mapi pts <| \(i, (x,y)) ->
    zonePoint id shape (addi "Point" i) [ attrNumTr "cx" x, attrNumTr "cy" y ]

zoneLine id shape zone (x1,y1) (x2,y2) =
  zoneBorder Svg.line id shape zone True [
      attrNumTr "x1" x1 , attrNumTr "y1" y1 , attrNumTr "x2" x2 , attrNumTr "y2" y2
    , cursorStyle "pointer"
    ]

--Zone building function (still under construction/prone to change)                
makeZones : String -> LangSvg.NodeId -> List Attr -> List Svg.Svg
makeZones shape id l =
  case shape of

    "rect" ->
        let mk zone x_ y_ w_ h_ =
          zoneBorder Svg.rect id shape zone True [
              attrNum "x" x_ , attrNum "y" y_
            , attrNum "width" w_ , attrNum "height" h_
            ]
        in
        let
          [x,y,w,h]     = List.map (toNum << Utils.find_ l) ["x","y","width","height"]
          gut           = 0.125
          (x0,x1,x2)    = (x, x + gut*w, x + (1-gut)*w)
          (y0,y1,y2)    = (y, y + gut*h, y + (1-gut)*h)
          (wSlim,wWide) = (gut*w, (1-2*gut)*w)
          (hSlim,hWide) = (gut*h, (1-2*gut)*h)
        in
          [ mk "Interior"       x1 y1 wWide hWide
          , mk "RightEdge"      x2 y1 wSlim hWide
          , mk "BotRightCorner" x2 y2 wSlim hSlim
          , mk "BotEdge"        x1 y2 wWide hSlim
          , mk "BotLeftCorner"  x0 y2 wSlim hSlim
          , mk "LeftEdge"       x0 y1 wSlim hWide
          , mk "TopLeftCorner"  x0 y0 wSlim hSlim
          , mk "TopEdge"        x1 y0 wWide hSlim
          , mk "TopRightCorner" x2 y0 wSlim hSlim
          ]

    "circle"  -> makeZonesEllipse shape id l
    "ellipse" -> makeZonesEllipse shape id l

    "line" ->
        let [x1,y1,x2,y2] = List.map (toNumTr << Utils.find_ l) ["x1","y1","x2","y2"] in
        let zLine = zoneLine id shape "Edge" (x1,y1) (x2,y2) in
        let zPts = zonePoints id shape [(x1,y1),(x2,y2)] in
        zLine :: zPts

    "polygon"  -> makeZonesPoly shape id l
    "polyline" -> makeZonesPoly shape id l

    _ -> []

makeZonesEllipse shape id l =
  let _ = Utils.assert "makeZonesEllipse" (shape == "circle" || shape == "ellipse") in
  let foo =
    let [cx,cy] = List.map (toNum << Utils.find_ l) ["cx","cy"] in
    [ attrNum "cx" cx , attrNum "cy" cy ] in
  let (f,bar) =
    if shape == "circle" then
      let [r] = List.map (toNum << Utils.find_ l) ["r"] in
      (Svg.circle, [ attrNum "r" r ])
    else
      let [rx,ry] = List.map (toNum << Utils.find_ l) ["rx","ry"] in
      (Svg.ellipse, [ attrNum "rx" rx , attrNum "ry" ry ]) in
  let zInterior = zoneBorder f id shape "Interior" False (foo ++ bar) in
  let zEdge = zoneBorder f id shape "Edge" True (foo ++ bar) in
  [zEdge, zInterior]

makeZonesPoly shape id l =
  let _ = Utils.assert "makeZonesPoly" (shape == "polygon" || shape == "polyline") in
  let pts = LangSvg.toPoints <| Utils.find_ l "points" in
  let zPts = zonePoints id shape pts in
  let zLines =
    let pairs = Utils.adjacentPairs (shape == "polygon") pts in
    let f (i,(pti,ptj)) = zoneLine id shape (addi "Edge" i) pti ptj in
    Utils.mapi f pairs in
  let zInterior =
    zoneBorder Svg.polygon id shape "Interior" False [
        LangSvg.compileAttr "points" (LangSvg.APoints pts)
      ] in
  let firstEqLast xs = Utils.head_ xs == Utils.head_ (List.reverse xs) in
  if | shape == "polygon" -> zInterior :: (zLines ++ zPts)
     | firstEqLast pts    -> zInterior :: (zLines ++ zPts)
     | otherwise          -> zLines ++ zPts

--Umbrella function for viewing a given model
view : (Int, Int) -> Model -> Html.Html
view (w,h) model =
  let 
    ui = model.ui
    windowsplit = (w, h * 9 // 10)
    viewtype = case model.mode of
      NoDirectMan  -> regularView windowsplit model
      AdHoc        -> regularView windowsplit model
      Live _       -> regularView windowsplit model
      SyncSelect l -> selectView windowsplit model l
  in
    Html.div
      [ Attr.style
        [ ("width", toString w)
        , ("height", toString h)
        , ("position", "absolute")
        , ("left", dimToPix 0)
        , ("top", dimToPix 0)
        ]
      ]
      [ Html.div --title banner
        [ Attr.style
          [ ("position", "absolute")
          , ("left", dimToPix 0)
          , ("top", dimToPix 0)
          , ("width", dimToPix w)
          , ("height", dimToPix (h // 10))
          ]
        ]
        [ Html.div 
            [Attr.style
                [ ("font-family", "Trebuchet MS, Helvetica, sans-serif")
                , ("font-size", "40px")
                , ("position", "absolute")
                , ("left", "10px")
                , ("top", "10px")
                ]
            ]
            [Html.text "Sketch-n-Sketch"]
        , Html.button  
          [ Attr.style
            [ ("position", "absolute")
            , ("left", dimToPix (w - 160))
            , ("top", dimToPix 20)
            , ("type", "button")
            , ("width", "140px")
            , ("height", "40px")
            ]
          , Events.onClick events.address (UIupdate 
            ({ ui | orient <- switchOrient ui.orient}))
          ]
          [Html.text ("Orientation: " ++ (toString model.ui.orient))]
        ]
      , viewtype
      ]



regularView (w,h) model =
  Html.div
    [ Attr.style
      [ ("position", "absolute")
      , ("width", toString w)
      , ("height", toString h)
      , ("left", dimToPix 0)
      , ("top", dimToPix (h // 9))
      ]
    ]
    --display code & visuals
    (orientButtonToggler (w,h) model)

orientButtonToggler : (Int, Int) -> Model -> List Html.Html
orientButtonToggler (w,h) model = 
  let
    testlist = 
      testIndices
        |> List.map (\i ->
             Html.option
               -- TODO: works in Firefox, but not in Chrome/Safari
               [ Events.onMouseOver events.address (SelectTest i) ]
               [ Html.text (toString i)] )
        |> List.reverse
    vAxis = w // 2 - 50
    vSpace = h // 4
    hAxis = w // 4
    hSpace = h // 2 - 40
  in
    case model.ui.orient of
      Vertical -> [ renderView (w,h) model
                  , renderButton vAxis vSpace
                  , modeToggle vAxis (vSpace + 60) model
                  , dropdownExamples vAxis (vSpace + 120) testlist
                  ]
                  ++
                  (case model.mode of
                      SyncSelect _ -> []
                      NoDirectMan -> []
                      Live _ -> []
                      AdHoc -> [syncButton vAxis (vSpace + 180)]
                  )
      Horizontal -> [ renderView (w,h) model
                    , renderButton hAxis hSpace
                    , modeToggle (hAxis + 120) hSpace model
                    , dropdownExamples (hAxis + 240) hSpace testlist
                    ]
                    ++
                    (case model.mode of
                        SyncSelect _ -> []
                        Live _ -> []
                        AdHoc -> [syncButton (hAxis + 360) hSpace]
                    )

renderButton : Int -> Int -> Html.Html
renderButton left top =
  Html.button
    [ Attr.style
        [ ("position", "absolute")
        , ("left", dimToPix left)
        , ("top", dimToPix top)
        , ("type", "button")
        , ("width", "100px")
        , ("height", "40px")
        ]
    , Events.onClick events.address Render
    , Attr.value "Render"
    , Attr.name "Render the Code"
    ]
    [Html.text "Render Code"]

modeToggle : Int -> Int -> Model -> Html.Html
modeToggle left top model =
  let opt s m =
    -- TODO: works in Firefox, but not in Chrome/Safari
    Html.option [Events.onClick events.address (SwitchMode m)] [Html.text s] in
  let optionLive =
    -- may want to delay this to when Live is selected
    let e = Utils.fromJust model.inputExp in
    let v = Eval.run e in
    let mode = Live <| Sync.prepareLiveUpdates e v in
    opt "Live" mode in
  let optionAdHoc = opt "Ad Hoc" AdHoc in
  let optionFreeze = opt "Freeze" NoDirectMan in
  Html.select
    [ Attr.style
        [ ("position", "absolute")
        , ("left", dimToPix left)
        , ("top", dimToPix top)
        , ("type", "button")
        , ("width", "100px")
        , ("height", "40px")
        ]
    ]
    [ optionLive, optionAdHoc, optionFreeze ]

dropdownExamples left top l =
  Html.select
    [ Attr.style
        [ ("position", "absolute")
        , ("left", dimToPix left)
        , ("top", dimToPix top)
        , ("type", "button")
        , ("width", "100px")
        , ("height", "40px")
        ]
    ]
    l

syncButton : Int -> Int -> Html.Html
syncButton left top =
  Html.button
    [ Attr.style
        [ ("position", "absolute")
        , ("left", dimToPix left)
        , ("top", dimToPix top)
        , ("type", "button")
        , ("width", "100px")
        , ("height", "40px")
        ]
    , Events.onClick events.address Sync
    , Attr.value "Sync"
    , Attr.name "Sync the code to the canvas"
    ]
    [Html.text "Synchronize"]

--When view is manipulatable, call this function for code & visuals
--to build corresponding panes
renderView : (Int, Int) -> Model -> Html.Html
renderView (w,h) model = 
  Html.div
    [ Attr.style
      [ ("width", toString w)
      , ("height", toString h)
      ]
    ]
    (orientViewToggler (w,h) model)

orientViewToggler : (Int,Int) -> Model -> List Html.Html
orientViewToggler (w,h) model =
  let
    dim = (Basics.toFloat (Basics.min w h)) / 2
    vWidth = w // 2 - 60
    vLeft = w // 2 + 60
    hHeight = h // 2 - 50
    hTop = h // 2 + 50
  in
    case model.ui.orient of
      Vertical -> [ codeBoxPlacer vWidth h 0 0 model
                  , visualsBoxPlacer vWidth h vLeft 0 dim model 
                  ]
      Horizontal -> [ codeBoxPlacer w hHeight 0 0 model
                    , visualsBoxPlacer w hHeight 0 hTop dim model 
                    ]

codeBoxPlacer : Int -> Int -> Int -> Int -> Model -> Html.Html
codeBoxPlacer w h left top model = 
  Html.div 
    [ Attr.style
      [ ("width", dimToPix w)
      , ("height", dimToPix h)
      , ("margin", "0")
      , ("position", "absolute")
      , ("left", dimToPix left)
      , ("top", dimToPix top)
      ]
    ]
    [codeBox model.code (syncBool model.mode)]

visualsBoxPlacer : Int -> Int -> Int -> Int -> Float -> Model -> Html.Html
visualsBoxPlacer w h left top dim model = 
  let showZones = case model.mode of {NoDirectMan -> False; _ -> True} in
  Html.div
    [ Attr.style
      [ ("width", dimToPix w)
      , ("height", dimToPix h)
      , ("margin", "0")
      , ("position", "absolute")
      , ("left", dimToPix left)
      , ("top", dimToPix top)
      ]
    ]    
    [visualsBox (buildVisual showZones model.workingSlate)]

--Build an Html of the iterations of renderOption over the possibleChanges
selectView : (Int, Int) -> Model -> PossibleChanges -> Html.Html
selectView (w,h) model possibleChanges =
    let
        dim = (Basics.toFloat (Basics.min w h)) / 2
    in
        Html.div
        []
        --index the possible changes and render these options
        (renderOption (w, h // 4) (Utils.mapi (\x -> x) possibleChanges) model dim)
            
--Given a possible Change, build code from Expr and visuals from the val, rank by priority w/ mapi
renderOption : (Int, Int) -> List (Int, ((Exp, Val), Float)) -> Model -> Float -> List Html.Html
renderOption (w,h) possiblechanges model dim =
    case possiblechanges of
        --if there is a possible change remaining, display this option
        (i, ((e,v), f))::ps -> 
            (Html.div
                [ Attr.style
                    [ ("width", toString w)
                    , ("height", toString h)
                    , ("top", dimToPix (h * (i-1)))
                    , ("position", "absolute")
                    ]
                ]
                [ Html.div 
                    [ Attr.style
                        [ ("width", dimToPix (w // 2 - 30))
                        , ("height", dimToPix h)
                        , ("margin", "0")
                        , ("position", "absolute")
                        , ("left", "0px")
                        , ("top", "0px") -- String.append (toString <| h * (i-1)) "px")
                        ]
                    ]
                    [codeBox (sExp e) (syncBool model.mode)]
                , Html.div
                    [ Attr.style
                        [ ("width", dimToPix (w // 2 - 50))
                        , ("height", dimToPix h)
                        , ("margin", "0")
                        , ("position", "absolute")
                        , ("left", dimToPix (w // 2))
                        , ("top", "0px") --String.append (toString <| h * (i-1)) "px")
                        ]
                    ]    
                    [visualsBox (buildVisual False <| snd <| LangSvg.valToIndexedTree v) ]
                  --TODO: parse val to svgs
--                , Html.button
--                    [ Attr.style
--                        [ ("position", "absolute")
--                        , ("left", String.append (toString <| w // 4) "px")
--                        , ("top", "0px) --String.append (toString <| h - 40) "px")
--                        , ("type", "button")
--                        , ("width", "100px")
--                        , ("height", "40px")
--                        ]
--                    , Events.onClick events.address (SelectOption ((e,v), f))
--                    , Attr.value "Select"
--                    , Attr.name "Select this codebox and visualbox"
--                    ]
--                    [Html.text "select"]
                --attach remaining option htmls
                ]) :: renderOption (w,h) ps model dim
        [] -> []
