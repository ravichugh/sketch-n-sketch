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
import LangSvg exposing (IndexedTree, NodeId, ShapeKind, Attr)
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
visualsBox : List Object -> Float -> Bool -> Html.Html
visualsBox objects dim switch =
  Svg.svg 
    [ Attr.style 
      [ ("width", "100%")
      , ("height", "100%")
      , ("border", "2px solid black")
      ]
    ]
    (List.map fst objects)

--Umbrella function for taking and indexed tree and calling buildSvg over it
buildVisual : LangSvg.IndexedTree -> List (Svg.Svg, List Attr)
buildVisual valDict = List.map buildSvg (Dict.toList valDict)

--Function for handling attributes and children of an indexed tree and building them
--into Svgs with attr lists to be updated as necessary
buildSvg : (LangSvg.NodeId, LangSvg.IndexedTreeNode) -> (Svg.Svg, List Attr)
buildSvg (nodeID, node) = case node of
    LangSvg.TextNode text ->
      let str = LangSvg.AString in
      (VirtualDom.text text, [("shape", str "TEXT"), ("text", str text)])
    LangSvg.SvgNode shape attrs childrenids ->
      let zones = makeZones shape nodeID attrs in
      let mainshape = (LangSvg.svg shape) (LangSvg.compileAttrs attrs) [] in
      (Svg.svg [] (mainshape :: zones), attrs)

compileAttrNum k v = LangSvg.attr k (toString v)
toFloat_ (LangSvg.ANum i) = i
-- toFloat_           = Utils.fromOk_ << String.toFloat
-- toInt_             = Utils.fromOk_ << String.toInt

onMouseDown = Svg.Events.onMouseDown << Signal.message events.address
onMouseUp   = Svg.Events.onMouseUp   << Signal.message events.address

--Zone building function (still under construction/prone to change)                
makeZones : String -> LangSvg.NodeId -> List Attr -> List Svg.Svg
makeZones shape nodeID l =
  case shape of

    -- TODO refactor common parts

    "circle" ->
        let [cx,cy,r] = List.map (toFloat_ << Utils.find_ l) ["cx","cy","r"] in
        let gutterPct = 0.100 in
        let zInterior =
          flip Svg.circle [] [
              compileAttrNum "cx" cx
            , compileAttrNum "cy" cy
            , compileAttrNum "r" (r * (1 - 2*gutterPct))
            , LangSvg.attr "stroke" "rgba(255,0,0,0.5)"
            , LangSvg.attr "strokeWidth" "3"
            , LangSvg.attr "fill" "rgba(0,0,0,0)"
            , LangSvg.attr "cursor" "move"
            , onMouseDown (SelectObject nodeID shape "Interior")
            , onMouseUp (DeselectObject nodeID)
            ] in
        let zEdge =
          flip Svg.circle [] [
              compileAttrNum "cx" cx
            , compileAttrNum "cy" cy
            , compileAttrNum "r" (r * (1))
            , LangSvg.attr "stroke" "rgba(255,0,0,0.5)"
            , LangSvg.attr "strokeWidth" "10"
            , LangSvg.attr "fill" "rgba(0,0,0,0)"
            , LangSvg.attr "cursor" "crosshair"
            , onMouseDown (SelectObject nodeID shape "Edge")
            , onMouseUp (DeselectObject nodeID)
            ] in
        [zEdge, zInterior] -- important that zEdge goes on top

    "rect" ->
        let mk zone x_ y_ w_ h_ s_ =
          flip Svg.rect [] ([
              compileAttrNum "x" x_
            , compileAttrNum "y" y_
            , compileAttrNum "width" w_
            , compileAttrNum "height" h_
            , LangSvg.attr "stroke" "rgba(255,0,0,0.5)"
            , LangSvg.attr "strokeWidth" "3"
            , LangSvg.attr "fill" "rgba(0,0,0,0)"
            , onMouseDown (SelectObject nodeID shape zone)
            , onMouseUp (DeselectObject nodeID)
            ] ++ List.map (\(a,b) -> LangSvg.attr a b) s_)
        in
        let
          [x,y,w,h]     = List.map (toFloat_ << Utils.find_ l) ["x","y","width","height"]
          gut           = 0.125
          (x0,x1,x2)    = (x, x + gut*w, x + (1-gut)*w)
          (y0,y1,y2)    = (y, y + gut*h, y + (1-gut)*h)
          (wSlim,wWide) = (gut*w, (1-2*gut)*w)
          (hSlim,hWide) = (gut*h, (1-2*gut)*h)
        in
          [ mk "Interior"       x1 y1 wWide hWide [("cursor", "move")]
          , mk "RightEdge"      x2 y1 wSlim hWide [("cursor", "ew-resize")]
          , mk "BotRightCorner" x2 y2 wSlim hSlim [("cursor", "nwse-resize")]
          , mk "BotEdge"        x1 y2 wWide hSlim [("cursor", "ns-resize")]
          , mk "BotLeftCorner"  x0 y2 wSlim hSlim [("cursor", "nesw-resize")]
          , mk "LeftEdge"       x0 y1 wSlim hWide [("cursor", "ew-resize")]
          , mk "TopLeftCorner"  x0 y0 wSlim hSlim [("cursor", "nwse-resize")]
          , mk "TopEdge"        x1 y0 wWide hSlim [("cursor", "ns-resize")]
          , mk "TopRightCorner" x2 y0 wSlim hSlim [("cursor", "nesw-resize")]
          ]

    "ellipse" ->
        let [cx,cy,rx,ry] = List.map (toFloat_ << Utils.find_ l) ["cx","cy","rx","ry"] in
        let gutterPct = 0.100 in
        let zInterior =
          flip Svg.ellipse [] [
              compileAttrNum "cx" cx
            , compileAttrNum "cy" cy
            , compileAttrNum "rx" (rx * (1 - 2*gutterPct))
            , compileAttrNum "ry" (ry * (1 - 2*gutterPct))
            , LangSvg.attr "stroke" "rgba(255,0,0,0.5)"
            , LangSvg.attr "strokeWidth" "3"
            , LangSvg.attr "fill" "rgba(0,0,0,0)"
            , LangSvg.attr "cursor" "move"
            , onMouseDown (SelectObject nodeID shape "Interior")
            , onMouseUp (DeselectObject nodeID)
            ] in
        let zEdge =
          flip Svg.ellipse [] [
              compileAttrNum "cx" cx
            , compileAttrNum "cy" cy
            , compileAttrNum "rx" (rx * (1))
            , compileAttrNum "ry" (ry * (1))
            , LangSvg.attr "stroke" "rgba(255,0,0,0.5)"
            , LangSvg.attr "strokeWidth" "10"
            , LangSvg.attr "fill" "rgba(0,0,0,0)"
            , LangSvg.attr "cursor" "crosshair"
            , onMouseDown (SelectObject nodeID shape "Edge")
            , onMouseUp (DeselectObject nodeID)
            ] in
        [zEdge, zInterior] -- important that zEdge goes on top

    _ -> []

--Umbrella function for viewing a given model
view : (Int, Int) -> Model -> Html.Html
view (w,h) model =
  let 
    ui = model.ui
    windowsplit = (w, h * 9 // 10)
    viewtype = case model.mode of
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
    let
        testlist = 
            List.reverse 
            <| List.map (\i -> 
                Html.option 
                    --callExp (a lazy function in Interfaceutils) 
                    --now searches the microtests for the corresponding
                    --test given a test number. note: callExp is lazy
                    [ Events.onMouseOver events.address 
                        (CodeUpdate (sExpK 1 (callExp i)))
                    , Events.onClick events.address 
                        (Render (Just (callExp i)))
                    ] 
                    [Html.text (toString i)]
                ) [15..42]
    in
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
            List.reverse 
            <| List.map (\i -> 
                Html.option 
                    --callExp (a lazy function in Interfaceutils) 
                    --now searches the microtests for the corresponding
                    --test given a test number. note: callExp is lazy
                    [ Events.onMouseOver events.address 
                        (CodeUpdate (sExpK 1 (callExp i)))
                    , Events.onClick events.address 
                        (Render (Just (callExp i)))
                    ] 
                    [Html.text (toString i)]
                ) [15..42]
    vAxis = w // 2 - 50
    vSpace = h // 4
    hAxis = w // 4
    hSpace = h // 2 - 40
  in
    case model.ui.orient of
      Vertical -> [ renderView (w,h) model
                  , renderButton vAxis vSpace
                  , modeToggle vAxis (vSpace + 60) model
                  , testToggle vAxis (vSpace + 120) testlist
                  ]
                  ++
                  (case model.mode of
                      SyncSelect _ -> []
                      Live _ -> []
                      AdHoc -> [syncButton vAxis (vSpace + 180)]
                  )
      Horizontal -> [ renderView (w,h) model
                    , renderButton hAxis hSpace
                    , modeToggle (hAxis + 120) hSpace model
                    , testToggle (hAxis + 240) hSpace testlist
                    ]
                    ++
                    (case model.mode of
                        SyncSelect _ -> []
                        Live _ -> []
                        AdHoc -> [syncButton (hAxis + 360) hSpace]
                    )

renderButton : Int -> Int -> Html.Html
renderButton left top = Html.button
                        [ Attr.style
                            [ ("position", "absolute")
                            , ("left", dimToPix left)
                            , ("top", dimToPix top)
                            , ("type", "button")
                            , ("width", "100px")
                            , ("height", "40px")
                            ]
                        , Events.onClick events.address (Render Nothing)
                        , Attr.value "Render"
                        , Attr.name "Render the Code"
                        ]
                        [Html.text "Render Code"]

modeToggle : Int -> Int -> Model -> Html.Html
modeToggle left top model = Html.select
                        [ Attr.style
                            [ ("position", "absolute")
                            , ("left", dimToPix left)
                            , ("top", dimToPix top)
                            , ("type", "button")
                            , ("width", "100px")
                            , ("height", "40px")
                            ]
                        ]
                        [ let e = Utils.fromJust model.inputExp in
                          let v = Eval.run e in
                          let mode = Live <| Sync.prepareLiveUpdates e v in
                            Html.option [Events.onClick events.address (SwitchMode mode)]
                                [Html.text "Live"]
                            , Html.option [Events.onClick events.address (SwitchMode AdHoc)] 
                                [Html.text "Ad Hoc"]
                        ]

testToggle left top testlist = Html.select
                        [ Attr.style
                            [ ("position", "absolute")
                            , ("left", dimToPix left)
                            , ("top", dimToPix top)
                            , ("type", "button")
                            , ("width", "100px")
                            , ("height", "40px")
                            ]
                        ]
                        testlist

syncButton : Int -> Int -> Html.Html
syncButton left top = Html.button
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
    [visualsBox (buildVisual model.workingSlate) dim (syncBool model.mode)]

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
                    [codeBox (sExpK 1 e) (syncBool model.mode)]
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
                    [visualsBox (buildVisual <| LangSvg.valToIndexedTree v) dim (syncBool model.mode)] --TODO: parse val to svgs
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
