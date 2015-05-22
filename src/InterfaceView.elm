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
visualsBox : List (Svg.Svg, List (String, String))-> Float -> Bool -> Html.Html
visualsBox objects dim switch =
    Svg.svg [ Attr.style
                [ ("width", "100%")
                , ("height", "100%")
                ]
            ] <| List.map (\(f,g) -> f) objects

--Umbrella function for taking and indexed tree and calling buildSvg over it
buildVisual : LangSvg.IndexedTree -> List (Svg.Svg, List (String, String))
buildVisual valDict = List.map buildSvg (Dict.toList valDict)

--Function for handling attributes and children of an indexed tree and building them
--into Svgs with attr lists to be updated as necessary
buildSvg : (LangSvg.NodeId, LangSvg.IndexedTreeNode) -> (Svg.Svg, List (String, String))
buildSvg (nodeID, node) = case node of
    --if text, call svg text creation
    LangSvg.TextNode text -> (VirtualDom.text text, [("shape", "TEXT"), ("text", text)])
    --If svg object, make objects with appropriate zones and attributes
    LangSvg.SvgNode shape attrs childrenids ->
       let attrstrs = getAttrs attrs
           zones = makeZones shape nodeID attrstrs
           mainshape = (LangSvg.svg shape <| LangSvg.valsToAttrs attrs) []
       in (Svg.svg [] (mainshape :: zones), attrstrs)

compileAttrNum k v = LangSvg.attr k (toString v)
toFloat_           = Utils.fromOk_ << String.toFloat
-- toInt_             = Utils.fromOk_ << String.toInt

onMouseDown = Svg.Events.onMouseDown << Signal.message events.address
onMouseUp   = Svg.Events.onMouseUp   << Signal.message events.address

--Zone building function (still under construction/prone to change)                
makeZones : String -> LangSvg.NodeId -> List (String, String) -> List Svg.Svg
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
            , onMouseDown (SelectObject nodeID shape "Edge")
            , onMouseUp (DeselectObject nodeID)
            ] in
        [zEdge, zInterior] -- important that zEdge goes on top

    "rect" ->
        let mk zone x_ y_ w_ h_ =
          flip Svg.rect [] [
              compileAttrNum "x" x_
            , compileAttrNum "y" y_
            , compileAttrNum "width" w_
            , compileAttrNum "height" h_
            , LangSvg.attr "stroke" "rgba(255,0,0,0.5)"
            , LangSvg.attr "strokeWidth" "3"
            , LangSvg.attr "fill" "rgba(0,0,0,0)"
            , onMouseDown (SelectObject nodeID shape zone)
            , onMouseUp (DeselectObject nodeID)
            ]
        in
        let
          [x,y,w,h]     = List.map (toFloat_ << Utils.find_ l) ["x","y","width","height"]
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
            , onMouseDown (SelectObject nodeID shape "Edge")
            , onMouseUp (DeselectObject nodeID)
            ] in
        [zEdge, zInterior] -- important that zEdge goes on top

    _ -> []

--Umbrella function for viewing a given model
view : (Int, Int) -> Model -> Html.Html
view wh model =
  case model.mode of
    AdHoc      -> regularView wh model
    Live       -> regularView wh model
    SyncSelect -> selectView wh model

regularView (w,h) model =
                  Html.div
                    [ Attr.style
                        [ ("width", toString w)
                        , ("height", toString h)
                        ]
                    ]
                    --display code & visuals
                    ([renderView (w,h) model
                    , Html.button
                        [ Attr.style
                            [ ("position", "absolute")
                            , ("left", String.append (toString <| w // 8) "px")
                            , ("top", String.append (toString <| h - 40) "px")
                            , ("type", "button")
                            , ("width", "100px")
                            , ("height", "40px")
                            ]
                        , Events.onClick events.address Render
                        , Attr.value "Render"
                        , Attr.name "Render the Code"
                        ]
                        [Html.text "render"]
                    , Html.select
                        [ Attr.style
                            [ ("position", "absolute")
                            , ("left", String.append (toString <| w // 4) "px")
                            , ("top", String.append (toString <| h - 40) "px")
                            , ("type", "button")
                            , ("width", "100px")
                            , ("height", "40px")
                            ]
                        ]
                        [ 
                            Html.option [Events.onClick events.address (SwitchMode Live)] 
                                [Html.text "live"]
                            , Html.option [Events.onClick events.address (SwitchMode AdHoc)] 
                                [Html.text "ad hoc"]
                        ]
                    ]
                    ++
                    (case model.mode of
                        SyncSelect -> []
                        Live -> []
                        AdHoc -> 
                            [Html.button
                                [ Attr.style
                                    [ ("position", "absolute")
                                    , ("left", String.append (toString <| w // 2) "px")
                                    , ("top", String.append (toString <| h - 40) "px")
                                    , ("type", "button")
                                    , ("width", "100px")
                                    , ("height", "40px")
                                    ]
                                , Events.onClick events.address Sync
                                , Attr.value "Sync"
                                , Attr.name "Sync the code to the canvas"
                                ]
                                [Html.text "sync"]
                            ]
                    ))

--When view is manipulatable, call this function for code & visuals
--to build corresponding panes
renderView : (Int, Int) -> Model -> Html.Html
renderView (w,h) model = 
    let
        dim = (Basics.toFloat (Basics.min w h)) / 2
    in
        Html.div
            [ Attr.style
                [ ("width", toString w)
                , ("height", toString h)
                ]
            ]
            [ Html.div 
                [ Attr.style
                    [ ("width", String.append (toString <| w // 2 - 1) "px")
                    , ("height", String.append (toString <| h - 60) "px")
                    , ("margin", "0")
                    , ("position", "absolute")
                    , ("left", "0px")
                    , ("top", "0px")
                    ]
                ]
                [codeBox model.code (syncBool model.mode)]
            , Html.div
                [ Attr.style
                    [ ("width", String.append (toString <| w // 2 - 1) "px")
                    , ("height", String.append (toString h) "px")
                    , ("margin", "0")
                    , ("position", "absolute")
                    , ("left", String.append (toString <| w // 2) "px")
                    , ("top", "0px")
                    ]
                ]    
                [visualsBox (buildVisual model.workingSlate) dim (syncBool model.mode)]
            ]

--Build an Html of the iterations of renderOption over the possibleChanges
selectView : (Int, Int) -> Model -> Html.Html
selectView (w,h) model =
    let
        dim = (Basics.toFloat (Basics.min w h)) / 2
    in
        Html.div
        []
        --index the possible changes and render these options
        (renderOption (w, h // 4) (Utils.mapi (\x -> x) model.possibleChanges) model dim)
            
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
                    , ("top", String.append (toString <| h * (i-1)) "px")
                    , ("position", "absolute")
                    ]
                ]
                [ Html.div 
                    [ Attr.style
                        [ ("width", String.append (toString <| w // 2 - 30) "px")
                        , ("height", String.append (toString <| h) "px")
                        , ("margin", "0")
                        , ("position", "absolute")
                        , ("left", "0px")
                        , ("top", "0px") -- String.append (toString <| h * (i-1)) "px")
                        ]
                    ]
                    [codeBox (sExpK 1 e) (syncBool model.mode)]
                , Html.div
                    [ Attr.style
                        [ ("width", String.append (toString <| w // 2 - 50) "px")
                        , ("height", String.append (toString h) "px")
                        , ("margin", "0")
                        , ("position", "absolute")
                        , ("left", String.append (toString <| w // 2) "px")
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
