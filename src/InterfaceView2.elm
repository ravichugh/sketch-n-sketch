--A set of functions to assist with viewing in
--Interface.elm
module InterfaceView2 where
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
import Text as T exposing (defaultStyle)
import Color

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


--------------------------------------------------------------------------------
-- Compiling to Svg

buildSvg : Bool -> LangSvg.IndexedTree -> LangSvg.NodeId -> Svg.Svg
buildSvg showZones d i =
  case Utils.justGet i d of
    LangSvg.TextNode text -> VirtualDom.text text
    LangSvg.SvgNode shape attrs js ->
      -- TODO: figure out: (LangSvg.attr "draggable" "false")
      let zones =
        if | showZones -> makeZones shape i attrs
           | otherwise -> [] in
      let children = List.map (buildSvg showZones d) js in
      let mainshape = (LangSvg.svg shape) (LangSvg.compileAttrs attrs) children in
      Svg.svg [] (mainshape :: zones)


--------------------------------------------------------------------------------
-- Defining Zones

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

-- TODO should take into account disabled zones in Live mode
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


--------------------------------------------------------------------------------
-- User Interface Layout

debugLayout = False
strVersion  = "v0.0"
strTitle    = "sketch-n-sketch " ++ strVersion

colorDebug c1 =
  if | debugLayout -> GE.color c1
     | otherwise   -> GE.color Color.darkGray

codebox : Int -> Int -> Model -> GE.Element
codebox w h model =
  let event =
    if | syncBool model.mode -> []
       | otherwise ->
           [Events.on "input" Events.targetValue
              (Signal.message events.address << CodeUpdate)]
  in
    Html.toElement w h <|
      Html.textarea
        ([ Attr.id "codeBox"
         , Attr.style
             [ ("height", "99%") , ("width",  "99%")
             , ("resize", "none") , ("overflow", "scroll") ]
         , Attr.value model.code
         ] ++ event)
        []

canvas : Int -> Int -> Model -> GE.Element
canvas w h model =
  let showZones = case model.mode of {NoDirectMan -> False; _ -> True} in
  let svg = buildSvg showZones model.workingSlate model.rootId in
  Html.toElement w h <|
    Svg.svg
      [ onMouseUp DeselectObject
      , Attr.style [ ("width", "99%") , ("height", "99%")
                   , ("border", "4px solid darkGray") ] ]
      [ svg ]

middleWidgets w h model =
  [ Html.fromElement <| GE.spacer w h
  , renderButton w h
  , modeToggle w h model
  , dropdownExamples w h
  ] ++ syncButton_ w h model

syncButton_ w h model =
  case model.mode of
    AdHoc -> [syncButton w h]
    _     -> []

wBtn = 90
hBtn = 30

buttonAttrs w h =
  Attr.style
    [ ("type", "button")
    , ("width", dimToPix w)
    , ("height", dimToPix h)
    ]

mainSectionVertical : Int -> Int -> Model -> GE.Element
mainSectionVertical w h model =
  let
    wGut    = 10
    wMiddle = wBtn
    wCode   = (w - wMiddle - wGut - wGut) // 2
    wCanvas = wCode
    hWidget = 50
  in

  let codeSection = codebox wCode h model in
  let canvasSection = canvas wCanvas h model in
  let gutter = colorDebug Color.darkBlue <| GE.spacer wGut h in

  let middleSection =
    colorDebug Color.lightBlue <|
      GE.size wMiddle h <|
        GE.flow GE.down <|
          List.map (Html.toElement wMiddle hWidget)
                   (middleWidgets wBtn hBtn model) in

  GE.flow GE.right <|
    [ codeSection, gutter, middleSection, gutter, canvasSection ]

mainSectionHorizontal : Int -> Int -> Model -> GE.Element
mainSectionHorizontal w h model =
  let
    hGut    = 10
    hMiddle = hBtn
    hCode   = (h - hMiddle - hGut - hGut) // 2
    hCanvas = hCode
    wWidget = 100
  in

  let codeSection = codebox w hCode model in
  let canvasSection = canvas w hCanvas model in
  let gutter = colorDebug Color.darkBlue <| GE.spacer w hGut in

  let middleSection =
    colorDebug Color.lightBlue <|
      GE.size w hMiddle <|
        GE.flow GE.right <|
          List.map (Html.toElement wWidget hMiddle)
                   (middleWidgets wBtn hBtn model) in

  GE.flow GE.down <|
    [ codeSection, gutter, middleSection, gutter, canvasSection ]

renderButton : Int -> Int -> Html.Html
renderButton w h =
  Html.button
    [ buttonAttrs w h
    , Events.onClick events.address Render
    , Attr.value "Render"
    , Attr.name "Render the Code"
    ]
    [Html.text "Render Code"]

syncButton : Int -> Int -> Html.Html
syncButton w h =
  Html.button
    [ buttonAttrs w h
    , Events.onClick events.address Sync
    , Attr.value "Sync"
    , Attr.name "Sync the code to the canvas"
    ]
    [Html.text "Synchronize"]

dropdownExamples : Int -> Int -> Html.Html
dropdownExamples w h =
  let testlist =
    testIndices
      |> List.map (\i ->
           Html.option
             -- TODO: works in Firefox, but not in Chrome/Safari
             [ Events.onMouseOver events.address (SelectTest i) ]
             [ Html.text (toString i)] )
      |> List.reverse
  in
  Html.select [ buttonAttrs w h ] testlist

modeToggle : Int -> Int -> Model -> Html.Html
modeToggle w h model =
  let opt s m =
    let yes =
      case (model.mode, m) of
        (Live _, Live _)           -> True
        (AdHoc, AdHoc)             -> True
        (NoDirectMan, NoDirectMan) -> True
        _                          -> False
    in
    -- TODO: onClick works in Firefox, but not in Chrome/Safari
    Html.option
        [ Attr.selected yes
        , Events.onClick events.address (SwitchMode m) ]
        [Html.text s]
  in
  let optionLive =
    -- may want to delay this to when Live is selected
    let e = Utils.fromJust model.inputExp in
    let v = Eval.run e in
    let mode = Live <| Sync.prepareLiveUpdates e v in
    opt "Live" mode in
  let optionAdHoc = opt "Ad Hoc" AdHoc in
  let optionFreeze = opt "Freeze" NoDirectMan in
  Html.select
    [ buttonAttrs w h ]
    [ optionLive, optionAdHoc, optionFreeze ]

orientationButton w h model =
  let ui = model.ui in
  Html.button
      [ Attr.style
        [ ("position", "absolute")
        , ("font-family", "Courier, monospace")
        , ("type", "button")
        , ("width", dimToPix w)
        , ("height", dimToPix h)
        ]
      , Events.onClick events.address
          (UIupdate ({ ui | orient <- switchOrient ui.orient}))
      ]
      [Html.text ("Orientation: " ++ (toString model.ui.orient))]

view : (Int, Int) -> Model -> GE.Element
view (w,h) model =
  let
    wAll = w - (2 * wGut) - 1
    wGut = 10
    hTop = 40
    hBot = 30
    hMid = h - hTop - hBot - 1
    hTot = hTop + hMid + hBot
  in

  let topSection =
    let
      title = GE.leftAligned <| T.style titleStyle (T.fromString strTitle)
      titleStyle =
        { defaultStyle | typeface <- ["Courier", "monospace"]
                       , height <- Just 18
                       , bold <- True }

      wBtnO = 200
      hBtnO = 30
      wJunk = 225 -- tweak this to alter gap between title and button

      wSep  = GE.spacer (wAll - (wBtnO + wJunk)) 1
      btnO  = Html.toElement wBtnO hBtnO <| orientationButton wBtnO hBtnO model
    in
      GE.size wAll hTop <| GE.flow GE.right [ title , wSep, btnO ]
  in

  let midSection =
    GE.size wAll hMid <|
      case (model.mode, model.ui.orient) of
        (SyncSelect _, _) -> Debug.crash "view SyncSelect"
        (_, Vertical)     -> mainSectionVertical wAll hMid model
        (_, Horizontal)   -> mainSectionHorizontal wAll hMid model in

  let botSection = GE.spacer wAll hBot in
  let sideGutter = colorDebug Color.black <| GE.spacer wGut hTot in

  GE.flow GE.right
    [ sideGutter
    , GE.flow GE.down
        [ colorDebug Color.lightYellow <| topSection
        , midSection
        , colorDebug Color.lightYellow <| botSection
        ]
    , sideGutter
    ]

