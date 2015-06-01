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
import Utils
import MicroTests
import InterfaceUtils exposing (..)
import LangSvg exposing (IndexedTree, NodeId, ShapeKind, Attr, toNum, toNumTr, addi)
import Examples

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

buildSvg : Bool -> Bool -> LangSvg.IndexedTree -> LangSvg.NodeId -> Svg.Svg
buildSvg addZones showZones d i =
  case Utils.justGet i d of
    LangSvg.TextNode text -> VirtualDom.text text
    LangSvg.SvgNode shape attrs js ->
      -- TODO: figure out: (LangSvg.attr "draggable" "false")
      let zones =
        if | addZones  -> makeZones showZones shape i attrs
           | otherwise -> [] in
      let children = List.map (buildSvg addZones showZones d) js in
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
zoneBorder svgFunc id shape zone flag show =
  flip svgFunc [] <<
  (++) (zoneEvents id shape zone) <<
  (++) [ if flag && show
         then LangSvg.attr "stroke" "rgba(255,0,0,0.5)"
         else LangSvg.attr "stroke" "rgba(0,0,0,0.0)"
       , LangSvg.attr "strokeWidth" (if flag then "5" else "0")
       , LangSvg.attr "fill" "rgba(0,0,0,0)"
       , cursorOfZone zone
       ]

zonePoint id shape zone show =
  flip Svg.circle [] <<
  (++) (zoneEvents id shape zone) <<
  (++) [ LangSvg.attr "r" "6"
       , if show
         then LangSvg.attr "fill" "rgba(255,0,0,0.5)"
         else LangSvg.attr "fill" "rgba(0,0,0,0.0)"
       , cursorStyle "pointer"
       ]

zonePoints id shape show pts =
  flip Utils.mapi pts <| \(i, (x,y)) ->
    zonePoint id shape (addi "Point" i) show [ attrNumTr "cx" x, attrNumTr "cy" y ]

zoneLine id shape zone show (x1,y1) (x2,y2) =
  zoneBorder Svg.line id shape zone True show [
      attrNumTr "x1" x1 , attrNumTr "y1" y1 , attrNumTr "x2" x2 , attrNumTr "y2" y2
    , cursorStyle "pointer"
    ]

--Zone building function (still under construction/prone to change)                
makeZones : Bool -> String -> LangSvg.NodeId -> List Attr -> List Svg.Svg
makeZones showZones shape id l =
  case shape of

    "rect" ->
        let mk zone x_ y_ w_ h_ =
          zoneBorder Svg.rect id shape zone True showZones [
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

    "circle"  -> makeZonesEllipse showZones shape id l
    "ellipse" -> makeZonesEllipse showZones shape id l

    "line" ->
        let [x1,y1,x2,y2] = List.map (toNumTr << Utils.find_ l) ["x1","y1","x2","y2"] in
        let zLine = zoneLine id shape "Edge" showZones (x1,y1) (x2,y2) in
        let zPts = zonePoints id shape showZones [(x1,y1),(x2,y2)] in
        zLine :: zPts

    "polygon"  -> makeZonesPoly showZones shape id l
    "polyline" -> makeZonesPoly showZones shape id l

    _ -> []

makeZonesEllipse showZones shape id l =
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
  let zInterior = zoneBorder f id shape "Interior" False showZones (foo ++ bar) in
  let zEdge = zoneBorder f id shape "Edge" True showZones (foo ++ bar) in
  [zEdge, zInterior]

makeZonesPoly showZones shape id l =
  let _ = Utils.assert "makeZonesPoly" (shape == "polygon" || shape == "polyline") in
  let pts = LangSvg.toPoints <| Utils.find_ l "points" in
  let zPts = zonePoints id shape showZones pts in
  let zLines =
    let pairs = Utils.adjacentPairs (shape == "polygon") pts in
    let f (i,(pti,ptj)) = zoneLine id shape (addi "Edge" i) showZones pti ptj in
    Utils.mapi f pairs in
  let zInterior =
    zoneBorder Svg.polygon id shape "Interior" False showZones [
        LangSvg.compileAttr "points" (LangSvg.APoints pts)
      ] in
  let firstEqLast xs = Utils.head_ xs == Utils.head_ (List.reverse xs) in
  if | shape == "polygon" -> zInterior :: (zLines ++ zPts)
     | firstEqLast pts    -> zInterior :: (zLines ++ zPts)
     | otherwise          -> zLines ++ zPts


--------------------------------------------------------------------------------
-- User Interface Layout

-- Configuration Parameters
-- Only constants in this record may be changed.
--
params =
  { strVersion = "v0.0"
  , debugLayout = False    -- displays colors for high-level layout structure
  , wGut = 10              -- width of left/right side gutters (spans entire height)
  , topSection =
     { h = 40              -- height of top space
     , wBtnO = 150         -- width...
     , hBtnO = 25          -- ... and height of orientation button
     , wJunk = 225         -- gap between title and orientation button
     }
  , botSection =
     { h = 30              -- height of bot space
     }
  , mainSection =
     { widgets =           -- Render/Sync buttons; Mode/Tests dropdowns
        { wBtn = 85
        , hBtn = 25
        , font = "Tahoma, sans-serif"
        , fontSize = "12px"
        }
     , vertical =
        { hWidget = 40     -- vertical space between widgets
        , wGut = 10        -- width of gutters in between code/widgets/canvas
        }
     , horizontal =
        { wWidget = 100    -- horizontal space between widgets
        , hGut = 10        -- height of gutters in between code/widgets/canvas
        }
     , canvas =
        { border = "0px solid darkGray"
        }
     , codebox =
        { border = "none"
        , font = "Courier, monospace"
        , fontSize = "14px"
        }
     }
  }

-- End Configuration Parameters
------------------------------------------------------------------------------


strTitle = "sketch-n-sketch " ++ params.strVersion

colorDebug c1 =
  if | params.debugLayout -> GE.color c1
     | otherwise          -> GE.color Color.darkGray

-- TODO: set readonly based on mode
codebox : Int -> Int -> Model -> GE.Element
codebox w h model =
  let event =
    if | syncBool model.mode -> []
       | otherwise ->
           [Events.on "input" Events.targetValue
              (Signal.message events.address << CodeUpdate)]
  in
    codebox_ w h event model.code

codebox_ w h event s =
  Html.toElement w h <|
    Html.textarea
      ([ Attr.id "codeBox"
       , Attr.spellcheck False
       , Attr.style
           [ ("font-family", params.mainSection.codebox.font)
           , ("font-size", params.mainSection.codebox.fontSize)
           , ("border", params.mainSection.codebox.border)
           , ("white-space", "nowrap")
           , ("height", "99%") , ("width", "99%")
           , ("resize", "none")
           , ("overflow", "auto")
           -- TODO "overflow-x" horizontal scrollbar still not showing up.
           ]
       , Attr.value s
       ] ++ event)
      []

canvas : Int -> Int -> Model -> GE.Element
canvas w h model =
  case model.mode of
    Print ->
      let v = Eval.run (parseE model.code) in
      let (i,tree) = LangSvg.valToIndexedTree v in
      let s = LangSvg.printSvg i tree in
      codebox_ w h [] s
    _ ->
      canvas_ w h model

canvas_ w h model =
  let addZones = case model.mode of {NoDirectMan -> False; _ -> True} in
  let svg = buildSvg addZones model.showZones model.workingSlate model.rootId in
  Html.toElement w h <|
    Svg.svg
      [ onMouseUp DeselectObject
      , Attr.style [ ("width", "99%") , ("height", "99%")
                   , ("border", params.mainSection.canvas.border)
                   ] ]
      [ svg ]

middleWidgets w h model =
  case model.mode of
    SyncSelect _ [] ->
      [ gapWidget w h
      , gapWidget w h
      , revertButton w h
      ]
    SyncSelect i l ->
      [ gapWidget w h
      , prevButton i w h
      , chooseButton w h
      , nextButton i l w h
      ]
    _ ->
      [ gapWidget w h
      , dropdownExamples w h
      , gapWidget w h
      , renderButton w h
      , printButton w h
      , gapWidget w h
      ] ++ zoneButton model w h ++
      [ modeToggle w h model
      ] ++ syncButton_ w h model

gapWidget w h = Html.fromElement <| GE.spacer w h

syncButton_ w h model =
  case model.mode of
    AdHoc -> [syncButton w h]
    _     -> []

wBtn = params.mainSection.widgets.wBtn
hBtn = params.mainSection.widgets.hBtn

buttonAttrs w h =
  Attr.style
    [ ("width", dimToPix w)
    , ("height", dimToPix h)
    , ("font-family", params.mainSection.widgets.font)
    , ("font-size", params.mainSection.widgets.fontSize)
    ]

mainSectionVertical : Int -> Int -> Model -> GE.Element
mainSectionVertical w h model =
  let
    wGut    = params.mainSection.vertical.wGut
    wMiddle = wBtn
    wCode   = (w - wMiddle - wGut - wGut) // 2
    wCanvas = wCode
    hWidget = params.mainSection.vertical.hWidget
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
    hGut    = params.mainSection.horizontal.hGut
    hMiddle = hBtn
    hCode   = (h - hMiddle - hGut - hGut) // 2
    hCanvas = hCode
    wWidget = params.mainSection.horizontal.wWidget
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

simpleButton : Event -> String -> String -> String -> Int -> Int -> Html.Html
simpleButton evt value name text w h =
  Html.button
    [ buttonAttrs w h
    , Events.onClick events.address evt
    , Attr.value value
    , Attr.name name
    ]
    [Html.text text]

renderButton =
  simpleButton Render "Render" "Run and Render to SVG" "Render SVG"

printButton =
  simpleButton PrintSvg "Print" "Run and Print to SVG" "Print SVG"

syncButton =
  simpleButton Sync "Sync" "Sync the code to the canvas" "Sync"

zoneButton model w h =
  if model.mode == NoDirectMan then [ gapWidget w h ]
  else let cap = if model.showZones then "Hide Zones" else "Show Zones" in
       [ simpleButton ToggleZones "ToggleZones" "Show/Hide Zones" cap w h ]

chooseButton =
  simpleButton SelectOption "Choose" "Choose" "Select This"

prevButton i =
  if | i > 1     -> simpleButton (TraverseOption -1) "Prev" "Prev" "Show Prev"
     | otherwise -> gapWidget

nextButton i l =
  let n = List.length l in
  if | i < n     -> simpleButton (TraverseOption 1) "Next" "Next" "Show Next"
     | otherwise -> gapWidget

revertButton =
  simpleButton Revert "Revert" "Revert" "Revert"

dropdownExamples : Int -> Int -> Html.Html
dropdownExamples w h =
  let examples =
    let foo (name,thunk) =
      Html.option
          -- TODO: works in Firefox, but not in Chrome/Safari
          [ Events.onMouseOver events.address (SelectExample name thunk) ]
          [ Html.text name ]
    in
    List.map foo Examples.list
  in
  Html.select [ buttonAttrs w h ] examples

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
  -- may want to delay this to when Live is selected
  let optionLive = opt "Live" (mkLive_ (Utils.fromJust model.inputExp)) in
  let optionAdHoc = opt "Ad Hoc" AdHoc in
  let optionFreeze = opt "Freeze" NoDirectMan in
  Html.select
    [ buttonAttrs w h ]
    [ optionLive, optionAdHoc, optionFreeze ]

orientationButton w h model =
  let ui = model.ui in
  Html.button
      [ buttonAttrs w h
      , Events.onClick events.address
          (UIupdate ({ ui | orient <- switchOrient ui.orient}))
      ]
      [Html.text ("Orientation: " ++ (toString model.ui.orient))]

view : (Int, Int) -> Model -> GE.Element
view (w,h) model =
  let
    wAll = w - (2 * wGut) - 1
    wGut = params.wGut
    hTop = params.topSection.h
    hBot = params.botSection.h
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

      wBtnO = params.topSection.wBtnO
      hBtnO = params.topSection.hBtnO
      wJunk = params.topSection.wJunk

      wSep  = GE.spacer (wAll - (wBtnO + wJunk)) 1
      btnO  = Html.toElement wBtnO hBtnO <| orientationButton wBtnO hBtnO model
    in
      GE.size wAll hTop <| GE.flow GE.right [ title , wSep, btnO ]
  in

  let midSection =
    GE.size wAll hMid <|
      case model.ui.orient of
        Vertical   -> mainSectionVertical wAll hMid model
        Horizontal -> mainSectionHorizontal wAll hMid model in

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

