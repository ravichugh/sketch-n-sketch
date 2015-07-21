module InterfaceView2 (view) where

--Import the little language and its parsing utilities
import Lang exposing (..) --For access to what makes up the Vals
import LangParser exposing (parseE, parseV)
import Sync
import Eval
import Utils
import MicroTests
import InterfaceModel exposing (..)
import LangSvg exposing (toNum, toNumTr, addi)
import ExamplesGenerated as Examples
import Config exposing (params)

import VirtualDom

--Core Libraries
import List 
import Dict
import String 
import Graphics.Element as GE 
import Graphics.Collage as GC
import Graphics.Input as GI
import Text as T exposing (defaultStyle)
import Color

--Signaling Libraries
import Mouse 
import Window 
import Task exposing (Task, andThen)

--Storage Libraries
import InterfaceStorage exposing (taskMailbox, saveStateLocally, loadLocalState)

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

dimToPix d = String.append (toString d) "px"


--------------------------------------------------------------------------------
-- Compiling to Svg

buildSvg : Bool -> Bool -> LangSvg.RootedIndexedTree -> Svg.Svg
buildSvg addZones showZones (i,d) = buildSvg_ addZones showZones d i

buildSvg_ : Bool -> Bool -> LangSvg.IndexedTree -> LangSvg.NodeId -> Svg.Svg
buildSvg_ addZones showZones d i =
  case Utils.justGet_ ("buildSvg_ " ++ toString i) i d of
    LangSvg.TextNode text -> VirtualDom.text text
    LangSvg.SvgNode shape attrs js ->
      -- TODO: figure out: (LangSvg.attr "draggable" "false")
      let zones =
        if | addZones  -> makeZones showZones shape i attrs
           | otherwise -> [] in
      let children = List.map (buildSvg_ addZones showZones d) js in
      let mainshape = (LangSvg.svg shape) (LangSvg.compileAttrs attrs) children in
      Svg.svg [] (mainshape :: zones)


--------------------------------------------------------------------------------
-- Defining Zones

-- compileAttr will throw away the trace anyway
attrNum k n    = LangSvg.compileAttr k (LangSvg.ANum (n, dummyTrace))
attrNumTr k nt = LangSvg.compileAttr k (LangSvg.ANum nt)

onMouseDown = Svg.Events.onMouseDown << Signal.message events.address
onMouseUp   = Svg.Events.onMouseUp   << Signal.message events.address
onMouseOver = Svg.Events.onMouseOver << Signal.message events.address
onMouseOut  = Svg.Events.onMouseOut  << Signal.message events.address

zoneEvents id shape zone =
  [ onMouseDown (SelectObject id shape zone)
  , onMouseUp MouseUp
  , onMouseOver (UpdateModel (\m -> { m | caption <- Just (Hovering (id, shape, zone)) }))
  , onMouseOut (UpdateModel (\m -> { m | caption <- Nothing }))
  ]

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

makeZones : Bool -> String -> LangSvg.NodeId -> List LangSvg.Attr -> List Svg.Svg
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

    "path" -> makeZonesPath showZones shape id l

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

makeZonesPath showZones shape id l =
  let _ = Utils.assert "makeZonesPoly" (shape == "path") in
  let cmds = fst <| LangSvg.toPath <| Utils.find_ l "d" in
  let (mi,pt) +++ acc = case mi of {Nothing -> acc; _ -> pt :: acc} in
  let pts =
    List.foldr (\c acc -> case c of
      LangSvg.CmdZ   s              -> acc
      LangSvg.CmdMLT s pt           -> pt +++ acc
      LangSvg.CmdHV  s n            -> acc
      LangSvg.CmdC   s pt1 pt2 pt3  -> pt1 +++ (pt2 +++ (pt3 +++ acc))
      LangSvg.CmdSQ  s pt1 pt2      -> pt1 +++ (pt2 +++ acc)
      LangSvg.CmdA   s a b c d e pt -> pt +++ acc) [] cmds
  in
  zonePoints id shape showZones pts


--------------------------------------------------------------------------------
-- User Interface

strTitle = " sketch-n-sketch " ++ params.strVersion

colorDebug_ c1 c2 =
  if | params.debugLayout -> GE.color c1
     | otherwise          -> GE.color c2

colorDebug c1 = colorDebug_ c1 Color.darkGray

codebox : Int -> Int -> Model -> GE.Element
codebox w h model =
  let event =
    case model.mode of
      SyncSelect _ _ -> []
      _ -> [Events.on "input" Events.targetValue
              (Signal.message events.address << CodeUpdate)]
  in
    codebox_ w h event model.code (not model.editingMode)

codebox_ w h event s readOnly =
  Html.toElement w h <|
    Html.textarea
      ([ Attr.id "codeBox"
       , Attr.spellcheck False
       , Attr.readonly readOnly
       , Attr.style
           [ ("font-family", params.mainSection.codebox.font)
           , ("font-size", params.mainSection.codebox.fontSize)
           , ("border", params.mainSection.codebox.border)
           -- TODO: "pre" preserves breaks on Firefox,
           --   but still no horizontal scrollbars on Chome/Safari
           , ("whiteSpace", "pre")
           , ("height", "99%") , ("width", "99%")
           , ("resize", "none")
           , ("overflow", "auto")
           ]
       , Attr.value s
       , Events.onMouseUp events.address MouseUp
       ] ++ event)
      []

canvas : Int -> Int -> Model -> GE.Element
canvas w h model =
  case model.mode of
    Print s -> codebox_ w h [] s True
    _       -> canvas_ w h model

canvas_ w h model =
  let addZones = not model.editingMode in
  let svg = buildSvg addZones model.showZones model.slate in
  Html.toElement w h <|
    Svg.svg
      [ onMouseUp MouseUp
      , Attr.style [ ("width", "99%") , ("height", "99%")
                   , ("border", params.mainSection.canvas.border)
                   ] ]
      [ svg ]

middleWidgets w h wWrap hWrap model =
  List.map (GE.container wWrap hWrap GE.middle) <|
    case (model.editingMode, model.mode) of
      (False, SyncSelect i options) ->
        [ gapWidget w h
        , gapWidget w h
        , prevButton i w h
        , chooseButton i options w h
        , nextButton i options w h
        ]
      (False, Print _) ->
        [ dropdownExamples model w h
        , editRunButton model w h
        , outputButton model w h
        ]
      (False, _) ->
        [ dropdownExamples model w h
        , editRunButton model w h
        , saveButton model w h
        , loadButton w h
        , outputButton model w h
        , gapWidget w h
        , zoneButton model w h
        , frozenButton model w h
        , modeButton model w h
        ] ++ (syncButton_ w h model)
      (True, _) ->
        [ dropdownExamples model w h
        , editRunButton model w h
        , saveButton model w h
        , loadButton w h
        ]

gapWidget w h = GE.spacer w h

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

gutterForResizing orient w h =
  let s = if orient == Vertical then "ew-resize" else "ns-resize" in
  colorDebug Color.darkBlue <|
    Html.toElement w h <|
      Html.div
          [ Events.onMouseDown events.address StartResizingMid
          , Events.onMouseUp events.address MouseUp
          , Attr.style
              [ ("width", dimToPix w) , ("height", dimToPix h)
              , ("cursor", s) ]
          ]
          [ ]

mainSectionVertical : Int -> Int -> Model -> GE.Element
mainSectionVertical w h model =
  let
    wGut    = params.mainSection.vertical.wGut
    wMiddle = wBtn
    wCode_  = (w - wMiddle - wGut - wGut) // 2
    wCode   = wCode_ + model.midOffsetX
    wCanvas = wCode_ - model.midOffsetX
    hCanvas = h - hZInfo
    hZInfo  = params.mainSection.canvas.hZoneInfo
    hWidget = params.mainSection.widgets.hBtn
                + params.mainSection.vertical.hExtra
  in

  let codeSection = codebox wCode h model in

  let canvasSection =
    GE.size wCanvas h <|
      GE.flow GE.down
        [ canvas wCanvas hCanvas model
        , caption model (wCanvas+1) hZInfo -- NOTE: +1 is a band-aid
        ]
  in

  let gutter = gutterForResizing model.orient wGut h in

  let middleSection =
    colorDebug Color.lightBlue <|
      GE.size wMiddle h <|
        GE.flow GE.down <|
          middleWidgets wBtn hBtn wMiddle hWidget model in
  GE.flow GE.right <|
    [ codeSection, gutter, middleSection, gutter, canvasSection ]

mainSectionHorizontal : Int -> Int -> Model -> GE.Element
mainSectionHorizontal w h model =
  let
    hGut    = params.mainSection.horizontal.hGut
    hMiddle = hBtn
    hCode_  = (h - hMiddle - hGut - hGut) // 2
    hCode   = hCode_ + model.midOffsetY
    hCanvas = hCode_ - model.midOffsetY - hZInfo
    hZInfo  = params.mainSection.canvas.hZoneInfo
    wWidget = params.mainSection.widgets.wBtn
                + params.mainSection.horizontal.wExtra
  in

  let codeSection = codebox w hCode model in

  let canvasSection =
    GE.size w (hCanvas + hZInfo) <|
      GE.flow GE.down
        [ canvas w hCanvas model
        , caption model w (hZInfo+1) -- NOTE: +1 is a band-aid
        ]
  in

  let gutter = gutterForResizing model.orient w hGut in

  let middleSection =
    colorDebug Color.lightBlue <|
      GE.size w hMiddle <|
        GE.flow GE.right <|
          middleWidgets wBtn hBtn wWidget hMiddle model in
  GE.flow GE.down <|
    [ codeSection, gutter, middleSection, gutter, canvasSection ]

simpleButton_ : Bool -> Event -> String -> String -> String -> Int -> Int -> GE.Element
simpleButton_ disabled evt value name text w h =
  Html.toElement w h <|
    Html.button
      [ buttonAttrs w h
      , Events.onClick events.address evt
      , Attr.value value
      , Attr.name name
      , Attr.disabled disabled
      ]
      [Html.text text]

simpleButton = simpleButton_ False

editRunButton model w h =
  let disabled = model.mode == AdHoc in
  case model.editingMode of
    True  -> simpleButton_ disabled Run "Run" "Run" "Run Code" w h
    False -> simpleButton_ disabled Edit "Edit" "Edit" "Edit Code" w h

outputButton model w h =
  let disabled = model.mode == AdHoc in
  let cap =
     case model.mode of
       Print _ -> "[Out] SVG"
       _       -> "[Out] Canvas"
  in
  simpleButton_ disabled ToggleOutput "Toggle Output" "Toggle Output" cap w h

syncButton =
  simpleButton Sync "Sync" "Sync the code to the canvas" "Sync"

zoneButton model =
  let cap = if model.showZones then "[Zones] Shown" else "[Zones] Hidden" in
  simpleButton ToggleZones "ToggleZones" "Show/Hide Zones" cap

frozenButton model =
  let cap = if model.syncOptions.thawedByDefault then "[Default] n?" else "[Default] n!" in
  simpleButton ToggleThawed "ToggleThawed " "Toggle ?/!" cap

chooseButton i (n,_) =
  let cap =
    if i == n + 2 then "Revert"
    else "Select " ++ Utils.parens (toString i ++ "/" ++ toString (n+1))
  in
  simpleButton SelectOption "Choose" "Choose" cap

prevButton i =
  let enabled = i > 1 in
  simpleButton_ (not enabled) (TraverseOption -1) "Prev" "Prev" "Show Prev"

nextButton i (n,l) =
  let enabled = i < n + 2 in
  simpleButton_ (not enabled) (TraverseOption 1) "Next" "Next" "Show Next"

saveButton : Model -> Int -> Int -> GE.Element
saveButton model w h =
    Html.toElement w h <|
      Html.button
        [ buttonAttrs w h
        , Events.onClick taskMailbox.address (saveStateLocally model.exName model)
        , Attr.value "Save"
        , Attr.name "Save"
        , Attr.title "Saves Code and Page Layout to Persistent Browser Storage"
        , Attr.disabled False
        ]
        [ Html.text "Save" ]

loadButton : Int -> Int -> GE.Element
loadButton w h =
    Html.toElement w h <| 
      Html.button
        [ buttonAttrs w h
        , Events.onClick taskMailbox.address loadLocalState
        , Attr.value "Revert"
        , Attr.name "Revert"
        , Attr.title 
            "Reverts Code and Page Layout to last save"
        , Attr.disabled False
        ]
        [ Html.text "Revert" ]

dropdownExamples : Model -> Int -> Int -> GE.Element
dropdownExamples model w h =
  let choices =
    case model.mode of
      AdHoc -> [(model.exName, Noop)]
      _ ->
        let foo (name,thunk) = (name, (SelectExample name thunk)) 
            bar saveName = (saveName, UpdateModel (\oldmodel -> oldmodel))
            --TODO make this so that a task is sent to the taskMailbox instead
            --of the events mailbox down at the bottom and that it involves
            --picking the appropriate load and such
        in List.append
            (List.map bar model.localSaves)
            (List.map foo Examples.list)
  in
    GI.dropDown (Signal.message events.address) choices

modeButton model =
  if model.mode == AdHoc
  then simpleButton_ True Noop "SwitchMode" "SwitchMode" "[Mode] Ad Hoc"
  else simpleButton_ False (SwitchMode AdHoc) "SwitchMode" "SwitchMode" "[Mode] Live"

{-
modeToggle : Int -> Int -> Model -> GE.Element
modeToggle w h model =
  let opt s m = (s, (SwitchMode m)) in
  let optionLive = opt "Live" (mkLive_ model.syncOptions (Utils.fromJust model.inputExp)) in
  let optionAdHoc = opt "Ad Hoc" AdHoc in
  GI.dropDown (Signal.message events.address) <|
    case model.mode of
      AdHoc -> [optionAdHoc]
      _     -> [optionLive, optionAdHoc]
-}

orientationButton w h model =
  Html.button
      [ buttonAttrs w h
      , Events.onClick events.address SwitchOrient
      ]
      [Html.text ("[Orientation] " ++ (toString model.orient))]

caption : Model -> Int -> Int -> GE.Element
caption model w h =
  let eStr = GE.leftAligned << T.monospace << T.fromString in
  colorDebug Color.orange <|
    GE.container w h GE.topLeft <|
      case (model.caption, model.mode, model.mouseMode) of
        (Just (Hovering (i,k,z)), Live info, MouseNothing) ->
          case hoverInfo info (i,k,z) of
            Nothing -> GE.empty
            Just l ->
              let numLocs = List.map (\(s,n) -> toString n ++ Utils.braces s) l in
              let line1 = (k ++ toString i) ++ " " ++ z in
              let line2 = Utils.spaces numLocs in
              eStr (" " ++ line1 ++ "\n " ++ line2)
        (Just (LangError err), _, _) ->
          eStr err
        _ ->
          GE.empty

hoverInfo info (i,k,z) =
  let err y = "hoverInfo: " ++ toString y in
  flip Utils.bindMaybe (Dict.get i info.assignments) <| \d ->
  flip Utils.bindMaybe (Dict.get z d)                <| \locs ->
    Just <|
      List.map (\(lid,_,x) ->
        let n = Utils.justGet_ (err (i,z,lid)) lid info.initSubst in
        if | x == ""   -> ("loc_" ++ toString lid, n)
           | otherwise -> (x, n)) locs

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
                       , bold <- False }

      wLogo = params.topSection.wLogo
      logo  = GE.image wLogo wLogo "sketch-n-sketch-logo.png"

      wBtnO = params.topSection.wBtnO
      hBtnO = params.topSection.hBtnO
      wJunk = params.topSection.wJunk

      wSep  = GE.spacer (wAll - (wLogo + wBtnO + wJunk)) 1
      btnO  = Html.toElement wBtnO hBtnO <| orientationButton wBtnO hBtnO model
    in
      GE.size wAll hTop <|
        GE.flow GE.right
          [ GE.container wLogo hTop GE.middle logo
          , GE.container (wAll - wLogo) hTop GE.middle <|
              GE.flow GE.right [ title, wSep, btnO ]
          ]
  in

  let midSection =
    GE.size wAll hMid <|
      case model.orient of
        Vertical   -> mainSectionVertical wAll hMid model
        Horizontal -> mainSectionHorizontal wAll hMid model in

  let botSection = GE.spacer wAll hBot in
  let sideGutter = colorDebug Color.black <| GE.spacer wGut hTot in

  let saveElement = (if
        | model.mode == SaveDialog -> 
           let dim = GE.color Color.black <| GE.opacity 0.5 <| GE.spacer w h
               pickBox = GE.color Color.black
                            <| GE.opacity 0.5 
                            <| GE.container w h GE.middle 
                            <| GE.color Color.lightBlue
                            <| Html.toElement 400 200
                            <| Html.div [Attr.style [("opacity", "1")]]
                            <| (\x -> [x])
                            <| Html.fromElement
                            <| GE.container 400 200 GE.middle 
                            <| GE.flow GE.down
                                [ GE.show "Boop!" ]
           in pickBox
                                         --TODO add dimming element and center
                                         -- input field + button that sends
                                         -- appropriate task + UpdateModel
        | otherwise -> GE.empty) in

  if | model.mode == SaveDialog ->
          GE.flow GE.inward
            [ saveElement
            , (GE.flow GE.right
                [ sideGutter
                , GE.flow GE.down
                    [ colorDebug Color.lightYellow <| topSection
                    , midSection
                    , colorDebug Color.lightYellow <| botSection
                    ]
                , sideGutter
                ]
              )
            ]
     | otherwise -> 
        GE.flow GE.right
          [ sideGutter
          , GE.flow GE.down
            [ colorDebug Color.lightYellow <| topSection
            , midSection
            , colorDebug Color.lightYellow <| botSection
            ]
          , sideGutter
          ]

-- TODO: add onMouseUp DeselectObject event to all GE.Elements...

------------------------------------------------------------------------------

-- Re: dropdown boxes
--
-- used to use Html.select / Html.option / Events.onMouseOver,
-- but didn't work in Chrome and Safari. tried
--   Events.onMouseOver
--   Events.onClick
--   Events.on "onchange" Json.Decode.value

-- so now using GI.dropDown instead
--
-- keeping the following around for reference:

{-

import Json.Decode

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
        _                          -> False
    in
    -- TODO: onClick works in Firefox, but not in Chrome/Safari
    -- TODO: same goes for Events.on "change"
    Html.option
        [ Attr.selected yes
        , Events.on "onchange" Json.Decode.value
            (\_ -> Signal.message events.address SwitchOrient)
        , Events.onClick events.address (SwitchMode m)
        ]
        [Html.text s]
  in
  -- may want to delay this to when Live is selected
  let optionLive = opt "Live" (mkLive_ model.syncOptions (Utils.fromJust model.inputExp)) in
  let optionAdHoc = opt "Ad Hoc" AdHoc in
  Html.select
    [ buttonAttrs w h ]
    [ optionLive, optionAdHoc ]

-}
