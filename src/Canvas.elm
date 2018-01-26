module Canvas exposing (build)

-- Sketch-n-Sketch Libraries ---------------------------------------------------

import Config exposing (params)
import Utils
import Either exposing (Either(..))
import HtmlUtils exposing (..)

import Lang exposing (..)
import ValUnparser exposing (..)
import LangTools
import Provenance
import LangSvg exposing (NodeId, ShapeKind, attr)
import ShapeWidgets exposing
  ( RealZone, RealZone(..)
  , SelectableFeature(..), GenericFeature(..), PointFeature(..), DistanceFeature(..), OtherFeature(..), ShapeFeature(..)
  )
import SleekLayout exposing (canvasPosition)
import Sync
import Draw
import InterfaceModel exposing (..)
import InterfaceController as Controller
import FastParser exposing (parseE)

import LangUnparser
import Eval
import Syntax exposing (Syntax)

-- Elm Libraries ---------------------------------------------------------------

import String
import Dict
import Set
import Color
import Keys

import VirtualDom
import Json.Decode
import Svg exposing (Svg)
import Svg.Attributes as SAttr
import Svg.Events exposing (onMouseDown, onMouseUp, onMouseOver, onMouseOut)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events exposing
  ( onClick, onInput, onMouseEnter, onMouseLeave
  , onWithOptions, defaultOptions
  )


--------------------------------------------------------------------------------

strOldInterfaceColor = "rgba(52,73,94,1.0)"
strOldButtonTopColor = "rgba(231,76,60,1.0)" -- from InterfaceButtons example

--------------------------------------------------------------------------------

pixels n = toString n ++ "px"

svgLine      = flip Svg.line []
svgRect      = flip Svg.rect []
svgCircle    = flip Svg.circle []
svgEllipse   = flip Svg.ellipse []
svgPolygon   = flip Svg.polygon []
svgPath      = flip Svg.path []


--------------------------------------------------------------------------------

msgClickZone zoneKey = Msg ("Click Zone" ++ toString zoneKey) <| \old ->
  case old.outputMode of
    Live ->
      -- let _ = Debug.log ("Click Zone" ++ toString zoneKey) () in
      let (_, (mx, my)) = SleekLayout.clickToCanvasPoint old (mousePosition old) in
      let trigger = Sync.prepareLiveTrigger old.liveSyncInfo old.inputExp zoneKey in
      let dragInfo = (trigger, (mx, my), False) in
      { old | mouseMode = MouseDragZone zoneKey (Just dragInfo) }
    _ ->
      old

msgMouseClickCanvas = Msg "MouseClickCanvas" <| \old ->
  case (old.tool, old.mouseMode) of
    (Cursor, MouseDragZone _ _) -> old
    (Cursor, MouseNothing) ->
      let dragMode = MouseDragSelect (mousePosition old) old.selectedShapes old.selectedFeatures old.selectedBlobs in
      if old.keysDown == [Keys.keyShift]
      then { old | mouseMode = dragMode }
      else { old | mouseMode = dragMode, selectedShapes = Set.empty, selectedFeatures = Set.empty, selectedBlobs = Dict.empty }

    (_ , MouseNothing) -> startDrawing old Nothing
    _                  -> old


startDrawing : Model -> Maybe Clickable -> Model
startDrawing old maybeClickable =
  { old | mouseMode = MouseDrawNew (DrawJustStarted maybeClickable) -- No points until drag begins, or (for paths/polys) mouse-up
        , selectedShapes = Set.empty
        , selectedBlobs = Dict.empty }


--------------------------------------------------------------------------------


build : SleekLayout.BoundingBox -> Model -> List (Html Msg)
build dim model =
  let addZones = case (model.outputMode, model.preview) of
    (Live, Nothing) -> model.tool == Cursor
    _               -> False
  in
  let (widgets, slate) =
    case model.preview of
      Just (_, Ok (val, widgets, slate)) -> (widgets, slate)
      _                                  -> (model.widgets, model.slate)
  in
  let outputElement = buildHtml (model, addZones) slate in
  let newShape = drawNewShape model in
  let widgetsAndDistances =
    case (model.outputMode, model.showGhosts, model.preview) of
      (Live, True, Nothing) -> buildDistances model slate widgets ++ buildSvgWidgets dim.width dim.height widgets model -- Draw distances below other widgets
      _                     -> []
  in
  let selectBox = drawSelectBox model in
  if LangSvg.isSvg model.inputVal then
    [ Svg.svg
        [ onMouseDown msgMouseClickCanvas
        , Attr.style
            [ ("width", pixels dim.width)
            , ("height", pixels dim.height)
            ]
        ]
        ([outputElement] ++ newShape ++ widgetsAndDistances ++ selectBox)
    ]
  else
    [ outputElement
    , Svg.svg
        [ Attr.id "svgWidgetsLayer"
        , Attr.style
            [ ("left", pixels dim.x)
            , ("top", pixels dim.y)
            , ("width", pixels dim.width)
            , ("height", pixels dim.height)
            ]
        ]
        (widgetsAndDistances ++ selectBox)
    ]


--------------------------------------------------------------------------------
-- Compiling to Svg/Html

buildHtml : (Model, Bool) -> LangSvg.RootedIndexedTree -> (Svg Msg)
buildHtml (model, addZones) (i,d) = buildHtml_ (model, addZones) False d i

buildHtml_ : (Model, Bool) -> Bool -> LangSvg.IndexedTree -> LangSvg.NodeId -> (Svg Msg)
buildHtml_ (model, addZones) insideSvgNode d i =
  case Utils.justGet_ ("buildHtml_ " ++ toString i) i d |> .interpreted of
   LangSvg.TextNode text -> VirtualDom.text text
   LangSvg.SvgNode shape attrs childIndices ->
    case (model.showGhosts, Utils.maybeRemoveFirst "HIDDEN" attrs) of
     (False, Just _) -> Svg.svg [] []
     _ ->
      -- TODO: figure out: (LangSvg.attr "draggable" "false")
      let (zones, attrs_) =
        case (addZones, Utils.maybeRemoveFirst "ZONES" attrs) of
          (False, Nothing)     -> ([], attrs)
          (False, Just (_, l)) -> ([], l)
          (True, Nothing) ->
            (makeZones model shape i attrs, attrs)
          (True, Just (aval, l)) -> case aval.interpreted of
            _ ->
              (makeZones model shape i attrs, l)
            -- TODO breaking these for now; see ZoneOptions comment.
{-
            LangSvg.AString "none" ->
              (makeZones model zoneOptions0 shape i attrs, l)
            LangSvg.AString "basic" ->
              let options' = { options | addRot = False, addColor = False } in
              (makeZones model options_ shape i attrs, l)
            _ -> Debug.crash "buildHtml_"
-}
      in
      let (rawKind, compiledAttrs) =
        let canvasDim = SleekLayout.outputCanvas model in
        attrs_
          |> LangSvg.desugarShapeAttrs canvasDim.x canvasDim.y shape
          |> Tuple.mapSecond LangSvg.compileAttrs
      in
{-
      Not simply doing the following because want to treat 'text' node
      differently when it's inside an 'svg' node.

      let node =
        if List.member rawKind ["svg", "line", "rect", ...]
          then Svg.node   -- adds http://www.w3.org/2000/svg namespace attribute
          else Html.node
-}
      let (node, isSvgNode) =
        if rawKind == "svg" then (Svg.node, True)
        else if insideSvgNode then (Svg.node, True)
        else (Html.node, False)
      in
      let children = List.map (buildHtml_ (model, addZones) isSvgNode d) childIndices in
      let mainshape = (node rawKind) compiledAttrs children in
      if zones == []
        then mainshape
        else Svg.svg [] (mainshape :: zones)

--------------------------------------------------------------------------------

dragZoneEvents id shapeKind realZone =
  let zoneKey = (id, shapeKind, realZone) in
  [ onMouseDown (msgClickZone zoneKey)
  , onMouseOver (turnOnCaptionAndHighlights zoneKey)
  , onMouseOut turnOffCaptionAndHighlights
  ]

--------------------------------------------------------------------------------

drawNewShape model =
  case (model.tool, model.mouseMode) of
    -- (Line _,        MouseDrawNew (TwoPoints pt1 pt2))                            -> Draw.drawNewLine model pt1 pt2
    -- (Rect _,        MouseDrawNew (TwoPoints pt1 pt2))                            -> Draw.drawNewRect model.keysDown pt1 pt2
    -- (Oval _,        MouseDrawNew (TwoPoints pt1 pt2))                            -> Draw.drawNewEllipse model.keysDown pt1 pt2
    -- (Poly _,        MouseDrawNew (PolyPoints (ptLast::pts)))                     -> Draw.drawNewPolygon ptLast pts
    -- (Path _,        MouseDrawNew (PathPoints (ptLast::pts)))                     -> Draw.drawNewPath ptLast pts
    -- (PointOrOffset, MouseDrawNew (Offset1D pt2 snap (x1Val, y1Val))) -> drawNewPointAndOffset model (snap /= NoSnap) pt2 (round (valToNum x1Val), round (valToNum y1Val))
    -- (PointOrOffset, MouseDrawNew (TwoPoints (_, pt1) (_, pt2)))                  -> drawNewPointAndOffset model False pt1 pt2
    -- (HelperLine,    MouseDrawNew (TwoPoints pt1 pt2))                            -> Draw.drawNewLine model pt1 pt2
    -- (Lambda _,      MouseDrawNew (TwoPoints pt1 pt2))                            -> Draw.drawNewRect model.keysDown pt1 pt2
    -- (Function _,    MouseDrawNew (TwoPoints pt1 pt2))                            -> Draw.drawNewRect model.keysDown pt1 pt2
    -- (Text,          MouseDrawNew (TwoPoints pt1 pt2))                            -> Draw.drawNewRect model.keysDown pt1 pt2
    _                                                                            -> []


drawNewPointAndOffset model shouldHighlight (x2, y2) (x1, y1) =
  let (axis, sign, amount) = Draw.horizontalVerticalSnap (x1, y1) (x2, y2) in
  let xyDot = svgXYDot model (x1, y1) pointZoneStyles.fill.shown True [] in
  let (arrowParts, _) = svgOffsetWidget1DArrowPartsAndEndPoint model.inputExp Nothing ((toFloat x1, dummyTrace), (toFloat y1, dummyTrace)) axis sign (amount, dummyTrace) dummyVal shouldHighlight [] in
  [xyDot] ++ arrowParts


--------------------------------------------------------------------------------
-- Widget Layer

dummyVal : Val
dummyVal = { v_ = VList [], provenance = dummyProvenance, parents = Parents [] }

svgOffsetWidget1DArrowPartsAndEndPoint program modelRenamingInOutput ((baseX, baseXTr), (baseY, baseYTr)) axis sign (amount, amountTr) amountVal isSelected extraStyles =
  let (effectiveAmount, ((endX, endXTr), (endY, endYTr))) =
    offsetWidget1DEffectiveAmountAndEndPoint ((baseX, baseXTr), (baseY, baseYTr)) axis sign (amount, amountTr)
  in
  let lineStyle =
    if isSelected then
      [ attr "stroke" colorPointSelected
      , attr "stroke-width" "5px"
      ]
    else
      [ attr "stroke" "black"
      , attr "stroke-width" "1px"
      , attr "stroke-dasharray" "1,1"
      ]
  in
  let line =
    flip Svg.line [] <|
      [ attrNum "x1" baseX, attrNum "y1" baseY
      , attrNum "x2" endX,  attrNum "y2" endY
      ] ++ lineStyle ++ extraStyles
  in
  let endArrow =
    let arrowOffset = 12 in
    let (opX1, opY1, opX2, opY2) =
      case (axis, Utils.sgn effectiveAmount) of
        (X, 1)  -> ((-), (-), (-), (+))
        (X, -1) -> ((+), (-), (+), (+))
        (Y, 1)  -> ((-), (-), (+), (-))
        _       -> ((-), (+), (+), (+))
    in
    flip Svg.polyline [] <|
      [ attr "fill" "rgba(0,0,0,0.0)"
      , attr "cursor" "pointer"
      , attr "points" <| toString (opX1 endX arrowOffset) ++ "," ++ toString (opY1 endY arrowOffset) ++ " " ++
                         toString endX                    ++ "," ++ toString endY ++ " " ++
                         toString (opX2 endX arrowOffset) ++ "," ++ toString (opY2 endY arrowOffset) ++ " "
      ] ++ lineStyle ++ extraStyles
  in
  let caption =
    let (x, y, left, top, textAnchor) =
      case axis of
        X -> ((baseX + endX) / 2, baseY - 10,        (baseX + endX) / 2 - 25, baseY,                   "middle")
        Y -> (baseX + 10,         (baseY + endY) / 2, baseX + 10,             (baseY + endY) / 2 + 10, "start")
    in
    let maybeBold =
      if isSelected
      then [ attr "font-weight" "bold" ]
      else []
    in
    case Provenance.valToMaybeLetPat program amountVal of
      Just pat -> patInOutput modelRenamingInOutput False pat left top
      Nothing ->
        let string = toString amount in
        flip Svg.text_ [VirtualDom.text string] <|
          [ attr "font-family" params.mainSection.uiWidgets.font
          , attr "font-size" params.mainSection.uiWidgets.fontSize
          , attr "text-anchor" textAnchor
          , attr "x" (toString x)
          , attr "y" (toString y)
          ] ++ maybeBold ++ extraStyles
  in
  ([line, caption, endArrow], ((endX, endXTr), (endY, endYTr)))


patAsHTML : Maybe (PId, String) -> Bool -> Pat -> Html Msg
patAsHTML modelRenamingInOutput showRemover pat  =
  let nameStr = LangTools.patToMaybeIdent pat |> Maybe.withDefault "" in
  let pid = pat.val.pid in
  let text =
    Html.span
        [ Attr.class "pat"
        , Attr.title <| "Click to rename " ++ nameStr
        , onMouseDownAndStop (Controller.msgActivateRenameInOutput pid)
        ] <|
        [ VirtualDom.text nameStr ] ++
        if showRemover
        then [ Html.span [Attr.class "remove-arg", Attr.title <| "Remove arg "  ++ nameStr, onMouseDownAndStop (Controller.msgRemoveArg pid)] [VirtualDom.text "❌"] ]
        else []
      -- [ attr "font-family" params.mainSection.uiWidgets.font
      -- , attr "font-size" params.mainSection.uiWidgets.fontSize
      -- , attr "text-anchor" "start"
  in
  let maybeRenameBox =
    modelRenamingInOutput
    |> Utils.filterMaybe (\(renamingPId, _) -> renamingPId == pid)
    |> Maybe.map
        (\(renamingPId, renameStr) ->
          flip Html.input [] <|
            [ Attr.defaultValue renameStr
            , Attr.id "rename-box"
            , Attr.class "pat"
            , onInput Controller.msgUpdateRenameInOutputTextBox
            , onClickWithoutPropagation Controller.msgNoop
            , onKeyDown <|
                \keyCode ->
                  if keyCode == enterKeyCode then -- Enter button
                    Controller.msgDoRename renamingPId
                  else
                    Controller.msgNoop
            ]
        )
  in
  Maybe.withDefault text maybeRenameBox


patInOutput : Maybe (PId, String) -> Bool -> Pat -> Float -> Float -> Svg Msg
patInOutput modelRenamingInOutput showRemover pat left top =
  patsInOutput modelRenamingInOutput showRemover [pat] left top


patsInOutput : Maybe (PId, String) -> Bool -> List Pat -> Float -> Float -> Svg Msg
patsInOutput modelRenamingInOutput showRemover pats left top =
  let elements = pats |> List.map (patAsHTML modelRenamingInOutput showRemover) in
  flip Svg.foreignObject [Html.div [Attr.class "pats", Attr.style [("width", toString (100 * List.length pats) ++ "px")]] elements] <|
    [ attr "x" (toString (left - 2))
    , attr "y" (toString (top - 10 - 17))
    ]


buildSvgWidgets : Int -> Int -> Widgets -> InterfaceModel.Model -> List (Svg Msg)
buildSvgWidgets wCanvas hCanvas widgets model =
  let
    pad            = params.mainSection.uiWidgets.pad
    wSlider        = params.mainSection.uiWidgets.wSlider
    hSlider        = params.mainSection.uiWidgets.hSlider
    wCaption       = params.mainSection.uiWidgets.wCaption

    numWidgets    = List.length widgets
    wWidget       = wSlider + wCaption + 2*pad
    hWidget       = hSlider + 2*pad
    wToolBoxMax   = toFloat <| wCanvas - 2*pad
    numCols       = floor (wToolBoxMax / wWidget)
    numRows       = ceiling (toFloat numWidgets / toFloat numCols)
    wToolBox      = numCols * wWidget
    hToolBox      = numRows * hWidget
    xL            = pad
    yBL           = hCanvas - hWidget - pad
  in

  let drawNumWidget i_ widget locId cap_ minVal maxVal curVal =
    let i = i_ - 1 in
    let idAsShape = -2 - i_ in
    let
      (r,c) = (i % numRows, i // numRows)
      xi    = xL  + c*wWidget
      yi    = yBL - r*hWidget
      xi_   = xi + pad
      yi_   = yi + pad
    in
    let region =
      flip Svg.rect [] <|
        [ attr "fill" "lightgray"
        , attr "stroke" strOldInterfaceColor , attr "stroke-width" "3px"
        , attr "rx" "9px" , attr "ry" "9px"
        , attr "x" (toString (xL  + c*wWidget))
        , attr "y" (toString (yBL - r*hWidget))
        , attr "width" (toString wWidget) , attr "height" (toString hWidget)
        ]
    in
    let box =
      let selectableFeature = ShapeFeature idAsShape (OFeat Quantity) in
      let color =
        case model.tool of
          Cursor ->
            if Set.member selectableFeature model.selectedFeatures
              then colorPointSelected
              else strOldInterfaceColor -- colorPointNotSelected
          _ -> strOldInterfaceColor
      in
      flip Svg.rect [] <|
        [ attr "fill" color
        , attr "stroke" "20px", attr "stroke-width" "20px"
        , attr "x" (toString (xL  + c*wWidget + pad))
        , attr "y" (toString (yBL - r*hWidget + pad))
        , attr "width" (toString wSlider) , attr "height" (toString hSlider)
        , onMouseDownAndStop (toggleSelected [selectableFeature])
        ]
    in
    let ball =
      let (range, diff) = (maxVal - minVal, curVal - minVal) in
      let pct = diff / range in
      let cx = xi + pad + round (pct*wSlider) in
      let cy = yi + pad + (hSlider//2) in
      flip Svg.circle [] <|
        [ attr "stroke" "black" , attr "stroke-width" "2px"
        , attr "fill" strOldButtonTopColor
        , attr "r" params.mainSection.uiWidgets.rBall
        , attr "cx" (toString cx) , attr "cy" (toString cy)
        , cursorOfZone ZSlider "default"
        ] ++ dragZoneEvents idAsShape "slider" ZSlider
    in
    let text =
      let cap = cap_ ++ strNumTrunc 5 curVal in
      flip Svg.text_ [VirtualDom.text cap] <|
        [ attr "fill" "black"
        , attr "font-family" params.mainSection.uiWidgets.font
        , attr "font-size" params.mainSection.uiWidgets.fontSize
        , attr "x" (toString (xi_ + wSlider + 10))
        , attr "y" (toString (yi_ + 18))
        ]
    in
    [region, box, text, ball]
  in
  let drawPointWidget i_ widget (cx, cxTr) xVal (cy, cyTr) yVal =
    let idAsShape = -2 - i_ in
    zoneSelectCrossDot model True (idAsShape, "point", LonePoint) (cx, cxTr) xVal (cy, cyTr) yVal
    ++ if model.tool /= Cursor then [] else zonePoint model True idAsShape "point" (ZPoint LonePoint) [] (cx, cy)
  in
  let drawOffsetWidget1D i_ baseXNumTr baseYNumTr axis sign (amount, amountTr) amountVal endXVal endYVal =
    let idAsShape = -2 - i_ in
    let isSelected = Set.member (ShapeFeature idAsShape (DFeat Offset)) model.selectedFeatures in
    let dragStyle =
      if model.tool == Cursor then
        [ attr "cursor" "pointer"
        , onMouseEnter (addHoveredShape idAsShape)
        ] ++ dragZoneEvents idAsShape "offset" ZOffset1D
      else
        [ attr "cursor" "default" ]
    in
    let (arrowParts, (endXNumTr, endYNumTr)) =
      let shouldHighlight =
        isSelected || isShapeBeingDrawnSnappingToVal model amountVal
      in
      svgOffsetWidget1DArrowPartsAndEndPoint model.inputExp model.renamingInOutput (baseXNumTr, baseYNumTr) axis sign (amount, amountTr) amountVal shouldHighlight dragStyle
    in
    let endPt =
      zoneSelectCrossDot model False (idAsShape, "offset", EndPoint) endXNumTr endXVal endYNumTr endYVal
    in
    if amount /= 0 then
      [ Svg.g
          [onMouseLeave (removeHoveredShape idAsShape)]
          <| arrowParts ++ endPt
      ]
    else
      []
  in
  let drawCallWidget funcVal argVals retVal retWs model =
    let program = model.inputExp in
    let maybeBounds =
      retVal::argVals
      |> List.map ShapeWidgets.valToMaybeBounds
      |> (++) (retWs |> List.map ShapeWidgets.maybeWidgetBounds)
      |> Utils.filterJusts
      |> ShapeWidgets.maybeEnclosureOfAllBounds
    in
    case maybeBounds of
      Nothing -> []
      Just (left, top, right, bot) ->
        let padding = 25 in
        let (maybeFuncBody, maybeFuncPat, maybeArgPats) =
          case funcVal.v_ of
            VClosure maybeRecName argPats funcBody env ->
              case parentByEId program funcBody.val.eid of
                Just (Just funcExp) ->
                  case LangTools.findLetAndPatMatchingExpLoose funcExp.val.eid program of
                    Just (_, funcPat) -> (Just funcBody, Just funcPat, Just argPats)
                    _                 -> (Just funcBody, Nothing,      Just argPats)
                _ -> (Just funcBody, Nothing, Just argPats)
            _ -> (Nothing, Nothing, Nothing)
        in
        let maybeAddArg =
          -- TODO: ensure all selected items touch funcBody
          case (maybeFuncBody, nothingSelectedInOutput model) of
            (Just funcBody, False) ->
              Just <|
                flip Svg.text_ [Svg.title [] [VirtualDom.text "Add argument"], VirtualDom.text "➕"] <| -- plus symbol (doesn't show up on black editor background)
                  [ attr "font-family" params.mainSection.uiWidgets.font
                  , attr "font-size" params.mainSection.uiWidgets.fontSize
                  , attr "text-anchor" "end"
                  , attr "cursor" "pointer"
                  , attr "x" (toString (right + padding))
                  , attr "y" (toString (top - padding - 10))
                  , onMouseDownAndStop (Controller.msgAddArg funcBody)
                  ]
            _ ->
              Nothing
        in
        let box =
          flip Svg.rect [] <|
            [ attr "fill" "none"
            , attr "stroke" "black"
            , attr "stroke-width" "5px"
            , attr "stroke-dasharray" "20,10"
            , attr "opacity" "0.3"
            , attr "rx" (toString padding)
            , attr "ry" (toString padding)
            , attr "x" (toString (left - padding))
            , attr "y" (toString (top - padding))
            , attr "width" (toString (right - left + padding*2))
            , attr "height" (toString (bot - top + padding*2))
            ]
        in
        [ Just box
        , maybeFuncPat |> Maybe.map (\funcPat -> patInOutput  model.renamingInOutput False funcPat (left - padding) (top - padding - 20))
        , maybeArgPats |> Maybe.map (\argPats -> patsInOutput model.renamingInOutput True  argPats (left - padding) (top - padding))
        , maybeAddArg
        ] |> Utils.filterJusts
  in

  let draw (i_, widget) =
    case widget of

      WNumSlider _ _ _ _ _ _ True -> []
      WIntSlider _ _ _ _ _ _ True -> []

      WNumSlider minVal maxVal cap curVal val (k,_,_) False ->
        drawNumWidget i_ widget k cap minVal maxVal curVal

      WIntSlider a b cap c val (k,_,_) False ->
        let (minVal, maxVal, curVal) = (toFloat a, toFloat b, toFloat c) in
        drawNumWidget i_ widget k cap minVal maxVal curVal

      WPoint xNumTr xVal yNumTr yVal ->
        drawPointWidget i_ widget xNumTr xVal yNumTr yVal

      WOffset1D baseXNumTr baseYNumTr axis sign amountNumTr amountVal endXVal endYVal ->
        drawOffsetWidget1D i_ baseXNumTr baseYNumTr axis sign amountNumTr amountVal endXVal endYVal

      WCall funcVal argVals retVal retWs ->
        drawCallWidget funcVal argVals retVal retWs model
  in

  List.concat <| Utils.mapi1 draw widgets


buildDistances : Model -> LangSvg.RootedIndexedTree -> Widgets -> List (Svg Msg)
buildDistances model slate widgets =
  let selectedPoints = ShapeWidgets.featuresToSelectablePoints (Set.toList model.selectedFeatures) in
  let pointsAtEndOfSelectedDistances =
    model.selectedFeatures
    |> Set.toList
    |> List.concatMap
        (\selectedFeature ->
          case selectedFeature of
            DistanceBetweenFeatures selectablePointPair -> Set.toList selectablePointPair
            _                                           -> []
        )
  in
  let candidateEndpoints = selectedPoints ++ pointsAtEndOfSelectedDistances |> Utils.dedup in
  Utils.cartProd candidateEndpoints candidateEndpoints
  |> List.map (\(selectedPt1, selectedPt2) -> Set.fromList [selectedPt1, selectedPt2])
  |> List.filter (Set.size >> (==) 2)
  |> Utils.dedup -- Cartesian product produces both {a,b} and {b,a}, which are equivalent
  |> List.sortBy (\pointPairSet -> if Set.member (DistanceBetweenFeatures pointPairSet) model.selectedFeatures then 1 else 0) -- Draw selected distances on top of non-selected distances
  |> List.concatMap
      (\pointPairSet ->
        let (selectablePoint1, selectablePoint2) = ShapeWidgets.extractSelectablePoints pointPairSet in
        -- ...evaluates to concrete values...could speed up by evaluating earlier (here is n^2)...
        case ( ShapeWidgets.selectablePointToMaybeXY selectablePoint1 slate widgets
             , ShapeWidgets.selectablePointToMaybeXY selectablePoint2 slate widgets ) of
          (Just (x1, y1), Just (x2, y2)) ->
            let selectableFeature = DistanceBetweenFeatures (Set.fromList [selectablePoint1, selectablePoint2]) in
            let isSelected = Set.member selectableFeature model.selectedFeatures in
            let color = if isSelected then colorPointSelected else colorLineNotSelected in
            let deselectEndPoints model =
              if isSelected then
                model
              else
                let featuresToDeselect = List.concatMap (ShapeWidgets.selectablePointToSelectableFeatures >> Utils.pairToList) [selectablePoint1, selectablePoint2] in
                { model | selectedFeatures = model.selectedFeatures |> Set.filter (\selectedFeature -> not <| List.member selectedFeature featuresToDeselect) }
            in
            let line =
              svgLine [
                  attr "stroke" color
                , attr "stroke-width" hairStrokeWidth
                , attr "x1" (toString x1) , attr "y1" (toString y1)
                , attr "x2" (toString x2) , attr "y2" (toString y2)
                , onMouseDownAndStop (Msg "Toggle Selected Distance..." <| toggleSelectedLambda [selectableFeature] >> deselectEndPoints)
                ]
            in
            [line]
          _ ->
            []
      )


--------------------------------------------------------------------------------
-- Select Box

drawSelectBox : Model -> List (Svg Msg)
drawSelectBox model =
  case model.mouseMode of
    MouseDragSelect initialPosition _ _ _ ->
      let pos1 = canvasPosition model initialPosition in
      let pos2 = canvasPosition model (mousePosition model) in
      let top   = min pos1.y pos2.y in
      let left  = min pos1.x pos2.x in
      let bot   = max pos1.y pos2.y in
      let right = max pos1.x pos2.x in
      List.singleton <|
      flip Svg.rect [] <|
        [ attr "fill" "none"
        , attr "stroke" "black"
        , attr "stroke-width" "2px"
        , attr "stroke-dasharray" "5,5"
        , attr "x" (toString left)
        , attr "y" (toString top)
        , attr "width" (toString <| right - left)
        , attr "height" (toString <| bot - top)
        ]

    _ ->
      []

--------------------------------------------------------------------------------
-- Defining Zones

-- okay to use dummy VTraces/Traces here, b/c compileAttr throws them away
attrNum k n    = LangSvg.compileAttr k (LangSvg.aNum (n, dummyTrace))
attrNumTr k nt = LangSvg.compileAttr k (LangSvg.aNum nt)

onMouseDownAndStop = handleEventAndStop "mousedown"

-- TODO use RealZones rather than Zones more

removeHoveredShape id =
  Msg ("Remove Hovered Shape " ++ toString id) <| \m ->
    { m | hoveredShapes = Set.remove id m.hoveredShapes }

addHoveredShape id =
  Msg ("Add Hovered Shape " ++ toString id) <| \m ->
    if isMouseDown m
    then m
    else { m | hoveredShapes = Set.singleton id }
    -- { m | hoveredShapes = Set.insert id m.hoveredShapes }

addHoveredCrosshair tuple =
  Msg ("Add Hovered Crosshair " ++ toString tuple) <| \m ->
    if isMouseDown m
    then m
    else { m | hoveredCrosshairs = Set.insert tuple m.hoveredCrosshairs }

removeHoveredCrosshair tuple =
  Msg ("Remove Hovered Crosshair " ++ toString tuple) <| \m ->
    { m | hoveredCrosshairs = Set.remove tuple m.hoveredCrosshairs }

cursorStyle s = attr "cursor" s

-- TODO should take into account disabled zones in Live mode
cursorOfZone realZone default = case realZone of

  -- primary manipulation zones
  ZInterior        -> cursorStyle "move"
  ZPoint LonePoint -> cursorStyle "move"
  ZPoint LeftEdge  -> cursorStyle "ew-resize"
  ZPoint RightEdge -> cursorStyle "ew-resize"
  ZPoint TopLeft   -> cursorStyle "nwse-resize"
  ZPoint BotRight  -> cursorStyle "nwse-resize"
  ZPoint TopEdge   -> cursorStyle "ns-resize"
  ZPoint BotEdge   -> cursorStyle "ns-resize"
  ZPoint BotLeft   -> cursorStyle "nesw-resize"
  ZPoint TopRight  -> cursorStyle "nesw-resize"
  ZLineEdge        -> cursorStyle "pointer"
  ZPolyEdge _      -> cursorStyle "pointer"

  -- indirect manipulation zones
  ZOther _         -> cursorStyle "pointer"
  ZSlider          -> cursorStyle "pointer"

  _                -> cursorStyle default

isPrimaryZone realZone =
  case realZone of
    ZOther FillColor     -> False
    ZOther StrokeColor   -> False
    ZOther FillOpacity   -> False
    ZOther StrokeOpacity -> False
    ZOther StrokeWidth   -> False
    ZOther Rotation      -> False
    ZSlider              -> False
    _                    -> True

isFillStrokeZone realZone =
  case realZone of
    ZOther FillColor     -> True
    ZOther StrokeColor   -> True
    ZOther FillOpacity   -> True
    ZOther StrokeOpacity -> True
    ZOther StrokeWidth   -> True
    _                    -> False

isRotateZone realZone =
  case realZone of
    ZOther Rotation -> True
    _               -> False


-- Stuff for Basic Zones -------------------------------------------------------

draggableZone svgFunc addStroke model id shape realZone attrs =
  let showStroke = False in -- set to True for debugging
  flip svgFunc [] <|
    attrs ++
    dragZoneEvents id shape realZone ++
    [ cursorOfZone realZone "default"
    , attr "fill" "rgba(0,0,0,0.0)"
    , attr "stroke-width" <| if addStroke then "10" else "0"
    , attr "stroke" <| if showStroke
                               then "rgba(255,0,0,0.5)"
                               else "rgba(0,0,0,0.0)"
    ]

objectZoneIsCurrentlyBeingManipulated : Model -> NodeId -> (RealZone -> Bool) -> Bool
objectZoneIsCurrentlyBeingManipulated model nodeId zonePred =
  case model.mouseMode of
    MouseDragZone (id, _, zone) _ -> nodeId == id && zonePred zone
    _                             -> False

objectIsCurrentlyBeingManipulated model nodeId =
  objectZoneIsCurrentlyBeingManipulated model nodeId (always True)

boundingBoxZones model id (left, top, right, bot) shapeWidgets =
  let pad = 10 in
  let maybeBackgroundBox =
    if objectIsCurrentlyBeingManipulated model id then []
    else if not (Set.member id model.hoveredShapes) then []
    else
      List.singleton <| svgRect <|
        [ attr "x" (toString (left - pad))
        , attr "y" (toString (top - pad))
        , attr "width" (toString (right - left + 2 * pad))
        , attr "height" (toString (bot - top + 2 * pad))
        , attr "fill" "rgba(100,100,100,0.0)"
        , attr "stroke" "lightgray"
        , attr "stroke-width" "1"
        ]
  in
  -- using group so that the onMouseLeave handler gets attached to
  -- all the nested widgets. probably not needed if pad >> 0, since
  -- will have to mouseLeave the backgroundBox after any other shapes.
  Svg.g
    [onMouseLeave (removeHoveredShape id) ]
    (maybeBackgroundBox ++ shapeWidgets)

minLengthForMiddleZones = 30

eightCardinalZones model id shape transform (left, top, right, bot) =
  let (width, height) = (right - left, bot - top) in
  let ifEnoughSpace len xs = if len < minLengthForMiddleZones then [] else xs in
  let mkPoint realZone cx cy =
    zonePoint model False id shape realZone transform (cx, cy)
  in
    mkPoint (ZPoint TopLeft) left top ++
    mkPoint (ZPoint TopRight) right top ++
    mkPoint (ZPoint BotLeft) left bot ++
    mkPoint (ZPoint BotRight) right bot ++
    ifEnoughSpace height (mkPoint (ZPoint LeftEdge) left (top + height / 2)) ++
    ifEnoughSpace height (mkPoint (ZPoint RightEdge) right (top + height / 2)) ++
    ifEnoughSpace width (mkPoint (ZPoint TopEdge) (left + width / 2) top) ++
    ifEnoughSpace width (mkPoint (ZPoint BotEdge) (left + width / 2) bot)

pointZoneStyles =
  { radius = "6"
  , stroke = "black"
  , strokeWidth = "2"
  , fill =
      { shown = "white" -- "silver" -- "rgba(255,0,0,0.5)"
      , selectedShape = "yellow"
      , selectedBlob = "aqua" -- "rgba(255,255,0,1.0)"
      , hidden = "rgba(0,0,0,0.0)"
      }
  }

pointZoneStylesFillSelected model nodeId =
  let d = Dict.filter (\_ nodeId_ -> nodeId == nodeId_) model.selectedBlobs in
  if Dict.isEmpty d
    then pointZoneStyles.fill.selectedShape
    else pointZoneStyles.fill.selectedBlob

zonePoint model alwaysShow id shapeKind realZone transform (x_,y_) =
  let
    x = x_ - model.outputCanvasInfo.scrollLeft
    y = y_ - model.outputCanvasInfo.scrollTop
  in
  let maybeStyles =
    let maybeStyles_ () =
      if objectZoneIsCurrentlyBeingManipulated model id ((==) realZone) then
        Just (pointZoneStylesFillSelected model id)
      else if objectIsCurrentlyBeingManipulated model id then
        Nothing
      else if Set.member id model.selectedShapes then
        Just (pointZoneStylesFillSelected model id)
      else if Set.member id model.hoveredShapes || alwaysShow then
        Just pointZoneStyles.fill.shown
      else
        Nothing
    in
    case ShapeWidgets.zoneToMaybePointFeature realZone of
      Nothing -> maybeStyles_ ()
      Just pointFeature ->
        if Set.member (id, pointFeature) model.hoveredCrosshairs
        then Nothing
        else maybeStyles_ ()
  in
  case maybeStyles of
    Nothing -> []
    Just fill ->
      List.singleton <| svgCircle <|
        [ attrNum "cx" x, attrNum "cy" y
        , attr "r" pointZoneStyles.radius
        , attr "fill" fill
        , attr "stroke" pointZoneStyles.stroke
        , attr "stroke-width" pointZoneStyles.strokeWidth
        , cursorOfZone realZone "pointer"
        ] ++
        dragZoneEvents id shapeKind realZone ++
        transform

zonePoints model id shape transform pts =
  List.concat <| flip Utils.mapi1 pts <| \(i, (x,y)) ->
    zonePoint model False id shape (ZPoint (Point i)) transform
      (Tuple.first x, Tuple.first y)

-- TODO rename this once original zonePoints is removed
zonePoints2 model id shape transform pts =
  List.concat <| flip Utils.mapi1 pts <| \(i, (x,y)) ->
    zonePoint model False id shape (ZPoint (Point i)) transform
      (x, y)

zoneLine model id shape realZone (x1,y1) (x2,y2) attrs =
  draggableZone Svg.line True model id shape realZone <|
    [ attrNumTr "x1" x1 , attrNumTr "y1" y1
    , attrNumTr "x2" x2 , attrNumTr "y2" y2
    , cursorStyle "pointer"
    ] ++ attrs

-- TODO rename this once original zoneLine is removed
zoneLine2 model id shape realZone (x1,y1) (x2,y2) attrs =
  draggableZone Svg.line True model id shape realZone <|
    [ attrNum "x1" x1 , attrNum "y1" y1
    , attrNum "x2" x2 , attrNum "y2" y2
    , cursorStyle "pointer"
    ] ++ attrs

-- Stuff for Rotate Zones ------------------------------------------------------

rotZoneDelta = 20

maybeTransformCmds : List LangSvg.Attr -> Maybe (List LangSvg.TransformCmd)
maybeTransformCmds l =
  case Utils.maybeFind "transform" l of
    Just aval -> case aval.interpreted of
      LangSvg.ATransform cmds -> Just cmds
      _                       -> Nothing
    _                         -> Nothing

transformAttr cmds =
  [LangSvg.compileAttr "transform" (LangSvg.aTransform cmds)]

maybeTransformAttr l =
  case maybeTransformCmds l of
    Just cmds -> transformAttr cmds
    Nothing   -> []

zoneRotate model id shape (cx,cy) r maybeCmds =
  let pred z = isPrimaryZone z || isFillStrokeZone z in
  case ( Set.member id model.selectedShapes
       , objectZoneIsCurrentlyBeingManipulated model id pred
       , maybeCmds ) of
    (True, False, Just cmds) -> zoneRotate_ model id shape cx cy r cmds
    _                        -> []

zoneRotate_ model id shape cx cy r cmds =
  let (a, stroke, strokeWidth, rBall) =
      (20, "rgba(192,192,192,0.5)", "5", "7") in
  let (fillBall, swBall) = ("silver", "2") in
  let transform = transformAttr cmds in
  let circle =
    flip Svg.circle [] <|
      [ attr "fill" "none"
      , attr "stroke" stroke , attr "stroke-width" strokeWidth
      , attr "cx" (toString cx) , attr "cy" (toString cy)
      , attr "r"  (toString r)
      ]
  in
  let ball =
    flip Svg.circle [] <|
      [ attr "stroke" "black" , attr "stroke-width" swBall
      , attr "fill" fillBall
      , attr "cx" (toString cx) , attr "cy" (toString (cy - r))
      , attr "r"  rBall
      , cursorOfZone (ZOther Rotation) "default"
      ] ++ transform
        ++ dragZoneEvents id shape (ZOther Rotation)
  in
  let line =
    let (strokeColor, maybeEventHandler) =
      case (cmds, model.tool) of
        ([LangSvg.Rot (_,trace) _ _], Cursor) ->
          let selectableFeature = ShapeFeature id (OFeat Rotation) in
          let handler = [onMouseDownAndStop (toggleSelected [selectableFeature])] in
          if Set.member selectableFeature model.selectedFeatures
            then (colorPointSelected, handler)
            else (colorPointNotSelected, handler)
        _ ->
          (stroke, [])
    in
    flip Svg.line [] <|
      [ attr "stroke" strokeColor , attr "stroke-width" strokeWidth
      , attr "x1" (toString cx) , attr "y1" (toString cy)
      , attr "x2" (toString cx) , attr "y2" (toString (cy - r))
      ] ++ transform
        ++ maybeEventHandler
  in
  [circle, line, ball]


-- TODO redo callsite
zoneRotatePolyOrPath model id kind pts nodeAttrs =
  let (xMin, xMax, yMin, yMax) =
    Draw.boundingBoxOfPoints_ (List.map (\(x,y) -> (Tuple.first x, Tuple.first y)) pts) in
  let (w, h) = (xMax - xMin, yMax - yMin) in
  let (xMiddle, yMiddle) = (xMin + 0.5 * w, yMin + 0.5 * h) in
  let r = ((max w h) / 2) + rotZoneDelta in
  zoneRotate model id kind (xMiddle, yMiddle) r (maybeTransformCmds nodeAttrs)


--------------------------------------------------------------------------------

zonesStroke model id shape x y l =
  let (maybeColor, maybeOpacity) = maybeColorNumAttr "stroke" l in
  let maybeStrokeWidth = maybeStrokeWidthNumAttr l in
  zoneStrokeOpacity model id shape (x - wOpacityBox - 5) y maybeOpacity ++
  zoneStrokeColor model id shape x y maybeColor ++
  zoneStrokeWidth model id shape (x + wGradient + 5) y maybeStrokeWidth

zonesFill model id shape x y l =
  let (maybeColor, maybeOpacity) = maybeColorNumAttr "fill" l in
  zoneFillOpacity model id shape (x - wOpacityBox - 5) y maybeOpacity ++
  zoneFillColor model id shape x y maybeColor

zonesFillAndStroke model id shape x y l =
  zonesFill model id shape x y l ++
  zonesStroke model id shape x (y - hZoneColor - 5) l

zoneFillColor   = zoneColor (ZOther FillColor) (OFeat FillColor)
zoneStrokeColor = zoneColor (ZOther StrokeColor) (OFeat StrokeColor)

zoneFillOpacity   = zoneOpacity (ZOther FillOpacity) (OFeat FillOpacity)
zoneStrokeOpacity = zoneOpacity (ZOther StrokeOpacity) (OFeat StrokeOpacity)


-- Stuff for Color Zones -------------------------------------------------------

wGradient = ShapeWidgets.wColorSlider
hZoneColor = 20

maybeColorNumAttr : String -> List LangSvg.Attr -> (Maybe NumTr, Maybe NumTr)
maybeColorNumAttr k l =
  case Utils.maybeFind k l of
    Just aval -> case aval.interpreted of
      LangSvg.AColorNum (nt, maybeOpacity) -> (Just nt, maybeOpacity)
      _                                    -> (Nothing, Nothing)
    _                                      -> (Nothing, Nothing)

zoneColor realZone shapeFeature model id shape x y maybeColor =
  let pred z = isPrimaryZone z || isRotateZone z in
  let shapeSelected = Set.member id model.selectedShapes in
  let selectableFeature = ShapeFeature id shapeFeature in
  let featureSelected =
    Set.member selectableFeature
               model.selectedFeatures in
  case ( shapeSelected || featureSelected
       , objectZoneIsCurrentlyBeingManipulated model id pred
       , maybeColor ) of
    (True, False, Just nt) -> zoneColor_ realZone shapeFeature model id shape x y nt
    _                      -> []

zoneColor_ : RealZone -> ShapeFeature -> Model -> NodeId -> ShapeKind
          -> Num -> Num -> NumTr -> List (Svg Msg)
zoneColor_ realZone shapeFeature model id shape x y (n, trace) =
  let (w, h, a, stroke, strokeWidth, rBall) =
      (wGradient, hZoneColor, 20, "silver", "2", "7") in
  let yOff = a + rotZoneDelta in
  let selectableFeature = ShapeFeature id shapeFeature in
  let ball =
    let cx = x + (n / LangSvg.maxColorNum) * wGradient in
    let cy = y - yOff + (h/2) in
    flip Svg.circle [] <|
      [ attr "stroke" "black" , attr "stroke-width" strokeWidth
      , attr "fill" stroke
      , attr "cx" (toString cx) , attr "cy" (toString cy)
      , attr "r"  rBall
      , cursorOfZone realZone "default"
      ] ++ dragZoneEvents id shape realZone
  in
  let box =
    flip Svg.rect [] <|
      [ attr "fill" <|
          if Set.member selectableFeature model.selectedFeatures
            then colorPointSelected
            else "none" -- colorPointNotSelected
      , attr "stroke" stroke , attr "stroke-width" strokeWidth
      , attr "x" (toString x) , attr "y" (toString (y - yOff))
      , attr "width" (toString w) , attr "height" (toString h)
      ]
  in
  -- TODO would probably be faster with an image...
  let gradient () =
    List.map (\i ->
      let (r,g,b) = Utils.numToColor ShapeWidgets.wColorSlider i in

      let fill =
        "rgb" ++ Utils.parens (String.join "," (List.map toString [r,g,b]))
      in
      flip Svg.rect [] <|
        [ attr "fill" fill
        , attr "x" (toString (x+i)) , attr "y" (toString (y - yOff))
        , attr "width" "1" , attr "height" (toString h)
        ]) (List.map toFloat (List.range 0 w))
  in
  [ Svg.g
      [onMouseDownAndStop (toggleSelected [selectableFeature])]
      (gradient () ++ [box])
  , ball
  ]


-- Stuff for Color Opacity Zones -----------------------------------------------

wOpacityBox = ShapeWidgets.wOpacitySlider

-- TODO could abstract the zoneColor, zoneOpacity, and zoneStrokeWidth sliders

zoneOpacity realZone shapeFeature model id shape x y maybeOpacity =
  let pred z = isPrimaryZone z || isRotateZone z in
  let shapeSelected = Set.member id model.selectedShapes in
  let selectableFeature = ShapeFeature id shapeFeature in
  let featureSelected = Set.member selectableFeature model.selectedFeatures in
  case ( shapeSelected || featureSelected
       , objectZoneIsCurrentlyBeingManipulated model id pred
       , maybeOpacity ) of
    (True, False, Just nt) -> zoneOpacity_ realZone shapeFeature model id shape x y nt
    _                      -> []

zoneOpacity_
   : RealZone -> ShapeFeature -> Model -> NodeId -> ShapeKind
  -> Num -> Num -> NumTr -> List (Svg Msg)
zoneOpacity_ realZone shapeFeature model id shape x y (n, trace) =
  let (w, h, a, stroke, strokeWidth, rBall) =
      (wOpacityBox, 20, 20, "silver", "2", "7") in
  let yOff = a + rotZoneDelta in
  let selectableFeature = ShapeFeature id shapeFeature in
  let ball =
    let cx = x + n * wOpacityBox in
    let cy = y - yOff + (h/2) in
    flip Svg.circle [] <|
      [ attr "stroke" "black" , attr "stroke-width" strokeWidth
      , attr "fill" stroke
      , attr "cx" (toString cx) , attr "cy" (toString cy)
      , attr "r"  rBall
      , cursorOfZone realZone "default"
      ] ++ dragZoneEvents id shape realZone
  in
  let box =
    flip Svg.rect [] <|
      [ attr "fill" <|
          if Set.member selectableFeature model.selectedFeatures
            then colorPointSelected
            else "white" -- colorPointNotSelected
      , attr "stroke" stroke , attr "stroke-width" strokeWidth
      , attr "x" (toString x) , attr "y" (toString (y - yOff))
      , attr "width" (toString w) , attr "height" (toString h)
      ]
  in
  [ Svg.g
      [onMouseDownAndStop (toggleSelected [selectableFeature])]
      ([box])
  , ball
  ]


-- Stuff for Stroke Width Zones ------------------------------------------------

wStrokeWidthBox = ShapeWidgets.wStrokeWidthSlider

maybeStrokeWidthNumAttr : List LangSvg.Attr -> Maybe NumTr
maybeStrokeWidthNumAttr l =
  case Utils.maybeFind "stroke-width" l of
    Just aval -> case aval.interpreted of
      LangSvg.ANum n -> Just n
      _              -> Nothing
    _                -> Nothing

zoneStrokeWidth model id shape x y maybeStrokeWidth =
  let pred z = isPrimaryZone z || isRotateZone z in
  let shapeSelected = Set.member id model.selectedShapes in
  let selectableFeature = ShapeFeature id (OFeat StrokeWidth) in
  let featureSelected = Set.member selectableFeature model.selectedFeatures in
  case ( shapeSelected || featureSelected
       , objectZoneIsCurrentlyBeingManipulated model id pred
       , maybeStrokeWidth ) of
    (True, False, Just nt) -> zoneStrokeWidth_ model id shape x y nt
    _                      -> []

zoneStrokeWidth_ model id shape x y (n, trace) =
  let (w, h, a, stroke, strokeWidth, rBall) =
      (wStrokeWidthBox, LangSvg.maxStrokeWidthNum, 20, "silver", "2", "7") in
  let yOff = a + rotZoneDelta in
  let selectableFeature = ShapeFeature id (OFeat StrokeWidth) in
  let box =
    flip Svg.rect [] <|
      [ attr "fill" <|
          if Set.member selectableFeature model.selectedFeatures
            then colorPointSelected
            else "white" -- colorPointNotSelected
      , attr "stroke" stroke , attr "stroke-width" strokeWidth
      , attr "x" (toString x) , attr "y" (toString (y - yOff))
      , attr "width" (toString w) , attr "height" (toString h)
      ]
  in
  let ball =
    let cx = x + (n / LangSvg.maxStrokeWidthNum) * wStrokeWidthBox in
    let cy = y - yOff + (h/2) in
    flip Svg.circle [] <|
      [ attr "stroke" "black" , attr "stroke-width" strokeWidth
      , attr "fill" stroke
      , attr "cx" (toString cx) , attr "cy" (toString cy)
      , attr "r"  rBall
      , cursorOfZone (ZOther StrokeWidth) "default"
      ] ++ dragZoneEvents id shape (ZOther StrokeWidth)
  in
  let triangle =
    let (x0,y0) = (x                   , y - yOff + h/2 ) in
    let (x1,y1) = (x + wStrokeWidthBox , y - yOff       ) in
    let (x2,y2) = (x + wStrokeWidthBox , y - yOff + h   ) in
    svgPath <|
       [ attr "fill" "darkgray"
       , attr "d"
           ("M " ++ toString x0 ++ " " ++ toString y0 ++
           " L " ++ toString x1 ++ " " ++ toString y1 ++
           " L " ++ toString x2 ++ " " ++ toString y2 ++ " Z")
       ]
  in
  [ Svg.g
      [onMouseDownAndStop (toggleSelected [selectableFeature])]
      [box, triangle]
  , ball
  ]


-- Stuff for Delete Zones ------------------------------------------------------

zoneDelete id shape x y transform =
  let b = False in
  if b then zoneDelete_ id shape x y transform else []

zoneDelete_ id shape x y transform =
  let (w, h, stroke, strokeWidth) =
      (20, 20, "silver", "2") in
  let evt =
    let foo old =
      old
      -- { old | slate = Tuple.mapSnd (Dict.insert id LangSvg.dummySvgNode) old.slate }
    in
    onMouseDown (Msg "Delete..." foo) in
  let lines =
    let f x1 y1 x2 y2 =
      flip Svg.line [] <|
        [ attr "stroke" "darkred", attr "stroke-width" strokeWidth
        , attr "x1" (toString x1) , attr "y1" (toString y1)
        , attr "x2" (toString x2) , attr "y2" (toString y2)
        , evt
        ] ++ transform
      in
     [ f x y (x + w) (y + h) , f x (y + h) (x + w) y ] in
  let box =
    flip Svg.rect [] <|
      [ attr "fill" "white"
      , attr "stroke" stroke , attr "stroke-width" strokeWidth
      , attr "x" (toString x) , attr "y" (toString y)
      , attr "width" (toString w) , attr "height" (toString h)
      , evt
      ] ++ transform
  in
  [box] ++ lines


--------------------------------------------------------------------------------
-- Selection Zones

-- http://www.colorpicker.com/

colorPointSelected      = "#38F552" -- bright green
colorPointNotSelected   = "#F5B038" -- "orange"
colorLineSelected       = "#B4FADB" -- "blue"
colorLineNotSelected    = "#FAB4D3" -- "red"

hairStrokeWidth         = "9"

type alias NodeIdAndAttrName     = (LangSvg.NodeId, String)
type alias NodeIdAndTwoAttrNames = (LangSvg.NodeId, String, String)

type alias NodeIdAndFeature      = (LangSvg.NodeId, ShapeWidgets.ShapeFeature)


toggleSelected : List ShapeWidgets.SelectableFeature -> Msg
toggleSelected selectableFeatures =
  Msg "Toggle Selected..." <| toggleSelectedLambda selectableFeatures

toggleSelectedLambda : List ShapeWidgets.SelectableFeature -> Model -> Model
toggleSelectedLambda selectableFeatures =
  \model ->
    -- If only some of the features were selected, we want to select all of
    -- them, not toggle individually.
    let deselect = List.all (flip Set.member model.selectedFeatures) selectableFeatures in
    let updateSet selectableFeature acc =
      if deselect
        then Set.remove selectableFeature acc
        else Set.insert selectableFeature acc
    in
    { model | selectedFeatures = List.foldl updateSet model.selectedFeatures selectableFeatures }

svgXYDot model (x_, y_) fill isVisible extraAttrs =
  let
    x = toFloat x_ - model.outputCanvasInfo.scrollLeft
    y = toFloat y_ - model.outputCanvasInfo.scrollTop
  in
  svgCircle <|
    [ attr "cx" (toString x) , attr "cy" (toString y)
    , attr "fill" fill
    , attr "stroke" pointZoneStyles.stroke
    , attr "stroke-width" pointZoneStyles.strokeWidth
    , attr "r" <|
        if isVisible
        then pointZoneStyles.radius
        else "0"
    ] ++ extraAttrs


maybeZoneSelectCrossDot sideLength model thisCrosshair xNumTr xVal yNumTr yVal =
  if sideLength < minLengthForMiddleZones then []
  else zoneSelectCrossDot model False thisCrosshair xNumTr xVal yNumTr yVal

zoneSelectCrossDot : Model -> Bool -> (Int, ShapeKind, PointFeature)
                  -> NumTr -> Val -> NumTr -> Val -> List (Svg Msg)
zoneSelectCrossDot model alwaysShowDot (id, kind, pointFeature) xNumTr xVal yNumTr yVal =
  let ((xFloat, _), (yFloat, _)) = (xNumTr, yNumTr) in
  let (x, y) = (round xFloat, round yFloat) in
  let thisCrosshair = (id, pointFeature) in
  let len = 20 in
  let color selectableFeatures =
    if List.all (flip Set.member model.selectedFeatures) selectableFeatures
    then colorPointSelected
    else colorPointNotSelected
  in
  let
    xSelectableFeature = ShapeFeature id (XFeat pointFeature)
    ySelectableFeature = ShapeFeature id (YFeat pointFeature)
    (xColor, yColor) = (color [xSelectableFeature], color [ySelectableFeature])
  in
  let (backDisc, frontDisc) =
    let r =
      if Set.member thisCrosshair model.hoveredCrosshairs && model.tool == Cursor
        then toString len
        else "0"
    in
    let backDisc =
      svgCircle <|
        [ attr "r" r
        , attr "cx" (toString x) , attr "cy" (toString y)
        , attr "fill" "rgba(255,255,255,1.0)"
        ]
    in
    let frontDisc =
      svgCircle <|
        [ attr "r" r
        , attr "cx" (toString x) , attr "cy" (toString y)
        , attr "fill" "none"
        , attr "stroke" "black"
        , attr "stroke-width" pointZoneStyles.strokeWidth
        ]
    in
    (backDisc, frontDisc)
  in
  let xyDot =
    let dotFill =
      if Set.member id model.selectedShapes
        then pointZoneStylesFillSelected model id
        else pointZoneStyles.fill.shown
    in
    let isVisible =
      not (objectIsCurrentlyBeingManipulated model id)
         && (alwaysShowDot ||
             Set.member id model.selectedShapes ||
             Set.member id model.hoveredShapes ||
             Set.member thisCrosshair model.hoveredCrosshairs ||
             model.tool /= Cursor)
    in
    let extraAttrs =
      if model.tool == Cursor then
        [ onMouseDownAndStop <| Msg "Select Cross Dot..." <| \model ->
            if Set.member thisCrosshair model.hoveredCrosshairs
              then toggleSelectedLambda [xSelectableFeature, ySelectableFeature] model
              else { model | hoveredCrosshairs = Set.insert thisCrosshair model.hoveredCrosshairs }
        ]
      else
        [ onMouseDownAndStop <| Msg "Mouse Down On Point..." <| \model ->
            let maybeClickable = Just (PointWithProvenance xVal yVal) in
            let newModel = if model.mouseMode == MouseNothing then startDrawing model maybeClickable else model in -- maybeClickable for drag drawings
            { newModel | mouseState = (Just False, { x = x, y = y }, maybeClickable) } ] -- maybeClickable for click drawings (poly/path)
    in
    svgXYDot model (x, y) dotFill isVisible extraAttrs
  in
  let yLine =
    svgLine <|
      [ attr "stroke" yColor
      , attr "stroke-width" <|
          if (Set.member thisCrosshair model.hoveredCrosshairs && model.tool == Cursor) ||
             Set.member ySelectableFeature model.selectedFeatures
          then hairStrokeWidth
          else "0"
      , attr "x1" (toString (x-len)) , attr "y1" (toString y)
      , attr "x2" (toString (x+len)) , attr "y2" (toString y)
      ] ++ if model.tool /= Cursor then [] else
        [ onMouseDownAndStop (toggleSelected [ySelectableFeature]) ]
  in
  let xLine =
    svgLine <|
      [ attr "stroke" xColor
      , attr "stroke-width" <|
          if (Set.member thisCrosshair model.hoveredCrosshairs && model.tool == Cursor) ||
             Set.member xSelectableFeature model.selectedFeatures
          then hairStrokeWidth
          else "0"
      , attr "y1" (toString (y-len)) , attr "x1" (toString x)
      , attr "y2" (toString (y+len)) , attr "x2" (toString x)
      ] ++ if model.tool /= Cursor then [] else
        [ onMouseDownAndStop (toggleSelected [xSelectableFeature]) ]
  in
  -- using nested group for onMouseLeave handler
  List.singleton <| Svg.g
    [onMouseLeave (removeHoveredCrosshair thisCrosshair)]
    [backDisc, xLine, yLine, frontDisc, xyDot]

maybeZoneSelectLine sideLength model nodeId shapeFeature pt1 pt2 =
  if sideLength < minLengthForMiddleZones then []
  else zoneSelectLine model nodeId shapeFeature pt1 pt2

zoneSelectLine model nodeId shapeFeature pt1 pt2 =
  let selectableFeature = ShapeFeature nodeId shapeFeature in
  case model.mouseMode of
    MouseDragZone _ _ -> []
    _ ->
     if Set.member nodeId model.hoveredShapes ||
        Set.member selectableFeature model.selectedFeatures
     then zoneSelectLine_ model selectableFeature pt1 pt2
     else []

zoneSelectLine_ model selectableFeature (x1,y1) (x2,y2) =
  let color =
    if Set.member selectableFeature model.selectedFeatures
    then colorLineSelected
    else colorLineNotSelected
  in
  let line =
    svgLine [
        attr "stroke" color
      , attr "stroke-width" hairStrokeWidth
      , attr "x1" (toString x1) , attr "y1" (toString y1)
      , attr "x2" (toString x2) , attr "y2" (toString y2)
      , onMouseDownAndStop (toggleSelected [selectableFeature])
      ]
  in
  [line]

boxySelectZones model id kind boxyNums =

  let drawPoint maybeThreshold feature x y =
    case maybeThreshold of
      Just thresh -> maybeZoneSelectCrossDot thresh model (id, kind, feature) (x, dummyTrace) dummyVal (y, dummyTrace) dummyVal
      Nothing     -> zoneSelectCrossDot model False (id, kind, feature) (x, dummyTrace) dummyVal (y, dummyTrace) dummyVal in

  let drawLine threshold feature pt1 pt2 =
    maybeZoneSelectLine threshold model id feature pt1 pt2 in

  let {left, top, right, bot, cx, cy, width, height} = boxyNums in

  let distanceZone f =
    case f of
      DistanceFeature Width   -> drawLine height (DFeat Width) (left,cy) (right,cy)
      DistanceFeature Height  -> drawLine width (DFeat Height) (cx,top) (cx,bot)

      DistanceFeature Radius  -> drawLine width (DFeat Radius) (cx,cy) (right,cy)
      DistanceFeature RadiusX -> drawLine height (DFeat RadiusX) (cx,cy) (right,cy)
      DistanceFeature RadiusY -> drawLine width (DFeat RadiusY) (cx,top) (cx,cy)

      _ -> [] in

  let pointZone f =
    case f of
      PointFeature TopLeft  -> drawPoint Nothing TopLeft left top
      PointFeature TopRight -> drawPoint Nothing TopRight right top
      PointFeature BotLeft  -> drawPoint Nothing BotLeft left bot
      PointFeature BotRight -> drawPoint Nothing BotRight right bot

      PointFeature TopEdge   -> drawPoint (Just width) TopEdge cx top
      PointFeature BotEdge   -> drawPoint (Just width) BotEdge cx bot
      PointFeature LeftEdge  -> drawPoint (Just height) LeftEdge left cy
      PointFeature RightEdge -> drawPoint (Just height) RightEdge right cy
      PointFeature Center    -> drawPoint (Just (min width height)) Center cx cy

      _ -> [] in

  let features = Utils.find "boxySelectZones" ShapeWidgets.simpleKindGenericFeatures kind in
  List.concatMap distanceZone features ++ List.concatMap pointZone features
    -- draw distance zones below point zones


--------------------------------------------------------------------------------

-- TODO significantly refactor point selection zones, by using
-- ShapeWidgets.genericFeaturesOfShape, BoxyFeatureEquations, eval FeatureEquation, etc.

makeZones : Model -> String -> LangSvg.NodeId -> List LangSvg.Attr -> List (Svg Msg)
makeZones model shape id l =
  case shape of
    "line"     -> makeZonesLine model id l
    "rect"     -> makeZonesRectOrBox model id shape l
    "BOX"      -> makeZonesRectOrBox model id shape l
    "circle"   -> makeZonesCircle model id l
    "ellipse"  -> makeZonesEllipseOrOval model id shape l
    "OVAL"     -> makeZonesEllipseOrOval model id shape l
    "polygon"  -> makeZonesPoly model shape id l
    "polyline" -> makeZonesPoly model shape id l
    "path"     -> makeZonesPath model shape id l
    -- "g"        -> makeZonesGroup model id l
    _          -> []

makeZonesLine model id l =
  let transform = maybeTransformAttr l in
  let (x1,y1,x2,y2,cx,cy) = ShapeWidgets.evaluateLineFeatures l in
  let (pt1,pt2) = ((x1,y1), (x2,y2)) in
  let bounds =
    let (xMin,xMax) = minMax x1 x2 in
    let (yMin,yMax) = minMax y1 y2 in
    (xMin, yMin, xMax, yMax) in
  let zLine =
    let enter = [ onMouseEnter (addHoveredShape id) ] in
    zoneLine2 model id "line" ZLineEdge pt1 pt2 (transform ++ enter)
  in
  let zonesSelect =
    List.concat
       [ maybeZoneSelectCrossDot (Utils.distance pt1 pt2) model (id, "line", Center) (cx, dummyTrace) dummyVal (cy, dummyTrace) dummyVal
       , zoneSelectCrossDot model False (id, "line", Point 1) (x1, dummyTrace) dummyVal (y1, dummyTrace) dummyVal
       , zoneSelectCrossDot model False (id, "line", Point 2) (x2, dummyTrace) dummyVal (y2, dummyTrace) dummyVal]
  in
  let primaryWidgets =
    boundingBoxZones model id bounds <|
      [zLine] ++
      zonesSelect ++
      zonePoints2 model id "line" transform [pt1, pt2]
  in
  let extraWidgets =
    let c = Utils.midpoint pt1 pt2 in
    let r = (Utils.distance pt1 pt2 / 2) - rotZoneDelta in
    zoneRotate model id "line" (cx, cy) r (maybeTransformCmds l) ++
    zonesStroke model id "line" x2 y2 l
  in
  primaryWidgets :: extraWidgets

makeZonesRectOrBox model id shape l =
  let boxyNums = ShapeWidgets.evaluateBoxyNums shape l in
  let {left, top, right, bot, cx, cy, width, height} = boxyNums in
  let bounds = (left, top, right, bot) in
  let transform = maybeTransformAttr l in
  let zoneInterior =
    draggableZone Svg.rect False model id shape ZInterior <|
      [ attrNum "x" left , attrNum "y" top
      , attrNum "width" width , attrNum "height" height
      , onMouseEnter (addHoveredShape id)
      ] ++ transform
  in
  let zonesSelect = boxySelectZones model id shape boxyNums in
  let primaryWidgets =
    boundingBoxZones model id bounds <|
      [zoneInterior] ++
      zonesSelect ++
      eightCardinalZones model id shape transform bounds
  in
  let extraWidgets =
    let r = rotZoneDelta + (height/2) in
    zoneRotate model id shape (cx,cy) r (maybeTransformCmds l) ++
    zonesFillAndStroke model id shape left top l
  in
  primaryWidgets :: extraWidgets

makeZonesCircle model id l =
  let boxyNums = ShapeWidgets.evaluateBoxyNums "circle" l in
  let {left, top, right, bot, cx, cy, r} = boxyNums in
  let bounds = (left, top, right, bot) in
  let transform = maybeTransformAttr l in
  let zoneInterior =
    draggableZone Svg.circle False model id "circle" ZInterior <|
      [ attrNum "cx" cx, attrNum "cy" cy, attrNum "r" r
      , onMouseEnter (addHoveredShape id)
      ] ++ transform
  in
  let zonesSelect = boxySelectZones model id "circle" boxyNums in
  let primaryWidgets =
     boundingBoxZones model id bounds <|
       [zoneInterior] ++
       zonesSelect ++
       eightCardinalZones model id "circle" transform bounds
  in
  let extraWidgets =
    zoneRotate model id "circle" (cx,cy) (r + rotZoneDelta) (maybeTransformCmds l) ++
    zonesFillAndStroke model id "circle" (cx - r) (cy - r) l
  in
  primaryWidgets :: extraWidgets

makeZonesEllipseOrOval model id shape l =
  let boxyNums = ShapeWidgets.evaluateBoxyNums shape l in
  let {left, top, right, bot, width, height, cx, cy, rx, ry} = boxyNums in
  let bounds = (left, top, right, bot) in
  let transform = maybeTransformAttr l in
  let zoneInterior =
    draggableZone Svg.ellipse False model id shape ZInterior <|
      [ attrNum "cx" cx, attrNum "cy" cy, attrNum "rx" rx, attrNum "ry" ry
      , onMouseEnter (addHoveredShape id)
      ] ++ transform
  in
  let zonesSelect = boxySelectZones model id shape boxyNums in
  let primaryWidgets =
     boundingBoxZones model id bounds <|
       [zoneInterior] ++
       zonesSelect ++
       eightCardinalZones model id shape transform bounds
  in
  let extraWidgets =
    zoneRotate model id shape (cx,cy) (ry + rotZoneDelta) (maybeTransformCmds l) ++
    zonesFillAndStroke model id shape (cx - rx) (cy - ry) l
  in
  primaryWidgets :: extraWidgets

makeZonesPoly model shape id l =
  let _ = Utils.assert "makeZonesPoly" (shape == "polygon" || shape == "polyline") in
  let transform = maybeTransformAttr l in
  let pts = LangSvg.getPolyPoints l in
  let zPts = zonePoints model id shape transform pts in
  let zLines =
    let pairs = Utils.overlappingAdjacentPairs_ (shape == "polygon") pts in
    let f (i,(pti,ptj)) = zoneLine model id shape (ZPolyEdge i) pti ptj transform in
    Utils.mapi1 f pairs in
  let zInterior =
    draggableZone Svg.polygon False model id shape ZInterior <|
      [ LangSvg.compileAttr "points" (LangSvg.aPoints pts)
      , onMouseEnter (addHoveredShape id)
      ] ++ transform
  in
{-
  let zInterior =
    if shape == "polygon" || (shape == "polyline" && firstEqLast pts)
    then [ zoneBorder Svg.polygon model id shape "Interior" False transform <|
             [ LangSvg.compileAttr "points" (LangSvg.aPoints pts)
             , onMouseEnter (addHoveredShape id)
             ]
         ]
    else []
  in
-}
  let zRot = zoneRotatePolyOrPath model id "polygon" pts l in
  let zFillAndStroke =
    case pts of
      (((x0,_),(y0,_))::_) ->
        zonesFillAndStroke model id shape x0 y0 l
      _ ->
        Debug.crash "makeZonesPoly" in
  let zSelect =
    let midptCrossDot ((i1, ((xi1, xTr1), (yi1, yTr1))), (i2, ((xi2, xTr2), (yi2, yTr2)))) =
      let (midX, midXTr) = ((xi1+xi2)/2, TrOp Plus [xTr1, xTr2]) in -- Can't divide by two until we unify traces which will allow introduction of constants. Here it only affects drawing new offests (for now).
      let (midY, midYTr) = ((yi1+yi2)/2, TrOp Plus [yTr1, yTr2]) in -- Can't divide by two until we unify traces which will allow introduction of constants. Here it only affects drawing new offests (for now).
      zoneSelectCrossDot model False (id, shape, Midpoint i1) (midX, midXTr) dummyVal (midY, midYTr) dummyVal
    in
    let ptCrossDot (i, (xNumTrI, yNumTrI)) =
      zoneSelectCrossDot model False (id, shape, Point i) xNumTrI dummyVal yNumTrI dummyVal
    in
    let midptCrossDots =
      let ptsI = Utils.mapi1 identity pts in
      let ptsIPairs = Utils.circOverlappingAdjacentPairs ptsI in
      List.concatMap midptCrossDot ptsIPairs
    in
    let crossDots = List.concat <| Utils.mapi1 ptCrossDot pts in
    midptCrossDots ++ crossDots
  in
  let primaryWidgets =
    let (x1,x2,y1,y2) = Draw.boundingBoxOfPoints_ (List.map (\(x,y) -> (Tuple.first x, Tuple.first y)) pts) in
    boundingBoxZones model id (x1,y1,x2,y2) <|
      [zInterior] ++ zLines ++ zSelect ++ zPts
  in
  primaryWidgets :: zRot ++ zFillAndStroke

firstEqLast xs = Utils.head_ xs == Utils.head_ (List.reverse xs)

makeZonesPath : Model -> String -> Int -> List LangSvg.Attr -> List (Svg Msg)
makeZonesPath model shape id nodeAttrs =
  let _ = Utils.assert "makeZonesPoly" (shape == "path") in
  let transform = maybeTransformAttr nodeAttrs in
  let cmds = Tuple.first <| LangSvg.toPath <| Utils.find_ nodeAttrs "d" in
  let add (mi,pt) acc = case mi of Nothing -> acc
                                   _       -> (mi,pt) :: acc in
  let listOfMaybeIndexWithPt =
    List.foldr (\c acc -> case c of
      LangSvg.CmdZ   s              -> acc
      LangSvg.CmdMLT s pt           -> add pt acc
      LangSvg.CmdHV  s n            -> acc
      LangSvg.CmdC   s pt1 pt2 pt3  -> add pt1 (add pt2 (add pt3 acc))
      LangSvg.CmdSQ  s pt1 pt2      -> add pt1 (add pt2 acc)
      LangSvg.CmdA   s a b c d e pt -> add pt acc) [] cmds
  in
  let pts = List.map Tuple.second listOfMaybeIndexWithPt in
  let dots = zonePoints model id shape transform pts in
  let zRot = zoneRotatePolyOrPath model id "path" pts nodeAttrs in
  let zFillAndStroke =
    case pts of
      (((x0,_),(y0,_))::_) ->
        zonesFillAndStroke model id shape x0 y0 nodeAttrs
      _ ->
        Debug.crash "makeZonesPath"
  in
  let zSelect =
    let ptCrossDot (maybeIndex, (xNumTr, yNumTr)) =
      let i = Utils.fromJust maybeIndex in
      zoneSelectCrossDot model False (id, shape, Point i) xNumTr dummyVal yNumTr dummyVal
    in
    let crossDots = List.concatMap ptCrossDot listOfMaybeIndexWithPt in
    crossDots
  in
  let zInterior =
    draggableZone Svg.path False model id shape ZInterior <|
      [ LangSvg.compileAttr "d" (Utils.find_ nodeAttrs "d")
      , onMouseEnter (addHoveredShape id)
      ] ++ transform
  in
  -- TODO add "Edge" zones
  let primaryWidgets =
    let (x1,x2,y1,y2) = Draw.boundingBoxOfPoints_ (List.map (\(x,y) -> (Tuple.first x, Tuple.first y)) pts) in
    boundingBoxZones model id (x1,y1,x2,y2) <|
      [zInterior] ++
      zSelect ++
      dots
  in
  primaryWidgets :: zRot ++ zFillAndStroke


--------------------------------------------------------------------------------
-- Zone Caption and Highlights

turnOnCaptionAndHighlights zoneKey =
  Msg ("Turn On Caption " ++ toString zoneKey) <| \m ->
    let codeBoxInfo = m.codeBoxInfo in
    let hi = liveInfoToHighlights zoneKey m in
    { m | caption = Just (Hovering zoneKey)
        , codeBoxInfo = { codeBoxInfo | highlights = hi } }

turnOffCaptionAndHighlights =
  Msg "Turn Off Caption" <| \m ->
    let codeBoxInfo = m.codeBoxInfo in
    { m | caption = Nothing
        , codeBoxInfo = { codeBoxInfo | highlights = [] }
        }
{-
        , codeBoxInfo = { codeBoxInfo | highlights = DeuceWidgets.expRangesToHighlights m Nothing ++
                                                     DeuceWidgets.expTargetsToHighlights m Nothing ++
                                                     DeuceWidgets.patRangesToHighlights m Nothing ++
                                                     DeuceWidgets.patTargetsToHighlights m Nothing} }
-}
