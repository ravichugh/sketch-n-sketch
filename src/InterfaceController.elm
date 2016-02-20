module InterfaceController (upstate) where

import Lang exposing (..) --For access to what makes up the Vals
import LangParser2 exposing (parseE, freshen)
import LangUnparser exposing (unparse, equationToLittle, preceedingWhitespace, addPreceedingWhitespace)
import LangTransform
import Sync
import Eval
import Utils
import Keys
import InterfaceModel exposing (..)
import InterfaceView2 as View
import InterfaceStorage exposing (installSaveState, removeDialog)
import LangSvg exposing (toNum, toNumTr, toPoints, addi)
import ExamplesGenerated as Examples
import Config exposing (params)

import VirtualDom

--Core Libraries
import List
import Dict
import Set
import String
import Char
import Graphics.Element as GE
import Graphics.Collage as GC
import Regex as Re

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

debugLog = Config.debugLog Config.debugController

--------------------------------------------------------------------------------

slateToVal : LangSvg.RootedIndexedTree -> Val
slateToVal (rootId, tree) =
  let foo n =
    case n of
      LangSvg.TextNode s -> vList [vBase (String "TEXT"), vBase (String s)]
      LangSvg.SvgNode kind l1 l2 ->
        let vs1 = List.map LangSvg.valOfAttr l1 in
        let vs2 = List.map (foo << flip Utils.justGet tree) l2 in
        vList [vBase (String kind), vList vs1, vList vs2]
          -- NOTE: if relate needs the expression that led to this
          --  SvgNode, need to store it in IndexedTree
  in
  foo (Utils.justGet rootId tree)

upslate : LangSvg.NodeId -> (String, LangSvg.AVal) -> LangSvg.IndexedTree -> LangSvg.IndexedTree
upslate id newattr nodes = case Dict.get id nodes of
    Nothing   -> Debug.crash "upslate"
    Just node -> case node of
        LangSvg.TextNode x -> nodes
        LangSvg.SvgNode shape attrs children ->
            let newnode = LangSvg.SvgNode shape (Utils.update newattr attrs) children
            in Dict.insert id newnode nodes

refreshMode model e =
  case model.mode of
    Live _  -> mkLive_ model.syncOptions model.slideNumber model.movieNumber model.movieTime e
    Print _ -> mkLive_ model.syncOptions model.slideNumber model.movieNumber model.movieTime e
    m       -> m

refreshMode_ model = refreshMode model model.inputExp

refreshHighlights id zone model =
  let codeBoxInfo = model.codeBoxInfo in
  let hi = liveInfoToHighlights id zone model in
  { model | codeBoxInfo = { codeBoxInfo | highlights = hi } }

switchOrient m = case m of
  Vertical -> Horizontal
  Horizontal -> Vertical

{- -- TODO turning off delete zones for now
toggleShowZones x = (1 + x) % showZonesModeCount
-}
toggleShowZones x =
  let i = (1 + x) % showZonesModeCount in
  if i == showZonesDel then toggleShowZones i else i

-- if delete mode is not applicable but set, use oldMode instead
maybeLeaveDeleteMode newModel oldShowZones =
  case (newModel.mode, newModel.showZones == showZonesDel) of
    (Live _, True) -> { newModel | showZones = oldShowZones }
    _              -> newModel

-- may want to eventually have a maximum history length
addToHistory s h = (s :: fst h, [])

between1 i (j,k) = i `Utils.between` (j+1, k+1)

cleanExp =
  mapExpViaExp__ <| \e__ -> case e__ of
    EApp _ e0 [e1,_,_] _ -> case e0.val.e__ of
      EVar _ "inferred"  -> e1.val.e__
      _                  -> e__
    EApp _ e0 [_,e1] _   -> case e0.val.e__ of
      EVar _ "flow"      -> e1.val.e__
      _                  -> e__
    EOp _ op [e1,e2] _   ->
      case (op.val, e2.val.e__) of
        (Plus, EConst _ 0 _ _) -> e1.val.e__
        _                      -> e__
    _                    -> e__


-- If suggestedName is not in existing names, returns it.
-- Otherwise appends a number (starting at 2) that doesn't collide.
nonCollidingName : Ident -> Set.Set Ident -> Ident
nonCollidingName suggestedName existingNames =
  if not (Set.member suggestedName existingNames) then
    suggestedName
  else
    let nonCollidingName i =
      let newName = suggestedName ++ (toString i) in
      if not (Set.member newName existingNames)
      then newName
      else nonCollidingName (i+1)
    in
    nonCollidingName 2

-- this is a bit redundant with View.turnOn...
maybeStuff id shape zone m =
  case m.mode of
    Live info ->
      flip Utils.bindMaybe (Dict.get id info.assignments) <| \d ->
      flip Utils.bindMaybe (Dict.get zone d) <| \(yellowLocs,_) ->
        Just (info.initSubst, yellowLocs)
    _ ->
      Nothing

highlightChanges mStuff changes codeBoxInfo =
  case mStuff of
    Nothing -> codeBoxInfo
    Just (initSubstPlus, locs) ->

      let (hi,stringOffsets) =
        -- hi : List Highlight, stringOffsets : List (Pos, Int)
        --   where Pos is start pos of a highlight to offset by Int chars
        let f loc (acc1,acc2) =
          let (locid,_,_) = loc in
          let highlight c = makeHighlight initSubstPlus c loc in
          case (Dict.get locid initSubstPlus, Dict.get locid changes) of
            (Nothing, _)             -> Debug.crash "Controller.highlightChanges"
            (Just n, Nothing)        -> (highlight yellow :: acc1, acc2)
            (Just n, Just Nothing)   -> (highlight red :: acc1, acc2)
            (Just n, Just (Just n')) ->
              if n' == n.val then
                (highlight yellow :: acc1, acc2)
              else
                let (s, s') = (strNum n.val, strNum n') in
                let x = (acePos n.start, String.length s' - String.length s) in
                (highlight green :: acc1, x :: acc2)
        in
        List.foldl f ([],[]) (Set.toList locs)
      in

      let hi' =
        let g (startPos,extraChars) (old,new) =
          let bump pos = { pos | column = pos.column + extraChars } in
          let ret new' = (old, new') in
          ret <|
            if startPos.row    /= old.start.row         then new
            else if startPos.column >  old.start.column then new
            else if startPos.column == old.start.column then { start = new.start, end = bump new.end }
            else if startPos.column <  old.start.column then { start = bump new.start, end = bump new.end }
            else
              Debug.crash "highlightChanges"
        in
        -- hi has <= 4 elements, so not worrying about the redundant processing
        flip List.map hi <| \{color,range} ->
          let (_,range') = List.foldl g (range,range) stringOffsets in
          { color = color, range = range' }
      in

      { codeBoxInfo | highlights = hi' }

addSlateAndCode old (exp, val) =
  let (slate, code) = slateAndCode old (exp, val) in
  (exp, val, slate, code)

slateAndCode old (exp, val) =
  let slate =
    LangSvg.resolveToIndexedTree old.slideNumber old.movieNumber old.movieTime val
  in
  (slate, unparse exp)

--------------------------------------------------------------------------------

clickToCanvasPoint old (mx, my) =
  let (xOrigin, yOrigin) = case old.orient of
    Vertical   -> canvasOriginVertical old
    Horizontal -> canvasOriginHorizontal old
  in
  (mx - xOrigin, my - yOrigin)

-- the computations of the top-left corner of the canvas
-- are based on copying the computations from View
-- TODO: refactor these

canvasOriginVertical old =
  let
    sideGut = params.topSection.h
    wGut    = params.mainSection.vertical.wGut
    wMiddle = params.mainSection.widgets.wBtn
    wCode_  = (fst old.dimensions - sideGut - sideGut - wMiddle - wGut - wGut) // 2
    wCode   = if old.hideCode then 0
              else if old.hideCanvas then (fst old.dimensions - sideGut - sideGut - wMiddle - wGut - wGut)
              else wCode_ + old.midOffsetX
  in
    ( sideGut + wCode + 2*wGut + wMiddle
    , params.topSection.h
    )

canvasOriginHorizontal old =
  -- TODO the y-position in horizontal mode is off by a few pixels
  -- TODO in View, the height of codebox isn't the same as the canvas.
  --   hMid is calculated weirdly in View...
  let
    hGut    = params.mainSection.horizontal.hGut
    hCode_  = (snd old.dimensions - 2*hMid - 3*hGut) // 2 + hMid
    hCode   = hCode_ + old.midOffsetY
    -- TODO consider hideCode and hideCanvas
    hMid    = params.mainSection.widgets.hBtn
  in
    ( params.wGut
    , params.topSection.h + hCode + hMid
    )


--------------------------------------------------------------------------------
-- New Shapes

type alias TopDef  = (WS, Pat, Exp, WS)
type alias TopDefs = List TopDef

splitExp : Exp -> (TopDefs, Exp)
splitExp e =
  case e.val.e__ of
    ELet ws1 Def False p1 e1 e2 ws2 ->
      let (defs, main) = splitExp e2 in
      ((ws1,p1,e1,ws2)::defs, main)
    _ ->
      ([], e)

fuseExp : TopDefs -> Exp -> Exp
fuseExp defs main =
  let recurse defs =
    case defs of
      [] -> main
      (ws1,p1,e1,ws2)::defs' ->
        withDummyPos <| ELet ws1 Def False p1 e1 (recurse defs') ws2
  in
  recurse defs

-- when line is snapped, not enforcing the angle in code
addLineToCodeAndRun old (x2,y2) (x1,y1) =
  let (xb, yb) = View.snapLine old.keysDown (x2,y2) (x1,y1) in
  let color = if old.toolType == HelperLine then "aqua" else "gray" in
  let (f, args) =
    maybeGhost (old.toolType == HelperLine)
       (eVar0 "line")
       (eStr color :: eStr "5" :: List.map eVar ["x1","y1","x2","y2"])
  in
  addToCodeAndRun "line" old
    [ makeLet ["x1","x2"] (makeInts [x1,xb])
    , makeLet ["y1","y2"] (makeInts [y1,yb])
    ] f args

addRectToCodeAndRun old pt2 pt1 =
  if old.keysDown == Keys.shift then addSquare old pt2 pt1
  else addRect old pt2 pt1

addRect old pt2 pt1 =
  let (xa, xb, ya, yb) = View.boundingBox pt2 pt1 in
  let (x, y, w, h) = (xa, ya, xb - xa, yb - ya) in
  addToCodeAndRun "rect" old
    [ makeLet ["x","y","w","h"] (makeInts [x,y,w,h])
    , makeLet ["rot"] [eConst 0 dummyLoc] ]
    (eVar0 "rotatedRect")
    (eConst 100 dummyLoc :: List.map eVar ["x","y","w","h","rot"])

addSquare old pt2 pt1 =
  let (xa, xb, ya, yb) = View.squareBoundingBox pt2 pt1 in
  let (x, y, side) = (xa, ya, xb - xa) in
  addToCodeAndRun "square" old
    [ makeLet ["x","y","side"] (makeInts [x,y,side])
    , makeLet ["rot"] [eConst 0 dummyLoc] ]
    (eVar0 "rotatedRect")
    (eConst 50 dummyLoc :: List.map eVar ["x","y","side","side","rot"])

{- bounding box version...

addRectToCodeAndRun old (x2,y2) (x1,y1) =
  let
    (xa, xb)     = (min x1 x2, max x1 x2)
    (ya, yb)     = (min y1 y2, max y1 y2)
  in
  addToCodeAndRun "rect" old
    [ makeLet ["x","y","xw","yh","rot"] (makeInts [xa,ya,xb,yb,0]) ]
    (eVar0 "rectangle")
    (eConst 100 dummyLoc :: List.map eVar ["x","y","xw","yh","rot"])
-}

addEllipseToCodeAndRun old pt2 pt1 =
  if old.keysDown == Keys.shift then addCircle old pt2 pt1
  else addEllipse old pt2 pt1

addEllipse old pt2 pt1 =
  let (xa, xb, ya, yb) = View.boundingBox pt2 pt1 in
  let (rx, ry) = ((xb-xa)//2, (yb-ya)//2) in
  let (cx, cy) = (xa + rx, ya + ry) in
  addToCodeAndRun "ellipse" old
    [ makeLet ["cx","cy","rx","ry"] (makeInts [cx,cy,rx,ry]) ]
    (eVar0 "ellipse")
    (eConst 200 dummyLoc :: List.map eVar ["cx","cy","rx","ry"])

addCircle old pt2 pt1 =
  let (xa, xb, ya, yb) = View.squareBoundingBox pt2 pt1 in
  let r = (xb-xa)//2 in
  let (cx, cy) = (xa + r, ya + r) in
  addToCodeAndRun "circle" old
    [ makeLet ["cx","cy","r"] (makeInts [cx,cy,r]) ]
    (eVar0 "ellipse")
    (eConst 250 dummyLoc :: List.map eVar ["cx","cy","r","r"])

addHelperDotToCodeAndRun old (cx,cy) =
  -- style matches center of attr crosshairs (View.zoneSelectPoint_)
  let r = 6 in
  let (f, args) =
    ghost (eVar0 "circle")
          (eStr "aqua" :: List.map eVar ["cx","cy","r"])
  in
  addToCodeAndRun "helperDot" old
    [ makeLet ["cx","cy","r"] (makeInts [cx,cy,r]) ]
    f args

maybeFreeze n =
  if n == 0 || n == 1
    then toString n ++ "!"
    else toString n

addPolygonToCodeAndRun old points =
  if old.keysDown == Keys.shift
    then addStickyPolygon old points
    else addStretchablePolygon old points

addStretchablePolygon old points =
  let xMax = Utils.fromJust <| List.maximum (List.map fst points) in
  let xMin = Utils.fromJust <| List.minimum (List.map fst points) in
  let yMax = Utils.fromJust <| List.maximum (List.map snd points) in
  let yMin = Utils.fromJust <| List.minimum (List.map snd points) in
  let (width, height) = (xMax - xMin, yMax - yMin) in
  -- TODO string for now, since will unparse anyway...
  let sPcts =
    Utils.bracks <| Utils.spaces <|
      flip List.map (List.reverse points) <| \(x,y) ->
        let xPct = (toFloat x - toFloat xMin) / toFloat width in
        let yPct = (toFloat y - toFloat yMin) / toFloat height in
        let xStr = maybeFreeze xPct in
        let yStr = maybeFreeze yPct in
        Utils.bracks (Utils.spaces [xStr,yStr])
  in
  addToCodeAndRun "polygon" old
    [ makeLet ["left","top","right","bot"] (makeInts [xMin,yMin,xMax,yMax])
    , makeLet ["bounds"] [eList (eVar0 "left" :: List.map eVar ["top","right","bot"]) Nothing]
    , makeLet ["pcts"] [eVar sPcts] ]
    (eVar0 "stretchyPolygon")
    [eVar "bounds", eConst 300 dummyLoc, eStr "black", eConst 2 dummyLoc, eVar "pcts"]

addStickyPolygon old points =
  let xMax = Utils.fromJust <| List.maximum (List.map fst points) in
  let xMin = Utils.fromJust <| List.minimum (List.map fst points) in
  let yMax = Utils.fromJust <| List.maximum (List.map snd points) in
  let yMin = Utils.fromJust <| List.minimum (List.map snd points) in
  let (width, height) = (xMax - xMin, yMax - yMin) in
  -- TODO string for now, since will unparse anyway...
  let sOffsets =
    Utils.bracks <| Utils.spaces <|
      flip List.map (List.reverse points) <| \(x,y) ->
        let
          (dxLeft, dxRight) = (x - xMin, x - xMax)
          (dyTop , dyBot  ) = (y - yMin, y - yMax)
          xOff = if dxLeft <= abs dxRight
                   then Utils.bracks (Utils.spaces ["left", toString dxLeft])
                   else Utils.bracks (Utils.spaces ["right", toString dxRight])
          yOff = if dyTop <= abs dyBot
                   then Utils.bracks (Utils.spaces ["top", toString dyTop])
                   else Utils.bracks (Utils.spaces ["bot", toString dyBot])
        in
        Utils.bracks (Utils.spaces [xOff,yOff])
  in
  addToCodeAndRun "polygon" old
    [ makeLet ["left","top","right","bot"] (makeInts [xMin,yMin,xMax,yMax])
    , makeLet ["bounds"] [eList (eVar0 "left" :: List.map eVar ["top","right","bot"]) Nothing]
    , makeLet ["offsets"] [eVar sOffsets] ]
    (eVar0 "stickyPolygon")
    [eVar "bounds", eConst 350 dummyLoc, eStr "black", eConst 2 dummyLoc, eVar "offsets"]

addLambdaToCodeAndRun old pt2 pt1 =
  let funcName =
    case old.toolType of
      Lambda f -> f
      _        -> Debug.crash "addLambdaToCodeAndRun"
  in
  let (xa, xb, ya, yb) =
    if old.keysDown == Keys.shift
      then View.squareBoundingBox pt2 pt1
      else View.boundingBox pt2 pt1
  in
  addToCodeAndRun funcName old
    [ makeLet ["left","top","right","bot"] (makeInts [xa,ya,xb,yb])
    , makeLet ["bounds"] [eList (eVar0 "left" :: List.map eVar ["top","right","bot"]) Nothing]
    ]
    (eVar0 "with") [ eVar "bounds" , eVar funcName ]

addToCodeAndRun newShapeKind old newShapeLocals newShapeFunc newShapeArgs =

  let tmp = newShapeKind ++ toString old.genSymCount in
  let newDef = makeNewShapeDef old newShapeKind tmp newShapeLocals newShapeFunc newShapeArgs in
  let (defs, main) = splitExp old.inputExp in
  let code = unparse (fuseExp (defs ++ [newDef]) (addToMain (eVar tmp) main)) in

  upstate Run
    { old | code = code
          , history = addToHistory old.code old.history
          , genSymCount = old.genSymCount + 1
          , mouseMode = MouseNothing }

makeNewShapeDef model newShapeKind name locals func args =
  let newShapeName = withDummyRange (PVar " " name noWidgetDecl) in
  let recurse locals =
    case locals of
      [] ->
        let multi = -- check if (func args) returns List SVG or SVG
          case model.toolType of
            Poly -> True
            Lambda _ -> True
            _ -> False
        in
        if multi then
          withDummyPos (EApp "\n    " func args "")
        else
          let app = withDummyPos (EApp " " func args "") in
          withDummyPos (EList "\n    " [app] "" Nothing " ")
      (p,e)::locals' -> withDummyPos (ELet "\n  " Let False p e (recurse locals') "")
  in
  ("\n\n", newShapeName, recurse locals, "")

makeLet : List Ident -> List Exp -> (Pat, Exp)
makeLet vars exps =
  case (vars, exps) of
    ([x],[e])     -> (pVar x, e)
    (x::xs,e::es) -> let ps = List.map pVar xs in
                     let p = pVar0 x in
                     (pList (p::ps), eList (e::es) Nothing)
    _             -> Debug.crash "makeLet"

makeInts : List Int -> List Exp
makeInts nums =
  case nums of
    []    -> Debug.crash "makeInts"
    [n]   -> [eConst0 (toFloat n) dummyLoc]
    n::ns -> let e = eConst0 (toFloat n) dummyLoc in
             let es = List.map (\n' -> eConst (toFloat n') dummyLoc) ns in
             e::es

addToMain eNew main =
  let callAddShape () =
    let ws = "\n" in -- TODO take main into account
    withDummyPos (EApp ws (eVar0 "addShapes") [eNew, main] "")
  in
  case main.val.e__ of
    EApp ws1 e1 [eAppConcat] ws2 ->
      case (e1.val.e__, eAppConcat.val.e__) of
        (EVar _ "svg", EApp ws3 eConcat [e2] ws4) ->
          case (eConcat.val.e__, e2.val.e__) of
            (EVar _ "concat", EList ws5 shapes ws6 Nothing ws7) ->
              let
                e2' =
                  { e2 | val = { eid = e2.val.eid , e__ =
                      EList ws5 (shapes ++ [eNew]) ws6 Nothing ws7 } }
                eAppConcat' =
                  { eAppConcat | val = { eid = eAppConcat.val.eid , e__ =
                      EApp ws3 eConcat [e2'] ws4 } }
                main' =
                  { main | val = { eid = main.val.eid , e__ =
                      EApp ws1 e1 [eAppConcat'] ws2 } }
              in
              if ws1 == "" then addPreceedingWhitespace "\n\n" main'
              else if ws1 == "\n" then addPreceedingWhitespace "\n" main'
              else main'

            _ -> callAddShape ()
        _     -> callAddShape ()
    _         -> callAddShape ()

maybeGhost b f args =
  if b
    then (eVar0 "ghost", [ withDummyPos (EApp " " f args "") ])
    else (f, args)

ghost = maybeGhost True

switchToCursorTool old =
  { old | mouseMode = MouseNothing , toolType = Cursor }


--------------------------------------------------------------------------------
-- Retrieving Selected Attributes
-- TODO

pluckSelectedFeatureEquationsNamed selectedFeatures slate =
  let (_, tree) = slate in
  let foo (nodeId, feature) acc =
    case nodeIdAndFeatureToEquation (nodeId, feature) tree of
      Just eqn -> (feature, eqn) :: acc
      Nothing  -> acc
  in
  Set.foldr foo [] selectedFeatures

pluckSelectedFeatureEquations selectedFeatures slate =
  List.map snd <| pluckSelectedFeatureEquationsNamed selectedFeatures slate

nodeIdAndFeatureToEquation (nodeId, feature) tree =
  case Dict.get nodeId tree of
    Just (LangSvg.SvgNode kind nodeAttrs _) ->
      Just (featureEquation nodeId kind feature nodeAttrs)

    Just (LangSvg.TextNode _) ->
      Nothing

    Nothing ->
      Debug.crash <| "nodeIdAndFeatureToEquation " ++ (toString nodeId) ++ " " ++ (toString tree)

equationVals eqn =
  case eqn of
    EqnVal val   -> [val]
    EqnOp _ eqns -> List.concatMap equationVals eqns

featureEquation nodeId kind feature nodeAttrs =
  let eqnVal attr = EqnVal <| maybeFindAttr nodeId kind attr nodeAttrs in
  let eqnVal2     = EqnVal <| vConst (2, dummyTrace) in
  let handleRect () =
    if feature == LangSvg.rectTLX then eqnVal "x"
    else if feature == LangSvg.rectTLY then eqnVal "y"
    else if feature == LangSvg.rectTRX then EqnOp Plus [eqnVal "x", eqnVal "width"]
    else if feature == LangSvg.rectTRY then eqnVal "y"
    else if feature == LangSvg.rectBLX then eqnVal "x"
    else if feature == LangSvg.rectBLY then EqnOp Plus [eqnVal "y", eqnVal "height"]
    else if feature == LangSvg.rectBRX then EqnOp Plus [eqnVal "x", eqnVal "width"]
    else if feature == LangSvg.rectBRY then EqnOp Plus [eqnVal "y", eqnVal "height"]
    else if feature == LangSvg.rectCX then EqnOp Plus [eqnVal "x", EqnOp Div [eqnVal "width",  eqnVal2]]  -- x + w/2
    else if feature == LangSvg.rectCY then EqnOp Plus [eqnVal "y", EqnOp Div [eqnVal "height", eqnVal2]] -- y + h/2
    else if feature == LangSvg.rectWidth  then eqnVal "width"
    else if feature == LangSvg.rectHeight then eqnVal "height"
    else Debug.crash <| "Rectangles do not have this feature: " ++ feature
  in
  let handleBox () =
    if feature == LangSvg.boxTLX then eqnVal "LEFT"
    else if feature == LangSvg.boxTLY then eqnVal "TOP"
    else if feature == LangSvg.boxTRX then eqnVal "RIGHT"
    else if feature == LangSvg.boxTRY then eqnVal "TOP"
    else if feature == LangSvg.boxBLX then eqnVal "LEFT"
    else if feature == LangSvg.boxBLY then eqnVal "BOT"
    else if feature == LangSvg.boxBRX then eqnVal "RIGHT"
    else if feature == LangSvg.boxBRY then eqnVal "BOT"
    else if feature == LangSvg.boxCX then EqnOp Div [EqnOp Plus [eqnVal "LEFT", eqnVal "RIGHT"], eqnVal2]  -- (left + right)/2
    else if feature == LangSvg.boxCY then EqnOp Div [EqnOp Plus [eqnVal "TOP", eqnVal "BOT"], eqnVal2] -- (top + bottom)/2
    else if feature == LangSvg.boxWidth  then EqnOp Minus [eqnVal "RIGHT", eqnVal "LEFT"] -- (right - left)
    else if feature == LangSvg.boxHeight then EqnOp Minus [eqnVal "BOT", eqnVal "TOP"] -- (bottom - top)
    else Debug.crash <| "Boxes do not have this feature: " ++ feature
  in
  let handleCircle () =
    if feature == LangSvg.circleCX then eqnVal "cx"
    else if feature == LangSvg.circleCY then eqnVal "cy"
    else if feature == LangSvg.circleR then eqnVal "r"
    else Debug.crash <| "Circles do not have this feature: " ++ feature
  in
  let handleEllipse () =
    if feature == LangSvg.ellipseCX then eqnVal "cx"
    else if feature == LangSvg.ellipseCY then eqnVal "cx"
    else if feature == LangSvg.ellipseRX then eqnVal "rx"
    else if feature == LangSvg.ellipseRY then eqnVal "ry"
    else Debug.crash <| "Ellipses do not have this feature: " ++ feature
  in
  let handleLine () =
    if feature == LangSvg.lineX1 then eqnVal "x1"
    else if feature == LangSvg.lineY1 then eqnVal "y1"
    else if feature == LangSvg.lineX2 then eqnVal "x2"
    else if feature == LangSvg.lineY2 then eqnVal "y2"
    else if feature == LangSvg.lineCX then EqnOp Div [EqnOp Plus [eqnVal "x1", eqnVal "x2"], eqnVal2] -- (x1 + x2) / 2
    else if feature == LangSvg.lineCY then EqnOp Div [EqnOp Plus [eqnVal "y1", eqnVal "y2"], eqnVal2] -- (y1 + y2) / 2
    else Debug.crash <| "Lines do not have this feature: " ++ feature
  in
  let handlePolyPath () =
    let ptCount = getPtCount nodeAttrs in
    let x i = eqnVal ("x" ++ toString i) in
    let y i = eqnVal ("y" ++ toString i) in
    if String.startsWith LangSvg.polyPathPtX feature then
      let iStr = String.dropLeft (String.length LangSvg.polyPathPtX) feature in
      let i    = Utils.fromOk_ <| String.toInt iStr in
      x i
    else if String.startsWith LangSvg.polyPathPtY feature then
      let iStr = String.dropLeft (String.length LangSvg.polyPathPtY) feature in
      let i    = Utils.fromOk_ <| String.toInt iStr in
      y i
    else if String.startsWith LangSvg.polyPathMidptX feature then
      let i1Str = String.dropLeft (String.length LangSvg.polyPathMidptX) feature in
      let i1    = Utils.fromOk_ <| String.toInt i1Str in
      let i2    = if i1 == ptCount then 1 else i1 + 1 in
      EqnOp Div [EqnOp Plus [(x i1), (x i2)], eqnVal2] -- (x1 + x2) / 2
    else if String.startsWith LangSvg.polyPathMidptY feature then
      let i1Str = String.dropLeft (String.length LangSvg.polyPathMidptY) feature in
      let i1    = Utils.fromOk_ <| String.toInt i1Str in
      let i2    = if i1 == ptCount then 1 else i1 + 1 in
      EqnOp Div [EqnOp Plus [(y i1), (y i2)], eqnVal2] -- (y1 + y2) / 2
    else Debug.crash <| "Polygons/polylines do not have this feature: " ++ feature
  in
  case kind of
    "rect"     -> handleRect ()
    "BOX"      -> handleBox ()
    "circle"   -> handleCircle ()
    "ellipse"  -> handleEllipse ()
    "line"     -> handleLine ()
    "polygon"  -> handlePolyPath ()
    "polyline" -> handlePolyPath ()
    _          -> Debug.crash <| "Shape features not implemented yet: " ++ kind


pluckSelectedVals selectedFeatures slate =
  let featureEquations = pluckSelectedFeatureEquations selectedFeatures slate in
  List.concatMap equationVals featureEquations

maybeFindAttr_ id kind attr attrs =
  case Utils.maybeFind attr attrs of
    Just aval -> LangSvg.valOfAVal aval
    Nothing   -> Debug.crash <| toString ("RelateAttrs 2", id, kind, attr, attrs)

getXYi attrs si fstOrSnd =
  let i = Utils.fromOk_ <| String.toInt si in
  case Utils.maybeFind "points" attrs of
    Just aval -> case aval.av_ of
      LangSvg.APoints pts -> LangSvg.valOfAVal <| LangSvg.aNum <| fstOrSnd <| Utils.geti i pts
      _                   -> Debug.crash "getXYi 2"
    _ -> Debug.crash "getXYi 1"

getPtCount attrs =
  case Utils.maybeFind "points" attrs of
    Just aval -> case aval.av_ of
      LangSvg.APoints pts -> List.length pts
      _                   -> Debug.crash "getPtCount 2"
    _ -> Debug.crash "getPtCount 1"

maybeFindAttr id kind attr attrs =
  case (kind, String.uncons attr) of
    ("polygon", Just ('x', si)) -> getXYi attrs si fst
    ("polygon", Just ('y', si)) -> getXYi attrs si snd
    _                           -> maybeFindAttr_ id kind attr attrs


--------------------------------------------------------------------------------
-- Updating the Model

upstate : Event -> Model -> Model
upstate evt old = case debugLog "Event" evt of

    Noop -> old

    WindowDimensions wh -> { old | dimensions = wh }

    Edit -> { old | editingMode = Just old.code }

    Run ->
      case parseE old.code of
        Ok e ->
         let h = case old.editingMode of
           Nothing -> old.history
           Just "" -> old.history -- "" from InterfaceStorage
           Just s  -> addToHistory s old.history
         in
         let (newVal,ws) = (Eval.run e) in
         let (newSlideCount, newMovieCount, newMovieDuration, newMovieContinue, newSlate) = LangSvg.fetchEverything old.slideNumber old.movieNumber 0.0 newVal in
         let new =
           { old | inputExp      = e
                 , inputVal      = newVal
                 , code          = unparse e
                 , slideCount    = newSlideCount
                 , movieCount    = newMovieCount
                 , movieTime     = 0
                 , movieDuration = newMovieDuration
                 , movieContinue = newMovieContinue
                 , runAnimation  = newMovieDuration > 0
                 , slate         = newSlate
                 , widgets       = ws
                 , history       = h
                 , editingMode   = Nothing
                 , caption       = Nothing
                 , syncOptions   = Sync.syncOptionsOf old.syncOptions e }
          in
          { new | mode = refreshMode_ new
                , errorBox = Nothing }
        Err err ->
          { old | caption = Just (LangError ("PARSE ERROR!\n" ++ err)) }

    StartAnimation -> upstate Redraw { old | movieTime = 0
                                           , runAnimation = True }

    Redraw ->
      case old.inputVal of
        val ->
          let (newSlideCount, newMovieCount, newMovieDuration, newMovieContinue, newSlate) = LangSvg.fetchEverything old.slideNumber old.movieNumber old.movieTime val in
          { old | slideCount    = newSlideCount
                , movieCount    = newMovieCount
                , movieDuration = newMovieDuration
                , movieContinue = newMovieContinue
                , slate         = newSlate }

    ToggleOutput ->
      let m = case old.mode of
        Print _ -> refreshMode_ old
        _       -> Print (LangSvg.printSvg old.showWidgets old.slate)
      in
      { old | mode = m }

    CodeUpdate newcode -> { old | code = newcode }

    StartResizingMid ->
      if old.hideCode then old
      else if old.hideCanvas then old
      else { old | mouseMode = MouseResizeMid Nothing }

    MouseClickCanvas ->
      case (old.mouseMode, old.toolType) of
        (MouseNothing, Line) -> { old | mouseMode = MouseDrawNew "line" [] }
        (MouseNothing, Rect) -> { old | mouseMode = MouseDrawNew "rect" [] }
        (MouseNothing, Oval) -> { old | mouseMode = MouseDrawNew "ellipse" [] }
        (MouseNothing, Poly) -> { old | mouseMode = MouseDrawNew "polygon" [] }
        (MouseNothing, HelperDot) -> { old | mouseMode = MouseDrawNew "DOT" [] }
        (MouseNothing, HelperLine) -> { old | mouseMode = MouseDrawNew "line" [] }
        (MouseNothing, Lambda _) -> { old | mouseMode = MouseDrawNew "LAMBDA" [] }
        _                    ->   old

    MouseClick click ->
      case old.mouseMode of
        MouseDrawNew "polygon" points ->
          let pointOnCanvas = clickToCanvasPoint old click in
          let add () =
            let points' = pointOnCanvas :: points in
            { old | mouseMode = MouseDrawNew "polygon" points' }
          in
          if points == [] then add ()
          else
            let initialPoint = Utils.last_ points in
            if Utils.distanceInt pointOnCanvas initialPoint > View.drawNewPolygonDotSize then add ()
            else if List.length points == 2 then { old | mouseMode = MouseNothing }
            else if List.length points == 1 then switchToCursorTool old
            else addPolygonToCodeAndRun old points
        MouseDrawNew "DOT" [] ->
          let pointOnCanvas = clickToCanvasPoint old click in
          { old | mouseMode = MouseDrawNew "DOT" [pointOnCanvas] }
        _ ->
          old

    MousePos (mx0, my0) ->
      let (mx, my) = clickToCanvasPoint old (mx0, my0) in
      case old.mouseMode of

        MouseNothing -> old

        MouseResizeMid Nothing ->
          let f =
            case old.orient of
              Vertical   -> \(mx1,_) -> (old.midOffsetX + mx1 - mx0, old.midOffsetY)
              Horizontal -> \(_,my1) -> (old.midOffsetY, old.midOffsetY + my1 - my0)
          in
          { old | mouseMode = MouseResizeMid (Just f) }

        MouseResizeMid (Just f) ->
          let (x,y) = f (mx0, my0) in
          { old | midOffsetX = x , midOffsetY = y }

        MouseObject objid kind zone Nothing ->
          let onNewPos = createMousePosCallback mx my objid kind zone old in
          let mStuff = maybeStuff objid kind zone old in
          let blah = Just (old.code, mStuff, onNewPos) in
          { old | mouseMode = MouseObject objid kind zone blah }

        MouseObject _ _ _ (Just (_, mStuff, onNewPos)) ->
          let (newE,newV,changes,newSlate,newWidgets) = onNewPos (mx, my) in
          { old | code = unparse newE
                , inputExp = newE
                , inputVal = newV
                , slate = newSlate
                , widgets = newWidgets
                , codeBoxInfo = highlightChanges mStuff changes old.codeBoxInfo
                }

        MouseSlider widget Nothing ->
          let onNewPos = createMousePosCallbackSlider mx my widget old in
          { old | mouseMode = MouseSlider widget (Just (old.code, onNewPos)) }

        MouseSlider widget (Just (_, onNewPos)) ->
          let (newE,newV,newSlate,newWidgets) = onNewPos (mx, my) in
          { old | code = unparse newE
                , inputExp = newE
                , inputVal = newV
                , slate = newSlate
                , widgets = newWidgets
                }

        MouseDrawNew "polygon" _ -> old -- handled by MouseClick instead

        MouseDrawNew k [] ->
          let pointOnCanvas = (mx, my) in
          { old | mouseMode = MouseDrawNew k [pointOnCanvas, pointOnCanvas] }

        MouseDrawNew k (_::points) ->
          let pointOnCanvas = (mx, my) in
          { old | mouseMode = MouseDrawNew k (pointOnCanvas::points) }

    SelectObject id kind zone ->
      case old.mode of
        AdHoc       -> { old | mouseMode = MouseObject id kind zone Nothing }
        Live info ->
          case Dict.get id info.triggers of
            Nothing -> { old | mouseMode = MouseNothing }
            Just dZones ->
              case Dict.get zone dZones of
                Just (Just _) -> { old | mouseMode = MouseObject id kind zone Nothing }
                _             -> { old | mouseMode = MouseNothing }
        _ -> old

    MouseUp ->
      case (old.mode, old.mouseMode) of

        (Print _, _) -> old

        (_, MouseObject i k z (Just (s, _, _))) ->
          -- 8/10: re-parsing to get new position info after live sync-ing
          -- TODO: could update positions within highlightChanges
          -- TODO: update inputVal?
          let e = Utils.fromOk_ <| parseE old.code in
          let old' = { old | inputExp = e } in
          refreshHighlights i z
            { old' | mouseMode = MouseNothing, mode = refreshMode_ old'
                   , history = addToHistory s old'.history }

        (_, MouseSlider _ (Just (s, _))) ->
          let e = Utils.fromOk_ <| parseE old.code in
          let old' = { old | inputExp = e } in
            { old' | mouseMode = MouseNothing, mode = refreshMode_ old'
                   , history = addToHistory s old'.history }

        (_, MouseDrawNew _ [])                 -> switchToCursorTool old
        (_, MouseDrawNew "line" [pt2, pt1])    -> addLineToCodeAndRun old pt2 pt1
        (_, MouseDrawNew "rect" [pt2, pt1])    -> addRectToCodeAndRun old pt2 pt1
        (_, MouseDrawNew "ellipse" [pt2, pt1]) -> addEllipseToCodeAndRun old pt2 pt1
        (_, MouseDrawNew "DOT" [pt])           -> addHelperDotToCodeAndRun old pt
        (_, MouseDrawNew "LAMBDA" [pt2, pt1])  -> addLambdaToCodeAndRun old pt2 pt1
        (_, MouseDrawNew "polygon" points)     -> old

        _ -> { old | mouseMode = MouseNothing, mode = refreshMode_ old }

    TickDelta deltaT ->
      case old.mode of
        SyncSelect _ ->
          -- Prevent "jump" after slow first frame render.
          let adjustedDeltaT = if old.syncSelectTime == 0.0 then clamp 0.0 50 deltaT else deltaT in
          upstate Redraw { old | syncSelectTime = old.syncSelectTime + (adjustedDeltaT / 1000) }
        _ ->
          if old.movieTime < old.movieDuration then
            -- Prevent "jump" after slow first frame render.
            let adjustedDeltaT = if old.movieTime == 0.0 then clamp 0.0 50 deltaT else deltaT in
            let newMovieTime = clamp 0.0 old.movieDuration (old.movieTime + (adjustedDeltaT / 1000)) in
            upstate Redraw { old | movieTime = newMovieTime }
          else if old.movieContinue == True then
            upstate NextMovie old
          else
            { old | runAnimation = False }


    RelateAttrs ->
      let selectedVals = debugLog "selectedVals" <| pluckSelectedVals old.selectedFeatures old.slate in
      let revert = (old.inputExp, old.inputVal) in
      let (nextK, l) = Sync.relate old.genSymCount old.inputExp selectedVals in
      let possibleChanges = List.map (addSlateAndCode old) l in
        { old | mode = SyncSelect possibleChanges
              , genSymCount = nextK
              , selectedFeatures = Set.empty -- TODO
              , runAnimation = True
              , syncSelectTime = 0.0
              }

    DigHole ->
      let selectedFeatureEquationsNamed =
        debugLog "selectedFeatureEquations" <|
          pluckSelectedFeatureEquationsNamed old.selectedFeatures old.slate
      in
      let selectedVals =
        debugLog "selectedVals" <|
          pluckSelectedVals old.selectedFeatures old.slate
      in
      let tracesLocsets =
        List.map ((Sync.locsOfTrace old.syncOptions) << valToTrace) selectedVals
      in
      let locset =
        List.foldl Set.union Set.empty tracesLocsets
      in
      let locsetList =
        Set.toList locset
      in
      let isLocsetNode exp =
        case exp.val.e__ of
          EConst ws n loc wd -> Set.member loc locset
          _                  -> False
      in
      let locToNumber =
        let accumulateLocToNumbers exp__ dict =
          case exp__ of
            EConst ws n loc wd ->
              if Set.member loc locset then
                Dict.insert loc n dict
              else
                dict
            _ -> dict
        in
        foldExpViaE__
            accumulateLocToNumbers
            Dict.empty
            old.inputExp
      in
      let locsAncestors = debugLog "locsAncestors" <|
        findAllWithAncestors isLocsetNode old.inputExp
      in
      -- isScope needs to see the node's parent...because case statements
      -- produce many scopes out of one expression
      -- The below adds a maybe parent to each node, so we get List (List
      -- (Maybe Exp, Exp))
      let locsAncestorsWithParents = debugLog "locsAncestorsWithParents" <|
        List.map
            (\locAncestors ->
              Utils.zip (Nothing :: (List.map Just locAncestors)) locAncestors
            )
            locsAncestors
      in
      let locsAncestorScopesWithParents = debugLog "locsAncestorScopesWithParents" <|
        List.map
            (List.filter (\(parent, node) -> isScope parent node))
            locsAncestorsWithParents
      in
      let deepestCommonScopeWithParent = debugLog "deepestCommonAncestorWithParent" <|
        -- If no common scope, we will wrap the root node.
        let commonPrefix = debugLog "commonPrefix" <|
          [(Nothing, old.inputExp)] ++
          Utils.commonPrefix locsAncestorScopesWithParents
        in
        Utils.last_ commonPrefix
      in
      let (deepestCommonScopeParent, deepestCommonScope) =
        deepestCommonScopeWithParent
      in
      -- Avoid name collisions here
      let existingNames = identifiersSet old.inputExp in
      let locIdNameOrigNamePrime =
        let (newUsedNames, result) =
          List.foldr
              (\(locId, frozen, ident) (usedNames, result) ->
                let baseIdent = if ident == "" then "k"++(toString locId) else ident in
                let baseIdentOrig  = baseIdent ++ "Orig" in
                let baseIdentPrime = baseIdent ++ "Prime" in
                let identOrig  = nonCollidingName baseIdentOrig usedNames in
                let identPrime = nonCollidingName baseIdentPrime usedNames in
                (
                  Set.union usedNames (Set.fromList [identOrig, identPrime]),
                  (locId, identOrig, identPrime)::result
                )
              )
              (existingNames, [])
              locsetList
        in
        result
      in
      let newNames = List.concatMap (\(_, n1, n2) -> [n1, n2]) locIdNameOrigNamePrime in
      let namesToAvoid = Set.union existingNames (Set.fromList newNames) in
      let locIdToNewName = debugLog "locIdToNewName" <|
        Dict.fromList
          <| List.map (\(locId, nameOrig, namePrime) -> (locId, namePrime))
          <| locIdNameOrigNamePrime
      in
      let replaceConstsWithVars exp__ =
        case exp__ of
          EConst ws n (locId, frozen, ident) wd ->
            case Dict.get locId locIdToNewName of
              Just newName -> EVar ws newName
              Nothing      -> exp__
          _ -> exp__
      in
      let commonScopeReplaced =
        mapExpViaExp__ replaceConstsWithVars deepestCommonScope
      in
      let newlyWrappedCommonScope =
        let origNames  = List.reverse <| List.map Utils.snd3 locIdNameOrigNamePrime in
        let primeNames = List.reverse <| List.map Utils.thd3 locIdNameOrigNamePrime in
        let valueStrs =
          List.map
              (\loc ->
                toString (Utils.justGet loc locToNumber)
              )
              (List.reverse locsetList)
        in
        let featureNamesWithExpressionStrs =
          let locIdToOrigName =
            Dict.fromList
              <| List.map (\(locId, nameOrig, namePrime) -> (locId, nameOrig))
              <| locIdNameOrigNamePrime
          in
          let substStr =
            Dict.union
                locIdToOrigName
                (LangParser2.substStrOf old.inputExp)
          in
          List.map (Utils.mapSnd <| equationToLittle substStr) selectedFeatureEquationsNamed
        in
        -- Remove expressions of only one term
        let significantFeatureNamesWithExpressionStrs =
          List.filter
              (\(name, expStr) -> String.contains " " expStr)
              featureNamesWithExpressionStrs
        in
        let featureNames          = List.map fst significantFeatureNamesWithExpressionStrs in
        let featureExpressionStrs = List.map snd significantFeatureNamesWithExpressionStrs in
        let nonCollidingFeatureNames =
          let (newNamesToAvoid, result) =
            List.foldr
                (\featureName (usedNames, result) ->
                  let featureName' = nonCollidingName featureName usedNames in
                  (
                    Set.insert featureName' usedNames,
                    featureName'::result
                  )
                )
                (namesToAvoid, [])
                featureNames
          in
          result
        in
        let oldPreceedingWhitespace = preceedingWhitespace commonScopeReplaced in
        let extraWhitespace =
          if String.contains "\n" oldPreceedingWhitespace then "" else "\n"
        in
        let templateStr =
          let constantOrigNamesStr  = String.join " " origNames in
          let constantPrimeNamesStr = String.join " " primeNames in
          let constantValuesStr     = String.join " " valueStrs in
          let featureNamesStr       = String.join " " nonCollidingFeatureNames in
          let featureExpressionsStr = String.join " " featureExpressionStrs in
          let includeFeatures       = (List.length featureNames) > 0 in
          let originalsLet body =
            oldPreceedingWhitespace
            ++ "(let ["++constantOrigNamesStr++"] ["++constantValuesStr++"]"
            ++ body ++ ")"
          in
          let tracesLet body =
            if includeFeatures then
              extraWhitespace ++ oldPreceedingWhitespace
              ++ "(let ["++featureNamesStr++"] ["++featureExpressionsStr++"]"
              ++ body ++ ")"
            else
              body
          in
          let primesLet body =
            extraWhitespace ++ oldPreceedingWhitespace
            ++ "(let ["++constantPrimeNamesStr++"] ["++constantOrigNamesStr++"]"
            ++ body ++ ")"
          in
          originalsLet
          <| tracesLet
          <| primesLet
          <| "\n  'dummy body'"
        in
        let template =
          case parseE templateStr of
            Ok templateExp -> templateExp
            Err err        -> Debug.crash <| "Dig template err: " ++ err
        in
        -- Now replace the dummy body:
        let newLet =
          mapExpViaExp__
              (\e__ ->
                case e__ of
                  EBase _ (String "dummy body") -> (addPreceedingWhitespace extraWhitespace commonScopeReplaced).val.e__
                  _                             -> e__
              )
              template
        in
        newLet
      in
      -- Debug only:
      let newSubtreeStr = debugLog "newlyWrappedCommonScope" <| unparse newlyWrappedCommonScope in
      let newExp =
        freshen <|
        replaceExpNode deepestCommonScope newlyWrappedCommonScope old.inputExp
      in
      let (newVal, newWidgets) = Eval.run newExp in
      let (newSlate, newCode)  = slateAndCode old (newExp, newVal) in
      debugLog "new model" <|
        { old | code             = newCode
              , inputExp         = newExp
              , inputVal         = newVal
              , history          = addToHistory old.code old.history
              , slate            = newSlate
              , widgets          = newWidgets
              , previewCode      = Nothing
              , mode             = mkLive old.syncOptions old.slideNumber old.movieNumber old.movieTime newExp newVal
              , selectedFeatures = Set.empty
        }


    RelateShapes ->
      let newval = slateToVal old.slate in
      let l = Sync.inferNewRelationships old.inputExp old.inputVal newval in
      let possibleChanges = List.map (addSlateAndCode old) l in
        { old | mode = SyncSelect possibleChanges, runAnimation = True, syncSelectTime = 0.0 }

    -- TODO AdHoc/Sync not used at the moment
    Sync ->
      case (old.mode, old.inputExp) of
        (AdHoc, ip) ->
          let
            -- If stuff breaks, try re-adding this.
            -- We forgot why it was here.
            -- inputval   = fst <| Eval.run ip
            -- inputSlate = LangSvg.resolveToIndexedTree old.slideNumber old.movieNumber old.movieTime inputval
            -- inputval'  = slateToVal inputSlate
            newval     = slateToVal old.slate
            local      = Sync.inferLocalUpdates old.syncOptions ip old.inputVal newval
            struct     = Sync.inferStructuralUpdates ip old.inputVal newval
            delete     = Sync.inferDeleteUpdates ip old.inputVal newval
            relatedG   = Sync.inferNewRelationships ip old.inputVal newval
            relatedV   = Sync.relateSelectedAttrs old.genSymCount ip old.inputVal newval
          in
          let addSlateAndCodeToAll list = List.map (addSlateAndCode old) list in
            case (local, relatedV) of
              (Ok [], (_, [])) -> { old | mode = mkLive_ old.syncOptions old.slideNumber old.movieNumber old.movieTime ip }
              (Ok [], (nextK, changes)) ->
                let _ = debugLog ("no live updates, only related var") () in
                let m = SyncSelect (addSlateAndCodeToAll changes) in
                { old | mode = m, genSymCount = nextK, runAnimation = True, syncSelectTime = 0.0 }
              (Ok live, _) ->
                let n = debugLog "# of live updates" (List.length live) in
                let changes = live ++ delete ++ relatedG ++ struct in
                let m = SyncSelect (addSlateAndCodeToAll changes) in
                { old | mode = m, runAnimation = True, syncSelectTime = 0.0 }
              (Err e, _) ->
                let _ = debugLog ("no live updates: " ++ e) () in
                let changes = delete ++ relatedG ++ struct in
                let m = SyncSelect (addSlateAndCodeToAll changes) in
                { old | mode = m, runAnimation = True, syncSelectTime = 0.0 }
        _ -> Debug.crash "upstate Sync"

    SelectOption (exp, val, slate, code) ->
      maybeLeaveDeleteMode
        { old | code          = code
              , inputExp      = exp
              , inputVal      = val
              , history       = addToHistory old.code old.history
              , slate         = slate
              , previewCode   = Nothing
              , toolType      = Cursor
              , mode          = mkLive old.syncOptions old.slideNumber old.movieNumber old.movieTime exp val }
        showZonesNone


    PreviewCode maybeCode ->
      { old | previewCode = maybeCode }

    CancelSync ->
      upstate Run { old | mode = mkLive_ old.syncOptions old.slideNumber old.movieNumber old.movieTime old.inputExp }

    SelectExample name thunk ->
      if name == Examples.scratchName then
        upstate Run { old | exName = name, code = old.scratchCode, history = ([],[]) }
      else

      let {e,v,ws} = thunk () in
      let (so, m) =
        case old.mode of
          Live _  -> let so = Sync.syncOptionsOf old.syncOptions e in (so, mkLive so old.slideNumber old.movieNumber old.movieTime e v)
          Print _ -> let so = Sync.syncOptionsOf old.syncOptions e in (so, mkLive so old.slideNumber old.movieNumber old.movieTime e v)
          _      -> (old.syncOptions, old.mode)
      in
      let scratchCode' =
        if old.exName == Examples.scratchName then old.code else old.scratchCode
      in
      let (slideCount, movieCount, movieDuration, movieContinue, slate) = LangSvg.fetchEverything old.slideNumber old.movieNumber old.movieTime v in
      { old | scratchCode   = scratchCode'
            , exName        = name
            , inputExp      = e
            , inputVal      = v
            , code          = unparse e
            , history       = ([],[])
            , mode          = m
            , syncOptions   = so
            , slideNumber   = 1
            , slideCount    = slideCount
            , movieCount    = movieCount
            , movieTime     = 0
            , movieDuration = movieDuration
            , movieContinue = movieContinue
            , runAnimation  = movieDuration > 0
            , slate         = slate
            , widgets       = ws
            }

    SwitchMode m -> { old | mode = m }

    SwitchOrient -> { old | orient = switchOrient old.orient }

    SelectZonesMode i ->
      maybeLeaveDeleteMode { old | showZones = i } old.showZones

    Undo ->
      case (old.code, old.history) of
        (_, ([],_)) -> old                -- because of keyboard shortcuts
        (current, (s::past, future)) ->
          let new = { old | history = (past, current::future) } in
          upstate Run (upstate (CodeUpdate s) new)

    Redo ->
      case (old.code, old.history) of
        (_, (_,[])) -> old                -- because of keyboard shorcuts
        (current, (past, s::future)) ->
          let new = { old | history = (current::past, future) } in
          upstate Run (upstate (CodeUpdate s) new)

    NextSlide ->
      if old.slideNumber >= old.slideCount then
        upstate StartAnimation { old | slideNumber = old.slideNumber
                                     , movieNumber = old.movieCount }
      else
        upstate StartAnimation { old | slideNumber = old.slideNumber + 1
                                     , movieNumber = 1 }

    PreviousSlide ->
      if old.slideNumber <= 1 then
        upstate StartAnimation { old | slideNumber = 1
                                     , movieNumber = 1 }
      else
        let previousSlideNumber    = old.slideNumber - 1 in
        case old.inputExp of
          exp ->
            let previousVal = fst <| Eval.run exp in
            let previousMovieCount = LangSvg.resolveToMovieCount previousSlideNumber previousVal in
            upstate StartAnimation { old | slideNumber = previousSlideNumber
                                         , movieNumber = previousMovieCount }

    NextMovie ->
      if old.movieNumber == old.movieCount && old.slideNumber < old.slideCount then
        upstate NextSlide old
      else if old.movieNumber < old.movieCount then
        upstate StartAnimation { old | movieNumber = old.movieNumber + 1 }
      else
        -- Last movie of slide show; skip to its end.
        upstate Redraw { old | movieTime    = old.movieDuration
                             , runAnimation = False }

    PreviousMovie ->
      if old.movieNumber == 1 then
        upstate PreviousSlide old
      else
        upstate StartAnimation { old | movieNumber = old.movieNumber - 1 }

    KeysDown l ->
      -- let _ = Debug.log "keys" (toString l) in
      { old | keysDown = l }

{-      case old.mode of
          SaveDialog _ -> old
          _ -> case editingMode old of
            True -> if
              | l == keysMetaShift -> upstate Run old
              | otherwise -> old
            False -> if
              | l == keysE -> upstate Edit old
              | l == keysZ -> upstate Undo old
              -- | l == keysShiftZ -> upstate Redo old
              | l == keysY -> upstate Redo old
              | l == keysG || l == keysH -> -- for right- or left-handers
                  upstate ToggleZones old
              | l == keysO -> upstate ToggleOutput old
              | l == keysP -> upstate SwitchOrient old
              | l == keysS ->
                  let _ = Debug.log "TODO Save" () in
                  upstate Noop old
              | l == keysShiftS ->
                  let _ = Debug.log "TODO Save As" () in
                  upstate Noop old
              | l == keysRight -> adjustMidOffsetX old 25
              | l == keysLeft  -> adjustMidOffsetX old (-25)
              | l == keysUp    -> adjustMidOffsetY old (-25)
              | l == keysDown  -> adjustMidOffsetY old 25
              | otherwise -> old
-}

{-
      let fire evt = upstate evt old in

      case editingMode old of

        True ->
          if l == keysEscShift then fire Run
          else                      fire Noop

        False ->

          -- events for any non-editing mode
          if      l == keysO          then fire ToggleOutput
          else if l == keysP          then fire SwitchOrient
          else if l == keysShiftRight then adjustMidOffsetX old 25
          else if l == keysShiftLeft  then adjustMidOffsetX old (-25)
          else if l == keysShiftUp    then adjustMidOffsetY old (-25)
          else if l == keysShiftDown  then adjustMidOffsetY old 25

          -- events for specific non-editing mode
          else case old.mode of

              Live _ ->
                if      l == keysE        then fire Edit
                else if l == keysZ        then fire Undo
                else if l == keysY        then fire Redo
                else if l == keysT        then fire (SwitchMode AdHoc)
                else if l == keysS        then fire Noop -- placeholder for Save
                else if l == keysShiftS   then fire Noop -- placeholder for Save As
                else                           fire Noop

              AdHoc ->
                if      l == keysZ        then fire Undo
                else if l == keysY        then fire Redo
                else if l == keysT        then fire Sync
                else                           fire Noop

              _                       -> fire Noop
-}

    CleanCode ->
      case parseE old.code of
        Err err ->
          { old | caption = Just (LangError ("PARSE ERROR!\n" ++ err)) }
        Ok reparsed ->
          let cleanedExp =
            reparsed
            |> cleanExp
            |> LangTransform.simplify
            |> freshen
          in
          let code' = unparse cleanedExp in
          let history' =
            if old.code == code'
              then old.history
              else addToHistory old.code old.history
          in
          let _ = debugLog "Cleaned: " code' in
          { old | inputExp = cleanedExp, code = code', history = history' }

    -- Elm does not have function equivalence/pattern matching, so we need to
    -- thread these events through upstate in order to catch them to rerender
    -- appropriately (see CodeBox.elm)
    InstallSaveState -> installSaveState old
    RemoveDialog makeSave saveName -> removeDialog makeSave saveName old
    ToggleBasicCodeBox -> { old | basicCodeBox = not old.basicCodeBox }
    UpdateFieldContents fieldContents -> { old | fieldContents = fieldContents }

    UpdateModel f -> f old

    -- Lets multiple events be executed in sequence (useful for CodeBox.elm)
    MultiEvent evts -> case evts of
      [] -> old
      e1 :: es -> upstate e1 old |> upstate (MultiEvent es)

    WaitRun -> old
    WaitSave saveName -> { old | exName = saveName }
    WaitClean -> old
    WaitCodeBox -> old

adjustMidOffsetX old dx =
  case old.orient of
    Vertical   -> { old | midOffsetX = old.midOffsetX + dx }
    Horizontal -> upstate SwitchOrient old

adjustMidOffsetY old dy =
  case old.orient of
    Horizontal -> { old | midOffsetY = old.midOffsetY + dy }
    Vertical   -> upstate SwitchOrient old



--------------------------------------------------------------------------------
-- Mouse Callbacks for Zones

type alias OnMouse =
  { posX : Num -> Num , posY : Num -> Num
  , negX : Num -> Num , negY : Num -> Num
  -- , posXposY : Num -> Num
  }

createMousePosCallback mx my objid kind zone old =
 case Utils.justGet_ "#3" objid (snd old.slate) of
 LangSvg.TextNode _ -> Debug.crash "createMousePosCallback TextNode"
 LangSvg.SvgNode _ attrs _ ->
  let numAttr = toNum << Utils.find_ attrs in
  let mapNumAttr f a =
    let av = Utils.find_ attrs a in
    let (n,trace) = toNumTr av in
    (a, LangSvg.AVal (LangSvg.ANum (f n, trace)) av.vtrace) in
      -- preserve existing VTrace

  \(mx',my') ->

    let scaledPosX scale n = n + scale * (toFloat mx' - toFloat mx) in

    let posX n = n - toFloat mx + toFloat mx' in
    let posY n = n - toFloat my + toFloat my' in
    let negX n = n + toFloat mx - toFloat mx' in
    let negY n = n + toFloat my - toFloat my' in

    -- let posXposY n =
    --   let dx = toFloat mx - toFloat mx' in
    --   let dy = toFloat my - toFloat my' in
    --   if | abs dx >= abs dy  -> n - dx
    --      | otherwise         -> n - dy in

    let onMouse =
      { posX = posX, posY = posY, negX = negX, negY = negY } in

    -- let posX' (n,tr) = (posX n, tr) in
    -- let posY' (n,tr) = (posY n, tr) in
    -- let negX' (n,tr) = (negX n, tr) in
    -- let negY' (n,tr) = (negY n, tr) in

    let fx  = mapNumAttr posX in
    let fy  = mapNumAttr posY in
    let fx_ = mapNumAttr negX in
    let fy_ = mapNumAttr negY in

    let fxColorBall =
      mapNumAttr (LangSvg.clampColorNum << scaledPosX View.scaleColorBall) in

    let ret l = (l, l) in

    let (newRealAttrs,newFakeAttrs) =
      case (kind, zone) of

        -- first match zones that can be attached to different shape kinds...

        (_, "FillBall")   -> ret [fxColorBall "fill"]
        (_, "RotateBall") -> createCallbackRotate (toFloat mx) (toFloat my)
                                                  (toFloat mx') (toFloat my')
                                                  kind objid old

        -- ... and then match each kind of shape separately

        ("rect", "Interior")       -> ret [fx "x", fy "y"]
        ("rect", "RightEdge")      -> ret [fx "width"]
        ("rect", "BotRightCorner") -> ret [fx "width", fy "height"]
        ("rect", "BotEdge")        -> ret [fy "height"]
        ("rect", "BotLeftCorner")  -> ret [fx "x", fx_ "width", fy "height"]
        ("rect", "LeftEdge")       -> ret [fx "x", fx_ "width"]
        ("rect", "TopLeftCorner")  -> ret [fx "x", fy "y", fx_ "width", fy_ "height"]
        ("rect", "TopEdge")        -> ret [fy "y", fy_ "height"]
        ("rect", "TopRightCorner") -> ret [fy "y", fx "width", fy_ "height"]

        ("BOX", "Interior")        -> ret [fx "LEFT", fy "TOP", fx "RIGHT", fy "BOT"]
        ("BOX", "RightEdge")       -> ret [fx "RIGHT"]
        ("BOX", "BotRightCorner")  -> ret [fx "RIGHT", fy "BOT"]
        ("BOX", "BotEdge")         -> ret [fy "BOT"]
        ("BOX", "BotLeftCorner")   -> ret [fx "LEFT", fy "BOT"]
        ("BOX", "LeftEdge")        -> ret [fx "LEFT"]
        ("BOX", "TopLeftCorner")   -> ret [fx "LEFT", fy "TOP"]
        ("BOX", "TopEdge")         -> ret [fy "TOP"]
        ("BOX", "TopRightCorner")  -> ret [fy "TOP", fx "RIGHT"]

        ("circle", "Interior") -> ret [fx "cx", fy "cy"]
        ("circle", "Edge") ->
          let (cx,cy) = Utils.unwrap2 <| List.map numAttr ["cx", "cy"] in
          let dx = if toFloat mx >= cx then mx' - mx else mx - mx' in
          let dy = if toFloat my >= cy then my' - my else my - my' in
          ret [ (mapNumAttr (\r -> r + toFloat (max dx dy)) "r") ]

        ("ellipse", "Interior") -> ret [fx "cx", fy "cy"]
        ("ellipse", "Edge")     ->
          let (cx,cy) = Utils.unwrap2 <| List.map numAttr ["cx", "cy"] in
          let dx = if toFloat mx >= cx then fx else fx_ in
          let dy = if toFloat my >= cy then fy else fy_ in
          ret [dx "rx", dy "ry"]

        ("line", "Edge") -> ret [fx "x1", fx "x2", fy "y1", fy "y2"]
        ("line", _) ->
          case LangSvg.realZoneOf zone of
            LangSvg.ZPoint i -> ret [fx (addi "x" i), fy (addi "y" i)]
            _                -> Debug.crash "createMousePosCallback line"

        ("polygon", _)  -> createCallbackPoly zone kind objid old onMouse
        ("polyline", _) -> createCallbackPoly zone kind objid old onMouse

        ("path", _) -> createCallbackPath zone kind objid old onMouse

        _ -> Debug.crash "createMousePosCallback"

    in
    let newTree = List.foldr (upslate objid) (snd old.slate) newRealAttrs in
      case old.mode of
        AdHoc -> (old.inputExp, old.inputVal, Dict.empty, (fst old.slate, newTree), old.widgets)
        Live info ->
          case Utils.justGet_ "#4" zone (Utils.justGet_ "#5" objid info.triggers) of
            -- Nothing -> (Utils.fromJust old.inputExp, newSlate)
            Nothing -> Debug.crash "shouldn't happen due to upstate SelectObject"
            Just trigger ->
              -- let (newE,otherChanges) = trigger (List.map (Utils.mapSnd toNum) newFakeAttrs) in
              let (newE,changes) = trigger (List.map (Utils.mapSnd toNum) newFakeAttrs) in
              if not Sync.tryToBeSmart then
                let (newV,newWidgets) = Eval.run newE in
                (newE, newV, changes, LangSvg.resolveToIndexedTree old.slideNumber old.movieNumber old.movieTime newV, newWidgets)
              else
                Debug.crash "Controller tryToBeSmart"
              {-
              let newSlate' =
                Dict.foldl (\j dj acc1 ->
                  let _ = Debug.crash "TODO: dummyTrace is probably a problem..." in
                  Dict.foldl
                    (\a n acc2 -> upslate j (a, LangSvg.ANum (n, dummyTrace)) acc2) acc1 dj
                  ) newSlate otherChanges
              in
              (newE, newSlate')
              -}
        _ -> Debug.crash "createMousePosCallback"

-- Callbacks for Polygons/Polylines

createCallbackPoly zone shape =
  let _ = Utils.assert "createCallbackPoly" (shape == "polygon" || shape == "polyline") in
  case LangSvg.realZoneOf zone of
    LangSvg.Z "Interior" -> polyInterior shape
    LangSvg.ZPoint i     -> polyPoint i shape
    LangSvg.ZEdge i      -> polyEdge i shape
    _                    -> Debug.crash "createCallbackPoly"

-- TODO:
--  - differentiate between "polygon" and "polyline" for interior
--  - rethink/refactor point/edge zones

lift : (Num -> Num) -> (NumTr -> NumTr)
lift f (n,t) = (f n, t)

-- TODO everywhere aNum, aTransform, etc is called, preserve vtrace

polyInterior shape objid old onMouse =
  case Dict.get objid (snd old.slate) of
    Just (LangSvg.SvgNode _ nodeAttrs _) ->
      let pts = toPoints <| Utils.find_ nodeAttrs "points" in
      let accs =
        let foo (j,(xj,yj)) (acc1,acc2) =
          let (xj',yj') = (lift onMouse.posX xj, lift onMouse.posY yj) in
          let acc2' = (addi "x"j, LangSvg.aNum xj') :: (addi "y"j, LangSvg.aNum yj') :: acc2 in
          ((xj',yj')::acc1, acc2')
        in
        Utils.foldli foo ([],[]) pts
      in
      let (acc1,acc2) = Utils.reverse2 accs in
      ([("points", LangSvg.aPoints acc1)], acc2)
    _ ->
      Debug.crash "polyInterior"

polyPoint i shape objid old onMouse =
  case Dict.get objid (snd old.slate) of
    Just (LangSvg.SvgNode _ nodeAttrs _) ->
      let pts = toPoints <| Utils.find_ nodeAttrs "points" in
      let accs =
        let foo (j,(xj,yj)) (acc1,acc2) =
          if i /= j
            then ((xj,yj)::acc1, acc2)
            else let (xj',yj') = (lift onMouse.posX xj, lift onMouse.posY yj) in
                 let acc2' = (addi "x"i, LangSvg.aNum xj')
                             :: (addi "y"i, LangSvg.aNum yj')
                             :: acc2 in
                 ((xj',yj')::acc1, acc2')
        in
        Utils.foldli foo ([],[]) pts
      in
      let (acc1,acc2) = Utils.reverse2 accs in
      ([("points", LangSvg.aPoints acc1)], acc2)
    _ ->
      Debug.crash "polyPoint"

polyEdge i shape objid old onMouse =
  case Dict.get objid (snd old.slate) of
    Just (LangSvg.SvgNode _ nodeAttrs _) ->
      let pts = toPoints <| Utils.find_ nodeAttrs "points" in
      let n = List.length pts in
      let accs =
        let foo (j,(xj,yj)) (acc1,acc2) =
          if i == j || (i == n && j == 1) || (i < n && j == i+1) then
            let (xj',yj') = (lift onMouse.posX xj, lift onMouse.posY yj) in
            let acc2' = (addi "x"j, LangSvg.aNum xj')
                        :: (addi "y"j, LangSvg.aNum yj')
                        :: acc2 in
            ((xj',yj')::acc1, acc2')
          else
            ((xj,yj)::acc1, acc2)
        in
        Utils.foldli foo ([],[]) pts
      in
      let (acc1,acc2) = Utils.reverse2 accs in
      ([("points", LangSvg.aPoints acc1)], acc2)
    _ ->
      Debug.crash "polyEdge"

-- Callbacks for Paths

createCallbackPath zone shape =
  let _ = Utils.assert "createCallbackPath" (shape == "path") in
  case LangSvg.realZoneOf zone of
    LangSvg.ZPoint i -> pathPoint i
    _                -> Debug.crash "createCallbackPath"

pathPoint i objid old onMouse =

  let updatePt (mj,(x,y)) =
    if mj == Just i
      then (mj, (lift onMouse.posX x, lift onMouse.posY y))
      else (mj, (x, y)) in
  let addFakePts =
    List.foldl <| \(mj,(x,y)) acc ->
      if mj == Just i
        then (addi "x"i, LangSvg.aNum x) :: (addi "y"i, LangSvg.aNum y) :: acc
        else acc in

  case Dict.get objid (snd old.slate) of
    Just (LangSvg.SvgNode _ nodeAttrs _) ->
      let (cmds,counts) = LangSvg.toPath <| Utils.find_ nodeAttrs "d" in
      let accs =
        let foo c (acc1,acc2) =
          let (c',acc2') = case c of
            LangSvg.CmdZ s ->
              (LangSvg.CmdZ s, acc2)
            LangSvg.CmdMLT s pt ->
              let pt' = updatePt pt in
              (LangSvg.CmdMLT s pt', addFakePts acc2 [pt'])
            LangSvg.CmdHV s n ->
              (LangSvg.CmdHV s n, acc2)
            LangSvg.CmdC s pt1 pt2 pt3 ->
              let (pt1',pt2',pt3') = Utils.unwrap3 <| List.map updatePt [pt1,pt2,pt3] in
              (LangSvg.CmdC s pt1' pt2' pt3', addFakePts acc2 [pt1',pt2',pt3'])
            LangSvg.CmdSQ s pt1 pt2 ->
              let (pt1',pt2') = Utils.unwrap2 <| List.map updatePt [pt1,pt2] in
              (LangSvg.CmdSQ s pt1' pt2' , addFakePts acc2 [pt1',pt2'])
            LangSvg.CmdA s a b c d e pt ->
              let pt' = updatePt pt in
              (LangSvg.CmdA s a b c d e pt', addFakePts acc2 [pt'])
          in
          (c' :: acc1, acc2')
        in
        List.foldr foo ([],[]) cmds
      in
      let (acc1,acc2) = Utils.reverse2 accs in
      ([("d", LangSvg.aPath2 (acc1, counts))], acc2)

    _ ->
      Debug.crash "pathPoint"

-- Callbacks for Rotate zones

createCallbackRotate mx0 my0 mx1 my1 shape objid old =
  case Dict.get objid (snd old.slate) of
    Just (LangSvg.SvgNode _ nodeAttrs _) ->
      let (rot,cx,cy) = LangSvg.toTransformRot <| Utils.find_ nodeAttrs "transform" in
      let rot' =
        let a0 = Utils.radiansToDegrees <| atan2 (fst cy - my0) (mx0 - fst cx) in
        let a1 = Utils.radiansToDegrees <| atan2 (fst cy - my1) (mx1 - fst cx) in
        (fst rot + (a0 - a1), snd rot) in
      let real = [("transform", LangSvg.aTransform [LangSvg.Rot rot' cx cy])] in
      let fake = [("transformRot", LangSvg.aNum rot')] in
      (real, fake)
    _ -> Debug.crash "createCallbackRotate"


--------------------------------------------------------------------------------
-- Mouse Callbacks for UI Widgets

wSlider = params.mainSection.uiWidgets.wSlider

createMousePosCallbackSlider mx my widget old =

  let (maybeRound, minVal, maxVal, curVal, locid) =
    case widget of
      WIntSlider a b _ curVal (locid,_,_) ->
        (toFloat << round, toFloat a, toFloat b, toFloat curVal, locid)
      WNumSlider a b _ curVal (locid,_,_) ->
        (identity, a, b, curVal, locid)
  in
  let range = maxVal - minVal in

  \(mx',my') ->
    let newVal =
      curVal + (toFloat (mx' - mx) / toFloat wSlider) * range
        |> clamp minVal maxVal
        |> maybeRound
    in
    -- unlike the live triggers via Sync,
    -- this substitution only binds the location to change
    let subst = Dict.singleton locid newVal in
    let newE = applyLocSubst subst old.inputExp in
    let (newVal,newWidgets) = Eval.run newE in
    -- Can't manipulate slideCount/movieCount/movieDuration/movieContinue via sliders at the moment.
    let newSlate = LangSvg.resolveToIndexedTree old.slideNumber old.movieNumber old.movieTime newVal in
    (newE, newVal, newSlate, newWidgets)
