module LangSvg where
-- module LangSvg (valToHtml, valToIndexedTree, printIndexedTree) where

import Html
import Html.Attributes as HA
import Svg
import Svg.Attributes as A
import VirtualDom

-- in Svg.elm:
--   type alias Svg = VirtualDom.Node
--   type alias Attribute = VirtualDom.Property

-- in Html.elm:
--   type alias Html = VirtualDom.Node

import Debug
import Set
import String
import Char
import Dict exposing (Dict)
import ColorNum

import Lang exposing (..)
import Utils

------------------------------------------------------------------------------

-- TODO probably want to factor HTML attributes and SVG attributes into
-- records rather than lists of lists of ...

valToHtml : Int -> Int -> Val -> Html.Html
valToHtml w h (VList [VBase (String "svg"), VList vs1, VList vs2]) =
  let wh = [numAttrToVal "width" w, numAttrToVal "height" h] in
  let v' = VList [VBase (String "svg"), VList (wh ++ vs1), VList vs2] in
  compileValToNode v'
    -- NOTE: not checking if width/height already in vs1

compileValToNode : Val -> VirtualDom.Node
compileValToNode v = case v of
  VList [VBase (String "TEXT"), VBase (String s)] -> VirtualDom.text s
  VList [VBase (String f), VList vs1, VList vs2] ->
    (svg f) (compileAttrVals vs1) (compileNodeVals vs2)

compileNodeVals = List.map compileValToNode
compileAttrVals = List.map (uncurry compileAttr << valToAttr)
compileAttrs    = List.map (uncurry compileAttr)
compileAttr k v = (attr k) (strAVal v)

numAttrToVal a i =
  VList [VBase (String a), VConst (toFloat i, dummyTrace)]

type AVal
  = ANum NumTr
  | AString String
  | APoints (List Point)
  | ARgba Rgba
  | AColorNum NumTr -- Utils.numToColor [0,500)
  | APath2 (List PathCmd, PathCounts)
  | ATransform (List TransformCmd)

type alias Point = (NumTr, NumTr)
type alias Rgba  = (NumTr, NumTr, NumTr, NumTr)

type PathCmd
  = CmdZ   Cmd
  | CmdMLT Cmd IdPoint
  | CmdHV  Cmd NumTr
  | CmdC   Cmd IdPoint IdPoint IdPoint
  | CmdSQ  Cmd IdPoint IdPoint
  | CmdA   Cmd NumTr NumTr NumTr NumTr NumTr IdPoint

type TransformCmd
  = Rot NumTr NumTr NumTr

type alias PathCounts = {numPoints : Int}

type alias Cmd = String -- single uppercase/lowercase letter

type alias IdPoint = (Maybe Int, Point)

-- toNum    (ANum (i,_)) = i
-- toNumTr  (ANum (i,t)) = (i,t)

-- temporary way to ignore numbers specified as strings (also see Sync)

toNum a = case a of
  ANum (n,_) -> n
  AString s  -> case String.toFloat s of
                  Ok n -> n

toNumTr a = case a of
  ANum (n,t) -> (n,t)
  AString s  -> case String.toFloat s of
                  Ok n -> (n, dummyTrace)

toPoints (APoints pts) = pts
toPath   (APath2 p) = p

valToAttr (VList [VBase (String k), v]) =
  case (k, v) of
    ("points", VList vs)  -> (k, APoints <| List.map valToPoint vs)
    ("fill", VList vs)    -> (k, ARgba <| valToRgba vs)
    ("fill", VConst it)   -> (k, AColorNum it)
    ("stroke", VList vs)  -> (k, ARgba <| valToRgba vs)
    -- TODO "stroke" AColorNum
    ("d", VList vs)       -> (k, APath2 (valsToPath2 vs))
    ("transform", v1)     -> (k, ATransform (valToTransform v1))
    (_, VConst it)        -> (k, ANum it)
    (_, VBase (String s)) -> (k, AString s)

valToPoint (VList [VConst x, VConst y]) = (x,y)
pointToVal (x,y) = (VList [VConst x, VConst y])

valToRgba [VConst r, VConst g, VConst b, VConst a] = (r,g,b,a)
rgbaToVal (r,g,b,a) = [VConst r, VConst g, VConst b, VConst a]

strPoint (x_,y_) =
  let [x,y] = List.map fst [x_,y_] in
  toString x ++ "," ++ toString y

strRgba (r_,g_,b_,a_) =
  strRgba_ (List.map fst [r_,g_,b_,a_])

strRgba_ rgba =
  "rgba" ++ Utils.parens (Utils.commas (List.map toString rgba))

strAVal a = case a of
  AString s -> s
  ANum it   -> toString (fst it)
  APoints l -> Utils.spaces (List.map strPoint l)
  ARgba tup -> strRgba tup
  APath2 p  -> strAPath2 (fst p)
  ATransform l -> Utils.spaces (List.map strTransformCmd l)
  AColorNum n ->
    -- slight optimization:
    strRgba_ (ColorNum.convert (fst n))
    -- let (r,g,b) = Utils.numToColor 500 (fst n) in
    -- strRgba_ [r,g,b,1]

valOfAVal a = case a of
  AString s -> VBase (String s)
  ANum it   -> VConst it
  APoints l -> VList (List.map pointToVal l)
  ARgba tup -> VList (rgbaToVal tup)
  APath2 p  -> VList (List.concatMap valsOfPathCmd (fst p))
  AColorNum nt -> VConst nt

valsOfPathCmd c =
  let fooPt (_,(x,y)) = [VConst x, VConst y] in
  case c of
    CmdZ   s              -> vStr s :: []
    CmdMLT s pt           -> vStr s :: fooPt pt
    CmdHV  s n            -> vStr s :: [VConst n]
    CmdC   s pt1 pt2 pt3  -> vStr s :: List.concatMap fooPt [pt1,pt2,pt3]
    CmdSQ  s pt1 pt2      -> vStr s :: List.concatMap fooPt [pt1,pt2]
    CmdA   s a b c d e pt -> vStr s :: List.map VConst [a,b,c,d,e] ++ fooPt pt

valOfAttr (k,a) = VList [VBase (String k), valOfAVal a]

-- https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths
-- http://www.w3schools.com/svg/svg_path.asp
--
-- NOTES:
--  . using different representation of points in d than in points
--    to make it less verbose and easier to copy-and-paste raw SVG examples
--  . looks like commas are optional

valsToPath2 = valsToPath2_ {numPoints = 0}

valsToPath2_ : PathCounts -> List Val -> (List PathCmd, PathCounts)
valsToPath2_ counts vs = case vs of
  [] -> ([], counts)
  VBase (String cmd) :: vs' ->
    if | matchCmd cmd "Z" -> CmdZ cmd +++ valsToPath2_ counts vs'
       | matchCmd cmd "MLT" ->
           let ([x,y],vs'') = projConsts 2 vs' in
           let (counts',[pt]) = addIdPoints cmd counts [(x,y)] in
           CmdMLT cmd pt +++ valsToPath2_ counts' vs''
       | matchCmd cmd "HV" ->
           let ([i],vs'') = projConsts 1 vs' in
           CmdHV cmd i +++ valsToPath2_ counts vs''
       | matchCmd cmd "C" ->
           let ([x1,y1,x2,y2,x,y],vs'') = projConsts 6 vs' in
           let (counts',[pt1,pt2,pt3]) = addIdPoints cmd counts [(x1,y1),(x2,y2),(x,y)] in
           CmdC cmd pt1 pt2 pt3 +++ valsToPath2_ counts' vs''
       | matchCmd cmd "SQ" ->
           let ([x1,y1,x,y],vs'') = projConsts 4 vs' in
           let (counts',[pt1,pt2]) = addIdPoints cmd counts [(x1,y1),(x,y)] in
           CmdSQ cmd pt1 pt2 +++ valsToPath2_ counts' vs''
       | matchCmd cmd "A" ->
           let ([rx,ry,axis,flag,sweep,x,y],vs'') = projConsts 7 vs' in
           let (counts',[pt]) = addIdPoints cmd counts [(x,y)] in
           CmdA cmd rx ry axis flag sweep pt +++ valsToPath2_ counts' vs''

x +++ (xs,stuff) = (x::xs, stuff)

addIdPoints : Cmd -> PathCounts -> List Point -> (PathCounts, List IdPoint)
addIdPoints cmd counts pts =
  let [c] = String.toList cmd in
  if | Char.isLower c -> (counts, List.map ((,) Nothing) pts)
     | Char.isUpper c ->
         let (counts',l) =
           List.foldl (\pt (acc1,acc2) ->
             let nextId = 1 + acc1.numPoints in
             let acc1'  = {acc1 | numPoints <- nextId} in
             let acc2'  = (Just nextId, pt) :: acc2 in
             (acc1', acc2')) (counts, []) pts
         in
         (counts', List.reverse l)

strAPath2 =
  let strPt (_,(it,jt)) = toString (fst it) ++ " " ++ toString (fst jt) in
  let strNum (n,_) = toString n in

  let strPathCmd c = case c of
    CmdZ   s              -> s
    CmdMLT s pt           -> Utils.spaces [s, strPt pt]
    CmdHV  s n            -> Utils.spaces [s, strNum n]
    CmdC   s pt1 pt2 pt3  -> Utils.spaces (s :: List.map strPt [pt1,pt2,pt3])
    CmdSQ  s pt1 pt2      -> Utils.spaces (s :: List.map strPt [pt1,pt2])
    CmdA   s a b c d e pt ->
      Utils.spaces (s :: List.map strNum [a,b,c,d,e] ++ [strPt pt])
  in
  Utils.spaces << List.map strPathCmd

projConsts k vs =
  if k == 0 then ([], vs)
  else case vs of
         VConst it ::vs' ->
           let (l1,l2) = projConsts (k-1) vs' in
           (it::l1, l2)

matchCmd cmd s =
  let [c] = String.toList cmd in
  let cs  = String.toList s in
  List.member c (cs ++ List.map Char.toLower cs)

-- transform commands

valToTransform (VList vs) = List.map valToTransformCmd vs

valToTransformCmd (VList (VBase (String k) :: vs)) =
  case (k, vs) of
    ("rotate", [VConst n1, VConst n2, VConst n3]) -> Rot n1 n2 n3

strTransformCmd cmd = case cmd of
  Rot n1 n2 n3 ->
    let nums = List.map (toString << fst) [n1,n2,n3] in
    "rotate" ++ Utils.parens (Utils.spaces nums)


{- old way of doing things with APath...

valToPath = Utils.spaces << valToPath_

valToPath_ vs =
  let pt (i,_) (j,_) = toString i ++ " " ++ toString j in
  case vs of
    [] -> []
    VBase (String cmd) :: vs' ->
      if | matchCmd cmd "Z" -> cmd :: valToPath_ vs'
         | matchCmd cmd "MLT" ->
             let ([sx,sy],vs'') = projConsts 2 vs' in
             cmd :: pt sx sy :: valToPath_ vs''
         | matchCmd cmd "HV" ->
             let ([i],vs'') = projConsts 1 vs' in
             cmd :: toString i :: valToPath_ vs''
         | matchCmd cmd "C" ->
             let ([x1,y1,x2,y2,x,y],vs'') = projConsts 6 vs' in
             let pts = String.join " , " [pt x1 y1, pt x2 y2, pt x y] in
             cmd :: pts :: valToPath_ vs''
         | matchCmd cmd "SQ" ->
             let ([x1,y1,x,y],vs'') = projConsts 4 vs' in
             let pts = String.join " , " [pt x1 y1, pt x y] in
             cmd :: pts :: valToPath_ vs''
         | matchCmd cmd "A" ->
             let (ns,vs'') = projConsts 7 vs' in
             let blah = Utils.spaces (List.map toString ns) in
             cmd :: blah :: valToPath_ vs'' -- not worrying about commas

-}


------------------------------------------------------------------------------

funcsSvg = [
  ("circle", Svg.circle)
  , ("ellipse", Svg.ellipse)
  , ("g", Svg.g)
  , ("line", Svg.line)
  , ("path", Svg.path)
  , ("polygon", Svg.polygon)
  , ("polyline", Svg.polyline)
  , ("rect", Svg.rect)
  , ("svg", Svg.svg)
  , ("text", Svg.text)
  , ("tspan", Svg.tspan)
  ]

funcsAttr = [
  ("cursor", A.cursor)
  , ("cx", A.cx)
  , ("cy", A.cy)
  , ("d", A.d)
  , ("draggable", HA.draggable) -- TODO figure this out
  , ("fill", A.fill)
  , ("font-family", A.fontFamily)
  , ("font-size", A.fontSize)
  , ("height", A.height)
  , ("opacity", A.opacity)
  , ("points", A.points)
  , ("r", A.r)
  , ("rx", A.rx)
  , ("ry", A.ry)
  , ("stroke", A.stroke)
  , ("stroke-width", A.strokeWidth)
  , ("strokeWidth", A.strokeWidth)
  , ("style", A.style)
  , ("transform", A.transform)
  , ("viewbox", A.viewBox)
  , ("viewBox", A.viewBox)
  , ("width", A.width)
  , ("x", A.x)
  , ("x1", A.x1)
  , ("x2", A.x2)
  , ("y", A.y)
  , ("y1", A.y1)
  , ("y2", A.y2)
  ]

find d s = Utils.find ("LangSvg.find: " ++ s) d s

attr = find funcsAttr
svg  = find funcsSvg


------------------------------------------------------------------------------

type alias ShapeKind = String
type alias NodeId = Int
type alias IndexedTree = Dict NodeId IndexedTreeNode
type alias Attr = (String, AVal)
type IndexedTreeNode
  = TextNode String
  | SvgNode ShapeKind (List Attr) (List NodeId)
type alias RootedIndexedTree = (NodeId, IndexedTree)

children n = case n of {TextNode _ -> []; SvgNode _ _ l -> l}

emptyTree : RootedIndexedTree
emptyTree = valToIndexedTree <| VList [VBase (String "svg"), VList [], VList []]

valToIndexedTree : Val -> RootedIndexedTree
valToIndexedTree v =
  let (nextId,tree) = valToIndexedTree_ v (1, Dict.empty) in
  let rootId = nextId - 1 in
  (rootId, tree)

valToIndexedTree_ v (nextId, d) = case v of

  VList [VBase (String "TEXT"), VBase (String s)] ->
    (1 + nextId, Dict.insert nextId (TextNode s) d)

  VList [VBase (String kind), VList vs1, VList vs2] ->
    let processChild vi (a_nextId, a_graph , a_children) =
      let (a_nextId',a_graph') = valToIndexedTree_ vi (a_nextId, a_graph) in
      let a_children'          = (a_nextId' - 1) :: a_children in
      (a_nextId', a_graph', a_children') in
    let (nextId',d',children) = List.foldl processChild (nextId,d,[]) vs2 in
    let node = SvgNode kind (List.map valToAttr vs1) (List.reverse children) in
    (1 + nextId', Dict.insert nextId' node d')

  _ -> Debug.crash ("LangSvg.valToIndexTree_: " ++ strVal v)

printIndexedTree : Val -> String
printIndexedTree = valToIndexedTree >> snd >> strEdges

strEdges : IndexedTree -> String
strEdges =
     Dict.toList
  >> List.map (\(i,n) ->
       let l = List.map toString (children n) in
       toString i ++ " " ++ Utils.braces (Utils.spaces l))
  >> Utils.lines


------------------------------------------------------------------------------
-- Printing to SVG format

printSvg : RootedIndexedTree -> String
printSvg (rootId, tree) = printNode 0 tree rootId

printNode k slate i =
  case Utils.justGet i slate of
    TextNode s -> s
    SvgNode kind l1 [] ->
      let l1' = addAttrs kind l1 in
      Utils.delimit "<" ">" (kind ++ printAttrs l1') ++
      Utils.delimit "</" ">" kind
    SvgNode kind l1 l2 ->
      let l1' = addAttrs kind l1 in
      Utils.delimit "<" ">" (kind ++ printAttrs l1') ++ "\n" ++
      printNodes (k+1) slate l2 ++ "\n" ++
      tab k ++ Utils.delimit "</" ">" kind

printNodes k slate =
  Utils.lines << List.map ((++) (tab k) << printNode k slate)

printAttrs l = case l of
  [] -> ""
  _  -> " " ++ Utils.spaces (List.map printAttr l)

printAttr (k,v) =
  k ++ "=" ++ Utils.delimit "'" "'" (strAVal v)

addAttrs kind attrs =
  if | kind == "svg" -> ("xmlns", AString "http://www.w3.org/2000/svg") :: attrs
     | otherwise     -> attrs


------------------------------------------------------------------------------
-- Zones

type alias Zone = String

-- NOTE: would like to use only the following definition, but datatypes
-- aren't comparable... so using Strings for storing in dictionaries, but
-- using the following for pattern-matching purposes

type RealZone = Z String | ZPoint Int | ZEdge Int

addi s i = s ++ toString i

realZoneOf s =
  Maybe.withDefault (Z s) (toZPoint s `Utils.plusMaybe` toZEdge s)

toZPoint s =
  Utils.mapMaybe
    (ZPoint << Utils.fromOk_ << String.toInt)
    (Utils.munchString "Point" s)

toZEdge s =
  Utils.mapMaybe
    (ZEdge << Utils.fromOk_ << String.toInt)
    (Utils.munchString "Edge" s)

-- TODO perhaps define Interface callbacks here

zones = [
    ("svg", [])
  , ("circle",
      [ ("Interior", ["cx", "cy"])
      , ("Edge", ["r"])
      ])
  , ("ellipse",
      [ ("Interior", ["cx", "cy"])
      , ("Edge", ["rx", "ry"])
      ])
  , ("rect",
      [ ("Interior", ["x", "y"])
      , ("TopLeftCorner", ["x", "y", "width", "height"])
      , ("TopRightCorner", ["y", "width", "height"])
      , ("BotRightCorner", ["width", "height"])
      , ("BotLeftCorner", ["x", "width", "height"])
      , ("LeftEdge", ["x", "width"])
      , ("TopEdge", ["y", "height"])
      , ("RightEdge", ["width"])
      , ("BotEdge", ["height"])
      ])
  , ("line",
      [ ("Point1", ["x1", "y1"])
      , ("Point2", ["x2", "y2"])
      , ("Edge", ["x1", "y1", "x2", "y2"])
      ])
  -- TODO
  , ("g", [])
  , ("text", [])
  , ("tspan", [])

  -- symptom of the Sync.Dict0 type. see Sync.nodeToAttrLocs_.
  , ("DUMMYTEXT", [])

  -- NOTE: these are computed in Sync.getZones
  -- , ("polygon", [])
  -- , ("polyline", [])
  -- , ("path", [])
  ]

