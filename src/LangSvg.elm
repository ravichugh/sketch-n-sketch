module LangSvg exposing
  ( attr
  , NodeId, ShapeKind
  , AVal, AVal_(..), PathCounts, PathCmd(..), TransformCmd(..)
  , Attr, IndexedTree, RootedIndexedTree, IndexedTreeNode, IndexedTreeNode_(..)
  , NodeInfo, foldSlateNodeInfo
  , maxColorNum, maxStrokeWidthNum
  , dummySvgNode
  , valToIndexedTree
  , printSvg
  , compileAttr, compileAttrs, desugarShapeAttrs -- TODO remove in favor of compileSvg
  , buildSvgSimple
  , strAVal
  , aNum, aPoints, aTransform
  , toNum, toColorNum, toTransformRot, toPath
  , valsToPath2
  , findNumishAttr, findAVal
  , getPolyPoints, getPolyPoint, getPtCount
  , pathIndexPoints, getPathPoint
  , maybeFindBounds, maybeFindBlobId
  , justGetSvgNode
  , resolveToIndexedTree, resolveToMovieCount, fetchEverything
  )

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
import Regex

import ColorNum

import Lang exposing (..)
import Eval
import Utils
import Either exposing (Either(..))

import ImpureGoodies

------------------------------------------------------------------------------

attr = VirtualDom.attribute

expectedButGot x s      = Err <| expectedButGotStr x s
expectedButGotCrash x s = Debug.crash <| expectedButGotStr x s
expectedButGotStr x s   = "expected " ++ x ++ ", but got: " ++ s


------------------------------------------------------------------------------
-- SVG Attribute Values

type alias WithVal a =
  { interpreted : a
  , val : Val
  }

type alias AVal = WithVal AVal_

type AVal_
  = ANum NumTr
  | AString String
  | APoints (List Point)
  | ARgba Rgba
  | AColorNum (NumTr, Maybe NumTr) -- Utils.numToColor [0,500), and opacity
  | APath2 (List PathCmd, PathCounts)
  | ATransform (List TransformCmd)
  | ABounds (NumTr, NumTr, NumTr, NumTr)

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
  | Scale NumTr NumTr
  | Trans NumTr NumTr

type alias PathCounts = {numPoints : Int}

type alias Cmd = String -- single uppercase/lowercase letter

type alias IdPoint = (Maybe Int, Point)


-- Max Attribute Values for Shape Widget Sliders --

maxColorNum = 500
maxStrokeWidthNum = 20


------------------------------------------------------------------------------
-- RootedIndexedTree (a.k.a. "Slate"): tree representation of SVG Canvas Value

type alias ShapeKind = String
type alias NodeId = Int
type alias Attr = (String, AVal)

type alias RootedIndexedTree = (NodeId, IndexedTree)
type alias IndexedTree = Dict NodeId IndexedTreeNode

type alias IndexedTreeNode = WithVal IndexedTreeNode_

type IndexedTreeNode_
  = TextNode String
  | SvgNode ShapeKind (List Attr) (List NodeId)


------------------------------------------------------------------------------
-- Convert Raw Value to SVG Slate

valToIndexedTree : Val -> Result String RootedIndexedTree
valToIndexedTree v =
  valToIndexedTree_ v (1, Dict.empty)
  |> Result.map (\(nextId,tree) ->
      let rootId = nextId - 1 in
      (rootId, tree)
    )

valToIndexedTree_ : Val -> RootedIndexedTree -> Result String RootedIndexedTree
valToIndexedTree_ v (nextId, d) =
  let thunk () =
    case v.v_ of
      VList vs -> case List.map .v_ vs of

        [VBase (VString "TEXT"), VBase (VString s)] ->
          let node = { interpreted = TextNode s, val = v } in
          Ok (1 + nextId, Dict.insert nextId node d)

        [VBase (VString kind), VList vs1, VList vs2] ->
          let processChild vi acc =
            case acc of
              Err s -> acc
              Ok (a_nextId, a_graph , a_children) ->
                valToIndexedTree_ vi (a_nextId, a_graph)
                |> Result.map (\(a_nextId_,a_graph_) ->
                    let a_children_ = (a_nextId_ - 1) :: a_children in
                    (a_nextId_, a_graph_, a_children_)
                  )
          in
          List.foldl processChild (Ok (nextId,d,[])) vs2
          |> Result.andThen (\(nextId_,d_,children) ->
              List.map valToAttr vs1
              |> Utils.projOk
              |> Result.map (\attrs ->
                let node =
                  { interpreted = SvgNode kind attrs (List.reverse children)
                  , val = v
                  }
                in
                (1 + nextId_, Dict.insert nextId_ node d_)
              )
            )

        _ -> expectedButGot "an SVG node" (strVal v)

      _ -> expectedButGot "an SVG node" (strVal v)
  in
  ImpureGoodies.crashToError thunk
  |> Utils.unwrapNestedResult


------------------------------------------------------------------------------
-- Convert Raw Value to SVG Attribute

valToAttr : Val -> Result String Attr
valToAttr v = case v.v_ of
  VList [v1,v2] -> case (v1.v_, v2.v_) of
    (VBase (VString k), v2_) ->
      let avRes =
        case (k, v2_) of
          ("points", VList vs)    -> vs |> List.map valToPoint |> Utils.projOk |> Result.map APoints

          ("fill"  , VList [v1,v2,v3,v4]) -> valsToRgba [v1,v2,v3,v4] |> Result.map ARgba
          ("stroke", VList [v1,v2,v3,v4]) -> valsToRgba [v1,v2,v3,v4] |> Result.map ARgba

          ("fill",   VConst _ it) -> Ok <| AColorNum (it, Nothing)
          ("stroke", VConst _ it) -> Ok <| AColorNum (it, Nothing)

          ("fill",   VList [v1,v2]) ->
            case (v1.v_, v2.v_) of
              (VConst _ it1, VConst _ it2) -> Ok  <| AColorNum (it1, Just it2)
              _                            -> Err <| "bad fill: " ++ strVal v2
          ("stroke", VList [v1,v2]) ->
            case (v1.v_, v2.v_) of
              (VConst _ it1, VConst _ it2) -> Ok  <| AColorNum (it1, Just it2)
              _                            -> Err <| "bad stroke: " ++ strVal v2

          ("d", VList vs)         -> valsToPath2 vs |> Result.map APath2

          ("transform", VList vs) -> valsToTransform vs |> Result.map ATransform

          ("BOUNDS", VList vs)    -> valToBounds vs |> Result.map ABounds

          (_, VConst _ it)        -> Ok <| ANum it
          (_, VBase (VString s))  -> Ok <| AString s

          _                       -> Err <| "bad SVG attribute value for " ++ k ++ ": " ++ strVal v2
      in
      avRes |> Result.map (\av -> (k, { interpreted = av, val = v2 }))

    _ ->
      Err <| "malformed attribute pair: " ++ strVal v

  _ ->
    Err <| "malformed attribute list, bad element: " ++ strVal v


-- Points --

valToPoint : Val -> Result String Point
valToPoint v = case v.v_ of
  VList vs -> case List.map .v_ vs of
    [VConst _ x, VConst _ y] -> Ok (x,y)
    _                        -> expectedButGot "a point" (strVal v)
  _                          -> expectedButGot "a point" (strVal v)

strPoint : Point -> String
strPoint (x_,y_) =
  let (x,y) = Utils.unwrap2 <| List.map Tuple.first [x_,y_] in
  toString x ++ "," ++ toString y


-- RGBA --

valsToRgba : List Val -> Result String Rgba
valsToRgba vs = case List.map .v_ vs of
  [VConst _ r, VConst _  g, VConst _ b, VConst _ a] -> Ok (r,g,b,a)
  _                                                 -> expectedButGot "rgba" <| "[" ++ String.join ", " (List.map strVal vs) ++ "]"

strRgba : Rgba -> String
strRgba (r_,g_,b_,a_) =
  strRgba_ (List.map Tuple.first [r_,g_,b_,a_])

strRgba_ rgba =
  "rgba" ++ Utils.parens (Utils.commas (List.map toString rgba))


-- Path Commands --

-- https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths
-- http://www.w3schools.com/svg/svg_path.asp
--
-- NOTES:
--  . using different representation of points in d than in points
--    to make it less verbose and easier to copy-and-paste raw SVG examples
--  . looks like commas are optional

valsToPath2 : List Val -> Result String (List PathCmd, PathCounts)
valsToPath2 = valsToPath2_ {numPoints = 0}

valsToPath2_ : PathCounts -> List Val -> Result String (List PathCmd, PathCounts)
valsToPath2_ counts vs = case vs of
  []     -> Ok ([], counts)
  v::vs_ -> case v.v_ of
    VBase (VString cmd) ->
      if matchCmd cmd "Z" then
        valsToPath2_ counts vs_ |> Result.map (\rest -> CmdZ cmd +++ rest)
      else if matchCmd cmd "MLT" then
        let ((x,y),vs__) = Tuple.mapFirst Utils.unwrap2 <| projConsts 2 vs_ in
        let (counts_,pt) = Tuple.mapSecond Utils.unwrap1 <| addIdPoints cmd counts [(x,y)] in
        valsToPath2_ counts_ vs__ |> Result.map (\rest -> CmdMLT cmd pt +++ rest)
      else if matchCmd cmd "HV" then
        let (i,vs__) = Tuple.mapFirst Utils.unwrap1 <| projConsts 1 vs_ in
        valsToPath2_ counts vs__ |> Result.map (\rest -> CmdHV cmd i +++ rest)
      else if matchCmd cmd "C" then
        let ((x1,y1,x2,y2,x,y),vs__) = Tuple.mapFirst Utils.unwrap6 <| projConsts 6 vs_ in
        let (counts_,(pt1,pt2,pt3)) = Tuple.mapSecond Utils.unwrap3 <| addIdPoints cmd counts [(x1,y1),(x2,y2),(x,y)] in
        valsToPath2_ counts_ vs__ |> Result.map (\rest -> CmdC cmd pt1 pt2 pt3 +++ rest)
      else if matchCmd cmd "SQ" then
        let ((x1,y1,x,y),vs__) = Tuple.mapFirst Utils.unwrap4 <| projConsts 4 vs_ in
        let (counts_,(pt1,pt2)) = Tuple.mapSecond Utils.unwrap2 <| addIdPoints cmd counts [(x1,y1),(x,y)] in
        valsToPath2_ counts_ vs__ |> Result.map (\rest -> CmdSQ cmd pt1 pt2 +++ rest)
      else if matchCmd cmd "A" then
        let ((rx,ry,axis,flag,sweep,x,y),vs__) = Tuple.mapFirst Utils.unwrap7 <| projConsts 7 vs_ in
        let (counts_,pt) = Tuple.mapSecond Utils.unwrap1 <| addIdPoints cmd counts [(x,y)] in
        valsToPath2_ counts_ vs__ |> Result.map (\rest -> CmdA cmd rx ry axis flag sweep pt +++ rest)
      else
        Err "valsToPath2_"
    _ ->
      Err "valsToPath2_"

(+++) x (xs,stuff) = (x::xs, stuff)

addIdPoints : Cmd -> PathCounts -> List Point -> (PathCounts, List IdPoint)
addIdPoints cmd counts pts =
  let c = Utils.unwrap1 <| String.toList cmd in
  if Char.isLower c then
    (counts, List.map ((,) Nothing) pts)
  else if Char.isUpper c then
    let (counts_,l) =
      List.foldl (\pt (acc1,acc2) ->
        let nextId = 1 + acc1.numPoints in
        let acc1_  = {acc1 | numPoints = nextId} in
        let acc2_  = (Just nextId, pt) :: acc2 in
        (acc1_, acc2_)) (counts, []) pts
    in
    (counts_, List.reverse l)
  else
    Debug.crash "addIdPoints"

projConsts k vs =
  case (k == 0, vs) of
    (True, _)       -> ([], vs)
    (False, v::vs_) ->
      case v.v_ of
        VConst _ it ->
          let (l1,l2) = projConsts (k-1) vs_ in
          (it::l1, l2)
        _ ->
          Debug.crash "projConsts"
    _ ->
      Debug.crash "projConsts"

matchCmd cmd s =
  let c  = Utils.unwrap1 <| String.toList cmd in
  let cs = String.toList s in
  List.member c (cs ++ List.map Char.toLower cs)


strAPathCmds : List PathCmd -> String
strAPathCmds =
  let strPt (_,(it,jt)) = toString (Tuple.first it) ++ " " ++ toString (Tuple.first jt) in
  -- TODO turn this into a debug mode for printing traces
  -- let strPt (_,(it,jt)) = strVal_ True (VConst it) ++ " " ++ strVal_ True (VConst jt) in
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


-- Transform Commands --

valsToTransform : List Val -> Result String (List TransformCmd)
valsToTransform = List.map valToTransformCmd >> Utils.projOk

valToTransformCmd : Val -> Result String TransformCmd
valToTransformCmd v = case v.v_ of
  VList vs1 -> case List.map .v_ vs1 of
    (VBase (VString k) :: vs) ->
      case (k, vs) of
        ("rotate",    [VConst _ n1, VConst _ n2, VConst _ n3]) -> Ok <| Rot n1 n2 n3
        ("scale",     [VConst _ n1, VConst _ n2])              -> Ok <| Scale n1 n2
        ("translate", [VConst _ n1, VConst _ n2])              -> Ok <| Trans n1 n2
        _ -> expectedButGot "a transform command" (strVal v)
    _     -> expectedButGot "a transform command" (strVal v)
  _       -> expectedButGot "a transform command" (strVal v)

strTransformCmd : TransformCmd -> String
strTransformCmd cmd = case cmd of
  Rot n1 n2 n3 ->
    let nums = List.map (toString << Tuple.first) [n1,n2,n3] in
    "rotate" ++ Utils.parens (Utils.spaces nums)
  Scale n1 n2 ->
    let nums = List.map (toString << Tuple.first) [n1,n2] in
    "scale" ++ Utils.parens (Utils.spaces nums)
  Trans n1 n2 ->
    let nums = List.map (toString << Tuple.first) [n1,n2] in
    "translate" ++ Utils.parens (Utils.spaces nums)


-- Bounds --

valToBounds vs = case List.map .v_ vs of
  [VConst _ a, VConst _ b, VConst _ c, VConst _ d] -> Ok (a,b,c,d)
  _                                                -> expectedButGot "bounds" <| "[" ++ String.join ", " (List.map strVal vs) ++ "]"

strBounds (left,top,right,bot) =
  Utils.spaces (List.map (toString << Tuple.first) [left,top,right,bot])


------------------------------------------------------------------------------
-- Compiling to SVG (Text Format)

printSvg : Bool -> RootedIndexedTree -> String
printSvg showGhosts (rootId, tree) =
  let s = printNode showGhosts 0 tree rootId in
  Regex.replace Regex.All (Regex.regex "[ ]+\\n") (\_ -> "") s

printNode showGhosts k slate i =
  case Utils.justGet i slate |> .interpreted of
    TextNode s -> s
    SvgNode kind_ l1_ l2 ->
      let (kind,l1) = desugarShapeAttrs kind_ l1_ in
      case (showGhosts, Utils.maybeRemoveFirst "HIDDEN" l1) of
        (False, Just _) -> ""
        _ ->
          if l2 == [] then
            let l1_ = addAttrs kind (removeSpecialAttrs l1) in
            Utils.delimit "<" ">" (kind ++ printAttrs l1_) ++
            Utils.delimit "</" ">" kind
          else
            let l1_ = addAttrs kind (removeSpecialAttrs l1) in
            Utils.delimit "<" ">" (kind ++ printAttrs l1_) ++ "\n" ++
            printNodes showGhosts (k+1) slate l2 ++ "\n" ++
            tab k ++ Utils.delimit "</" ">" kind

printNodes showGhosts k slate =
  Utils.lines << List.map ((++) (tab k) << printNode showGhosts k slate)

printAttrs l = case l of
  [] -> ""
  _  -> " " ++ Utils.spaces (List.map printAttr l)

printAttr (k,v) =
  k ++ "=" ++ Utils.delimit "'" "'" (strAVal v)

addAttrs kind attrs =
  if kind == "svg"
    then ("xmlns", aString "http://www.w3.org/2000/svg") :: attrs
    else attrs

specialAttrs = ["HIDDEN", "ZONES"]
  -- not removing 'BLOB' and 'BOUNDS' since they are useful
  -- for understanding and debugging

removeSpecialAttrs =
  List.filter (\(s,_) -> not (List.member s specialAttrs))

desugarShapeAttrs shape0 attrs0 =
  let mkNum n = aNum (n, dummyTrace) in
  Maybe.withDefault (shape0, attrs0) <|
    case shape0 of
      "BOX" ->
        Utils.mapMaybe (\(left, top, right, bot, restOfAttrs) ->
          let newAttrs =
             [ ("x", mkNum left)
             , ("y", mkNum top)
             , ("width", mkNum (right - left))
             , ("height", mkNum (bot - top))
             ]
          in ("rect", newAttrs ++ restOfAttrs)
        ) (getBoundsAttrs attrs0)
      "OVAL" ->
        Utils.mapMaybe (\(left, top, right, bot, restOfAttrs) ->
          let newAttrs =
             [ ("cx", mkNum (left + (right - left) / 2))
             , ("cy", mkNum (top + (bot - top) / 2))
             , ("rx", mkNum ((right - left) / 2))
             , ("ry", mkNum ((bot - top) / 2))
             ]
          in ("ellipse", newAttrs ++ restOfAttrs)
        ) (getBoundsAttrs attrs0)
      _ ->
        Nothing

getBoundsAttrs attrs0 =
  Utils.maybeRemoveFirst "LEFT"  attrs0 |> Maybe.andThen (\(vL,attrs1) ->
  Utils.maybeRemoveFirst "RIGHT" attrs1 |> Maybe.andThen (\(vR,attrs2) ->
  Utils.maybeRemoveFirst "TOP"   attrs2 |> Maybe.andThen (\(vT,attrs3) ->
  Utils.maybeRemoveFirst "BOT"   attrs3 |> Maybe.andThen (\(vB,attrs4) ->
    case (vL.interpreted, vT.interpreted, vR.interpreted, vB.interpreted) of
      (ANum (left,_), ANum (top,_), ANum (right,_), ANum (bot,_)) ->
        Just (left, top, right, bot, attrs4)
      _ -> Nothing
  ))))

strAVal : AVal -> String
strAVal a = case a.interpreted of
  AString s -> s
  ANum it   -> toString (Tuple.first it)
  APoints l -> Utils.spaces (List.map strPoint l)
  ARgba tup -> strRgba tup
  APath2 p  -> strAPathCmds (Tuple.first p)
  ATransform l -> Utils.spaces (List.map strTransformCmd l)
  ABounds bounds -> strBounds bounds
  AColorNum (n, Nothing) ->
    -- slight optimization:
    strRgba_ (ColorNum.convert (Tuple.first n))
  AColorNum (n, Just (opacity, _)) ->
    let (r,g,b) = Utils.numToColor maxColorNum (Tuple.first n) in
    strRgba_ [toFloat r, toFloat g, toFloat b, opacity]


------------------------------------------------------------------------------
-- Compiling to SVG (DOM)

compileAttrs : List Attr -> List (Svg.Attribute a)
compileAttrs = List.map (uncurry compileAttr)

compileAttr : String -> AVal -> Svg.Attribute a
compileAttr k v = (attr k) (strAVal v)


buildSvgSimple : RootedIndexedTree -> Svg.Svg a
buildSvgSimple (rootI, tree) =
  buildSvgSimple_ tree rootI

buildSvgSimple_ : IndexedTree -> NodeId -> Svg.Svg a
buildSvgSimple_ tree i  =
  case Utils.justGet_ ("LangSvg.buildSvgSimple_ " ++ toString i) i tree |> .interpreted of
    TextNode text -> VirtualDom.text text
    SvgNode shape attrs childIndices ->
      case Utils.maybeRemoveFirst "HIDDEN" attrs of
        Just _ -> Svg.svg [] []
        _ ->
          let attrs_ =
            case Utils.maybeRemoveFirst "ZONES" attrs of
              Nothing        -> attrs
              Just (_, rest) -> rest
          in
          let children = List.map (buildSvgSimple_ tree) childIndices in
          let (rawKind, rawAttrs) = desugarShapeAttrs shape attrs_ in
          Svg.node rawKind (compileAttrs rawAttrs) children


------------------------------------------------------------------------------
-- Misc AVal Helpers

toNum a = case a.interpreted of
  ANum nt -> nt
  _       -> expectedButGotCrash "a number" (strAVal a)

toColorNum a = case a.interpreted of
  AColorNum nt -> nt
  _            -> expectedButGotCrash "a color number" (strAVal a)

toNumIsh a = case a.interpreted of
  ANum nt           -> nt
  AColorNum (nt, _) -> nt
  _       -> expectedButGotCrash "a number or color number" (strAVal a)

toPoints a = case a.interpreted of
  APoints pts -> pts
  _           -> expectedButGotCrash "a list of points" (strAVal a)

toPath : AVal -> (List PathCmd, PathCounts)
toPath a = case a.interpreted of
  APath2 p -> p
  _        -> expectedButGotCrash "path commands" (strAVal a)

toTransformRot a = case a.interpreted of
  ATransform [Rot n1 n2 n3] -> (n1,n2,n3)
  _                         -> expectedButGotCrash "a rotation transform" (strAVal a)


-- Avoid using these going foward: no way to determine provenance.
aVal av_      = { interpreted = av_, val = { v_ = VList [], provenance = dummyProvenance, parents = Parents [] } }
aNum          = aVal << ANum
aString       = aVal << AString
aTransform    = aVal << ATransform
aPoints       = aVal << APoints


------------------------------------------------------------------------------
-- Misc Attribute Helpers


findNumishAttr : String -> List Attr -> NumTr
findNumishAttr attr attrs = toNumIsh <| findAVal attr attrs


findAVal : String -> List Attr -> AVal
findAVal attr attrs =
  case Utils.maybeFind attr attrs of
    Just aval -> aval
    Nothing   -> Debug.crash <| "findAVal: " ++ attr


getPtCount attrs = List.length (getPolyPoints attrs)

getPolyPoint attrs i = Utils.geti i (getPolyPoints attrs)

getPolyPoints attrs =
  case Utils.maybeFind "points" attrs of
    Just aval -> toPoints aval
    Nothing   -> Debug.crash "getPolyPoints"


getPathPoint attrs i =
  case Utils.maybeFind i (pathIndexPoints attrs) of
    Just pt -> pt
    Nothing -> Debug.crash "getPathPoint"


-- Return list of (i, pt).
-- (Includes control points.)
pathIndexPoints : List Attr -> List (Int, Point)
pathIndexPoints nodeAttrs =
  let cmds =
    Utils.find ("pathPoints nodeAttrs looking for \"d\" in " ++ (toString nodeAttrs)) nodeAttrs "d"
    |> toPath
    |> Tuple.first
  in
  let pts =
    cmds
    |> List.concatMap
        (\cmd -> case cmd of
          CmdZ   s              -> []
          CmdMLT s pt           -> [pt]
          CmdHV  s n            -> []
          CmdC   s pt1 pt2 pt3  -> [pt1, pt2, pt3]
          CmdSQ  s pt1 pt2      -> [pt1, pt2]
          CmdA   s a b c d e pt -> [pt]
        )
    |> List.filterMap
        (\(maybeIndex, pt) -> case maybeIndex of
          Nothing -> Nothing
          Just i  -> Just (i, pt)
        )
  in
  pts


maybeFindBlobId l =
  case Utils.maybeFind "BLOB" l of
    Nothing -> Nothing
    Just av ->
      case av.interpreted of
        AString sBlobId -> Just (Utils.parseInt sBlobId)
        _               -> Nothing


maybeFindBounds l =
  case Utils.maybeFind "BOUNDS" l of
    Nothing -> Nothing
    Just av ->
      let roundBounds = True in
      case (av.interpreted, roundBounds) of
        (ABounds bounds, False) -> Just bounds
        (ABounds (a,b,c,d), True) ->
          let f = Tuple.mapFirst (toFloat << round) in
          Just (f a, f b, f c, f d)
        _ ->
          Nothing


justGetSvgNode : String -> NodeId -> RootedIndexedTree -> (ShapeKind, List Attr)
justGetSvgNode cap nodeId (_, indexedTree) =
  case Utils.justGet_ cap nodeId indexedTree |> .interpreted of
    SvgNode kind attrs _ -> (kind, attrs)
    TextNode _           -> Debug.crash (cap ++ ": TextNode ?")


dummySvgNode =
  let zero = aNum (0, dummyTrace) in
  SvgNode "circle" (List.map (\k -> (k, zero)) ["cx","cy","r"]) []


-- dummySvgVal =
--   let zero = vConst (0, dummyTrace) in
--   let attrs = vList <| List.map (\k -> vList [vStr k, zero]) ["cx","cy","r"] in
--   let children = vList [] in
--   vList [vStr "circle", attrs, children]


------------------------------------------------------------------------------
-- Slate Traversal

foldSlate : RootedIndexedTree -> a -> (NodeId -> IndexedTreeNode -> a -> a) -> a
foldSlate (rootId, dict) acc f =
  let
 -- foldNode : NodeId -> a -> a
    foldNode i acc =
      let node = Utils.justGet_ "foldSlate" i dict in
      case node.interpreted of
        TextNode _       -> f i node acc
        SvgNode _ _ kids -> f i node (List.foldl foldNode acc kids)
  in
  foldNode rootId acc

type alias NodeInfo =
  Either
    (NodeId, String)                -- for TextNodes
    (NodeId, ShapeKind, List Attr)  -- for SvgNodes

foldSlateNodeInfo : RootedIndexedTree -> a -> (NodeInfo -> a -> a) -> a
foldSlateNodeInfo slate acc f =
  foldSlate slate acc <| \i node ->
    case node.interpreted of
      TextNode s           -> f (Left (i, s))
      SvgNode kind attrs _ -> f (Right (i, kind, attrs))


------------------------------------------------------------------------------
-- Little Animations

-- [
--   slideCount
--   (slideNumber -> [ ; Slide Val
--     slideMovieCount
--     (slideMovieNumber -> [ ; Movie Val
--       "Static"
--       (slideNumber -> movieNumber -> SVG array structure) ; Frame Val
--     ])
--   ])
-- ]
-- or
-- [
--   slideCount
--   (slideNumber -> [ ; Slide Val
--     slideMovieCount
--     (slideMovieNumber -> [ ; Movie Val
--       "Dynamic"
--       movieDuration
--       (slideNumber -> movieNumber -> movieTime -> SVG array structure) ; Frame Val
--       continueBool
--     ])
--   ])
-- ]

-- TODO use this to reduce clutter
type alias AnimationKey = (Int, Int, Float)

-- HACK: see LocEqn.traceToLocEquation...
-- TODO: streamline Trace, LocEquation, etc.
vNumFrozen n = { v_ = VConst Nothing (n, TrLoc (-999, frozen, toString n)), provenance = Provenance [] (eConstDummyLoc0 n) [], parents = Parents [] }
vIntFrozen i = vNumFrozen (toFloat i)

resolveToMovieCount : Int -> Val -> Result String Int
resolveToMovieCount slideNumber val =
  fetchSlideVal slideNumber val
  |> Result.map fetchMovieCount

resolveToMovieFrameVal : Int -> Int -> Float -> Val -> Result String Val
resolveToMovieFrameVal slideNumber movieNumber movieTime val =
  fetchEverything_ slideNumber movieNumber movieTime val
  |> Result.map (\(_, _, _, _, movieFrameVal) -> movieFrameVal)

resolveToIndexedTree : Int -> Int -> Float -> Val -> Result String RootedIndexedTree
resolveToIndexedTree slideNumber movieNumber movieTime val =
  fetchEverything slideNumber movieNumber movieTime val
  |> Result.map (\(_, _, _, _, indexedTree) -> indexedTree)

fetchEverything_ : Int -> Int -> Float -> Val -> Result String (Int, Int, Float, Bool, Val)
fetchEverything_ slideNumber movieNumber movieTime val =
  let slideCount = fetchSlideCount val in
  fetchSlideVal slideNumber val |>
  Result.andThen (\slideVal ->
    let movieCount = fetchMovieCount slideVal in
    fetchMovieVal movieNumber slideVal |>
    Result.andThen (\movieVal ->
      let (movieDuration, continue) = fetchMovieDurationAndContinueBool movieVal in
      fetchMovieFrameVal slideNumber movieNumber movieTime movieVal
      |> Result.map (\movieFrameVal ->
        (slideCount, movieCount, movieDuration, continue, movieFrameVal)
      )
    )
  )

fetchEverything : Int -> Int -> Float -> Val -> Result String (Int, Int, Float, Bool, RootedIndexedTree)
fetchEverything slideNumber movieNumber movieTime val =
  fetchEverything_ slideNumber movieNumber movieTime val |>
  Result.andThen (\(slideCount, movieCount, movieDuration, continue, movieVal) ->
    valToIndexedTree movieVal
    |> Result.map (\indexedTree -> (slideCount, movieCount, movieDuration, continue, indexedTree))
  )

fetchSlideCount : Val -> Int
fetchSlideCount val =
  case unwrapVList val of
    Just [VConst _ (slideCount, _), _] -> round slideCount
    _ -> 1 -- Program returned a plain SVG array structure...we hope.

fetchMovieCount : Val -> Int
fetchMovieCount slideVal =
  case unwrapVList slideVal of
    Just [VConst _ (movieCount, _), _] -> round movieCount
    _ -> 1 -- Program returned a plain SVG array structure...we hope.

fetchSlideVal : Int -> Val -> Result String Val
fetchSlideVal slideNumber val =
  case unwrapVList val of
    Just [VConst _ (slideCount, _), VClosure _ [pat] fexp fenv] ->
      -- Program returned the slide count and a
      -- function from slideNumber -> SVG array structure.
      case pat.val.p__ of -- Find that function's argument name
        PVar _ argumentName _ ->
          -- Bind the slide number to the function's argument.
          let fenv_ = (argumentName, vIntFrozen slideNumber) :: fenv in
          Eval.doEval fenv_ fexp
          |> Result.map (\((returnVal, _), _) -> returnVal)
        _ -> Err ("expected slide function to take a single argument, got " ++ (toString pat.val.p__))
    _ -> Ok val -- Program returned a plain SVG array structure...we hope.

-- This is nasty b/c a two-arg function is really a function that returns a function...
fetchMovieVal : Int -> Val -> Result String Val
fetchMovieVal movieNumber slideVal =
  case unwrapVList slideVal of
    Just [VConst _ (movieCount, _), VClosure _ [pat] fexp fenv] ->
      case pat.val.p__ of -- Find the function's argument name
        PVar _ movieNumberArgumentName _ ->
          let fenv_ = (movieNumberArgumentName, vIntFrozen movieNumber) :: fenv in
          Eval.doEval fenv_ fexp
          |> Result.map (\((returnVal, _), _) -> returnVal)
        _ -> Err ("expected movie function to take a single argument, got " ++ (toString pat.val.p__))
    _ -> Ok slideVal -- Program returned a plain SVG array structure...we hope.

fetchMovieDurationAndContinueBool : Val -> (Float, Bool)
fetchMovieDurationAndContinueBool movieVal =
  case unwrapVList movieVal of
    Just [VBase (VString "Static"), VClosure _ _ _ _] ->
      (0.0, False)
    Just [VBase (VString "Dynamic"), VConst _ (movieDuration, _), VClosure _ _ _ _, VBase (VBool continue)] ->
      (movieDuration, continue)
    _ ->
      (0.0, False) -- Program returned a plain SVG array structure...we hope.

-- This is nasty b/c a two-arg function is really a function that returns a function...
fetchMovieFrameVal : Int -> Int -> Float -> Val -> Result String Val
fetchMovieFrameVal slideNumber movieNumber movieTime movieVal =
  case unwrapVList movieVal of
    -- [
    --   "Static"
    --   (slideNumber -> movieNumber -> SVG array structure) ; Frame Val
    -- ]
    Just [VBase (VString "Static"), VClosure _ _ _ _] ->
      let getFrameValClosure = movieVal |> vListToVals "fetchMovieFrameVal1" |> Utils.geti 2 in
      Eval.doEval [("getFrameVal", getFrameValClosure)] (eCall "getFrameVal" [eConstDummyLoc (toFloat slideNumber), eConstDummyLoc (toFloat movieNumber)])
      |> Result.map (\((returnVal, _), _) -> returnVal)

    -- [
    --   "Dynamic"
    --   movieDuration
    --   (slideNumber -> movieNumber -> movieTime -> SVG array structure) ; Frame Val
    --   continueBool
    -- ]
    Just [VBase (VString "Dynamic"), VConst _ (movieDuration, _), VClosure _ _ _ _, VBase (VBool _)] ->
      let getFrameValClosure = movieVal |> vListToVals "fetchMovieFrameVal2" |> Utils.geti 3 in
      Eval.doEval [("getFrameVal", getFrameValClosure)] (eCall "getFrameVal" [eConstDummyLoc (toFloat slideNumber), eConstDummyLoc (toFloat movieNumber), eConstDummyLoc movieTime])
      |> Result.map (\((returnVal, _), _) -> returnVal)

    _ -> Ok movieVal -- Program returned a plain SVG array structure...we hope.
