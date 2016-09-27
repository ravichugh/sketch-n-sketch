module LangSvg
  ( attr
  , NodeId, ShapeKind
  , AVal, AVal_(..), PathCounts, PathCmd(..), TransformCmd(..)
  , Attr, IndexedTree, RootedIndexedTree, IndexedTreeNode(..)
  , NodeInfo, foldSlateNodeInfo
  , maxColorNum, maxStrokeWidthNum
  , emptyTree, dummySvgNode
  , valToIndexedTree
  , printSvg
  , compileAttr, compileAttrs, desugarShapeAttrs -- TODO remove in favor of compileSvg
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
  ) where

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


------------------------------------------------------------------------------

attr = VirtualDom.attribute

expectedButGot x s = crashWithMsg <| expectedButGotStr x s
expectedButGotStr x s = "expected " ++ x ++ ", but got: " ++ s


------------------------------------------------------------------------------
-- SVG Attribute Values

type alias AVal = { av_ : AVal_, vtrace : VTrace }

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
type IndexedTreeNode
  = TextNode String
  | SvgNode ShapeKind (List Attr) (List NodeId)

emptyTree : RootedIndexedTree
emptyTree =
  Utils.fromOk "LangSvg.emptyTree" <|
  valToIndexedTree <| vList [vBase (VString "svg"), vList [], vList []]


------------------------------------------------------------------------------
-- Convert Raw Value to SVG Slate

valToIndexedTree : Val -> Result String RootedIndexedTree
valToIndexedTree v =
  valToIndexedTree_ v (1, Dict.empty)
  |> Result.map (\(nextId,tree) ->
      let rootId = nextId - 1 in
      (rootId, tree)
    )

valToIndexedTree_ v (nextId, d) = case v.v_ of

  VList vs -> case List.map .v_ vs of

    [VBase (VString "TEXT"), VBase (VString s)] ->
      Ok (1 + nextId, Dict.insert nextId (TextNode s) d)

    [VBase (VString kind), VList vs1, VList vs2] ->
      let processChild vi acc =
        case acc of
          Err s -> acc
          Ok (a_nextId, a_graph , a_children) ->
            valToIndexedTree_ vi (a_nextId, a_graph)
            |> Result.map (\(a_nextId',a_graph') ->
                let a_children' = (a_nextId' - 1) :: a_children in
                (a_nextId', a_graph', a_children')
              )
      in
      List.foldl processChild (Ok (nextId,d,[])) vs2
      |> Result.map (\(nextId',d',children) ->
          let node = SvgNode kind (List.map valToAttr vs1) (List.reverse children) in
          (1 + nextId', Dict.insert nextId' node d')
        )

    _ -> Err <| "an SVG node" `expectedButGotStr` strVal v

  _ -> Err <| "an SVG node" `expectedButGotStr` strVal v


------------------------------------------------------------------------------
-- Convert Raw Value to SVG Attribute

valToAttr : Val -> Attr
valToAttr v = case v.v_ of
  VList [v1,v2] -> case (v1.v_, v2.v_) of
    (VBase (VString k), v2_) ->
     let (k',av_) =
      case (k, v2_) of
        ("points", VList vs)    -> (k, APoints <| List.map valToPoint vs)

        ("fill"  , VList [v1,v2,v3,v4]) -> (k, ARgba <| valsToRgba [v1,v2,v3,v4])
        ("stroke", VList [v1,v2,v3,v4]) -> (k, ARgba <| valsToRgba [v1,v2,v3,v4])

        ("fill",   VConst it) -> (k, AColorNum (it, Nothing))
        ("stroke", VConst it) -> (k, AColorNum (it, Nothing))

        ("fill",   VList [v1,v2]) ->
          case (v1.v_, v2.v_) of
            (VConst it1, VConst it2) -> (k, AColorNum (it1, Just it2))
            _                        -> Debug.crash "valToAttr: fill"
        ("stroke", VList [v1,v2]) ->
          case (v1.v_, v2.v_) of
            (VConst it1, VConst it2) -> (k, AColorNum (it1, Just it2))
            _                        -> Debug.crash "valToAttr: stroke"

        ("d", VList vs)         -> (k, APath2 (valsToPath2 vs))

        ("transform", VList vs) -> (k, ATransform (valsToTransform vs))

        ("BOUNDS", VList vs)    -> (k, ABounds <| valToBounds vs)

        (_, VConst it)          -> (k, ANum it)
        (_, VBase (VString s))  -> (k, AString s)

        _                       -> Debug.crash "valToAttr"
     in
     (k', AVal av_ v2.vtrace)
    _ ->
      Debug.crash "valToAttr"
  _ ->
    Debug.crash "valToAttr"


-- Points --

valToPoint : Val -> Point
valToPoint v = case v.v_ of
  VList vs -> case List.map .v_ vs of
    [VConst x, VConst y] -> (x,y)
    _                    -> "a point" `expectedButGot` strVal v
  _                      -> "a point" `expectedButGot` strVal v

strPoint : Point -> String
strPoint (x_,y_) =
  let (x,y) = Utils.unwrap2 <| List.map fst [x_,y_] in
  toString x ++ "," ++ toString y


-- RGBA --

valsToRgba : List Val -> Rgba
valsToRgba vs = case List.map .v_ vs of
  [VConst r, VConst g, VConst b, VConst a] -> (r,g,b,a)
  _                                        -> "rgba" `expectedButGot` strVal (vList vs)

strRgba : Rgba -> String
strRgba (r_,g_,b_,a_) =
  strRgba_ (List.map fst [r_,g_,b_,a_])

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

valsToPath2 : List Val -> (List PathCmd, PathCounts)
valsToPath2 = valsToPath2_ {numPoints = 0}

valsToPath2_ : PathCounts -> List Val -> (List PathCmd, PathCounts)
valsToPath2_ counts vs = case vs of
  []     -> ([], counts)
  v::vs' -> case v.v_ of
    VBase (VString cmd) ->
      if matchCmd cmd "Z" then
        CmdZ cmd +++ valsToPath2_ counts vs'
      else if matchCmd cmd "MLT" then
        let ((x,y),vs'') = Utils.mapFst Utils.unwrap2 <| projConsts 2 vs' in
        let (counts',pt) = Utils.mapSnd Utils.unwrap1 <| addIdPoints cmd counts [(x,y)] in
        CmdMLT cmd pt +++ valsToPath2_ counts' vs''
      else if matchCmd cmd "HV" then
        let (i,vs'') = Utils.mapFst Utils.unwrap1 <| projConsts 1 vs' in
        CmdHV cmd i +++ valsToPath2_ counts vs''
      else if matchCmd cmd "C" then
        let ((x1,y1,x2,y2,x,y),vs'') = Utils.mapFst Utils.unwrap6 <| projConsts 6 vs' in
        let (counts',(pt1,pt2,pt3)) = Utils.mapSnd Utils.unwrap3 <| addIdPoints cmd counts [(x1,y1),(x2,y2),(x,y)] in
        CmdC cmd pt1 pt2 pt3 +++ valsToPath2_ counts' vs''
      else if matchCmd cmd "SQ" then
        let ((x1,y1,x,y),vs'') = Utils.mapFst Utils.unwrap4 <| projConsts 4 vs' in
        let (counts',(pt1,pt2)) = Utils.mapSnd Utils.unwrap2 <| addIdPoints cmd counts [(x1,y1),(x,y)] in
        CmdSQ cmd pt1 pt2 +++ valsToPath2_ counts' vs''
      else if matchCmd cmd "A" then
        let ((rx,ry,axis,flag,sweep,x,y),vs'') = Utils.mapFst Utils.unwrap7 <| projConsts 7 vs' in
        let (counts',pt) = Utils.mapSnd Utils.unwrap1 <| addIdPoints cmd counts [(x,y)] in
        CmdA cmd rx ry axis flag sweep pt +++ valsToPath2_ counts' vs''
      else
        Debug.crash "valsToPath2_"
    _ ->
      Debug.crash "valsToPath2_"

(+++) x (xs,stuff) = (x::xs, stuff)

addIdPoints : Cmd -> PathCounts -> List Point -> (PathCounts, List IdPoint)
addIdPoints cmd counts pts =
  let c = Utils.unwrap1 <| String.toList cmd in
  if Char.isLower c then
    (counts, List.map ((,) Nothing) pts)
  else if Char.isUpper c then
    let (counts',l) =
      List.foldl (\pt (acc1,acc2) ->
        let nextId = 1 + acc1.numPoints in
        let acc1'  = {acc1 | numPoints = nextId} in
        let acc2'  = (Just nextId, pt) :: acc2 in
        (acc1', acc2')) (counts, []) pts
    in
    (counts', List.reverse l)
  else
    Debug.crash "addIdPoints"

projConsts k vs =
  case (k == 0, vs) of
    (True, _)       -> ([], vs)
    (False, v::vs') ->
      case v.v_ of
        VConst it ->
          let (l1,l2) = projConsts (k-1) vs' in
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
  let strPt (_,(it,jt)) = toString (fst it) ++ " " ++ toString (fst jt) in
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

valsToTransform : List Val -> List TransformCmd
valsToTransform = List.map valToTransformCmd

valToTransformCmd : Val -> TransformCmd
valToTransformCmd v = case v.v_ of
  VList vs1 -> case List.map .v_ vs1 of
    (VBase (VString k) :: vs) ->
      case (k, vs) of
        ("rotate",    [VConst n1, VConst n2, VConst n3]) -> Rot n1 n2 n3
        ("scale",     [VConst n1, VConst n2])            -> Scale n1 n2
        ("translate", [VConst n1, VConst n2])            -> Trans n1 n2
        _ -> "a transform command" `expectedButGot` strVal v
    _     -> "a transform command" `expectedButGot` strVal v
  _       -> "a transform command" `expectedButGot` strVal v

strTransformCmd : TransformCmd -> String
strTransformCmd cmd = case cmd of
  Rot n1 n2 n3 ->
    let nums = List.map (toString << fst) [n1,n2,n3] in
    "rotate" ++ Utils.parens (Utils.spaces nums)
  Scale n1 n2 ->
    let nums = List.map (toString << fst) [n1,n2] in
    "scale" ++ Utils.parens (Utils.spaces nums)
  Trans n1 n2 ->
    let nums = List.map (toString << fst) [n1,n2] in
    "translate" ++ Utils.parens (Utils.spaces nums)


-- Bounds --

valToBounds vs = case List.map .v_ vs of
  [VConst a, VConst b, VConst c, VConst d] -> (a,b,c,d)
  _                                        -> "bounds" `expectedButGot` strVal (vList vs)

strBounds (left,top,right,bot) =
  Utils.spaces (List.map (toString << fst) [left,top,right,bot])


------------------------------------------------------------------------------
-- Compiling to SVG (Text Format)

printSvg : Bool -> RootedIndexedTree -> String
printSvg showGhosts (rootId, tree) =
  let s = printNode showGhosts 0 tree rootId in
  Regex.replace Regex.All (Regex.regex "[ ]+\\n") (\_ -> "") s

printNode showGhosts k slate i =
  case Utils.justGet i slate of
    TextNode s -> s
    SvgNode kind_ l1_ l2 ->
      let (kind,l1) = desugarShapeAttrs kind_ l1_ in
      case (showGhosts, Utils.maybeRemoveFirst "HIDDEN" l1) of
        (False, Just _) -> ""
        _ ->
          if l2 == [] then
            let l1' = addAttrs kind (removeSpecialAttrs l1) in
            Utils.delimit "<" ">" (kind ++ printAttrs l1') ++
            Utils.delimit "</" ">" kind
          else
            let l1' = addAttrs kind (removeSpecialAttrs l1) in
            Utils.delimit "<" ">" (kind ++ printAttrs l1') ++ "\n" ++
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
  Utils.maybeRemoveFirst "LEFT"  attrs0 `Maybe.andThen` \(vL,attrs1) ->
  Utils.maybeRemoveFirst "RIGHT" attrs1 `Maybe.andThen` \(vR,attrs2) ->
  Utils.maybeRemoveFirst "TOP"   attrs2 `Maybe.andThen` \(vT,attrs3) ->
  Utils.maybeRemoveFirst "BOT"   attrs3 `Maybe.andThen` \(vB,attrs4) ->
    case (vL.av_, vT.av_, vR.av_, vB.av_) of
      (ANum (left,_), ANum (top,_), ANum (right,_), ANum (bot,_)) ->
        Just (left, top, right, bot, attrs4)
      _ -> Nothing

strAVal : AVal -> String
strAVal a = case a.av_ of
  AString s -> s
  ANum it   -> toString (fst it)
  APoints l -> Utils.spaces (List.map strPoint l)
  ARgba tup -> strRgba tup
  APath2 p  -> strAPathCmds (fst p)
  ATransform l -> Utils.spaces (List.map strTransformCmd l)
  ABounds bounds -> strBounds bounds
  AColorNum (n, Nothing) ->
    -- slight optimization:
    strRgba_ (ColorNum.convert (fst n))
  AColorNum (n, Just (opacity, _)) ->
    let (r,g,b) = Utils.numToColor maxColorNum (fst n) in
    strRgba_ [toFloat r, toFloat g, toFloat b, opacity]


------------------------------------------------------------------------------
-- Compiling to SVG (DOM)

compileAttrs : List Attr -> List Svg.Attribute
compileAttrs = List.map (uncurry compileAttr)

compileAttr : String -> AVal -> Svg.Attribute
compileAttr k v = (attr k) (strAVal v)

  -- TODO move rest of View.buildSvg here


------------------------------------------------------------------------------
-- Misc AVal Helpers

toNum a = case a.av_ of
  ANum nt -> nt
  _       -> "a number" `expectedButGot` strAVal a

toColorNum a = case a.av_ of
  AColorNum nt -> nt
  _            -> "a color number" `expectedButGot` strAVal a

toNumIsh a = case a.av_ of
  ANum nt           -> nt
  AColorNum (nt, _) -> nt
  _       -> "a number or color number" `expectedButGot` strAVal a

toPoints a = case a.av_ of
  APoints pts -> pts
  _           -> "a list of points" `expectedButGot` strAVal a

toPath : AVal -> (List PathCmd, PathCounts)
toPath a = case a.av_ of
  APath2 p -> p
  _        -> "path commands" `expectedButGot` strAVal a

toTransformRot a = case a.av_ of
  ATransform [Rot n1 n2 n3] -> (n1,n2,n3)
  _                         -> "a rotation transform" `expectedButGot` strAVal a


-- these are for when the VTrace doesn't matter
aVal          = flip AVal [-1]
aNum          = aVal << ANum
aString       = aVal << AString
aTransform    = aVal << ATransform
aColorNum     = aVal << AColorNum
aPoints       = aVal << APoints
aPath2        = aVal << APath2


------------------------------------------------------------------------------
-- Misc Attribute Helpers


findNumishAttr : NodeId -> String -> List Attr -> NumTr
findNumishAttr id attr attrs = toNumIsh <| findAVal attr attrs


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
    |> fst
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
      case av.av_ of
        AString sBlobId -> Just (Utils.parseInt sBlobId)
        _               -> Nothing


maybeFindBounds l =
  case Utils.maybeFind "BOUNDS" l of
    Nothing -> Nothing
    Just av ->
      let roundBounds = True in
      case (av.av_, roundBounds) of
        (ABounds bounds, False) -> Just bounds
        (ABounds (a,b,c,d), True) ->
          let f = Utils.mapFst (toFloat << round) in
          Just (f a, f b, f c, f d)
        _ ->
          Nothing


justGetSvgNode : String -> NodeId -> RootedIndexedTree -> (ShapeKind, List Attr)
justGetSvgNode cap nodeId (_, indexedTree) =
  case Utils.justGet_ cap nodeId indexedTree of
    SvgNode kind attrs _ -> (kind, attrs)
    TextNode _           -> Debug.crash (cap ++ ": TextNode ?")


dummySvgNode =
  let zero = aNum (0, dummyTrace) in
  SvgNode "circle" (List.map (\k -> (k, zero)) ["cx","cy","r"]) []


dummySvgVal =
  let zero = vConst (0, dummyTrace) in
  let attrs = vList <| List.map (\k -> vList [vStr k, zero]) ["cx","cy","r"] in
  let children = vList [] in
  vList [vStr "circle", attrs, children]


------------------------------------------------------------------------------
-- Slate Traversal

foldSlate : RootedIndexedTree -> a -> (NodeId -> IndexedTreeNode -> a -> a) -> a
foldSlate (rootId, dict) acc f =
  let
 -- foldNode : NodeId -> a -> a
    foldNode i acc =
      let node = Utils.justGet_ "foldSlate" i dict in
      case node of
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
    case node of
      TextNode s           -> f (Left (i, s))
      SvgNode kind attrs _ -> f (Right (i, kind, attrs))


------------------------------------------------------------------------------
-- Little Animations

-- TODO use options for better error messages

-- TODO use this to reduce clutter
type alias AnimationKey = (Int, Int, Float)

-- HACK: see LocEqn.traceToLocEquation...
-- TODO: streamline Trace, LocEquation, etc.
vNumFrozen n = val (VConst (n, TrLoc (-999, frozen, toString n)))
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
  fetchSlideVal slideNumber val
  `Result.andThen` (\slideVal ->
    let movieCount = fetchMovieCount slideVal in
    fetchMovieVal movieNumber slideVal
    `Result.andThen` (\movieVal ->
      let (movieDuration, continue) = fetchMovieDurationAndContinueBool movieVal in
      fetchMovieFrameVal slideNumber movieNumber movieTime movieVal
      |> Result.map (\movieFrameVal ->
        (slideCount, movieCount, movieDuration, continue, movieFrameVal)
      )
    )
  )

fetchEverything : Int -> Int -> Float -> Val -> Result String (Int, Int, Float, Bool, RootedIndexedTree)
fetchEverything slideNumber movieNumber movieTime val =
  fetchEverything_ slideNumber movieNumber movieTime val
  `Result.andThen` (\(slideCount, movieCount, movieDuration, continue, movieVal) ->
    valToIndexedTree movieVal
    |> Result.map (\indexedTree -> (slideCount, movieCount, movieDuration, continue, indexedTree))
  )

fetchSlideCount : Val -> Int
fetchSlideCount val =
  case unwrapVList val of
    Just [VConst (slideCount, _), _] -> round slideCount
    _ -> 1 -- Program returned a plain SVG array structure...we hope.

fetchMovieCount : Val -> Int
fetchMovieCount slideVal =
  case unwrapVList slideVal of
    Just [VConst (movieCount, _), _] -> round movieCount
    _ -> 1 -- Program returned a plain SVG array structure...we hope.

fetchSlideVal : Int -> Val -> Result String Val
fetchSlideVal slideNumber val =
  case unwrapVList val of
    Just [VConst (slideCount, _), VClosure _ pat fexp fenv] ->
      -- Program returned the slide count and a
      -- function from slideNumber -> SVG array structure.
      case pat.val of -- Find that function's argument name
        PVar _ argumentName _ ->
          -- Bind the slide number to the function's argument.
          let fenv' = (argumentName, vIntFrozen slideNumber) :: fenv in
          Eval.eval fenv' [] fexp
          |> Result.map (\((returnVal, _), _) -> returnVal)
        _ -> Err ("expected slide function to take a single argument, got " ++ (toString pat.val))
    _ -> Ok val -- Program returned a plain SVG array structure...we hope.

-- This is nasty b/c a two-arg function is really a function that returns a function...
fetchMovieVal : Int -> Val -> Result String Val
fetchMovieVal movieNumber slideVal =
  case unwrapVList slideVal of
    Just [VConst (movieCount, _), VClosure _ pat fexp fenv] ->
      case pat.val of -- Find the function's argument name
        PVar _ movieNumberArgumentName _ ->
          let fenv' = (movieNumberArgumentName, vIntFrozen movieNumber) :: fenv in
          Eval.eval fenv' [] fexp
          |> Result.map (\((returnVal, _), _) -> returnVal)
        _ -> Err ("expected movie function to take a single argument, got " ++ (toString pat.val))
    _ -> Ok slideVal -- Program returned a plain SVG array structure...we hope.

fetchMovieDurationAndContinueBool : Val -> (Float, Bool)
fetchMovieDurationAndContinueBool movieVal =
  case unwrapVList movieVal of
    Just [VBase (VString "Static"), VClosure _ _ _ _] ->
      (0.0, False)
    Just [VBase (VString "Dynamic"), VConst (movieDuration, _), VClosure _ _ _ _, VBase (VBool continue)] ->
      (movieDuration, continue)
    _ ->
      (0.0, False) -- Program returned a plain SVG array structure...we hope.

-- This is nasty b/c a two-arg function is really a function that returns a function...
fetchMovieFrameVal : Int -> Int -> Float -> Val -> Result String Val
fetchMovieFrameVal slideNumber movieNumber movieTime movieVal =
  case unwrapVList movieVal of
    Just [VBase (VString "Static"), VClosure _ pat fexp fenv] ->
      case pat.val of -- Find the function's argument names
        PVar _ slideNumberArgumentName _ ->
          let fenv' = (slideNumberArgumentName, vIntFrozen slideNumber) :: fenv in
          case Eval.eval fenv' [] fexp |> Result.map (\((innerVal, _), _) -> innerVal.v_) of
            Ok (VClosure _ patInner fexpInner fenvInner) ->
              case patInner.val of
                PVar _ movieNumberArgumentName _ ->
                  let fenvInner' = (movieNumberArgumentName, vIntFrozen movieNumber) :: fenvInner in
                  Eval.eval fenvInner' [] fexpInner
                  |> Result.map (\((returnVal, _), _) -> returnVal)
                _ -> Err ("expected static movie frame function to take two arguments, got " ++ (toString patInner.val))
            Ok v_ -> Err ("expected static movie frame function to take two arguments, got " ++ (toString v_))
            Err s -> Err s
        _ -> Err ("expected static movie frame function to take two arguments, got " ++ (toString pat.val))
    Just [VBase (VString "Dynamic"), VConst (movieDuration, _), VClosure _ pat fexp fenv, VBase (VBool _)] ->
      case pat.val of -- Find the function's argument names
        PVar _ slideNumberArgumentName _ ->
          let fenv' = (slideNumberArgumentName, vIntFrozen slideNumber) :: fenv in
          case Eval.eval fenv' [] fexp |> Result.map (\((innerVal1, _), _) -> innerVal1.v_) of
            Ok (VClosure _ patInner1 fexpInner1 fenvInner1) ->
              case patInner1.val of
                PVar _ movieNumberArgumentName _ ->
                  let fenvInner1' = (movieNumberArgumentName, vIntFrozen movieNumber) :: fenvInner1 in
                  case Eval.eval fenvInner1' [] fexpInner1 |> Result.map (\((innerVal2, _), _) -> innerVal2.v_) of
                    Ok (VClosure _ patInner2 fexpInner2 fenvInner2) ->
                      case patInner2.val of
                        PVar _ movieSecondsArgumentName _ ->
                          let fenvInner2' = (movieSecondsArgumentName, vNumFrozen movieTime) :: fenvInner2 in
                          Eval.eval fenvInner2' [] fexpInner2
                          |> Result.map (\((returnVal, _), _) -> returnVal)
                        _ -> Err ("expected dynamic movie frame function to take four arguments, got " ++ (toString patInner2.val))
                    Ok innerV2_ -> Err ("expected dynamic movie frame function to take four arguments, got " ++ (toString innerV2_))
                    Err s -> Err s
                _ -> Err ("expected dynamic movie frame function to take four arguments, got " ++ (toString patInner1.val))
            Ok innerV1_ -> Err ("expected dynamic movie frame function to take four arguments, got " ++ (toString innerV1_))
            Err s -> Err s
        _ -> Err ("expected dynamic movie frame function to take four arguments, got " ++ (toString pat.val))
    _ -> Ok movieVal -- Program returned a plain SVG array structure...we hope.
