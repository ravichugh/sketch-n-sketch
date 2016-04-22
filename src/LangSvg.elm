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
import Regex

import ColorNum

import Lang exposing (..)
import Utils
import Eval

------------------------------------------------------------------------------

attr = VirtualDom.attribute

-- TODO probably want to factor HTML attributes and SVG attributes into
-- records rather than lists of lists of ...

valToHtml : Int -> Int -> Val -> Html.Html
valToHtml w h v = case v.v_ of
  VList vs ->
    case List.map .v_ vs of
      [VBase (String "svg"), VList vs1, VList vs2] ->
        let wh = [numAttrToVal "width" w, numAttrToVal "height" h] in
        let v' = vList [vStr "svg", vList (wh ++ vs1), vList vs2] in
        compileValToNode v'
          -- NOTE: not checking if width/height already in vs1
      _ ->
        Debug.crash "valToHtml"
  _ ->
    Debug.crash "valToHtml"

compileValToNode : Val -> VirtualDom.Node
compileValToNode v = case v.v_ of
  VList vs ->
    case List.map .v_ vs of
      [VBase (String "TEXT"), VBase (String s)] -> VirtualDom.text s
      [VBase (String f), VList vs1, VList vs2] ->
        (Svg.node f) (compileAttrVals vs1) (compileNodeVals vs2)
      _ ->
        Debug.crash "compileValToNode"
  _ ->
    Debug.crash "compileValToNode"

compileNodeVals : List Val -> List Svg.Svg
compileNodeVals = List.map compileValToNode

compileAttrVals : List Val -> List Svg.Attribute
compileAttrVals = List.map (uncurry compileAttr << valToAttr)

compileAttrs    : List Attr -> List Svg.Attribute
compileAttrs    = List.map (uncurry compileAttr)

compileAttr : String -> AVal -> Svg.Attribute
compileAttr k v = (attr k) (strAVal v)

numAttrToVal a i =
  vList [vBase (String a), vConst (toFloat i, dummyTrace)]

type alias AVal = { av_ : AVal_, vtrace : VTrace }

type AVal_
  = ANum NumTr
  | AString String
  | APoints (List Point)
  | ARgba Rgba
  | AColorNum NumTr -- Utils.numToColor [0,500)
  | APath2 (List PathCmd, PathCounts)
  | ATransform (List TransformCmd)
  | ABounds (NumTr, NumTr, NumTr, NumTr)

-- these versions are for when the VTrace doesn't matter
aVal          = flip AVal [-1]
aNum          = aVal << ANum
aString       = aVal << AString
aTransform    = aVal << ATransform
aColorNum     = aVal << AColorNum
aPoints       = aVal << APoints
aPath2        = aVal << APath2

maxColorNum   = 500
clampColorNum = Utils.clamp 0 (maxColorNum - 1)

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

-- toNum    (ANum (i,_)) = i
-- toNumTr  (ANum (i,t)) = (i,t)

strValOfAVal = strVal << valOfAVal

expectedButGot x s = errorMsg <| "expected " ++ x ++", but got: " ++ s

-- temporary way to ignore numbers specified as strings (also see Sync)

toNum : AVal -> Num
toNum a = case a.av_ of
  ANum (n,_) -> n
  AString s  ->
    case String.toFloat s of
      Ok n -> n
      _    -> "a number" `expectedButGot` strValOfAVal a
  _        -> "a number" `expectedButGot` strValOfAVal a

toNumTr a = case a.av_ of
  ANum (n,t) -> (n,t)
  AColorNum (n,t) -> (n,t)
  AString s  ->
    case String.toFloat s of
      Ok n -> (n, dummyTrace)
      _    -> "a number" `expectedButGot` strValOfAVal a
  _        -> "a number" `expectedButGot` strValOfAVal a

toPoints a = case a.av_ of
  APoints pts -> pts
  _           -> "a list of points" `expectedButGot` strValOfAVal a

toPath : AVal -> (List PathCmd, PathCounts)
toPath a = case a.av_ of
  APath2 p -> p
  _        -> "path commands" `expectedButGot` strValOfAVal a

toTransformRot a = case a.av_ of
  ATransform [Rot n1 n2 n3] -> (n1,n2,n3)
  _                         -> "a rotation transform" `expectedButGot` strValOfAVal a

-- TODO will need to change AVal also
--   and not insert dummy VTraces (using the v* functions)

valToAttr v = case v.v_ of
  VList [v1,v2] -> case (v1.v_, v2.v_) of
    (VBase (String k), v2_) ->
     -- NOTE: Elm bug? undefined error when shadowing k (instead of choosing k')
     let (k',av_) =
      case (k, v2_) of
        ("points", VList vs)    -> (k, APoints <| List.map valToPoint vs)
        ("fill", VList vs)      -> (k, ARgba <| valToRgba vs)
        ("fill", VConst it)     -> (k, AColorNum it)
        ("stroke", VList vs)    -> (k, ARgba <| valToRgba vs)
        ("stroke", VConst it)   -> (k, AColorNum it)
        ("d", VList vs)         -> (k, APath2 (valsToPath2 vs))
        ("transform", VList vs) -> (k, ATransform (valsToTransform vs))
        ("BOUNDS", VList vs)    -> (k, ABounds <| valToBounds vs)
        (_, VConst it)          -> (k, ANum it)
        (_, VBase (String s))   -> (k, AString s)
        _                       -> Debug.crash "valToAttr"
     in
     (k', AVal av_ v2.vtrace)
    _ ->
      Debug.crash "valToAttr"
  _ ->
    Debug.crash "valToAttr"


valToPoint v = case v.v_ of
  VList vs -> case List.map .v_ vs of
    [VConst x, VConst y] -> (x,y)
    _                    -> "a point" `expectedButGot` strVal v
  _                      -> "a point" `expectedButGot` strVal v

pointToVal (x,y) = (vList [vConst x, vConst y])

valToRgba vs = case List.map .v_ vs of
  [VConst r, VConst g, VConst b, VConst a] -> (r,g,b,a)
  _                                        -> "rgba" `expectedButGot` strVal (vList vs)

rgbaToVal (r,g,b,a) = [vConst r, vConst g, vConst b, vConst a]

strPoint (x_,y_) =
  let (x,y) = Utils.unwrap2 <| List.map fst [x_,y_] in
  toString x ++ "," ++ toString y

strRgba (r_,g_,b_,a_) =
  strRgba_ (List.map fst [r_,g_,b_,a_])

strRgba_ rgba =
  "rgba" ++ Utils.parens (Utils.commas (List.map toString rgba))

strAVal : AVal -> String
strAVal a = case a.av_ of
  AString s -> s
  ANum it   -> toString (fst it)
  -- ANum it   -> toString (fst it) ++ Utils.parens (strTrace (snd it))
  APoints l -> Utils.spaces (List.map strPoint l)
  ARgba tup -> strRgba tup
  APath2 p  -> strAPath2 (fst p)
  ATransform l -> Utils.spaces (List.map strTransformCmd l)
  AColorNum n ->
    -- slight optimization:
    strRgba_ (ColorNum.convert (fst n))
    -- let (r,g,b) = Utils.numToColor maxColorNum (fst n) in
    -- strRgba_ [r,g,b,1]
  ABounds bounds -> strBounds bounds

valOfAVal : AVal -> Val
valOfAVal a = flip Val a.vtrace <| case a.av_ of
  AString s    -> VBase (String s)
  ANum it      -> VConst it
  APoints l    -> VList (List.map pointToVal l)
  ARgba tup    -> VList (rgbaToVal tup)
  APath2 p     -> VList (List.concatMap valsOfPathCmd (fst p))
  AColorNum nt -> VConst nt
  _            -> Debug.crash "valOfAVal"

valsOfPathCmd c =
  Debug.crash "restore valsOfPathCmd"
{-
  let fooPt (_,(x,y)) = [vConst x, vConst y] in
  case c of
    CmdZ   s              -> vStr s :: []
    CmdMLT s pt           -> vStr s :: fooPt pt
    CmdHV  s n            -> vStr s :: [vConst n]
    CmdC   s pt1 pt2 pt3  -> vStr s :: List.concatMap fooPt [pt1,pt2,pt3]
    CmdSQ  s pt1 pt2      -> vStr s :: List.concatMap fooPt [pt1,pt2]
    CmdA   s a b c d e pt -> vStr s :: List.map vConst [a,b,c,d,e] ++ fooPt pt
-}

-- Return list of (i, pt).
-- (Includes control points.)
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


valOfAttr (k,a) = vList [vBase (String k), valOfAVal a]
  -- no VTrace to preserve...

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
  []     -> ([], counts)
  v::vs' -> case v.v_ of
    VBase (String cmd) ->
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

strAPath2 =
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

-- transform commands

valsToTransform : List Val -> List TransformCmd
valsToTransform = List.map valToTransformCmd

valToTransformCmd v = case v.v_ of
  VList vs1 -> case List.map .v_ vs1 of
    (VBase (String k) :: vs) ->
      case (k, vs) of
        ("rotate",    [VConst n1, VConst n2, VConst n3]) -> Rot n1 n2 n3
        ("scale",     [VConst n1, VConst n2])            -> Scale n1 n2
        ("translate", [VConst n1, VConst n2])            -> Trans n1 n2
        _ -> "a transform command" `expectedButGot` strVal v
    _     -> "a transform command" `expectedButGot` strVal v
  _       -> "a transform command" `expectedButGot` strVal v

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

valToBounds vs = case List.map .v_ vs of
  [VConst a, VConst b, VConst c, VConst d] -> (a,b,c,d)
  _                                        -> "bounds" `expectedButGot` strVal (vList vs)

strBounds (left,top,right,bot) =
  Utils.spaces (List.map (toString << fst) [left,top,right,bot])

desugarKind shape =
  case shape of
    "BOX" -> "rect"
    _     -> shape

desugarShapeAttrs shape0 attrs0 =
  Maybe.withDefault (shape0, attrs0) <|
    case shape0 of
      "BOX" ->
        Utils.maybeRemoveFirst "LEFT"  attrs0 `Maybe.andThen` \(vL,attrs1) ->
        Utils.maybeRemoveFirst "RIGHT" attrs1 `Maybe.andThen` \(vR,attrs2) ->
        Utils.maybeRemoveFirst "TOP"   attrs2 `Maybe.andThen` \(vT,attrs3) ->
        Utils.maybeRemoveFirst "BOT"   attrs3 `Maybe.andThen` \(vB,attrs4) ->
          case (vL.av_, vT.av_, vR.av_, vB.av_) of
            (ANum (x,_), ANum (y,_), ANum (xw,_), ANum (yh,_)) ->
              let width = aNum (xw - x, dummyTrace) in
              let height = aNum (yh - y, dummyTrace) in
              let attrs = [("x",vL),("y",vT),("width",width),("height",height)] ++ attrs4 in
              Just ("rect", attrs)
            _ ->
              Nothing
      _ ->
        Nothing


------------------------------------------------------------------------------

type alias ShapeKind = String
type alias NodeId = Int
type alias IndexedTree = Dict NodeId IndexedTreeNode
type alias Attr = (String, AVal)
type IndexedTreeNode
  = TextNode String
  | SvgNode ShapeKind (List Attr) (List NodeId)
type alias RootedIndexedTree = (NodeId, IndexedTree)

-- Must be a comparable to be put in a Set
-- Otherwise, this shouldn't be a string
type alias ShapeFeature = String

-- Make sure that coordinate features match /.+[X|Y]\d*$/
-- So we can gather them back up into (X,Y) pairs again.
shapeFill = "fill"
shapeStroke = "stroke"
shapeRotation = "rotation"
rectTLX = "rectTLX"
rectTLY = "rectTLY"
rectTRX = "rectTRX"
rectTRY = "rectTRY"
rectBLX = "rectBLX"
rectBLY = "rectBLY"
rectBRX = "rectBRX"
rectBRY = "rectBRY"
rectTCX = "rectTCX"
rectTCY = "rectTCY"
rectCRX = "rectCRX"
rectCRY = "rectCRY"
rectBCX = "rectBCX"
rectBCY = "rectBCY"
rectCLX = "rectCLX"
rectCLY = "rectCLY"
rectCX = "rectCX"
rectCY = "rectCY"
rectWidth = "rectWidth"
rectHeight = "rectHeight"
boxTLX = "boxTLX"
boxTLY = "boxTLY"
boxTRX = "boxTRX"
boxTRY = "boxTRY"
boxBLX = "boxBLX"
boxBLY = "boxBLY"
boxBRX = "boxBRX"
boxBRY = "boxBRY"
boxTCX = "boxTCX"
boxTCY = "boxTCY"
boxCRX = "boxCRX"
boxCRY = "boxCRY"
boxBCX = "boxBCX"
boxBCY = "boxBCY"
boxCLX = "boxCLX"
boxCLY = "boxCLY"
boxCX = "boxCX"
boxCY = "boxCY"
boxWidth = "boxWidth"
boxHeight = "boxHeight"
circleTCX = "circleTCX"
circleTCY = "circleTCY"
circleCRX = "circleCRX"
circleCRY = "circleCRY"
circleBCX = "circleBCX"
circleBCY = "circleBCY"
circleCLX = "circleCLX"
circleCLY = "circleCLY"
circleCX = "circleCX"
circleCY = "circleCY"
circleR = "circleR"
ellipseTCX = "ellipseTCX"
ellipseTCY = "ellipseTCY"
ellipseCRX = "ellipseCRX"
ellipseCRY = "ellipseCRY"
ellipseBCX = "ellipseBCX"
ellipseBCY = "ellipseBCY"
ellipseCLX = "ellipseCLX"
ellipseCLY = "ellipseCLY"
ellipseCX = "ellipseCX"
ellipseCY = "ellipseCY"
ellipseRX = "ellipseRX"
ellipseRY = "ellipseRY"
lineX1 = "lineX1"
lineY1 = "lineY1"
lineX2 = "lineX2"
lineY2 = "lineY2"
lineCX = "lineCX"
lineCY = "lineCY"
polyPtX = "polyPtX"
polyPtY = "polyPtY"
polyMidptX = "polyMidptX"
polyMidptY = "polyMidptY"
pathPtX = "pathPtX"
pathPtY = "pathPtY"

children n = case n of
  TextNode _    -> []
  SvgNode _ _ l -> l

emptyTree : RootedIndexedTree
emptyTree = valToIndexedTree <| vList [vBase (String "svg"), vList [], vList []]

-- TODO use options for better error messages

resolveToMovieCount : Int -> Val -> Int
resolveToMovieCount slideNumber val =
  let slideVal = fetchSlideVal slideNumber val in
  fetchMovieCount slideVal

resolveToMovieFrameVal : Int -> Int -> Float -> Val -> Val
resolveToMovieFrameVal slideNumber movieNumber movieTime val =
  let (_, _, _, _, movieFrameVal) = fetchEverything_ slideNumber movieNumber movieTime val in
    movieFrameVal

resolveToIndexedTree : Int -> Int -> Float -> Val -> RootedIndexedTree
resolveToIndexedTree slideNumber movieNumber movieTime val =
  let (_, _, _, _, indexedTree) = fetchEverything slideNumber movieNumber movieTime val in
    indexedTree

fetchEverything_ : Int -> Int -> Float -> Val -> (Int, Int, Float, Bool, Val)
fetchEverything_ slideNumber movieNumber movieTime val =
  let
    slideCount = fetchSlideCount val
    slideVal   = fetchSlideVal slideNumber val
  in
  let
    movieCount = fetchMovieCount slideVal
    movieVal   = fetchMovieVal movieNumber slideVal
  in
  let
    (movieDuration, continue) = fetchMovieDurationAndContinueBool movieVal
    movieFrameVal             = fetchMovieFrameVal slideNumber movieNumber movieTime movieVal
  in
    (slideCount, movieCount, movieDuration, continue, movieFrameVal)

fetchEverything : Int -> Int -> Float -> Val -> (Int, Int, Float, Bool, RootedIndexedTree)
fetchEverything slideNumber movieNumber movieTime val =
  let (slideCount, movieCount, movieDuration, continue, movieVal) = fetchEverything_ slideNumber movieNumber movieTime val in
    (slideCount, movieCount, movieDuration, continue, valToIndexedTree movieVal)

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

fetchSlideVal : Int -> Val -> Val
fetchSlideVal slideNumber val =
  case unwrapVList val of
    Just [VConst (slideCount, _), VClosure _ pat fexp fenv] ->
      -- Program returned the slide count and a
      -- function from slideNumber -> SVG array structure.
      case pat.val of -- Find that function's argument name
        PVar _ argumentName _ ->
          -- Bind the slide number to the function's argument.
          let fenv' = (argumentName, vConst (toFloat slideNumber, dummyTrace)) :: fenv in
          let ((returnVal, _), _) = Eval.eval fenv' fexp in
          returnVal
        _ -> Debug.crash ("expected slide function to take a single argument, got " ++ (toString pat.val))
    _ -> val -- Program returned a plain SVG array structure...we hope.

-- This is nasty b/c a two-arg function is really a function that returns a function...
fetchMovieVal : Int -> Val -> Val
fetchMovieVal movieNumber slideVal =
  case unwrapVList slideVal of
    Just [VConst (movieCount, _), VClosure _ pat fexp fenv] ->
      case pat.val of -- Find the function's argument name
        PVar _ movieNumberArgumentName _ ->
          let fenv' = (movieNumberArgumentName, vConst (toFloat movieNumber, dummyTrace)) :: fenv in
          let ((returnVal, _), _) = Eval.eval fenv' fexp in
          returnVal
        _ -> Debug.crash ("expected movie function to take a single argument, got " ++ (toString pat.val))
    _ -> slideVal -- Program returned a plain SVG array structure...we hope.

fetchMovieDurationAndContinueBool : Val -> (Float, Bool)
fetchMovieDurationAndContinueBool movieVal =
  case unwrapVList movieVal of
    Just [VBase (String "Static"), VClosure _ _ _ _] ->
      (0.0, False)
    Just [VBase (String "Dynamic"), VConst (movieDuration, _), VClosure _ _ _ _, VBase (Bool continue)] ->
      (movieDuration, continue)
    _ ->
      (0.0, False) -- Program returned a plain SVG array structure...we hope.

-- This is nasty b/c a two-arg function is really a function that returns a function...
fetchMovieFrameVal : Int -> Int -> Float -> Val -> Val
fetchMovieFrameVal slideNumber movieNumber movieTime movieVal =
  case unwrapVList movieVal of
    Just [VBase (String "Static"), VClosure _ pat fexp fenv] ->
      case pat.val of -- Find the function's argument names
        PVar _ slideNumberArgumentName _ ->
          let fenv' = (slideNumberArgumentName, vConst (toFloat slideNumber, dummyTrace)) :: fenv in
          let ((innerVal, _), _) = Eval.eval fenv' fexp in
          case innerVal.v_ of
            VClosure _ patInner fexpInner fenvInner ->
              case patInner.val of
                PVar _ movieNumberArgumentName _ ->
                  let fenvInner' = (movieNumberArgumentName, vConst (toFloat movieNumber, dummyTrace)) :: fenvInner in
                  let ((returnVal, _), _) = Eval.eval fenvInner' fexpInner in
                  returnVal
                _ -> Debug.crash ("expected static movie frame function to take two arguments, got " ++ (toString patInner.val))
            _ -> Debug.crash ("expected static movie frame function to take two arguments, got " ++ (toString innerVal))
        _ -> Debug.crash ("expected static movie frame function to take two arguments, got " ++ (toString pat.val))
    Just [VBase (String "Dynamic"), VConst (movieDuration, _), VClosure _ pat fexp fenv, VBase (Bool _)] ->
      case pat.val of -- Find the function's argument names
        PVar _ slideNumberArgumentName _ ->
          let fenv' = (slideNumberArgumentName, vConst (toFloat slideNumber, dummyTrace)) :: fenv in
          let ((innerVal1, _), _) = Eval.eval fenv' fexp in
          case innerVal1.v_ of
            VClosure _ patInner1 fexpInner1 fenvInner1 ->
              case patInner1.val of
                PVar _ movieNumberArgumentName _ ->
                  let fenvInner1' = (movieNumberArgumentName, vConst (toFloat movieNumber, dummyTrace)) :: fenvInner1 in
                  let ((innerVal2, _), _) = Eval.eval fenvInner1' fexpInner1 in
                  case innerVal2.v_ of
                    VClosure _ patInner2 fexpInner2 fenvInner2 ->
                      case patInner2.val of
                        PVar _ movieSecondsArgumentName _ ->
                          let fenvInner2' = (movieSecondsArgumentName, vConst (movieTime, dummyTrace)) :: fenvInner2 in
                          let ((returnVal, _), _) = Eval.eval fenvInner2' fexpInner2 in
                          returnVal
                        _ -> Debug.crash ("expected dynamic movie frame function to take four arguments, got " ++ (toString patInner2.val))
                    _ -> Debug.crash ("expected dynamic movie frame function to take four arguments, got " ++ (toString innerVal2))
                _ -> Debug.crash ("expected dynamic movie frame function to take four arguments, got " ++ (toString patInner1.val))
            _ -> Debug.crash ("expected dynamic movie frame function to take four arguments, got " ++ (toString innerVal1))
        _ -> Debug.crash ("expected dynamic movie frame function to take four arguments, got " ++ (toString pat.val))
    _ -> movieVal -- Program returned a plain SVG array structure...we hope.


valToIndexedTree : Val -> RootedIndexedTree
valToIndexedTree v =
  let (nextId,tree) = valToIndexedTree_ v (1, Dict.empty) in
  let rootId = nextId - 1 in
  (rootId, tree)

valToIndexedTree_ v (nextId, d) = case v.v_ of

  VList vs -> case List.map .v_ vs of

    [VBase (String "TEXT"), VBase (String s)] ->
      (1 + nextId, Dict.insert nextId (TextNode s) d)

    [VBase (String kind), VList vs1, VList vs2] ->
      let processChild vi (a_nextId, a_graph , a_children) =
        let (a_nextId',a_graph') = valToIndexedTree_ vi (a_nextId, a_graph) in
        let a_children'          = (a_nextId' - 1) :: a_children in
        (a_nextId', a_graph', a_children') in
      let (nextId',d',children) = List.foldl processChild (nextId,d,[]) vs2 in
      let node = SvgNode kind (List.map valToAttr vs1) (List.reverse children) in
      (1 + nextId', Dict.insert nextId' node d')

    _ ->
      "an SVG node" `expectedButGot` strVal v

  _ ->
    "an SVG node" `expectedButGot` strVal v

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
  , ("BOX",
      [ ("Interior", ["LEFT", "TOP", "RIGHT", "BOT"])
      , ("TopLeftCorner", ["LEFT", "TOP"])
      , ("TopRightCorner", ["TOP", "RIGHT"])
      , ("BotRightCorner", ["RIGHT", "BOT"])
      , ("BotLeftCorner", ["LEFT", "BOT"])
      , ("LeftEdge", ["LEFT"])
      , ("TopEdge", ["TOP"])
      , ("RightEdge", ["RIGHT"])
      , ("BotEdge", ["BOT"])
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


------------------------------------------------------------------------------

dummySvgNode =
  let zero = aNum (0, dummyTrace) in
  SvgNode "circle" (List.map (\k -> (k, zero)) ["cx","cy","r"]) []

-- TODO break up and move slateToVal here
dummySvgVal =
  let zero = vConst (0, dummyTrace) in
  let attrs = vList <| List.map (\k -> vList [vStr k, zero]) ["cx","cy","r"] in
  let children = vList [] in
  vList [vStr "circle", attrs, children]
