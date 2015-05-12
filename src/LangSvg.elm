module LangSvg (valToHtml) where

import Html
import Svg
import Svg.Attributes as A
import VirtualDom

-- in Svg.elm:
--   type alias Svg = VirtualDom.Node
--   type alias Attribute = VirtualDom.Property

import Debug
import Set
import String
import Char

import Lang exposing (..)
import Utils
import Sync

------------------------------------------------------------------------------

-- TODO probably want to factor HTML attributes and SVG attributes into
-- records rather than lists of lists of ...

valToHtml : Int -> Int -> Val -> Html.Html
valToHtml w h v =
  let wh = [A.width (toString w), A.height (toString h)] in
  case v of
    VList (VBase (String "svg") :: VList attrs :: vs) ->
      Svg.svg (List.map valToAttr attrs ++ wh) (valToSvgList (VList vs))
    VList _ ->
      Svg.svg wh (valToSvgList v)

valToSvgList : Val -> List Svg.Svg
valToSvgList (VList vs) = List.map valToSvgNode vs

valToSvgNode : Val -> VirtualDom.Node
valToSvgNode (VList [VBase (String f), VList vs1, VList vs2]) =
  (svg f) (valsToAttrs vs1) (valsToNodes vs2)

valToNode : Val -> VirtualDom.Node
valToNode v = case v of
  VList [VBase (String "TEXT"), VBase (String s)] -> VirtualDom.text s
  VList [VBase (String f), VList vs1, VList vs2]  -> (svg f) (valsToAttrs vs1) (valsToNodes vs2)

          -- TODO bug?
          --   in second case above, JS error when calling valToSvgNode v instead.
          --   related to desugaring, as with recursive parsers?

valsToNodes = List.map valToNode
valsToAttrs = List.map valToAttr

valToAttr v =
  case v of
    VList [VBase (String a), VConst i _]       -> (attr a) (toString i)
    VList [VBase (String a), VBase (String s)] -> (attr a) s
    VList [VBase (String "points"), VList vs]  -> (attr "points") (valToPoints vs)
    VList [VBase (String "fill"), VList vs]    -> (attr "fill") (valToRgba vs)
    VList [VBase (String "stroke"), VList vs]  -> (attr "stroke") (valToRgba vs)
    VList [VBase (String "d"), VList vs]       -> (attr "d") (valToPath vs)

valToPoints = Utils.spaces << List.map valToPoint

valToPoint (VList [VConst x _, VConst y _]) =
  toString x ++ "," ++ toString y

valToRgba v = case v of
  [VConst r _, VConst g _, VConst b _, VConst a _] ->
    "rgba" ++ Utils.parens (Utils.commas (List.map toString [r,g,b,a]))

-- https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths
-- http://www.w3schools.com/svg/svg_path.asp
--
-- NOTES:
--  . using different representation of points in d than in points
--    to make it less verbose and easier to copy-and-paste raw SVG examples
--  . looks like commas are optional

valToPath = Utils.spaces << valToPath_

valToPath_ vs =
  let pt i j = toString i ++ " " ++ toString j in
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

projConsts k vs =
  if k == 0 then ([], vs)
  else case vs of
         VConst i _ ::vs' ->
           let (l1,l2) = projConsts (k-1) vs' in
           (i::l1, l2)

matchCmd cmd s =
  let [c] = String.toList cmd in
  let cs  = String.toList s in
  List.member c (cs ++ List.map Char.toLower cs)


------------------------------------------------------------------------------

funcsSvg = [
    ("circle", Svg.circle)
  , ("ellipse", Svg.ellipse)
  , ("line", Svg.line)
  , ("path", Svg.path)
  , ("polygon", Svg.polygon)
  , ("polyline", Svg.polyline)
  , ("rect", Svg.rect)
  , ("text", Svg.text)
  , ("tspan", Svg.tspan)
  ]

funcsAttr = [
    ("cx", A.cx)
  , ("cy", A.cy)
  , ("d", A.d)
  , ("fill", A.fill)
  , ("height", A.height)
  , ("opacity", A.opacity)
  , ("points", A.points)
  , ("r", A.r)
  , ("rx", A.rx)
  , ("ry", A.ry)
  , ("stroke", A.stroke)
  , ("strokeWidth", A.strokeWidth)
  , ("style", A.style)
  , ("transform", A.transform)
  , ("viewBox", A.viewBox)
  , ("width", A.width)
  , ("x", A.x)
  , ("x1", A.x1)
  , ("x2", A.x2)
  , ("y", A.y)
  , ("y1", A.y1)
  , ("y2", A.y2)
  ]

find d s =
  case Utils.maybeFind s d of
    Just f  -> f
    Nothing -> Debug.crash <| "MainSvg.find: " ++ s

attr = find funcsAttr
svg  = find funcsSvg

