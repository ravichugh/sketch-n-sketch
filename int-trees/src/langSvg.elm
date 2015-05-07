module LangSvg (valToHtml) where

import Svg
import Svg.Attributes as A
import Html
import Debug
import Set
import String

import Lang exposing (..)
import Utils
import Sync

------------------------------------------------------------------------------

-- TODO probably want to factor HTML attributes and SVG attributes into
-- records rather than lists of lists of ...

valToSvg : Val -> List Svg.Svg
valToSvg v = case v of
  VList vs -> flip List.map vs <| \v1 -> case v1 of
    VList (VBase (String shape) :: vs') ->
      let attrs = flip List.map vs' <| \v2 -> case v2 of
        VList [VBase (String a), VConst i _]       -> (attr a) (toString i)
        VList [VBase (String a), VBase (String s)] -> (attr a) s
        VList [VBase (String "points"), VList pts] ->
          let s =
            Utils.spaces <|
              flip List.map pts <| \v3 -> case v3 of
                VList [VConst x _, VConst y _] ->
                  toString x ++ "," ++ toString y
          in
          (attr "points") s
      in
      (svg shape) attrs []

valToHtml : Val -> Html.Html
valToHtml v = case v of
  VList (VList [VBase (String "svgAttrs"), VList l] :: vs) ->
    let f v1 = case v1 of
      VList [VBase (String a), VBase (String s)] -> (attr a) s in
    Svg.svg (List.map f l) (valToSvg (VList vs))
  VList _ ->
    Svg.svg [] (valToSvg v)


------------------------------------------------------------------------------

funcsSvg = [
    ("circle", Svg.circle)
  , ("ellipse", Svg.ellipse)
  , ("line", Svg.line)
  , ("polygon", Svg.polygon)
  , ("rect", Svg.rect)
  ]

funcsAttr = [
    ("cx", A.cx)
  , ("cy", A.cy)
  , ("fill", A.fill)
  , ("height", A.height)
  , ("points", A.points)
  , ("r", A.r)
  , ("rx", A.rx)
  , ("ry", A.ry)
  , ("stroke", A.stroke)
  , ("strokeWidth", A.strokeWidth)
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

