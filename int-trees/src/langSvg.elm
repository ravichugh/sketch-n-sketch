module LangSvg (valToSvg, shapesToZoneTable) where

import Svg
import Svg.Attributes as A
import Html
import Debug
import Dict
import Set
import String

import Lang exposing (..)
import Utils
import Sync

------------------------------------------------------------------------------

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

------------------------------------------------------------------------------

zones = [
    ("circle",
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
  , ("polygon",
      [ ("TODO", [])
      ])
  ]

------------------------------------------------------------------------------

type alias ShapeId = Int
type alias ShapeKind = String
type alias Attr = String
type alias Locs = Set.Set Loc

shapesToAttrLocs : Val -> Dict.Dict ShapeId (ShapeKind, Dict.Dict Attr Locs)
shapesToAttrLocs v = case v of
  VList vs ->
    let processShape (i,shape) dShapes = case shape of
      VList (VBase (String shape) :: vs') ->
        let processAttr v' dAttrs = case v' of
          VList [VBase (String a), VConst _ tr] ->
            Dict.insert a (Sync.locsOfTrace tr) dAttrs
          -- VList [VBase (String "points"), VList pts] -> -- TODO
          _ -> dAttrs
        in
        let attrs = List.foldl processAttr Dict.empty vs' in
        Dict.insert i (shape, attrs) dShapes
    in
    Utils.foldli processShape Dict.empty vs

shapesToZoneTable : Val -> String
shapesToZoneTable v =
  let foo i (k,d) acc =
    acc ++ "Shape " ++ toString i ++ " " ++ Utils.parens k ++ "\n"
        ++ shapeToZoneInfo k d ++ "\n"
  in
  Dict.foldl foo "" (shapesToAttrLocs v)

shapeToZoneInfo kind d =
  let get = flip justGet d in
  Utils.maybeFind kind zones
    |> Utils.fromJust
    |> List.map (\(s,l) -> "  "
         ++ String.padRight 18 ' ' s
         ++ (List.map get l
              |> Utils.cartProdWithDiff
              |> List.map (Utils.braces << Utils.commas << List.map strLoc_)
              |> Utils.spaces))
    |> Utils.lines
    |> flip (++) "\n"

justGet k d = Utils.fromJust (Dict.get k d)

strLoc_ l =
  let (_,mx) = l in
  if | mx == ""  -> strLoc l
     | otherwise -> mx

