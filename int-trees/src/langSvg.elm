module LangSvg (valToHtml, printZoneTable) where

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
type alias LocSet = Set.Set Loc
type alias Locs = List Loc
type alias Zone = String
type ExtraInfo = None | NumPoints Int

type alias Dict0 = Dict.Dict ShapeId (ShapeKind, ExtraInfo, Dict.Dict Attr LocSet)
type alias Dict1 = Dict.Dict ShapeId (ShapeKind, List (Zone, (List Locs)))
type alias Dict2 = Dict.Dict ShapeId (ShapeKind, List (Zone, (Locs, List Locs)))

printZoneTable : Val -> String
printZoneTable v =
  shapesToAttrLocs v         -- Step 1: Val   -> Dict0
    |> shapesToZoneTable     -- Step 2: Dict0 -> Dict1
    |> assignTriggers        -- Step 3: Dict1 -> Dict2
    |> strTable              -- Step 4: Dict2 -> String

-- Step 1 --

shapesToAttrLocs : Val -> Dict0
shapesToAttrLocs v = case v of
  VList (VList [VBase (String "svgAttrs"), _] :: vs) ->
    shapesToAttrLocs (VList vs)
  VList vs ->
    let processShape (i,shape) dShapes = case shape of
      VList (VBase (String shape) :: vs') ->
        let processAttr v' (extra,dAttrs) = case v' of
          VList [VBase (String a), VConst _ tr] ->
            (extra, Dict.insert a (Sync.locsOfTrace tr) dAttrs)
          VList [VBase (String "points"), VList pts] ->
            let acc' =
              Utils.foldli (\(i,vPt) acc ->
                case vPt of
                  VList [VConst _ trx, VConst _ try] ->
                    let (ax,ay) = ("x" ++ toString i, "y" ++ toString i) in
                    acc |> Dict.insert ax (Sync.locsOfTrace trx)
                        |> Dict.insert ay (Sync.locsOfTrace try)) dAttrs pts in
            (NumPoints (List.length pts), acc')
          _ ->
            (extra, dAttrs)
        in
        let (extra,attrs) = List.foldl processAttr (None, Dict.empty) vs' in
        Dict.insert i (shape, extra, attrs) dShapes
    in
    Utils.foldli processShape Dict.empty vs

-- Step 2 --

shapesToZoneTable : Dict0 -> Dict1
shapesToZoneTable d0 =
  let foo i stuff acc =
    let (kind,_,_) = stuff in
    Dict.insert i (kind, shapeToZoneInfo stuff) acc in
  Dict.foldl foo Dict.empty d0

shapeToZoneInfo :
  (ShapeKind, ExtraInfo, Dict.Dict Attr LocSet) -> List (Zone, (List Locs))
shapeToZoneInfo (kind, extra, d) =
  let zones = getZones kind extra in
  let f (s,l) acc =
    let sets =
      l |> List.map (\a -> justGet a d)
        |> Utils.cartProdWithDiff in
    (s, sets) :: acc
  in
  List.foldr f [] zones

justGet k d = Utils.fromJust (Dict.get k d)

getZones : ShapeKind -> ExtraInfo -> List (Zone, List Attr)
getZones kind extra =
  let foo s i = s ++ toString i in
  let xy i    = [foo "x" i, foo "y" i] in
  let pt i    = (foo "Point" i, xy i) in
  case (kind, extra) of
    ("polygon", NumPoints n) ->
      List.map pt [1..n] ++ [("Interior", List.concatMap xy [1..n])]
    _ ->
      Utils.fromJust (Utils.maybeFind kind zones)

-- Step 3 --

-- NOTE: choosing same name setSeen for both accumulators leads
--       to JS undefined error. perhaps due to a shadowing bug?

assignTriggers : Dict1 -> Dict2
assignTriggers d1 =
  let f i (kind,zoneLists) (setSeen1,acc) =
    let g (zone,sets) (setSeen2,acc) =
      case (Utils.findFirst (not << flip Set.member setSeen2) sets, sets) of
        (Nothing, [])         -> (setSeen2, (zone,([],[]))::acc)
        (Nothing, set::sets') -> (setSeen2, (zone,(set,sets'))::acc)
        (Just x,  _)          ->
          let setSeen3 = Set.insert x setSeen2 in
          let acc' = (zone, (x, Utils.removeFirst x sets)) :: acc in
          (setSeen3, acc')
    in
    let (setSeen,zoneLists') = List.foldl g (setSeen1,[]) zoneLists in
    (setSeen, Dict.insert i (kind, List.reverse zoneLists') acc)
  in
  snd <| Dict.foldl f (Set.empty, Dict.empty) d1

-- Step 4 --

strTable : Dict2 -> String
strTable d =
  Dict.toList d
    |> List.map (\(i,(kind,di)) ->
         let s1 = "Shape " ++ toString i ++ " " ++ Utils.parens kind in
         let sRows = List.map strRow di in
         Utils.lines (s1::sRows))
    |> String.join "\n\n"

strRow (zone,(set,sets)) =
     String.padRight 18 ' ' zone
  ++ String.padRight 25 ' ' (if set == [] then "" else strLocs set)
  ++ Utils.spaces (List.map strLocs sets)

strLocs = Utils.braces << Utils.commas << List.map strLoc_

strLoc_ l =
  let (_,mx) = l in
  if | mx == ""  -> strLoc l
     | otherwise -> mx

