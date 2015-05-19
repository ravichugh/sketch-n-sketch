-- InterfaceUtils.elm
-- This provide utility and helper functions to Interface.elm
module InterfaceUtils where
--Import the little language and its parsing utilities
--TODO: clean up this import list...some redundancy here
import Lang exposing (..) --For access to what makes up the Vals
import LangParser exposing (parseE, parseV)
import Sync exposing (sync)
import Eval exposing (run)
import MainSvg
import Utils
import MicroTests
import LangSvg

import List 
import Dict
import String 
import Graphics.Element as GE 
import Graphics.Collage as GC

import Mouse 
import Window 
import Html 
import Html.Attributes as Attr
import Html.Events as Events

import Svg
import Svg.Attributes
import Svg.Events
import Svg.Lazy

import Debug


--- Borrowed from LangSvg.elm ---
funcsSvg = [
    ("circle", Svg.circle)
  , ("line", Svg.line)
  , ("polygon", Svg.polygon)
  , ("rect", Svg.rect)
  ]

funcsAttr = [
    ("cx", Svg.Attributes.cx)
  , ("cy", Svg.Attributes.cy)
  , ("fill", Svg.Attributes.fill)
  , ("height", Svg.Attributes.height)
  , ("points", Svg.Attributes.points)
  , ("r", Svg.Attributes.r)
  , ("stroke", Svg.Attributes.stroke)
  , ("strokeWidth", Svg.Attributes.strokeWidth)
  , ("width", Svg.Attributes.width)
  , ("x", Svg.Attributes.x)
  , ("x1", Svg.Attributes.x1)
  , ("x2", Svg.Attributes.x2)
  , ("y", Svg.Attributes.y)
  , ("y1", Svg.Attributes.y1)
  , ("y2", Svg.Attributes.y2)
  ]

find d s =
  case Utils.maybeFind s d of
    Just f  -> f
    Nothing -> Debug.crash <| "find: " ++ s

attr = find funcsAttr
svg  = find funcsSvg
--- ---

--Update Utilities

updateAttrStrs : List (String, String) -> List (String, String) -> 
                List (String, String)
updateAttrStrs newattrs oldattrs = case newattrs of
    [] -> oldattrs
    (a1, v1) :: xs -> updateAttrStrs xs (replace (a1,v1) oldattrs)

replace : (String, String) -> List (String, String) -> List (String, String)
replace (a1, v1) attrs = case attrs of
    [] -> [(a1,v1)]
    (a2, v2) :: xs -> if | a1 == a2 -> (a1, v1) :: xs
                         | otherwise -> (a2, v2) :: replace (a1, v1) xs

adjustCoords : (Int, Int) -> (Int, Int) -> (Int, Int)
adjustCoords (w,h) (mx, my) = (mx - (w // 2), my)

getFirstAttrs : List Val -> List (Svg.Attribute, (String, String))
getFirstAttrs vals = List.map 
    (\x -> case Debug.log "x" x of
        VList [VBase (String a), VConst i pos] -> ((attr a) <| toString i
              , (a, toString i))
        VList [VBase (String a), VBase (String s)] -> ((attr a) s
              , (a,s))
        VList [VBase (String "points"), VList pts] ->
            let s = Utils.spaces <| List.map
                    (\y -> case y of
                        VList [VConst x1 _, VConst y1 _] ->
                            toString x1 ++ "," ++ toString y1)
                    pts
            in ((attr "points") s, ("points", s)))
    vals

getAttrs : List Val -> List (String, String)
getAttrs vals = List.map
    (\x -> case x of
        VList [VBase (String a), VConst i pos] -> (a, toString i)
        VList [VBase (String a), VBase (String s)] -> (a, s)
        VList [VBase (String "points"), VList pts] ->
            let s = Utils.spaces <| List.map
                    (\y -> case y of
                        VList [VConst x1 _, VConst y1 _] ->
                            toString x1 ++ "," ++ toString y1)
                    pts
            in ("points", s)
    )
    vals

--Takes a list of attributes and pulls out the location
-- information for the constants into a separate list
cleanAttrs : List (String, String) -> ( List (String, String)
                                      , List (String, String))
                                   -> ( List (String, String)
                                      , List (String, String))
cleanAttrs = \l (acc1, acc2) -> case l of
    (key, val) :: xs -> case String.split "|" val of
        [v1, loc] -> cleanAttrs xs
                                ((key, v1) :: acc1
                                , (String.append key "loc", loc) :: acc2)
        _         -> cleanAttrs xs
                                ((key, val) :: acc1, acc2)
    []        -> (acc1, acc2)

updateVal : Val -> String -> (String, String) -> Val
updateVal v index (attrname, attrval) = case Debug.log "update v" v of
    VList [VBase (String "svg"), VList [], VList vs] -> VList [VBase (String "svg"), VList [], VList <| flip List.map (Utils.mapi (\s -> s) vs) <| \v1 -> 
        case v1 of
            (i, VList (VBase (String shape) :: VList vs' :: cs)) -> 
                VList (VBase (String shape) :: 
                    List.append (changeAttr i vs' index (attrname, attrval)) cs)
        ]
        
updateSlate : LangSvg.NodeId -> 
                (String, String) -> 
                LangSvg.IndexedTree ->
                LangSvg.IndexedTree
updateSlate id newattr nodes = case Dict.get id nodes of
    Nothing   -> nodes
    Just node -> case node of
        LangSvg.TextNode x -> nodes
        LangSvg.SvgNode shape attrs children -> 
            let newnode = LangSvg.SvgNode shape (updateAttrs newattr attrs) children
            in Dict.insert id newnode nodes
            
updateAttrs : (String, String) -> List Val -> List Val
updateAttrs (attr, value) vals = case vals of
    VList [VBase (String a), VConst ix pos] :: vs ->
        if | a == attr -> case String.toFloat value of
                Ok f -> VList [VBase (String a), VConst f pos] :: vs
           | otherwise -> 
                VList [VBase (String a), VConst ix pos] :: updateAttrs (attr, value) vs
    VList [VBase (String a), VBase (String s)] :: vs ->
        if | a == attr ->
                VList [VBase (String a), VBase (String value)] :: vs
           | otherwise ->
                VList [VBase (String a), VBase (String s)] :: updateAttrs (attr,value) vs
    [] -> []

indexedTreeToVal : LangSvg.IndexedTree -> Val
indexedTreeToVal slate = VList []

--helper function for updateVal
changeAttr : Int -> List Val -> String -> (String, String) -> List Val
changeAttr i vs' index (attrname, attrval) =
    if | toString i == index ->
          List.map (\x -> case x of
                    VList [VBase (String a), VConst ix pos] ->
                      if | (a == attrname) ->
                            case (String.toFloat attrval) of
                              Ok f -> VList [VBase (String attrname), VConst f pos]
                         | otherwise -> VList [VBase (String a), VConst ix pos]
                    VList [VBase (String a), VBase (String s)] ->
                      if | (a == attrname) ->
                            VList [VBase (String a), VBase (String attrval)]
                         | otherwise -> VList [VBase (String a), VBase (String s)]
                    _ -> x
                    ) vs'
       | otherwise -> vs'
            
