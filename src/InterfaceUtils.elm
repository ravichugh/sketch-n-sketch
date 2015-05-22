-- InterfaceUtils.elm
-- This provide utility and helper functions to Interface.elm
module InterfaceUtils where
--Import the little language and its parsing utilities
--TODO: clean up this import list...some redundancy here
--Import the little language and its parsing utilities
import Lang exposing (..) --For access to what makes up the Vals
import LangParser exposing (parseE, parseV)
import Sync exposing (sync, Triggers)
import Eval exposing (run)
import MainSvg
import Utils
import MicroTests
import LangSvg exposing (IndexedTree, NodeId, ShapeKind)
import VirtualDom

--Core Libraries
import List 
import Dict
import String 
import Graphics.Element as GE 
import Graphics.Collage as GC

--Signaling Libraries
import Mouse 
import Window 

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

-- Model --
--Fields:
-- code            - Text currently in the textbox
--inputExp         - input Expression
-- objects         - The workingVal translated to manipulable SVGs
-- movingObj       - If an object is being moved, which one
-- possibleChanges - The possible new expressions and their associated Vals, 
--                   as from the output of sync
-- mode            - Flag for current mode (ad hoc manipulation/selection of sync
--                   options/live updating)
type alias Model = { code : String
                   , inputExp : Maybe Exp
                   , movingObj : Maybe (NodeId, ShapeKind, Zone, Maybe MouseTrigger)
                   , workingSlate : IndexedTree
                   , triggers : Triggers
                   , possibleChanges : List ((Exp, Val), Float)
                   , mode : Mode
                   }

type alias MouseTrigger = (Int, Int) -> (Exp, IndexedTree)

type Mode = AdHoc | SyncSelect | Live

syncBool m = case m of
  Live       -> False -- TODO: dummy...
  AdHoc      -> False
  SyncSelect -> True

--Event
--CodeUpdate : carries updated string of code with it
--SelectObject : carries an id of an object and an identifying string for a zone
--DeselectObject : carries an id of an object which shall no longer be selected
--                  for alteration.
--MousePos : carries a position of mouse on a down click
--Sync : signals the system to enter selectMode
--SelectOption : carries a possiblechange pane from sync to be displayed as the new
--              console
--Render : display a given val from the code
type Event = CodeUpdate String
           | SelectObject Int ShapeKind Zone
           | DeselectObject Int
           | MousePos (Int, Int)
           | Sync
           | SwitchMode Mode
           | SelectOption ((Exp, Val), Float)
           | Render

--An Object is composed of an svg, list of attribute key/values
type alias Object = (Svg.Svg, List (String, String))

-- rkc TODO: move zone tables from Sync to LangSvg
type alias Zone = String

--A mailbox for signaling the model
events : Signal.Mailbox Event
events = Signal.mailbox <| CodeUpdate ""

--Update Utilities

--given a list of attributes and their new values, update an object and return it
updateObj : List (String, String) -> Object -> Object -> Object
updateObj newattrs (o1, a1) (o2, a2) = case Debug.log "index" (Utils.find_ a1 "index") of
  a ->
    --check for matching indices
    if | ((Utils.find_ a1 "index") == (Utils.find_ a2 "index")) ->
                --update the exsisting attrs
                let updatedattrs = updateAttrStrs newattrs a1
                    --since first 2 attrs are shape/id, remove them for svg creation
                    svgattrs = List.map (\(x,y) -> LangSvg.attr x <| y) (List.drop 2 updatedattrs)
                    --find correct shape function
                    shape = LangSvg.svg (Utils.find_ a1 "shape")
                in ((shape svgattrs []), updatedattrs) 
       | otherwise -> (o2, a2)

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

{-
getFirstAttrs : List Val -> List (Svg.Attribute, (String, String))
getFirstAttrs vals = List.map 
    (\x -> case Debug.log "x" x of
        VList [VBase (String a), VConst i pos] -> ((LangSvg.attr a) <| toString i
              , (a, toString i))
        VList [VBase (String a), VBase (String s)] -> ((LangSvg.attr a) s
              , (a,s))
        VList [VBase (String "points"), VList pts] ->
            let s = Utils.spaces <| List.map
                    (\y -> case y of
                        VList [VConst x1 _, VConst y1 _] ->
                            toString x1 ++ "," ++ toString y1)
                    pts
            in ((LangSvg.attr "points") s, ("points", s)))
    vals
-}

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

upslate = updateSlate
            
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
indexedTreeToVal slate =
  let foo n =
    case n of
      LangSvg.TextNode s -> VList [VBase (String "TEXT"), VBase (String s)]
      LangSvg.SvgNode kind vs1 l2 ->
        let vs2 = List.map (foo << flip Utils.justGet slate) l2 in
        VList [VBase (String kind), VList vs1, VList vs2]
  in
  foo (Utils.justGet 1 slate)

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
            
