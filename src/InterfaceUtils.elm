-- InterfaceUtils.elm
-- This provide utility and helper functions to Interface.elm
module InterfaceUtils where
import Lang exposing (..) --For access to what makes up the Vals
import Sync exposing (Triggers)
import Utils
import LangSvg exposing (IndexedTree, NodeId, ShapeKind, Attr, Zone)

import List 
import Dict
import Debug

import Svg

type alias Model =
  { code : String
  , inputExp : Maybe Exp
  , movingObj : Maybe (NodeId, ShapeKind, Zone, Maybe MouseTrigger)
  , workingSlate : IndexedTree
  , mode : Mode
  }

type alias MouseTrigger = (Int, Int) -> (Exp, IndexedTree)

type alias PossibleChanges = List ((Exp, Val), Float)

type Mode = AdHoc | SyncSelect PossibleChanges | Live Triggers

syncBool m = case m of
  Live _       -> False -- TODO: dummy...
  AdHoc        -> False
  SyncSelect _ -> True

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

type alias Object = (Svg.Svg, List Attr)

events : Signal.Mailbox Event
events = Signal.mailbox <| CodeUpdate ""

adjustCoords : (Int, Int) -> (Int, Int) -> (Int, Int)
adjustCoords (w,h) (mx, my) = (mx - (w // 2), my)
        
upslate : LangSvg.NodeId -> (String, LangSvg.AVal) -> LangSvg.IndexedTree -> LangSvg.IndexedTree
upslate id newattr nodes = case Dict.get id nodes of
    Nothing   -> Debug.crash "upslate"
    Just node -> case node of
        LangSvg.TextNode x -> nodes
        LangSvg.SvgNode shape attrs children -> 
            let newnode = LangSvg.SvgNode shape (updateAttrs newattr attrs) children
            in Dict.insert id newnode nodes

updateAttrs : (String, LangSvg.AVal) -> List Attr -> List Attr
updateAttrs (k1, value) vals =
  case value of
    LangSvg.ANum i1 ->
      case vals of
        [] -> []
        (k0, LangSvg.ANum i0) :: vs ->
          if | k0 == k1  -> (k0, aNum i1) :: vs
             | otherwise -> (k0, aNum i0) :: updateAttrs (k1, value) vs

aNum = LangSvg.ANum

indexedTreeToVal : LangSvg.IndexedTree -> Val
indexedTreeToVal slate =
  let foo n =
    case n of
      LangSvg.TextNode s -> VList [VBase (String "TEXT"), VBase (String s)]
      LangSvg.SvgNode kind l1 l2 ->
        let vs1 = List.map LangSvg.valOfAttr l1 in
        let vs2 = List.map (foo << flip Utils.justGet slate) l2 in
        VList [VBase (String kind), VList vs1, VList vs2]
  in
  foo (Utils.justGet 1 slate)

