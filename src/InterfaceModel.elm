module InterfaceModel where

import Lang exposing (..)
import Eval
import Sync
import Utils
import LangSvg exposing (RootedIndexedTree, NodeId, ShapeKind, Zone)
import ExamplesGenerated as Examples
import LangUnparser exposing (unparseE)

import List 
import Dict
import Debug
import String
import Char

import Svg
import Lazy

import Task exposing (Task, succeed, andThen)

type alias Code = String

type alias Model =
  { scratchCode : String
  , exName : String
  , code : Code
  , history : (List Code, List Code)
  , inputExp : Maybe Exp
  , slate : RootedIndexedTree
  , mode : Mode
  , mouseMode : MouseMode
  , orient : Orientation
  , midOffsetX : Int  -- extra codebox width in vertical orientation
  , midOffsetY : Int  -- extra codebox width in horizontal orientation
  , showZones : ShowZones
  , syncOptions : Sync.Options
  , editingMode : Maybe Code -- Nothing is False
                             -- Just s is True, where s is previous code
  , caption : Maybe Caption
  , localSaves : List String
  , fieldContents : DialogInfo 
  , startup : Bool
  }

type Mode
  = AdHoc | SyncSelect Code Int PossibleChanges | Live Sync.LiveInfo
  | Print RawSvg | SaveDialog Mode -- SaveDialog saves last mode

type alias DialogInfo = { value : String
                        , hint   : String
                        }

type alias RawSvg = String

type MouseMode
  = MouseNothing
  | MouseResizeMid (Maybe (MouseTrigger (Int, Int)))
  | MouseObject NodeId ShapeKind Zone (Maybe (Code, MouseTrigger (Exp, RootedIndexedTree)))
      -- the Code string is program upon initial zone click

type alias MouseTrigger a = (Int, Int) -> a

type Orientation = Vertical | Horizontal

type alias PossibleChanges =
  ( Int               -- num local changes
  , List (Exp, Val)   -- local changes ++ [structural change, revert change]
  )

-- using Int instead of datatype so serialization/deserialization in
-- InterfaceStorage is more succinct (Enum typeclass would be nice here...)
type alias ShowZones = Int

showZonesModes = 5

[showZonesNone, showZonesBasic, showZonesRot, showZonesColor, showZonesDel] =
  [ 0 .. (showZonesModes - 1) ]

type Caption
  = Hovering (Int, ShapeKind, Zone)
  | LangError String

type Event = CodeUpdate String
           | SelectObject Int ShapeKind Zone
           | MouseUp
           | MousePos (Int, Int)
           | Sync
           | TraverseOption Int -- offset from current index (+1 or -1)
           | SelectOption
           | SwitchMode Mode
           | SelectExample String (() -> {e:Exp, v:Val})
           | Edit
           | Run
           | ToggleOutput
           | ToggleZones
           | SwitchOrient
           | StartResizingMid
           | Undo | Redo
           | KeysDown (List Char.KeyCode)
           | Noop
           | UpdateModel (Model -> Model)
               -- TODO could write other events in terms of UpdateModel

events : Signal.Mailbox Event
events = Signal.mailbox <| CodeUpdate ""

mkLive opts e v = Live <| Sync.prepareLiveUpdates opts e v
mkLive_ opts e  = mkLive opts e (Eval.run e)

editingMode model = case model.editingMode of
  Nothing -> False
  Just _  -> True

sampleModel =
  let
    (name,f) = Utils.head_ Examples.list
    {e,v}    = f ()
  in
    { scratchCode   = Examples.scratch
    , exName        = name
    , code          = unparseE e
    , history       = ([], [])
    , inputExp      = Just e
    , slate         = LangSvg.valToIndexedTree v
    , mode          = mkLive Sync.defaultOptions e v
    , mouseMode     = MouseNothing
    , orient        = Vertical
    , midOffsetX    = 0
    , midOffsetY    = -100
    , showZones     = showZonesNone
    , syncOptions   = Sync.defaultOptions
    , editingMode   = Nothing
    , caption       = Nothing
    , localSaves    = []
    , fieldContents = { value = "", hint = "Input File Name" }
    , startup       = True
    }

