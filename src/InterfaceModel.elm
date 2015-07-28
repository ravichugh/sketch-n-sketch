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

import Svg
import Lazy

import Task exposing (Task, succeed, andThen)

type alias Model =
  { scratchCode : String
  , exName : String
  , code : String
  , inputExp : Maybe Exp
  , slate : RootedIndexedTree
  , mode : Mode
  , mouseMode : MouseMode
  , orient : Orientation
  , midOffsetX : Int  -- extra codebox width in vertical orientation
  , midOffsetY : Int  -- extra codebox width in horizontal orientation
  , showZones : ShowZones
  , syncOptions : Sync.Options
  , editingMode : Bool
  , caption : Maybe Caption
  , localSaves : List String
  , fieldContents : DialogInfo 
  , startup : Bool
  }

type Mode
  = AdHoc | SyncSelect Int PossibleChanges | Live Sync.LiveInfo
  | Print RawSvg | SaveDialog Mode -- SaveDialog saves last mode

type alias DialogInfo = { value : String
                        , hint   : String
                        }

type alias RawSvg = String

type MouseMode
  = MouseNothing
  | MouseObject (NodeId, ShapeKind, Zone, Maybe (MouseTrigger (Exp, RootedIndexedTree)))
  | MouseResizeMid (Maybe (MouseTrigger (Int, Int)))

type alias MouseTrigger a = (Int, Int) -> a

type Orientation = Vertical | Horizontal

type alias PossibleChanges =
  ( Int               -- num local changes
  , List (Exp, Val)   -- local changes ++ [structural change, revert change]
  )

-- using Int instead of datatype so serialization/deserialization in
-- InterfaceStorage is more succinct (Enum typeclass would be nice here...)
type alias ShowZones = Int
showZonesModes = 3
showZonesNone  = 0
showZonesBasic = 1
showZonesRot   = 2

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
           | ToggleThawed
           | SwitchOrient
           | StartResizingMid
           | Noop
           | UpdateModel (Model -> Model)
               -- TODO could write other events in terms of UpdateModel

events : Signal.Mailbox Event
events = Signal.mailbox <| CodeUpdate ""

mkLive opts e v = Live <| Sync.prepareLiveUpdates opts e v
mkLive_ opts e  = mkLive opts e (Eval.run e)

sampleModel =
  let
    (name,f) = Utils.head_ Examples.list
    {e,v}    = f ()
  in
    { scratchCode   = Examples.scratch
    , exName        = name
    , code          = unparseE e
    , inputExp      = Just e
    , slate         = LangSvg.valToIndexedTree v
    , mode          = mkLive Sync.defaultOptions e v
    , mouseMode     = MouseNothing
    , orient        = Vertical
    , midOffsetX    = 0
    , midOffsetY    = -100
    , showZones     = showZonesNone
    , syncOptions   = Sync.defaultOptions
    , editingMode   = False
    , caption       = Nothing
    , localSaves    = []
    , fieldContents = { value = "", hint = "Input File Name" }
    , startup       = True
    }

