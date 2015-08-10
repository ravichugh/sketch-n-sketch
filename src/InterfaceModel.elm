module InterfaceModel where

import Lang exposing (..)
import Eval
import Sync
import Utils
import LangSvg exposing (RootedIndexedTree, NodeId, ShapeKind, Zone)
import ExamplesGenerated as Examples
import LangUnparser exposing (unparseE)
import OurParser2 as P

import List 
import Debug
import String
import Dict
import Set

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
  , codeBoxInfo : CodeBoxInfo
  }

type Mode
  = AdHoc | SyncSelect Int PossibleChanges | Live Sync.LiveInfo
  | Print RawSvg | SaveDialog Mode -- SaveDialog saves last mode

type alias DialogInfo = { value : String
                        , hint   : String
                        }

type alias CodeBoxInfo =
  { cursorPos : AcePos
  , selections : List Range
  , highlights : List Highlight
  }

type alias Highlight =
  { range : Range, color : String }

type alias AcePos = { row : Int, column : Int }
type alias Range = { start : AcePos, end : AcePos }

type alias RawSvg = String

type MouseMode
  = MouseNothing
  | MouseResizeMid (Maybe (MouseTrigger (Int, Int)))
  | MouseObject NodeId ShapeKind Zone
      (Maybe ( Code                        -- the program upon initial zone click
             , Maybe (SubstPlus, LocSet)   -- loc-set assigned (live mode only)
             , MouseTrigger (Exp, SubstMaybeNum, RootedIndexedTree) ))

type alias MouseTrigger a = (Int, Int) -> a

type Orientation = Vertical | Horizontal

type alias PossibleChanges =
  ( Int               -- num local changes
  , List (Exp, Val)   -- local changes ++ [structural change, revert change]
  )

-- using Int instead of datatype so serialization/deserialization in
-- InterfaceStorage is more succinct (Enum typeclass would be nice here...)
type alias ShowZones = Int

showZonesModes = 4

[showZonesNone, showZonesBasic, showZonesRot, showZonesColor] =
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
           | Noop
           | UpdateModel (Model -> Model)
               -- TODO could write other events in terms of UpdateModel

events : Signal.Mailbox Event
events = Signal.mailbox <| CodeUpdate ""

--------------------------------------------------------------------------------

mkLive opts e v = Live <| Sync.prepareLiveUpdates opts e v
mkLive_ opts e  = mkLive opts e (Eval.run e)

editingMode model = case model.editingMode of
  Nothing -> False
  Just _  -> True

liveInfoToHighlights id zone model =
  case model.mode of
    Live info ->
      let subst = info.initSubst in
      Maybe.withDefault [] <|
        flip Utils.bindMaybe (Dict.get id info.assignments) <| \d ->
        flip Utils.bindMaybe (Dict.get zone d) <| \(yellowLocs,grayLocs) ->
        Just
          <| List.map (makeHighlight subst yellow) (Set.toList yellowLocs)
          ++ List.map (makeHighlight subst gray) (Set.toList grayLocs)
    _ ->
      []

--------------------------------------------------------------------------------

gray        = "lightgray"
yellow      = "khaki"
green       = "limegreen"
red         = "salmon"

acePos : P.Pos  -> AcePos
acePos p = { row = p.line, column = p.col }

aceRange : P.WithInfo a -> Range
aceRange x = { start = acePos x.start, end = acePos x.end }

makeHighlight : SubstPlus -> String -> Loc -> Highlight
makeHighlight subst color (locid,_,_) =
  case Dict.get locid subst of
    Just n  -> { color = color, range = aceRange n }
    Nothing -> Debug.crash "makeHighlight: locid not in subst"

--------------------------------------------------------------------------------

sampleModel : Model
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
    , codeBoxInfo   = { cursorPos = { row = round 0, column = round 0 }
                      , selections = []
                      , highlights = []
                      }
    }

