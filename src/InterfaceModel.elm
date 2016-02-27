module InterfaceModel where

import Lang exposing (..)
import Eval
import Sync
import Utils
import LangSvg exposing (RootedIndexedTree, NodeId, ShapeFeature, ShapeKind, Zone)
import ExamplesGenerated as Examples
import LangUnparser exposing (unparse)
import OurParser2 as P

import List
import Debug
import String
import Dict
import Set
import Char

import Svg
import Lazy

import Task exposing (Task, succeed, andThen)

type alias Code = String

type alias Model =
  { scratchCode : String
  , exName : String
  , code : Code
  , previewCode: Maybe Code
  , history : (List Code, List Code)
  , inputExp : Exp
  , inputVal : Val
  , slideNumber : Int
  , slideCount : Int
  , movieNumber : Int
  , movieCount : Int
  , movieTime : Float
  , movieDuration : Float
  , movieContinue : Bool
  , runAnimation : Bool
  , syncSelectTime : Float
  , slate : RootedIndexedTree
  , widgets : Widgets
  , mode : Mode
  , mouseMode : MouseMode
  , orient : Orientation
  , hideCode : Bool
  , hideCanvas : Bool
  , dimensions : (Int, Int)
  , midOffsetX : Int  -- extra codebox width in vertical orientation
  , midOffsetY : Int  -- extra codebox width in horizontal orientation
  , showZones : ShowZones
  , syncOptions : Sync.Options
  , editingMode : Maybe Code -- Nothing means not editing
                             -- Just s is True, where s is previous code
  , caption : Maybe Caption
  , showWidgets : Bool
  , localSaves : List String
  , fieldContents : DialogInfo
  , startup : Bool
  , codeBoxInfo : CodeBoxInfo
  , basicCodeBox : Bool
  , errorBox : Maybe String
  , genSymCount : Int
  , toolType : ToolType
  , selectedFeatures : Set.Set (NodeId, ShapeFeature)
  , keysDown : List Char.KeyCode
  }

type Mode
  = AdHoc
  | SyncSelect (List PossibleChange)
  | Live Sync.LiveInfo
  | Print RawSvg
  | SaveDialog Mode -- SaveDialog saves last mode

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
             , MouseTrigger (Exp, Val, SubstMaybeNum, RootedIndexedTree, Widgets) ))
  | MouseSlider Widget
      (Maybe ( Code                        -- the program upon initial click
             , MouseTrigger (Exp, Val, RootedIndexedTree, Widgets) ))
      -- may add info for hilites later
  | MouseDrawNew ShapeKind (List (Int, Int))
      -- invariant on length n of list of points:
      --   for line/rect/ellipse, n == 0 or n == 2
      --   for polygon,           n >= 0
      --   for helper dot,        n == 0 or n == 1
      --   for lambda,            n == 0 or n == 2

type alias MouseTrigger a = (Int, Int) -> a

type Orientation = Vertical | Horizontal

type alias PossibleChange = (Exp, Val, RootedIndexedTree, Code)

-- using Int instead of datatype so serialization/deserialization in
-- InterfaceStorage is more succinct (Enum typeclass would be nice here...)
type alias ShowZones = Int

showZonesModeCount = 5

showZonesModes = [ 0 .. (showZonesModeCount - 1) ]

(showZonesNone, showZonesBasic, showZonesSelect, showZonesExtra, showZonesDel) =
  Utils.unwrap5 showZonesModes

type ToolType
  = Cursor | SelectAttrs | SelectShapes
  | Line | Rect | Oval
  | Poly | Path | Text
  | HelperDot
  | HelperLine
  | Lambda Ident

type Caption
  = Hovering (Int, ShapeKind, Zone)
  | LangError String

type Event = CodeUpdate String -- TODO this doesn't help with anything
           | SelectObject Int ShapeKind Zone
           | MouseClickCanvas      -- used to initiate drawing new shape
           | MouseClick (Int, Int) -- used to add points to a new polygon
           | MouseUp
           | MousePos (Int, Int)
           | TickDelta Float -- 60fps time tick, Float is time since last tick
           | Sync
           | PreviewCode (Maybe Code)
           | SelectOption PossibleChange
           | CancelSync
           | RelateAttrs -- not using UpdateModel, since want to define handler in Controller
           | DigHole
           | RelateShapes
           | SwitchMode Mode
           | SelectExample String (() -> {e:Exp, v:Val, ws:Widgets})
           | Edit
           | Run
           | StartAnimation
           | Redraw
           | ToggleOutput
           | SelectZonesMode Int
           | NextSlide
           | PreviousSlide
           | NextMovie
           | PreviousMovie
           | SwitchOrient
           | InstallSaveState
           | RemoveDialog Bool String
           | ToggleBasicCodeBox
           | StartResizingMid
           | Undo | Redo
           | KeysDown (List Char.KeyCode)
           | WindowDimensions (Int, Int)
           | Noop
           | UpdateFieldContents DialogInfo
           | CleanCode
           | UpdateModel (Model -> Model)
               -- TODO could write other events in terms of UpdateModel
           | MultiEvent (List Event)
           --A state such that we're waiting for a response from Ace
           | WaitRun
           | WaitSave String
           | WaitClean
           | WaitCodeBox

events : Signal.Mailbox Event
events = Signal.mailbox <| CodeUpdate ""

--------------------------------------------------------------------------------

mkLive opts slideNumber movieNumber movieTime e v = Live <| Sync.prepareLiveUpdates opts slideNumber movieNumber movieTime e v
mkLive_ opts slideNumber movieNumber movieTime e  = mkLive opts slideNumber movieNumber movieTime e (fst (Eval.run e))
  -- TODO maybe put Val into model (in addition to slate)
  --   so that don't need to re-run in some calling contexts

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

codeToShow model =
  case model.previewCode of
     Just string -> string
     Nothing     -> model.code

--------------------------------------------------------------------------------

sampleModel : Model
sampleModel =
  let
    (name,_,f) = Utils.head_ Examples.list
    {e,v,ws}   = f ()
  in
  let (slideCount, movieCount, movieDuration, movieContinue, indexedTree) = LangSvg.fetchEverything 1 1 0.0 v in
    { scratchCode   = Examples.scratch
    , exName        = name
    , code          = unparse e
    , previewCode   = Nothing
    , history       = ([], [])
    , inputExp      = e
    , inputVal      = v
    , slideNumber   = 1
    , slideCount    = slideCount
    , movieNumber   = 1
    , movieCount    = movieCount
    , movieTime     = 0.0
    , movieDuration = movieDuration
    , movieContinue = movieContinue
    , runAnimation  = True
    , syncSelectTime = 0.0
    , slate         = indexedTree
    , widgets       = ws
    , mode          = mkLive Sync.defaultOptions 1 1 0.0 e v
    , mouseMode     = MouseNothing
    , orient        = Vertical
    , hideCode      = False
    , hideCanvas    = False
    , dimensions    = (1000, 800) -- dummy in case foldp' didn't get initial value
    , midOffsetX    = 0
    , midOffsetY    = -100
    , showZones     = showZonesNone
    , syncOptions   = Sync.defaultOptions
    , editingMode   = Nothing
    , caption       = Nothing
    , showWidgets   = True
    , localSaves    = []
    , fieldContents = { value = "", hint = "Input File Name" }
    , startup       = True
    , codeBoxInfo   = { cursorPos = { row = round 0, column = round 0 }
                      , selections = []
                      , highlights = []
                      }
    , basicCodeBox  = False
    , errorBox      = Nothing
    -- starting at 1 to match shape ids on blank canvas
    -- , genSymCount   = 0
    , genSymCount   = 1
    , toolType      = Cursor
    , selectedFeatures = Set.empty
    , keysDown      = []
    }

