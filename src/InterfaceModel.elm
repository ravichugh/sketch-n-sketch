module InterfaceModel where

import Lang exposing (..)
import Types exposing (AceTypeInfo)
import Eval
import Sync
import Utils
import LangSvg exposing (RootedIndexedTree, NodeId, ShapeKind)
import ShapeWidgets exposing (ShapeFeature, SelectedShapeFeature, Zone)
import ExamplesGenerated as Examples
import LangUnparser exposing (unparse)
import Ace

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

  , mouseState : (Maybe Bool, (Int, Int))
      -- mouseState ~= (Mouse.isDown, Mouse.position)
      --  Nothing    : isDown = False
      --  Just False : isDown = True and position unchanged since isDown became True
      --  Just True  : isDown = True and position has changed since isDown became True

  , syncOptions : Sync.Options
  , caption : Maybe Caption
  , showGhosts : ShowGhosts
  , localSaves : List String
  , fieldContents : DialogInfo
  , startup : Bool
  , codeBoxInfo : CodeBoxInfo
  , basicCodeBox : Bool
  , errorBox : Maybe String
  , genSymCount : Int
  , tool : Tool
  , hoveredShapes : Set.Set NodeId
  , hoveredCrosshairs : Set.Set (NodeId, ShapeFeature, ShapeFeature)
  , selectedShapes : Set.Set NodeId
  , selectedFeatures : Set.Set SelectedShapeFeature
  -- line/g ids assigned by blobs function
  , selectedBlobs : Dict.Dict Int NodeId
  , keysDown : List Char.KeyCode
  , randomColor : Int
  , lambdaTools : (Int, List LambdaTool)
  }

type Mode
  = AdHoc
  | SyncSelect (List PossibleChange)
  | Live Sync.LiveInfo
  | Print RawSvg
      -- TODO might add a print mode where <g BLOB BOUNDS> nodes are removed
  | SaveDialog Mode -- SaveDialog saves last mode

type alias DialogInfo = { value : String
                        , hint   : String
                        }

type alias CodeBoxInfo =
  { cursorPos : Ace.Pos
  , selections : List Ace.Range
  , highlights : List Ace.Highlight
  , annotations : List Ace.Annotation
  , tooltips : List Ace.Tooltip
  }

type alias RawSvg = String

type MouseMode
  = MouseNothing

  | MouseResizeMid (Maybe (MouseTrigger (Int, Int)))

  | MouseObject NodeId ShapeKind Zone
      (Maybe                -- Inactive (Nothing) or Active
        ( Sync.LiveTrigger      -- computes program update and highlights
        , (Int, Int)            -- initial click
        , Bool ))               -- dragged at least one pixel

  | MouseSlider Widget
      (Maybe ( MouseTrigger (Result String (Exp, Val, RootedIndexedTree, Widgets)) ))
      -- may add info for hilites later

  | MouseDrawNew (List (KeysDown, (Int, Int)))
      -- invariant on length n of list of points:
      --   for line/rect/ellipse, n == 0 or n == 2
      --   for polygon/path,      n >= 0
      --   for helper dot,        n == 0 or n == 1
      --   for lambda,            n == 0 or n == 2

type alias MouseTrigger a = (Int, Int) -> a

type Orientation = Vertical | Horizontal

type alias PossibleChange = (Exp, Val, RootedIndexedTree, Code)
  -- TODO this should have Widgets...

-- type alias ShowZones = Bool
-- type ShowWidgets = HideWidgets | ShowAnnotatedWidgets | ShowAllWidgets
type alias ShowGhosts = Bool

type Tool
  = Cursor
  | Line ShapeToolKind
  | Rect ShapeToolKind
  | Oval ShapeToolKind
  | Poly ShapeToolKind
  | Path ShapeToolKind
  | Text
  | HelperDot
  | HelperLine
  | Lambda

type ShapeToolKind
  = Raw
  | Stretchy
  | Sticky

type LambdaTool
  = LambdaBounds Exp
  | LambdaAnchor Exp

type Caption
  = Hovering (Int, ShapeKind, Zone)
  | LangError String

type alias KeysDown = List Char.KeyCode

type ReplicateKind
  = HorizontalRepeat
  | LinearRepeat
  | RadialRepeat

type Event = SelectObject Int ShapeKind Zone
           | MouseClickCanvas      -- used to initiate drawing new shape
           | MouseIsDown Bool
           | MousePosition (Int, Int)
           | TickDelta Float -- 60fps time tick, Float is time since last tick
           | Sync
           | PreviewCode (Maybe Code)
           | SelectOption PossibleChange
           | CancelSync
           | DigHole
           | MakeEqual
           | MakeEquidistant
           | GroupBlobs
           | AbstractBlobs
           | DuplicateBlobs
           | MergeBlobs
           | ReplicateBlob ReplicateKind
           | SwitchMode Mode
           | SelectExample String (() -> {e:Exp, v:Val, ws:Widgets, ati:AceTypeInfo})
           | Run
           | StartAnimation
           | Redraw
           | ToggleOutput
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
           | KeysDown KeysDown
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
events = Signal.mailbox <| Noop

--------------------------------------------------------------------------------

mkLive opts slideNumber movieNumber movieTime e (val, widgets) =
  let slate = LangSvg.valToIndexedTree val in
  Sync.prepareLiveUpdates opts slideNumber movieNumber movieTime e (slate, widgets)
  |> Result.map (Live)

mkLive_ opts slideNumber movieNumber movieTime e  =
  Eval.run e `Result.andThen` mkLive opts slideNumber movieNumber movieTime e

--------------------------------------------------------------------------------

liveInfoToHighlights id zone model =
  case model.mode of
    Live info -> Sync.yellowAndGrayHighlights id zone info
    _         -> []

--------------------------------------------------------------------------------

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
  let (slideCount, movieCount, movieDuration, movieContinue, slate) =
    Utils.fromOk "generating sample model" <| LangSvg.fetchEverything 1 1 0.0 v
  in
  let code = unparse e in
    { scratchCode   = Examples.scratch
    , exName        = name
    , code          = code
    , previewCode   = Nothing
    , history       = ([code], [])
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
    , slate         = slate
    , widgets       = ws
    , mode          = Utils.fromOk "mkLive sample model" <| mkLive Sync.defaultOptions 1 1 0.0 e (v, ws)
    , mouseMode     = MouseNothing
    , orient        = Vertical
    , hideCode      = False
    , hideCanvas    = False
    , dimensions    = (1000, 800) -- dummy in case foldp' didn't get initial value
    , midOffsetX    = 0
    , midOffsetY    = -100
    , mouseState    = (Nothing, (0, 0))
    , syncOptions   = Sync.defaultOptions
    , caption       = Nothing
    , showGhosts    = True
    , localSaves    = []
    , fieldContents = { value = "", hint = "Input File Name" }
    , startup       = True
    , codeBoxInfo   = { cursorPos = { row = round 0, column = round 0 }
                      , selections = []
                      , highlights = []
                      , annotations = []
                      , tooltips = []
                      }
    , basicCodeBox  = False
    , errorBox      = Nothing
    -- starting at 1 to match shape ids on blank canvas
    -- , genSymCount   = 0
    , genSymCount   = 1
    , tool          = Line Raw
    , hoveredShapes = Set.empty
    , hoveredCrosshairs = Set.empty
    , selectedShapes = Set.empty
    , selectedFeatures = Set.empty
    , selectedBlobs = Dict.empty
    , keysDown      = []
    , randomColor   = 100
    , lambdaTools   = (1, [LambdaBounds (eVar "star")])
    }

