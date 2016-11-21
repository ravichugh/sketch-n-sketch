module InterfaceModel exposing (..)

import Lang exposing (..)
import Types exposing (AceTypeInfo)
import Eval
import Sync exposing (ZoneKey)
import Utils
import LangSvg exposing (RootedIndexedTree, NodeId, ShapeKind)
import ShapeWidgets exposing (ShapeFeature, SelectedShapeFeature, Zone)
import ExamplesGenerated as Examples
import LangUnparser exposing (unparse)
import Ace
import Either exposing (Either(..))

import Dict exposing (Dict)
import Set exposing (Set)
import Char
import Window
import Mouse

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
  , dimensions : Window.Size
  , midOffsetX : Int  -- extra codebox width in vertical orientation
  , midOffsetY : Int  -- extra codebox width in horizontal orientation

  , mouseState : (Maybe Bool, Mouse.Position)
      -- mouseState ~= (Mouse.isDown, Mouse.position)
      --  Nothing    : isDown = False
      --  Just False : isDown = True and position unchanged since isDown became True
      --  Just True  : isDown = True and position has changed since isDown became True

  , syncOptions : Sync.Options
  , caption : Maybe Caption
  , showGhosts : ShowGhosts
  , localSaves : List String
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
  , selectedBlobs : Dict Int NodeId
  , keysDown : List Char.KeyCode
  , randomColor : Int
  , lambdaTools : (Int, List LambdaTool)
  , layoutOffsets : LayoutOffsets
  }

type Mode
  = AdHoc
  | SyncSelect (List PossibleChange)
  | Live Sync.LiveInfo
  | Print RawSvg
      -- TODO might add a print mode where <g BLOB BOUNDS> nodes are removed

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
  | MouseDragLayoutWidget (MouseTrigger (Model -> Model))

  | MouseDragZone
      ZoneKey               -- Left shapeZone, Right widget
      (Maybe                -- Inactive (Nothing) or Active
        ( Sync.LiveTrigger      -- computes program update and highlights
        , (Int, Int)            -- initial click
        , Bool ))               -- dragged at least one pixel

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
  = Hovering ZoneKey
  | LangError String

type alias KeysDown = List Char.KeyCode

type ReplicateKind
  = HorizontalRepeat
  | LinearRepeat
  | RadialRepeat

type Msg
  = ClickZone ZoneKey
  | MouseClickCanvas      -- used to initiate drawing new shape
  | MouseIsDown Bool
  | MousePosition Mouse.Position
  | TickDelta Float -- 60fps time tick, Float is time since last tick
  -- | Sync
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
  | CodeUpdate String -- for basic codebox
  | Run
  | TryParseRun Model
  | StartAnimation
  | Redraw
  | ToggleOutput
  | NextSlide
  | PreviousSlide
  | NextMovie
  | PreviousMovie
  | SwitchOrient
  | ToggleBasicCodeBox
  | StartResizingMid
  | Undo | Redo
  | KeyPress Char.KeyCode
  | KeyDown Char.KeyCode
  | KeyUp Char.KeyCode
  | WindowDimensions Window.Size
  | Noop
  | CleanCode
  | UpdateModel (Model -> Model)
      -- TODO could write other events in terms of UpdateModel
  | AceMsg AceCodeBoxInfo

type alias AceCodeBoxInfo = -- subset of Model
  { code : String
  , codeBoxInfo : CodeBoxInfo
  }

type alias Offsets = {dx:Int, dy:Int}

type alias LayoutOffsets =
  { codeBox : Offsets
  , canvas : Offsets
  , fileToolBox : Offsets
  , codeToolBox : Offsets
  , drawToolBox : Offsets
  , attributeToolBox : Offsets
  , blobToolBox : Offsets
  , outputToolBox : Offsets
  }


initialLayoutOffsets : LayoutOffsets
initialLayoutOffsets =
  let init = { dx = 0, dy = 0 } in
  { codeBox = init
  , canvas = init
  , fileToolBox = init
  , codeToolBox = init
  , drawToolBox = init
  , attributeToolBox = init
  , blobToolBox = init
  , outputToolBox = init
  }


--------------------------------------------------------------------------------

mkLive opts slideNumber movieNumber movieTime e (val, widgets) =
  LangSvg.valToIndexedTree val |> Result.andThen (\slate ->
    Sync.prepareLiveUpdates opts slideNumber movieNumber movieTime e (slate, widgets)
      |> Result.map (Live)
  )

mkLive_ opts slideNumber movieNumber movieTime e  =
  Eval.run e |> Result.andThen (mkLive opts slideNumber movieNumber movieTime e)

--------------------------------------------------------------------------------

liveInfoToHighlights zoneKey model =
  case model.mode of
    Live info -> Sync.yellowAndGrayHighlights zoneKey info
    _         -> []

--------------------------------------------------------------------------------

codeToShow model =
  case model.previewCode of
     Just string -> string
     Nothing     -> model.code

--------------------------------------------------------------------------------

initModel : Model
initModel =
  let
    (name,(_,f)) = Utils.head_ Examples.list
    {e,v,ws}     = f ()
  in
  let unwrap = Utils.fromOk "generating initModel" in
  let (slideCount, movieCount, movieDuration, movieContinue, slate) =
    unwrap (LangSvg.fetchEverything 1 1 0.0 v)
  in
  let liveModeInfo = unwrap (mkLive Sync.defaultOptions 1 1 0.0 e (v, ws)) in
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
    , mode          = liveModeInfo
    , mouseMode     = MouseNothing
    , orient        = Vertical
    , hideCode      = False
    , hideCanvas    = False
    , dimensions    = { width = 1000, height = 800 } -- dummy in case initCmd fails
    , midOffsetX    = 0
    , midOffsetY    = -100
    , mouseState    = (Nothing, {x = 0, y = 0})
    , syncOptions   = Sync.defaultOptions
    , caption       = Nothing
    , showGhosts    = True
    , localSaves    = []
    , startup       = True
    , codeBoxInfo   = { cursorPos = { row = round 0, column = round 0 }
                      , selections = []
                      , highlights = []
                      , annotations = []
                      , tooltips = []
                      }
    , basicCodeBox  = False
    , errorBox      = Nothing
    , genSymCount   = 1 -- starting at 1 to match shape ids on blank canvas
    , tool          = Line Raw
    , hoveredShapes = Set.empty
    , hoveredCrosshairs = Set.empty
    , selectedShapes = Set.empty
    , selectedFeatures = Set.empty
    , selectedBlobs = Dict.empty
    , keysDown      = []
    , randomColor   = 100
    , lambdaTools   = (1, [LambdaBounds (eVar "star")])
    , layoutOffsets = initialLayoutOffsets
    }

