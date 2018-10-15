module Model exposing (..)

import Updatable exposing (Updatable)
import Lang exposing (..)
import LeoParser
import Info exposing (..)
import Types exposing (AceTypeInfo)
import Eval
import EvalUpdate
import Sync exposing (ZoneKey)
import Utils
import LangSvg exposing (RootedIndexedTree, NodeId, ShapeKind)
import ShapeWidgets exposing (SelectableFeature, SelectablePoint)
import ExamplesGenerated as Examples
import DefaultIconTheme
import DependenceGraph exposing (ScopeGraph)
import Ace
import DeuceWidgets exposing (DeuceState)
import Either exposing (Either(..))
import Keys
import Svg
import LangSvg exposing (attr)
import Syntax exposing (Syntax)
import LangUnparser
import File exposing (Filename, File, FileIndex)
import Solver
-- import Update -- move valToString to ValUnparser
-- import ValUnparser
import History exposing (History)
import Config

import Dict exposing (Dict)
import Set exposing (Set)
import Char
import Time
import Window
import Mouse
import Html exposing (Html)
import Html.Attributes as Attr
import VirtualDom
import LangTools
import LangUtils
import Pos exposing (Pos)

import ImpureGoodies

type alias Code = String

type alias TrackedValues =
  { code : Code
  , selectedDeuceWidgets : List DeuceWidgets.DeuceWidget
  , mbKeyboardFocusedWidget : Maybe DeuceWidgets.DeuceWidget
  }

type alias Position = { col : Int, line : Int }

type alias ViewState =
  { menuActive : Bool
  }

type alias DiffPreview = List Exp

type alias Preview =
  Maybe (Code, DiffPreview, Result String (Val, Widgets, RootedIndexedTree))

type TextSelectMode
    -- Only match the exact range
  = Strict
    -- Match the smallest superset range
  | Superset
    -- Match the largest subset range
  | Subset
    -- Match the largest subset range, but also allow additional surrounding
    -- whitespace characters
  | SubsetExtra
    -- Select only atomic values ("words") like constants and variables
  | Word

type DeuceRightClickMenuMode =
  ShowPossible

type alias DeuceKeyboardPopupInfo =
  { title : String
  , text : String
  , textToTransformationResults : String -> List TransformationResult
  , smartCompleteSelection : String
  }

type alias PopupPanelPositions =
  { deuce : (Int, Int)
  , editCode : (Int, Int)
  , deuceRightClickMenu : (Int, Int)
  , autoOutputTools : (Int, Int)
  }

type ColorScheme
  = Light
  | Dark

type CodeToolsMenuMode
  = CTAll
  | CTActive
  | CTDisabled

type CodeEditorMode
  = CEText
  | CEDeuceClick
  | CETypeInspector

type alias Model =
  { code : Code
  , lastParsedCode : Code
  , lastRunCode : Code
  , runFailuresInARowCount : Int
  , preview : Preview
  , previewdiffs : Maybe DiffPreview
  , previewdiffsDelay: Int
  , history : History TrackedValues
  , inputExp : Exp
  , inputVal : Val
  , inputEnv : Env
  , slideNumber : Int
  , slideCount : Int
  , movieNumber : Int
  , movieCount : Int
  , movieTime : Float
  , movieDuration : Float
  , movieContinue : Bool
  , runAnimation : Bool
  -- TODO rename slate/slateCount
  , slate : RootedIndexedTree
  , slateCount : Int -- trying to count number of times translated to DOM
  , addDummyDivAroundCanvas : Maybe Bool
      -- Just False and Just True force previous VirtualDom to be
      -- invalidated by adding a dummy div around canvas
  , widgets : Widgets
  , outputMode : OutputMode
  , syncMode : SyncMode
  , liveSyncInfo : Sync.LiveInfo
      -- info for when syncMode == TracesAndTriggers _
  , autoSyncDelay: Int
  , caretPosition: Maybe Int
      -- delay (in milliseconds) for when syncMode == ValueBackprop True
  , mouseMode : MouseMode
  , dimensions : Window.Size

  , mouseState : (Maybe Bool, Mouse.Position, Maybe Clickable)
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
  , outputCanvasInfo : OutputCanvasInfo
  , basicCodeBox : Bool
  , errorBox : Maybe String
  , genSymCount : Int
  , tool : Tool
  , hoveredShapes : Set.Set NodeId
  , hoveredCrosshairs : Set.Set SelectablePoint
  , selectedShapes : Set.Set NodeId
  , selectedFeatures : Set.Set SelectableFeature
  -- line/g ids assigned by blobs function
  , selectedBlobs : Dict Int NodeId
  , keysDown : List Char.KeyCode
  , autoSynthesis : Bool
  , problemsSentToSolver : List (Solver.Problem, Msg) -- Equation(s) sent to the solver server and and the message that should be re-run upon a reply. Shouldn't ever be more than a singleton in practice.
  , solutionsCache : Solver.SolutionsCache
  , synthesisResultsDict : Dict String (List SynthesisResult)
  , hoveredSynthesisResultPathByIndices : List Int
  , renamingInOutput : Maybe (PId, String)
  , randomColor : Int
  , lambdaTools : List LambdaTool
  , layoutOffsets : LayoutOffsets
  , needsSave : Bool
  , lastSaveState : Maybe Code
  , backupRecovery : Bool
  , filename : Filename
  , fileIndex : FileIndex
  , dialogBoxes : Set Int
  , filenameInput : String
  , fileToDelete : Filename
  , pendingFileOperation : Maybe Msg
  , fileOperationConfirmed : Bool
  , icons : Dict String (Html Msg)
  , showAllDeuceWidgets : Bool
  , hoveringCodeBox : Bool
  , scopeGraph : ScopeGraph
  , deuceState : DeuceWidgets.DeuceState
  , deuceToolsAndResults : List (List CachedDeuceTool)
  , deuceToolResultPreviews : DeuceToolResultPreviews
  , selectedDeuceTool : Maybe CachedDeuceTool
  , showOnlyBasicTools : Bool
  , viewState : ViewState
  , toolMode : ShapeToolKind
  , popupPanelPositions : PopupPanelPositions
  , mbDeuceKeyboardInfo : Maybe DeuceKeyboardPopupInfo
  , deuceRightClickMenuMode : Maybe DeuceRightClickMenuMode
  , enableDeuceBoxSelection : Bool
  , enableDeuceTextSelection : Bool
  , codeToolsMenuMode : CodeToolsMenuMode
  , outputToolsMenuMode : Bool
  , textSelectMode : TextSelectMode
  , enableTextEdits : Updatable Bool
  , allowMultipleTargetPositions : Bool
  , mainResizerX : Maybe Int
  , savedSelections : Maybe (List Ace.Range)
  , deucePopupPanelAbove : Bool
  , colorScheme : ColorScheme
  , pendingGiveUpMsg : Maybe Msg
  , giveUpConfirmed : Bool
  -- would be nice if lastSelectedTemplate = Nothing meant that
  -- this file did not start from a template (it started with a file
  -- from local storage)
  , lastSelectedTemplate : Maybe (String, Code)
  , valueEditorString : String
  , htmlEditorString: Maybe String
  , updatedValue: Maybe (Result String Val)
  , shapeUpdatesViaZones : Dict NodeId (List LangSvg.Attr)
  , syntax : Syntax
  , codeEditorMode : CodeEditorMode
  , deuceOverlayCache : Maybe (Html Msg)
  , doTypeChecking : Bool
  , isDeuceTextBoxFocused : Bool
  , needsToFocusOn : Maybe String
  }

type OutputMode
  = Graphics
  | HtmlText RawHtml
      -- TODO might add a print mode where <g BLOB BOUNDS> nodes are removed
  | ValueText
  | PrintScopeGraph (Maybe String)
                      -- Nothing        after sending renderDotGraph request
                      -- Just dataURI   after receiving the encoded image

isHtmlText outputMode =
  case outputMode of
    HtmlText _ -> True
    _          -> False

type SyncMode
  = TracesAndTriggers Bool
      -- True for live sync
      -- False for delayed live sync
  | ValueBackprop Bool
      -- True for auto-sync with delay of model.autoSyncDelay
      -- False for manual sync

valueBackpropPopupMenuTitle =
  "Output Editor"

valueBackpropToolName =
  -- "Update for New Output"
  "Update Program"

type alias CodeBoxInfo =
  { cursorPos : Ace.Pos
  , selections : List Ace.Range
  , highlights : List Ace.Highlight
  , annotations : List Ace.Annotation
  , tooltips : List Ace.Tooltip
  , fontSize : Int
  , lineHeight : Float
  , characterWidth : Float
  , offsetLeft: Float
  , offsetHeight: Float
  , gutterWidth: Float
  , firstVisibleRow: Int
  , lastVisibleRow: Int
  , marginTopOffset: Float
  , marginLeftOffset: Float
  , scrollerTop : Float
  , scrollerLeft : Float
  , scrollerWidth : Float
  , scrollerHeight : Float
  , contentLeft : Float
  , scrollTop : Float
  , scrollLeft : Float
  }

type alias OutputCanvasInfo =
  { scrollTop : Float
  , scrollLeft : Float
  }

type alias RawHtml = String

type Clickable
  = PointWithProvenance Val Val

type MouseMode
  = MouseNothing
  | MouseDrag (Mouse.Position -> Mouse.Position -> Model -> Model)
  | MouseDragLayoutWidget (MouseTrigger (Model -> Model))

  | MouseDragZone
      ZoneKey               -- (nodeId, shapeKind, zoneName)
      (Int, Int)            -- initial click
      Bool                  -- dragged at least one pixel
      Sync.LiveTrigger      -- computes program update and highlights

  | MouseDragSelect
      Mouse.Position              -- initial mouse position
      (Set.Set NodeId)            -- initial selected shapes
      (Set.Set SelectableFeature) -- initial selected features
      (Dict Int NodeId)           -- initial selected blobs

  | MouseDrawNew ShapeBeingDrawn
      -- invariant on length n of list of points:
      --   for line/rect/ellipse, n == 0 or n == 2
      --   for polygon/path,      n >= 0
      --   for helper dot,        n == 0 or n == 1
      --   for lambda,            n == 0 or n == 2

  | MouseDownInCodebox Mouse.Position

type alias MouseTrigger a = (Int, Int) -> a

mousePosition : Model -> Mouse.Position
mousePosition model = Utils.snd3 model.mouseState

isMouseDown : Model -> Bool
isMouseDown model =
  case model.mouseState of
    (Just _, _, _) -> True
    _              -> False

isShapeBeingDrawnSnappingToVal : Model -> Val -> Bool
isShapeBeingDrawnSnappingToVal model val =
  -- Only singular locs for now.
  case model.mouseMode of
    MouseDrawNew shapeBeingDrawn ->
      snapValsOfShapeBeingDrawn shapeBeingDrawn
      |> List.any
          (\valOfShapeBeingDraw ->
            valToNum val == valToNum valOfShapeBeingDraw &&
            valEId val   == valEId valOfShapeBeingDraw -- Maybe overly strict
          )

    _ ->
      False

snapValsOfShapeBeingDrawn : ShapeBeingDrawn -> List Val
snapValsOfShapeBeingDrawn shapeBeingDrawn =
  case shapeBeingDrawn of
    NoPointsYet                            -> []
    TwoPoints _ _                          -> []
    PolyPoints _                           -> []
    PathPoints _                           -> []
    Offset1DFromExisting _ NoSnap _        -> []
    Offset1DFromExisting _ (SnapVal val) _ -> [val]


type Snap
  = NoSnap
  | SnapVal Val

type alias IntSnap = (Int, Snap) -- like NumTr

type alias PointWithSnap = (IntSnap, IntSnap)

-- Oldest/base point is last in all of these.
type ShapeBeingDrawn
  = NoPointsYet -- For shapes drawn by dragging, no points until the mouse moves after the mouse-down.
  | TwoPoints (KeysDown, (Int, Int)) (KeysDown, (Int, Int)) -- KeysDown should probably be refactored out
  | PolyPoints (List PointWithSnap)
  | PathPoints (List (KeysDown, (Int, Int))) -- KeysDown should probably be replaced with a more semantic represenation of point type
  | Offset1DFromExisting (Int, Int) Snap (Val, Val) -- Snap is separate here because it is unidimensional


-- type alias ShowZones = Bool
-- type ShowWidgets = HideWidgets | ShowAnnotatedWidgets | ShowAllWidgets
type alias ShowGhosts = Bool

type Tool
  = Cursor
  | PointOrOffset
  | Text
  | Line ShapeToolKind
  | Rect ShapeToolKind
  | Oval ShapeToolKind
  | Poly ShapeToolKind
  | Path ShapeToolKind
  | HelperLine
  | Lambda Int -- 1-based index of selected LambdaTool
  | Function Ident -- Generalized lambda tool, hopefully will subsume Lambda tool

type ShapeToolKind
  = Raw
  | Stretchy
  | Sticky

type LambdaTool
  = LambdaBounds Exp
  | LambdaAnchor
      Exp
      (Maybe { width: Int, height: Int, xAnchor: Int, yAnchor: Int})

type Caption
  = Hovering ZoneKey
  | LangError String

type alias KeysDown = List Char.KeyCode

type ReplicateKind
  = HorizontalRepeat
  | LinearRepeat
  | RadialRepeat

{- NOTE: moved to Lang

type SynthesisResult =
  SynthesisResult { description : String
                  , exp         : Exp
                  , diffs       : List Exp -- These exps can be virtual, only the start and end position matter
                  , isSafe      : Bool -- Is this transformation considered "safe"?
                  , sortKey     : List Float -- For custom sorting criteria. Sorts ascending.
                  , children    : Maybe (List SynthesisResult) -- Nothing means not calculated yet.
                  }

synthesisResult description exp =
  SynthesisResult <|
    { description = description
    , exp         = exp
    , diffs       = []
    , isSafe      = True
    , sortKey     = []
    , children    = Nothing
    }
-}

synthesisResultDiffs description exp diffs =
  SynthesisResult <|
      { description = description
      , exp         = exp
      , diffs       = diffs
      , isSafe      = True
      , sortKey     = []
      , children    = Nothing
      , nextLazy    = Nothing
      }
synthesisResultDiffsLazy description exp diffs sibling =
  SynthesisResult <|
      { description = description
      , exp         = exp
      , diffs       = diffs
      , isSafe      = True
      , sortKey     = []
      , children    = Nothing
      , nextLazy    = Just sibling
      }

synthesisResultsNotEmpty : Model -> String -> Bool
synthesisResultsNotEmpty model resultsKey =
  Utils.getWithDefault resultsKey [] model.synthesisResultsDict
  |> (not << List.isEmpty)

mapResultSafe f (SynthesisResult result) =
  SynthesisResult { result | isSafe = f result.isSafe }

setResultSafe isSafe synthesisResult =
  mapResultSafe (\_ -> isSafe) synthesisResult

isResultSafe (SynthesisResult {isSafe}) =
  isSafe

resultDescription (SynthesisResult {description}) =
  description

resultExp (SynthesisResult {exp}) =
  exp

setResultDescription description (SynthesisResult result) =
  SynthesisResult { result | description = description }

setResultSortKey sortKey (SynthesisResult result) =
  SynthesisResult { result | sortKey = sortKey }


type Msg
  = ResponseFromSolver String
  | Msg String (Model -> Model)
  -- would be nice to deprecate Msg in favor of just NewModelAndCmd...
  | NewModelAndCmd String (Model -> (Model, Cmd Msg))


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
  , moreBlobToolBox : Offsets
  , outputToolBox : Offsets
  , animationToolBox : Offsets
  , textToolBox : Offsets
  , deuceToolBox : {pinned:Bool, offsets:Offsets}
  , synthesisResultsSelectBox : Offsets
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
  , moreBlobToolBox = init
  , outputToolBox = init
  , animationToolBox = init
  , textToolBox = init
  , deuceToolBox = {pinned=False, offsets=init}
  , synthesisResultsSelectBox = init
  }

--------------------------------------------------------------------------------

setAllUpdated : Model -> Model
setAllUpdated model =
  let
    old =
      model
  in
    { model
        | enableTextEdits =
            Updatable.setUpdated old.enableTextEdits
    }

--------------------------------------------------------------------------------

type DialogBox
  = New
  | SaveAs
  | Open
  | AlertSave
  | ImportCode

dialogBoxes =
  Utils.mapi0 identity
    [ New
    , SaveAs
    , Open
    , AlertSave
    , ImportCode
    ]

dbToInt : DialogBox -> Int
dbToInt db =
  case Utils.findFirst (Tuple.second >> (==) db) dialogBoxes of
    Just (i, _) -> i
    Nothing     -> Debug.crash <| "Undefined Dialog Box Type: " ++ toString db

intToDb : Int -> DialogBox
intToDb n =
  case Utils.maybeFind n dialogBoxes of
    Just db -> db
    Nothing -> Debug.crash <| "Undefined Dialog Box Id: " ++ toString n

openDialogBox : DialogBox -> Model -> Model
openDialogBox db model =
  { model | dialogBoxes = Set.insert (dbToInt db) model.dialogBoxes }

closeDialogBox : DialogBox -> Model -> Model
closeDialogBox db model =
  { model | dialogBoxes = Set.remove (dbToInt db) model.dialogBoxes }

cancelFileOperation : Model -> Model
cancelFileOperation model =
  closeDialogBox
    AlertSave
    { model
      | pendingFileOperation = Nothing
      , fileOperationConfirmed = False
    }

closeAllDialogBoxes : Model -> Model
closeAllDialogBoxes model =
  let
    noFileOpsModel =
      cancelFileOperation model
  in
    { noFileOpsModel | dialogBoxes = Set.empty }

isDialogBoxShowing : DialogBox -> Model -> Bool
isDialogBoxShowing db model =
  Set.member (dbToInt db) model.dialogBoxes

anyDialogShown : Model -> Bool
anyDialogShown =
  not << Set.isEmpty << .dialogBoxes

--------------------------------------------------------------------------------

showDeuceRightClickMenu
  : Int -> Int -> DeuceRightClickMenuMode -> Model -> Model
showDeuceRightClickMenu offsetX offsetY menuMode model =
  let
    mousePos =
      mousePosition model
    oldPopupPanelPositions =
      model.popupPanelPositions
  in
    { model
        | deuceRightClickMenuMode =
            Just menuMode
        , popupPanelPositions =
            { oldPopupPanelPositions
                | deuceRightClickMenu =
                    ( mousePos.x + offsetX
                    , mousePos.y + offsetY
                    )
            }
    }

hideDeuceRightClickMenu : Model -> Model
hideDeuceRightClickMenu model =
  { model | deuceRightClickMenuMode = Nothing }

deuceRightClickMenuShown : Model -> Bool
deuceRightClickMenuShown model =
  model.deuceRightClickMenuMode /= Nothing

--------------------------------------------------------------------------------

configurationPanelShown : Model -> Bool
configurationPanelShown model =
  model.selectedDeuceTool /= Nothing

--------------------------------------------------------------------------------

importCodeFileInputId = "import-code-file-input"

{- NOTE: moved to Lang

--------------------------------------------------------------------------------
-- Predicates
--------------------------------------------------------------------------------

type PredicateValue
    -- Good to go, and can accept no more arguments
  = FullySatisfied
    -- Good to go, but can accept more arguments if necessary
  | Satisfied
    -- Not yet good to go, but with more arguments may be okay
  | Possible
    -- Not good to go, and no additional arguments will make a difference
  | Impossible

-- NOTE: Descriptions should be an *action* in sentence case with no period at
--       the end, e.g.:
--         * Select a boolean value
--         * Select 4 integers
type alias Predicate =
  { description : String
  , value : PredicateValue
  }

predicateFullySatisfied : Predicate -> Bool
predicateFullySatisfied pred =
  case pred.value of
    FullySatisfied ->
      True
    Satisfied ->
      False
    Possible ->
      False
    Impossible ->
      False

predicateSatisfied : Predicate -> Bool
predicateSatisfied pred =
  case pred.value of
    FullySatisfied ->
      True
    Satisfied ->
      True
    Possible ->
      False
    Impossible ->
      False

predicatePossible : Predicate -> Bool
predicatePossible pred =
  case pred.value of
    FullySatisfied ->
      True
    Satisfied ->
      True
    Possible ->
      True
    Impossible ->
      False

predicateImpossible : Predicate -> Bool
predicateImpossible pred =
  case pred.value of
    FullySatisfied ->
      False
    Satisfied ->
      False
    Possible ->
      False
    Impossible ->
      True
-}

--------------------------------------------------------------------------------
-- Deuce Tools
--------------------------------------------------------------------------------

{- NOTE: moved to Lang

type alias DeuceSelections =
  ( List (LocId, (WS, Num, Loc, WidgetDecl))  -- number literals
  , List (EId, (WS, EBaseVal))                -- other base value literals
  , List EId                                  -- expressions (including literals)
  , List PathedPatternId                      -- patterns
  , List EId                                  -- equations
  , List ExpTargetPosition                    -- expression target positions
  , List PatTargetPosition                    -- pattern target positions
  )

type alias DeuceTransformation =
  () -> List SynthesisResult

type alias DeuceTool =
  { name : String
  , func : Maybe DeuceTransformation
  , reqs : List Predicate -- requirements to run the tool
  , id : String -- unique, unchanging identifier
  }

-}

type alias CachedDeuceTool =
  (DeuceTool, List TransformationResult, Bool)

type alias DeuceToolResultPreviews =
  Dict
    (List Int)   -- indexed by path
    ( Preview
    , String     -- CSS class
    )

--------------------------------------------------------------------------------


-- Elm typechecker should properly subtype Model < { slideNumber : Int, movieNumber : Int, movieTime : Float }
-- but for some reason it doesn't.
runAndResolve : Model -> Exp -> Result String (Val, Widgets, Env, RootedIndexedTree, Code)
runAndResolve model exp =
  runAndResolve_
    { movieNumber = model.movieNumber
    , movieTime   = model.movieTime
    , slideNumber = model.slideNumber
    , syntax      = model.syntax
    }
    exp


runAndResolve_ : { slideNumber : Int, movieNumber : Int, movieTime : Float, syntax : Syntax } -> Exp -> Result String (Val, Widgets, Env, RootedIndexedTree, Code)
runAndResolve_ model exp =
  let thunk () =
    EvalUpdate.runWithEnv model.syntax exp
    |> Result.andThen (\((val, widgets), env) -> slateAndCode model (exp, val)
    |> Result.map (\(slate, code) -> (val, widgets, env, slate, code)))
  in
  ImpureGoodies.crashToError thunk
  |> Utils.unwrapNestedResult


slateAndCode : { slideNumber : Int, movieNumber : Int, movieTime : Float, syntax : Syntax } -> (Exp, Val) -> Result String (RootedIndexedTree, Code)
slateAndCode old (exp, val) =
  LangSvg.resolveToRootedIndexedTree old.syntax old.slideNumber old.movieNumber old.movieTime val
  |> Result.map (\slate -> (slate, Syntax.unparser old.syntax exp))

--------------------------------------------------------------------------------

mkLive : Syntax -> Sync.Options -> Int -> Int -> Float -> Exp -> (Val, Widgets) -> Result String Sync.LiveInfo
mkLive syntax opts slideNumber movieNumber movieTime e (val, widgets) =
  LangSvg.resolveToRootedIndexedTree syntax slideNumber movieNumber movieTime val |> Result.andThen (\slate ->
  Sync.prepareLiveUpdates opts e (slate, widgets)                           |> Result.andThen (\liveInfo ->
    Ok liveInfo
  ))

--------------------------------------------------------------------------------

liveInfoToHighlights zoneKey model =
  case model.outputMode of
    Graphics -> Sync.yellowAndGrayHighlights zoneKey model.liveSyncInfo
    _        -> []

--------------------------------------------------------------------------------

codeToShow model =
  case model.preview of
     Just (string, _, _) -> string
     Nothing          -> model.code

--------------------------------------------------------------------------------

strLambdaTool lambdaTool =
  let strExp = String.trim << LangUnparser.unparse in
  case lambdaTool of
    LambdaBounds e -> Utils.parens <| "\\bounds. " ++ strExp e ++ " bounds"
    LambdaAnchor e _ -> Utils.parens <| "\\anchor. " ++ strExp e ++ " anchor"

--------------------------------------------------------------------------------

prependDescription newPrefix synthesisResult =
  { synthesisResult | description = (newPrefix ++ synthesisResult.description) }

--------------------------------------------------------------------------------

bufferName = ""

bufferFilename : Model -> Filename
bufferFilename model =
  { name =
      bufferName
  , extension =
      Syntax.sourceExtension model.syntax
  }

type PrettyFileNameOption
  = WithExtension
  | WithoutExtension

prettyFilename includeExtension model =
  if model.filename.name == bufferName then
    let
      prettyTemplate =
        case model.lastSelectedTemplate of
          Just (template, _) ->
            " (" ++ template ++ ")"
            -- if template /= Examples.initTemplate then
            --   " (" ++ template ++ ")"
            -- else
            --   ""
          Nothing ->
            ""
    in
      "Untitled" ++ prettyTemplate
  else
    let
      suffix =
        case includeExtension of
          WithExtension ->
            "." ++ File.fileExtensionToString model.filename.extension
          WithoutExtension ->
            ""
    in
      model.filename.name ++ suffix

--------------------------------------------------------------------------------

-- if this looks okay, use it in more places in View in place of model.needsSave
reallyNeedsSave : Model -> Bool
reallyNeedsSave model =
  -- use of bufferName, following prettyFilename...
  case (model.filename.name == bufferName, model.lastSelectedTemplate) of
    (True, Just (templateName, templateCode)) ->
      String.trimRight model.code /= String.trimRight templateCode
    _ ->
      model.needsSave

--------------------------------------------------------------------------------

iconNames = Dict.keys DefaultIconTheme.icons

--------------------------------------------------------------------------------

starLambdaTool = LambdaBounds (eVar "star")

starLambdaToolIcon = lambdaToolIcon starLambdaTool

lambdaToolIcon tool =
  { filename =
      { name =
          Utils.naturalToCamelCase (strLambdaTool tool)
      , extension =
          File.LittleIcon
      }
  , contents = case tool of
      LambdaBounds func ->
        "(svgViewBox 100 100 (" ++ LangUnparser.unparse func ++ " [10 10 90 90]))"
      LambdaAnchor func Nothing ->
        "(svgViewBox 100 100 (" ++ LangUnparser.unparse func ++ " [10 10]))"
      LambdaAnchor func (Just viewBoxAndAnchor) ->
        Utils.parens <|
          Utils.spaces <|
            [ "svgViewBox"
            , toString viewBoxAndAnchor.width
            , toString viewBoxAndAnchor.height
            , Utils.parens <|
                Utils.spaces <|
                  [ LangUnparser.unparse func
                  , Utils.bracks <|
                      Utils.spaces
                        [ toString viewBoxAndAnchor.xAnchor
                        , toString viewBoxAndAnchor.yAnchor
                        ]
                  ]
            ]
  }

--------------------------------------------------------------------------------

needsParse m =
  m.code /= m.lastParsedCode

needsRun m =
  m.code /= m.lastRunCode

valueEditorNeedsCallUpdate model =
  LangUtils.valToString model.inputVal /= model.valueEditorString

htmlEditorNeedsCallUpdate model =
  case model.outputMode of
    HtmlText htmlCode -> case model.htmlEditorString of
      Nothing -> False
      Just h -> htmlCode /= h
    _ -> False
  
domEditorNeedsCallUpdate model =
  not <| Utils.maybeIsEmpty model.updatedValue

--------------------------------------------------------------------------------

oneSafeResult : Exp -> List TransformationResult
oneSafeResult newExp =
  List.singleton <|
    basicTransformationResult ("NO DESCRIPTION B/C SELECTED AUTOMATICALLY") newExp

--------------------------------------------------------------------------------

deuceShortcutActive : Model -> Bool
deuceShortcutActive model =
  let
    shiftDown =
      List.member Keys.keyShift model.keysDown
  in
    Utils.and
      [ model.codeEditorMode == CEText
      , model.enableDeuceBoxSelection
      , not <| deuceRightClickMenuShown model
      , shiftDown
      ]

deuceActive : Model -> Bool
deuceActive model =
  Utils.and
    [ model.enableDeuceBoxSelection
    , not <| needsParse model
    , Utils.or
        [ deuceShortcutActive model
        , model.codeEditorMode == CEDeuceClick
        , model.codeEditorMode == CETypeInspector
        , configurationPanelShown model
        ]
    ]

modeActive : Model -> CodeEditorMode -> Bool
modeActive model mode =
  if deuceShortcutActive model then
    mode == CEDeuceClick
  else
    mode == model.codeEditorMode

--------------------------------------------------------------------------------

snippet : Ace.Range -> String -> String
snippet range =
     String.lines
  >> Utils.slice range.start.row (range.end.row + 1)
  >> Utils.mapLast (String.left range.end.column)
  >> Utils.mapHead (String.dropLeft range.start.column)
  >> String.concat

isRangeEqual : Ace.Range -> Ace.Range -> Bool
isRangeEqual =
  (==)

isSubsetRange : Ace.Range -> Ace.Range -> Bool
isSubsetRange innerRange outerRange =
  let
    startGood =
      (outerRange.start.row < innerRange.start.row) ||
      (outerRange.start.row == innerRange.start.row
        && outerRange.start.column <= innerRange.start.column)
    endGood =
      (innerRange.end.row < outerRange.end.row) ||
      (innerRange.end.row == outerRange.end.row
        && innerRange.end.column <= outerRange.end.column)
  in
    startGood && endGood

matchingRange : TextSelectMode -> Code -> Ace.Range -> List (Ace.Range, CodeObject) -> Maybe CodeObject
matchingRange textSelectMode code selectedRange =
  let
    (fold, matcher) =
      case textSelectMode of
        Strict ->
          (List.foldl, isRangeEqual)
        Superset ->
          (List.foldl, isSubsetRange)
        Subset ->
          (List.foldr, flip isSubsetRange)
        SubsetExtra ->
          ( List.foldr
          , ( \sr r ->
              if isSubsetRange r sr then
                let
                  validAdditionalSelectedCharacter : Char -> Bool
                  validAdditionalSelectedCharacter c =
                    c == ' ' || c == '\n'

                  snippetValid : String -> Bool
                  snippetValid =
                    String.all validAdditionalSelectedCharacter

                  beginValid : Bool
                  beginValid =
                    snippetValid << (flip snippet code) <|
                      { start =
                          sr.start
                      , end =
                          r.start
                      }

                  endValid : Bool
                  endValid =
                    snippetValid << (flip snippet code) <|
                      { start =
                          r.end
                      , end =
                          sr.end
                      }
                in
                  beginValid && endValid
              else
                False
            )
          )
        Word ->
          ( List.foldl
          , isSubsetRange
          )
  in
    fold
      ( \(range, val) previousVal ->
          case (matcher selectedRange range, previousVal) of
            (False, _) -> previousVal
            (True, Nothing) -> Just val
            (True, Just prevCO) ->
              if (not << isTarget) val || isTarget prevCO
              then Just val
              else previousVal
      )
      Nothing

-- Note that WithInfo is 1-indexed, but Ace.Range is 0-indexed.
rangeFromInfo : WithInfo a -> Ace.Range
rangeFromInfo info =
  { start =
      { row =
          info.start.line - 1
      , column =
          info.start.col - 1
      }
  , end =
      { row =
          info.end.line - 1
      , column =
          info.end.col - 1
      }
  }

codeObjectFromSelection : Bool -> Model -> Maybe CodeObject
codeObjectFromSelection allowSingleSelection model =
  case model.codeBoxInfo.selections of
    -- Note that when nothing is selected, Ace treats the current selection
    -- as just the range [cursorPos, cursorPos]. Thus, this pattern handles
    -- all the cases that we need.
    [ selection ] ->
      let
        textSelectMode : TextSelectMode
        textSelectMode =
          if
            allowSingleSelection &&
            selection.start == selection.end
          then
            Word
          else
            model.textSelectMode

        theFilter : CodeObject -> Bool
        theFilter codeObject =
          let
            notDef =
              case codeObject of
                E e ->
                  case (unwrapExp e) of
                    (ELet _ Def _ _ _) ->
                      False
                    _ ->
                      True
                _ ->
                  True
            textSelectable =
              Lang.isTextSelectable codeObject
            word =
              Lang.isWord codeObject
            activeFilters =
              [ notDef, textSelectable
              ] ++
              ( if textSelectMode == Word then
                  [ word ]
                else
                  []
              )
          in
            Utils.and activeFilters
      in
        matchingRange
          textSelectMode
          model.code
          selection
          ( E model.inputExp
             |> flattenToCodeObjects
             |> List.filter theFilter
             |> List.map
                  ( \codeObject ->
                      ( rangeFromInfo << extractInfoFromCodeObject <|
                          codeObject
                      , codeObject
                      )
                  )
          )
    _ ->
      Nothing

--------------------------------------------------------------------------------

getAllSelected : Model -> List DeuceWidgets.DeuceWidget
getAllSelected model =
  let
    selected = model.deuceState.selectedWidgets
    mbKeyboardFocusedWidget = model.deuceState.mbKeyboardFocusedWidget
    root = model.inputExp
  in
  case mbKeyboardFocusedWidget of
    Nothing ->
      selected
    Just focused ->
      if
        selected |>
          List.any (\slctd ->
            DeuceWidgets.isSubWidget root slctd focused ||
            DeuceWidgets.isSubWidget root focused slctd)
      then
        selected
      else
        focused :: selected

noCodeWidgetsSelected : Model -> Bool
noCodeWidgetsSelected model =
  List.isEmpty <| getAllSelected model

nothingSelectedInOutput : Model -> Bool
nothingSelectedInOutput model =
  Set.isEmpty model.selectedFeatures &&
  Set.isEmpty model.selectedShapes &&
  Dict.isEmpty model.selectedBlobs

allowOnlySingleSelection : Model -> Bool
allowOnlySingleSelection model =
  model.codeEditorMode == CETypeInspector


--------------------------------------------------------------------------------

deuceKeyboardPopupPanelTextBoxId : String
deuceKeyboardPopupPanelTextBoxId = "deuce-keyboard-popup-panel-text-box-7331"

deuceKeyboardPopupPanelShown : Model -> Bool
deuceKeyboardPopupPanelShown model =
  model.enableDeuceBoxSelection &&
  model.deuceState.mbKeyboardFocusedWidget /= Nothing &&
  model.mbDeuceKeyboardInfo /= Nothing

deucePopupPanelShown : Model -> Bool
deucePopupPanelShown model =
  Utils.and
    [ model.enableDeuceBoxSelection
    , Utils.or
        [ Utils.and
            [ not <| noCodeWidgetsSelected model
            , not <| deuceRightClickMenuShown model
            , not <| configurationPanelShown model
            ]
        , Utils.and
            [ model.codeEditorMode == CETypeInspector
            , not <| List.isEmpty model.deuceState.hoveredWidgets
            ]
        ]
    ]

autoOutputToolsPopupPanelShown : Model -> Bool
autoOutputToolsPopupPanelShown model =
  case model.syncMode of
    TracesAndTriggers _ ->
      -- for now, predicating the Output Tools menu on live sync mode
      Utils.or
        [ not <| Set.isEmpty model.selectedFeatures
        , not <| Set.isEmpty model.selectedShapes
        , not <| Dict.isEmpty model.selectedBlobs
        ]

    ValueBackprop _ ->
      needsValueBackprop model

needsValueBackprop model =
  case model.syncMode of
    TracesAndTriggers _ ->
      False

    ValueBackprop _ ->
      Utils.or
        [ model.outputMode == ValueText && valueEditorNeedsCallUpdate model
        , isHtmlText model.outputMode && htmlEditorNeedsCallUpdate model
        , domEditorNeedsCallUpdate model
        ]

--------------------------------------------------------------------------------

historyUpdateCondition : TrackedValues -> TrackedValues -> Bool
historyUpdateCondition previousValues currentValues =
  Utils.or
    [ -- trimRight to tolerate differences in newlines at the end
      String.trimRight previousValues.code
        /= String.trimRight currentValues.code
    -- Might need some sort of sorting because not a set?
    , previousValues.selectedDeuceWidgets
        /= currentValues.selectedDeuceWidgets
    , previousValues.mbKeyboardFocusedWidget
        /= currentValues.mbKeyboardFocusedWidget
    ]

modelCommit
  :  Code -> List DeuceWidgets.DeuceWidget -> Maybe DeuceWidgets.DeuceWidget
  -> History TrackedValues -> History TrackedValues
modelCommit code dws dwkf =
  History.commit
    historyUpdateCondition
    { code = code, selectedDeuceWidgets = dws, mbKeyboardFocusedWidget = dwkf }

modelModify
  :  Code -> List DeuceWidgets.DeuceWidget -> Maybe DeuceWidgets.DeuceWidget
  -> History TrackedValues -> Maybe (History TrackedValues)
modelModify code dws dwkf =
  History.modify
    { code = code, selectedDeuceWidgets = dws, mbKeyboardFocusedWidget = dwkf }

--------------------------------------------------------------------------------

-- The values that should trigger a Deuce cache update
type alias DeuceCacheTriggerValues =
  { code : String
  , inputExp : Exp
  , mbKeyboardFocusedWidget : Maybe DeuceWidgets.DeuceWidget
  , selectedWidgets : List DeuceWidgets.DeuceWidget
  -- , hoveredWidgets : List DeuceWidgets.DeuceWidget
  , colorScheme : ColorScheme
  , lineHeight : Float
  , characterWidth : Float
  , contentLeft : Float
  }

deuceCacheTriggerValues : Model -> DeuceCacheTriggerValues
deuceCacheTriggerValues m =
  { code = m.code
  , inputExp = m.inputExp
  , mbKeyboardFocusedWidget = m.deuceState.mbKeyboardFocusedWidget
  , selectedWidgets = m.deuceState.selectedWidgets
  -- , hoveredWidgets = m.deuceState.hoveredWidgets
  , colorScheme = m.colorScheme
  , lineHeight = m.codeBoxInfo.lineHeight
  , characterWidth = m.codeBoxInfo.characterWidth
  , contentLeft = m.codeBoxInfo.contentLeft
  }

deuceCacheNeedsUpdate : Model -> Model -> Bool
deuceCacheNeedsUpdate oldModel newModel =
  newModel.deuceOverlayCache == Nothing
    || deuceCacheTriggerValues oldModel /= deuceCacheTriggerValues newModel

--------------------------------------------------------------------------------

initColorScheme : ColorScheme
initColorScheme = Light

initTemplate =
  case LeoParser.preludeNotParsed of
    Nothing -> Examples.initTemplate
    Just msg -> Examples.badPreludeTemplate

loadTemplate name =
  let mbTheTemplate =  Utils.maybeFind name Examples.list |> Maybe.map Tuple.second in
  case mbTheTemplate of
    Nothing -> \() -> Err <| name ++ " not found"
    Just theTemplate ->
  if name /= Examples.badPreludeTemplate then
     theTemplate
  else
     case LeoParser.preludeNotParsed of
      Nothing -> theTemplate
      Just msg ->
        let _ = Debug.log "error in template" () in
        \() ->
          let t = theTemplate () in
          flip Result.map t <| \t ->
          let e = t.e in
          case childExpsExtractors e of -- e == ["pre" ...]
            ([a, b, content1], rebuilder1) -> case childExpsExtractors content1 of --content1 == [...]
              ([content2], rebuilder2) -> case childExpsExtractors content2 of --content2 == ["TEXT", """ ... """]
                ([c, content3], rebuilder3) -> case childExpsExtractors content3 of  --content3 == """ ... """
                  ([content4], rebuilder4) -> case (unwrapExp content4) of
                     EBase sp4 (EString q content) ->
                       let newContent4 = replaceE__ content4 <| EBase sp4 <| EString q <| (content |> String.split "ERROR_HERE" |> String.join (msg |> String.split "\"" |> String.join "\\\"")) in
                       { t | e = rebuilder1 [a, b, rebuilder2 [rebuilder3 [c, rebuilder4 [newContent4]]]] }
                     _ -> let _ = Debug.log "Not a string" (unwrapExp content4) in  t
                  _ -> let _ = Debug.log "Not a \"\"\"...\"\"\"" (unwrapExp content3) in  t
                _ -> let _ = Debug.log "Not a ['TEXT'...]" (unwrapExp content2) in  t
              _ ->  let _ = Debug.log "Not an [...]" (unwrapExp content1) in t
            _ ->  let _ = Debug.log "Not an ['pre'...]" (unwrapExp e) in t

initModel : Model
initModel =
  let f    = loadTemplate initTemplate () in
  let
    {e,v,ws,env} = case f of
         Err msg -> {e=eStr "Example did not parse", v=(builtinVal "" (VBase (VString (msg)))), ws=[], env=[]}
         Ok ff -> ff
  in
  let unwrap = Utils.fromOk "generating initModel" in
  let (slideCount, movieCount, movieDuration, movieContinue, slate) =
    unwrap (LangSvg.fetchEverything Syntax.Little 1 1 0.0 v)
  in
  let liveSyncInfo = unwrap (mkLive Syntax.Little Sync.defaultOptions 1 1 0.0 e (v, ws)) in
  let code = LangUnparser.unparse e in
    { code          = code
    , lastParsedCode = code
    , lastRunCode   = code
    , runFailuresInARowCount = 0
    , preview       = Nothing
    , previewdiffs  = Nothing
    , previewdiffsDelay = 1000 --ms
    , history       = History.begin { code = code, selectedDeuceWidgets = [], mbKeyboardFocusedWidget = Nothing }
    , inputExp      = e
    , inputVal      = v
    , inputEnv      = env
    , slideNumber   = 1
    , slideCount    = slideCount
    , movieNumber   = 1
    , movieCount    = movieCount
    , movieTime     = 0.0
    , movieDuration = movieDuration
    , movieContinue = movieContinue
    , runAnimation  = True
    , slate         = slate
    , slateCount    = 1
    , addDummyDivAroundCanvas = Nothing
    , widgets       = ws
    , outputMode    = Graphics
    , syncMode      = if Config.elmConfDemo
                        then TracesAndTriggers True
                        else ValueBackprop False
                        -- Native/outputCanvas.js:
                        --   initializeOutputCanvas() assumes initModel.syncMode == ValueBackprop b
                        --   set enableAutoUpdate = true/false to match b
    , liveSyncInfo  = liveSyncInfo
    , autoSyncDelay = 1000
    , caretPosition = Nothing -- Caret position in the outputCanvas
    , mouseMode     = MouseNothing
    , dimensions    = { width = 1000, height = 800 } -- dummy in case initCmd fails
    , mouseState    = (Nothing, {x = 0, y = 0}, Nothing)
    , syncOptions   = Sync.defaultOptions
    , caption       = (case f of
        Err msg -> Just (LangError msg)
        _ -> Nothing
      )
    , showGhosts    = True
    , localSaves    = []
    , startup       = True
    , codeBoxInfo   = { cursorPos = { row = round 0, column = round 0 }
                      , selections = []
                      , highlights = []
                      , annotations = []
                      , tooltips = []
                      , fontSize = 16
                      , characterWidth = 10.0
                      , lineHeight = 20.0
                      , offsetLeft = 10.0
                      , offsetHeight = 10.0
                      , gutterWidth = 50.0
                      , firstVisibleRow = 0
                      , lastVisibleRow = 10
                      , marginTopOffset = 0.0
                      , marginLeftOffset = 0.0
                      , scrollerTop = 0.0
                      , scrollerLeft = 0.0
                      , scrollerWidth = 0.0
                      , scrollerHeight = 0.0
                      , contentLeft = 0.0
                      , scrollLeft = 0.0
                      , scrollTop = 0.0
                      }
    , outputCanvasInfo =
        { scrollLeft = 0.0
        , scrollTop = 0.0
        }
    , basicCodeBox  = False
    , errorBox      = Nothing
    , genSymCount   = 1 -- starting at 1 to match shape ids on blank canvas
    , tool          = Cursor
    , hoveredShapes = Set.empty
    , hoveredCrosshairs = Set.empty
    , selectedShapes = Set.empty
    , selectedFeatures = Set.empty
    , selectedBlobs = Dict.empty
    , keysDown      = []
    , autoSynthesis = False
    , problemsSentToSolver = []
    , solutionsCache = Dict.empty
    , synthesisResultsDict = Dict.empty
    , hoveredSynthesisResultPathByIndices = []
    , renamingInOutput = Nothing
    , randomColor   = 100
    , lambdaTools   = [starLambdaTool]
    , layoutOffsets = initialLayoutOffsets
    , needsSave     = False
    , lastSaveState = Nothing
    , backupRecovery = True
    , filename      = { name = "", extension = File.LeoFile }
    , fileIndex     = []
    , dialogBoxes   = Set.empty
    , filenameInput = ""
    , fileToDelete  = { name = "", extension = File.LeoFile }
    , pendingFileOperation = Nothing
    , fileOperationConfirmed = False
    , icons = Dict.empty
    , scopeGraph = DependenceGraph.compute e
    , showAllDeuceWidgets = False
    , hoveringCodeBox = False
    , deuceState = DeuceWidgets.emptyDeuceState
    , deuceToolsAndResults = []
    , deuceToolResultPreviews = Dict.empty
    , selectedDeuceTool = Nothing
    , showOnlyBasicTools = True
    , viewState =
        { menuActive = False
        }
    , toolMode = Raw
    , popupPanelPositions =
        { deuce = (200, 200)
        , editCode = (400, 400)
        , deuceRightClickMenu = (400, 400)
        , autoOutputTools = (400, 100)
        }
    , mbDeuceKeyboardInfo = Nothing
    , deuceRightClickMenuMode = Nothing
    , enableDeuceBoxSelection = True
    , enableDeuceTextSelection = True
    , codeToolsMenuMode = CTAll
    , outputToolsMenuMode = True
    , textSelectMode = SubsetExtra
    , enableTextEdits =
        Updatable.setUpdated << Updatable.create <| True
    , allowMultipleTargetPositions =
        False
    , mainResizerX = Nothing
    , savedSelections = Nothing
    , deucePopupPanelAbove = True
    , colorScheme = initColorScheme
    , pendingGiveUpMsg = Nothing
    , giveUpConfirmed = False
    , lastSelectedTemplate = Just (Examples.initTemplate, code)
    , valueEditorString = LangUtils.valToString v
    , htmlEditorString = Nothing
    , updatedValue = Nothing
    , shapeUpdatesViaZones = Dict.empty
    , syntax = Syntax.Leo
    , codeEditorMode = CEText
    , deuceOverlayCache = Nothing
    , doTypeChecking = True
    , isDeuceTextBoxFocused = False
    , needsToFocusOn = Nothing
    }
