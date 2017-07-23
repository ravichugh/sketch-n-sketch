module InterfaceModel exposing (..)

import Updatable exposing (Updatable)
import Lang exposing (..)
import Types exposing (AceTypeInfo)
import Eval
import Sync exposing (ZoneKey)
import Utils
import LangSvg exposing (RootedIndexedTree, NodeId, ShapeKind)
import ShapeWidgets exposing (ShapeFeature, SelectedShapeFeature)
import ExamplesGenerated as Examples
import DefaultIconTheme
import LangUnparser exposing (unparse, unparsePat)
import DependenceGraph exposing (ScopeGraph)
import Ace
import DeuceWidgets exposing (DeuceState)
import Either exposing (Either(..))
import Keys
import Svg
import LangSvg exposing (attr)

import Dict exposing (Dict)
import Set exposing (Set)
import Char
import Window
import Mouse
import Html exposing (Html)
import Html.Attributes as Attr
import VirtualDom

type alias Code = String

type alias Filename = String

type alias FileIndex = List Filename

type alias File = {
  filename : Filename,
  code : Code
}

type alias Position = { col : Int, line : Int }

type alias IconName = String

type alias Icon =
  { iconName : IconName
  , code : Code
  }

type alias ViewState =
  { menuActive : Bool
  }

type alias Preview =
  Maybe (Code, Result String (Val, Widgets, RootedIndexedTree))

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

type DeuceRightClickMenuMode =
  ShowPossible

type alias PopupPanelPositions =
  { deuce : (Int, Int)
  , editCode : (Int, Int)
  , deuceRightClickMenu : (Int, Int)
  }

type alias Model =
  { code : Code
  , lastRunCode : Code
  , preview : Preview
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
  , slate : RootedIndexedTree
  , widgets : Widgets
  , mode : Mode
  , mouseMode : MouseMode
  , dimensions : Window.Size

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
  , autoSynthesis : Bool
  , synthesisResults : List SynthesisResult
  , hoveredSynthesisResultPathByIndices : List Int
  , randomColor : Int
  , lambdaTools : List LambdaTool
  , layoutOffsets : LayoutOffsets
  , needsSave : Bool
  , lastSaveState : Maybe Code
  , autosave : Bool
  , filename : Filename
  , fileIndex : FileIndex
  , dialogBoxes : Set Int
  , filenameInput : String
  , fileToDelete : Filename
  , pendingFileOperation : Maybe Msg
  , fileOperationConfirmed : Bool
  , icons : Dict IconName (Html Msg)
  , showAllDeuceWidgets : Bool
  , hoveringCodeBox : Bool
  , scopeGraph : ScopeGraph
  , deuceState : DeuceWidgets.DeuceState
  , deuceToolsAndResults : List (List CachedDeuceTool)
  , selectedDeuceTool : Maybe CachedDeuceTool
  , showOnlyBasicTools : Bool
  , viewState : ViewState
  , toolMode : ShapeToolKind
  , popupPanelPositions : PopupPanelPositions
  , deuceRightClickMenuMode : Maybe DeuceRightClickMenuMode
  , userStudyStateIndex : Int
  , enableDeuceBoxSelection : Bool
  , enableDeuceTextSelection : Bool
  , showDeuceInMenuBar : Bool
  , showEditCodeInMenuBar : Bool
  , showDeucePanel : Bool
  , showDeuceRightClickMenu : Bool
  , textSelectMode : TextSelectMode
  , enableTextEdits : Updatable Bool
  , codeClean : Bool
  , resizerX : Maybe Int
  , savedSelections : Maybe (List Ace.Range)
  }

type Mode
  = Live Sync.LiveInfo
  | Print RawSvg
      -- TODO put rawSvg in Model
      -- TODO might add a print mode where <g BLOB BOUNDS> nodes are removed
  | PrintScopeGraph (Maybe String)
                      -- Nothing        after sending renderDotGraph request
                      -- Just dataURI   after receiving the encoded image

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

type alias RawSvg = String

type MouseMode
  = MouseNothing
  | MouseDrag (Mouse.Position -> Mouse.Position -> Model -> Model)
  | MouseDragLayoutWidget (MouseTrigger (Model -> Model))

  | MouseDragZone
      ZoneKey               -- (nodeId, shapeKind, zoneName)
      (Maybe                -- Inactive (Nothing) or Active
        ( Sync.LiveTrigger      -- computes program update and highlights
        , (Int, Int)            -- initial click
        , Bool ))               -- dragged at least one pixel

  | MouseDrawNew ShapeBeingDrawn
      -- invariant on length n of list of points:
      --   for line/rect/ellipse, n == 0 or n == 2
      --   for polygon/path,      n >= 0
      --   for helper dot,        n == 0 or n == 1
      --   for lambda,            n == 0 or n == 2

  | MouseDownInCodebox Mouse.Position

type alias MouseTrigger a = (Int, Int) -> a

-- Oldest/base point is last in all of these.
type ShapeBeingDrawn
  = NoPointsYet -- For shapes drawn by dragging, no points until the mouse moves after the mouse-down.
  | TwoPoints (KeysDown, (Int, Int)) (KeysDown, (Int, Int)) -- KeysDown should probably be refactored out
  | PolyPoints (List (Int, Int))
  | PathPoints (List (KeysDown, (Int, Int))) -- KeysDown should probably be replaced with a more semantic represenation of point type
  | OffsetFromExisting (Int, Int) (NumTr, NumTr)


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

type SynthesisResult =
  SynthesisResult { description : String
                  , exp         : Exp
                  , isSafe      : Bool -- Is this transformation considered "safe"?
                  , sortKey     : List Float -- For custom sorting criteria. Sorts ascending.
                  , children    : Maybe (List SynthesisResult) -- Nothing means not calculated yet.
                  }

synthesisResult description exp =
  SynthesisResult <|
    { description = description
    , exp         = exp
    , isSafe      = True
    , sortKey     = []
    , children    = Nothing
    }

synthesisResultsNotEmpty : Model -> Bool
synthesisResultsNotEmpty =
  not << List.isEmpty << .synthesisResults

mapResultSafe f (SynthesisResult result) =
  SynthesisResult { result | isSafe = f result.isSafe }

setResultSafe isSafe synthesisResult =
  mapResultSafe (\_ -> isSafe) synthesisResult

isResultSafe (SynthesisResult {isSafe}) =
  isSafe

resultDescription (SynthesisResult {description}) =
  description

setResultDescription description (SynthesisResult result) =
  SynthesisResult { result | description = description }

type Msg
  = Msg String (Model -> Model)

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
  | Help HelpInfo

type HelpInfo
  = HelpSyntax
  | HelpTextSelectMode
  | HelpBoxSelectMode

dialogBoxes =
  Utils.mapi0 identity
    [ New
    , SaveAs
    , Open
    , AlertSave
    , ImportCode
    , Help HelpSyntax
    , Help HelpTextSelectMode
    , Help HelpBoxSelectMode
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
      Tuple.second model.mouseState
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

--------------------------------------------------------------------------------
-- Deuce Tools
--------------------------------------------------------------------------------

type alias DeuceTransformation =
  () -> List SynthesisResult

type alias DeuceTool =
  { name : String
  , func : Maybe DeuceTransformation
  , reqs : List Predicate -- requirements to run the tool
  , id : String -- unique, unchanging identifier
  }

type alias CachedDeuceTool =
  (DeuceTool, List SynthesisResult, Bool)

--------------------------------------------------------------------------------

runAndResolve : Model -> Exp -> Result String (Val, Widgets, RootedIndexedTree, Code)
runAndResolve model exp =
  Eval.run exp
  |> Result.andThen (\(val, widgets) -> slateAndCode model (exp, val)
  |> Result.map (\(slate, code) -> (val, widgets, slate, code)))

slateAndCode : Model -> (Exp, Val) -> Result String (RootedIndexedTree, Code)
slateAndCode old (exp, val) =
  LangSvg.resolveToIndexedTree old.slideNumber old.movieNumber old.movieTime val
  |> Result.map (\slate -> (slate, unparse exp))

--------------------------------------------------------------------------------

mkLive opts slideNumber movieNumber movieTime e (val, widgets) =
  LangSvg.resolveToIndexedTree slideNumber movieNumber movieTime val |> Result.andThen (\slate ->
  Sync.prepareLiveUpdates opts e (slate, widgets)                    |> Result.andThen (\liveInfo ->
    Ok (Live liveInfo)
  ))

mkLive_ opts slideNumber movieNumber movieTime e  =
  Eval.run e |> Result.andThen (mkLive opts slideNumber movieNumber movieTime e)

--------------------------------------------------------------------------------

liveInfoToHighlights zoneKey model =
  case model.mode of
    Live info -> Sync.yellowAndGrayHighlights zoneKey info
    _         -> []

--------------------------------------------------------------------------------

codeToShow model =
  case model.preview of
     Just (string, _) -> string
     Nothing          -> model.code

--------------------------------------------------------------------------------

strLambdaTool lambdaTool =
  let strExp = String.trim << unparse in
  case lambdaTool of
    LambdaBounds e -> Utils.parens <| "\\bounds. " ++ strExp e ++ " bounds"
    LambdaAnchor e -> Utils.parens <| "\\anchor. " ++ strExp e ++ " anchor"

--------------------------------------------------------------------------------

prependDescription newPrefix synthesisResult =
  { synthesisResult | description = (newPrefix ++ synthesisResult.description) }

--------------------------------------------------------------------------------

bufferName = ""

untitledName = "Untitled"

prettyFilename model =
  if model.filename == bufferName then
    untitledName
  else
    model.filename

getFile model = { filename = model.filename
                , code     = model.code
                }

--------------------------------------------------------------------------------

iconNames = Dict.keys DefaultIconTheme.icons

--------------------------------------------------------------------------------

starLambdaTool = LambdaBounds (eVar "star")

starLambdaToolIcon = lambdaToolIcon starLambdaTool

lambdaToolIcon tool =
  { iconName = Utils.naturalToCamelCase (strLambdaTool tool)
  , code = case tool of
      LambdaBounds func ->
        "(svgViewBox 100 100 (" ++ unparse func ++ " [10 10 90 90]))"
      LambdaAnchor func ->
        "(svgViewBox 100 100 (" ++ unparse func ++ " [10 10]))"
  }

--------------------------------------------------------------------------------

needsRun m =
  m.code /= m.lastRunCode

--------------------------------------------------------------------------------

oneSafeResult newExp =
  List.singleton <|
    synthesisResult ("NO DESCRIPTION B/C SELECTED AUTOMATICALLY") newExp

--------------------------------------------------------------------------------

deuceActive : Model -> Bool
deuceActive model =
  let
    shiftDown =
      List.member Keys.keyShift model.keysDown
  in
    model.codeClean &&
    Utils.or
      [ Utils.and
          [ model.enableDeuceBoxSelection
          , not <| deuceRightClickMenuShown model
          , shiftDown
          ]
      , configurationPanelShown model
      ]

--------------------------------------------------------------------------------

snippet : Ace.Range -> String -> String
snippet range =
     String.lines
  >> Utils.slice range.start.row (range.end.row + 1)
  >> Utils.modifyLast (String.left range.end.column)
  >> Utils.modifyFirst (String.dropLeft range.start.column)
  >> String.concat

codeSnippet : Model -> Ace.Range -> String
codeSnippet model range =
  snippet range model.code

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

matchingRange : Model -> Bool -> Ace.Range -> List (Ace.Range, a) -> Maybe a
matchingRange model allowSingleSelection selectedRange =
  let
    textSelectMode =
      if
        allowSingleSelection &&
        selectedRange.start == selectedRange.end
      then
        Superset
      else
        model.textSelectMode
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
                    snippetValid << (codeSnippet model) <|
                      { start =
                          sr.start
                      , end =
                          r.start
                      }

                  endValid : Bool
                  endValid =
                    snippetValid << (codeSnippet model) <|
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
  in
    fold
      ( \(range, val) previousVal ->
          if matcher selectedRange range then
            Just val
          else
            previousVal
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
      matchingRange
        model
        allowSingleSelection
        selection
        ( List.concatMap
            ( \codeObject ->
                let
                  default =
                    [ ( rangeFromInfo << extractInfoFromCodeObject <|
                          codeObject
                      , codeObject
                      )
                    ]
                in
                  -- Do not text-select non-text-selectable code objects like
                  -- comments or target positions
                  if Lang.isTextSelectable codeObject then
                    -- Also just ignore def code objects for now
                    case codeObject of
                      E e ->
                        case e.val.e__ of
                          (ELet _ Def _ _ _ _ _) ->
                            []
                          _ ->
                            default
                      _ ->
                        default
                  else
                    []
            )
            ( flattenToCodeObjects << E <|
                model.inputExp
            )
        )
    _ ->
      Nothing

--------------------------------------------------------------------------------

noWidgetsSelected : Model -> Bool
noWidgetsSelected model =
  List.isEmpty model.deuceState.selectedWidgets

--------------------------------------------------------------------------------

-- initTemplate = "BLANK"
initTemplate = "Deuce Study Start"

initModel : Model
initModel =
  let
    -- TODO unnecessary process to initTemplate, because of initCmd
    (_,f)    = Utils.find_ Examples.list initTemplate
    {e,v,ws} = f ()
  in
  let unwrap = Utils.fromOk "generating initModel" in
  let (slideCount, movieCount, movieDuration, movieContinue, slate) =
    unwrap (LangSvg.fetchEverything 1 1 0.0 v)
  in
  let liveModeInfo = unwrap (mkLive Sync.defaultOptions 1 1 0.0 e (v, ws)) in
  let code = unparse e in
    { code          = code
    , lastRunCode   = code
    , preview       = Nothing
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
    , slate         = slate
    , widgets       = ws
    , mode          = liveModeInfo
    , mouseMode     = MouseNothing
    , dimensions    = { width = 1000, height = 800 } -- dummy in case initCmd fails
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
    , synthesisResults = []
    , hoveredSynthesisResultPathByIndices = []
    , randomColor   = 100
    , lambdaTools   = [starLambdaTool]
    , layoutOffsets = initialLayoutOffsets
    , needsSave     = False
    , lastSaveState = Nothing
    , autosave      = False
    , filename      = ""
    , fileIndex     = []
    , dialogBoxes   = Set.empty
    , filenameInput = ""
    , fileToDelete  = ""
    , pendingFileOperation = Nothing
    , fileOperationConfirmed = False
    , icons = Dict.empty
    , scopeGraph = DependenceGraph.compute e
    , showAllDeuceWidgets = False
    , hoveringCodeBox = False
    , deuceState = DeuceWidgets.emptyDeuceState
    , deuceToolsAndResults = []
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
        }
    , deuceRightClickMenuMode = Nothing
    , userStudyStateIndex = 1
    , enableDeuceBoxSelection = False
    , enableDeuceTextSelection = False
    , showDeuceInMenuBar = False
    , showEditCodeInMenuBar = False
    , showDeucePanel = False
    , showDeuceRightClickMenu = False
    , textSelectMode = SubsetExtra
    , enableTextEdits =
        Updatable.setUpdated << Updatable.create <| True
    , codeClean = True
    , resizerX = Nothing
    , savedSelections = Nothing
    }
