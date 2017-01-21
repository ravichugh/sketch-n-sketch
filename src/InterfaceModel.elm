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
import OurParser2 as P
import DependenceGraph exposing (ScopeGraph)
import Ace
import Either exposing (Either(..))

import Dict exposing (Dict)
import Set exposing (Set)
import Char
import Window
import Mouse

type alias Code = String

type alias Filename = String

type alias FileIndex = List Filename

type alias File = {
  filename : Filename,
  code : Code
}

type alias Model =
  { code : Code
  , preview: Maybe (Code, Result String (Val, Widgets, RootedIndexedTree))
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
  , synthesisResults: List SynthesisResult
  , randomColor : Int
  , lambdaTools : (Int, List LambdaTool)
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
  , selectedEIds : Set.Set EId
  , deuceMode : Bool
  , hoveringCodeBox : Bool
  , expRanges : List (EId, Exp, P.Pos, P.Pos, P.Pos)
  , patRanges : List (Pat, P.Pos, P.Pos)
  , selectedPats : Set.Set (List Int)
  , selectedPatSpaces : Set.Set (List Int)
  , scopeGraph : ScopeGraph
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
  }

type alias RawSvg = String

type MouseMode 
  = MouseNothing
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

type alias SynthesisResult =
  { description : String
  , exp         : Exp
  }

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
  , synthesisResultsSelectBox = init
  }

type DialogBox = New | SaveAs | Open | AlertSave | ImportCode

dbToInt db =
  case db of
    New -> 0
    SaveAs -> 1
    Open -> 2
    AlertSave -> 3
    ImportCode -> 4

intToDb n =
  case n of
    0 -> New
    1 -> SaveAs
    2 -> Open
    3 -> AlertSave
    4 -> ImportCode
    _ -> Debug.crash "Undefined Dialog Box Type"

openDialogBox db model =
  { model | dialogBoxes = Set.insert (dbToInt db) model.dialogBoxes }

closeDialogBox db model =
  { model | dialogBoxes = Set.remove (dbToInt db) model.dialogBoxes }

isDialogBoxShowing db model =
  Set.member (dbToInt db) model.dialogBoxes

--------------------------------------------------------------------------------

importCodeFileInputId = "import-code-file-input"

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

computePatRanges expId path pathNum pat =
  case pat.val of
    PConst _ _              -> [((expId, path ++ [pathNum]), pat.val, pat.start, pat.end, pat.end)]
    PBase _ _               -> [((expId, path ++ [pathNum]), pat.val, pat.start, pat.end, pat.end)]
    PVar _ x _              -> [((expId, path ++ [pathNum]), pat.val, pat.start, pat.end, pat.end)]
    PList _ ps _ Nothing _  -> [((expId, path ++ [pathNum]), pat.val, pat.start, pat.end, { line = pat.start.line, col = pat.start.col + 1 })] 
                                ++ Utils.concatMapi (\(i,p) -> (computePatRanges expId (path ++ [pathNum]) i p)) ps
    PList _ ps _ (Just p) _ -> [((expId, path ++ [pathNum]), pat.val, pat.start, pat.end, { line = pat.start.line, col = pat.start.col + 1 })] 
                                ++ Utils.concatMapi (\(i,p) -> (computePatRanges expId (path ++ [pathNum]) i p)) (p::ps)
    PAs _ x _ p             -> [((expId, path ++ [pathNum]), pat.val, pat.start, pat.end, pat.end)] 
                                ++ computePatRanges expId (path ++ [pathNum]) (pathNum + 1) p

findPats e = 
  let find e acc = 
    case e.val.e__ of 
      EFun _ (p::ps) _ _ -> Utils.concatMapi (\(i,p) -> (computePatRanges e.val.eid [] i p)) (p::ps) ++ acc
      ETypeCase _ p _ _ -> (computePatRanges e.val.eid [] 1 p) ++ acc
      ELet _ _ _ p _ _ _ -> (computePatRanges e.val.eid [] 1 p) ++ acc
      ETyp _ p _ _ _ -> (computePatRanges e.val.eid [] 1 p) ++ acc
      ETypeAlias _ p _ _ _ -> (computePatRanges e.val.eid [] 1 p) ++ acc
      _ -> acc 
  in
  foldExp find [] e 

computePatSpaces expId path pathNum pat =
  case pat.val of
    PConst ws _                   -> [((expId, path ++ [pathNum], 0), pat.val, 
                                      {line = pat.start.line, col = pat.start.col - 1},
                                      {line = pat.start.line, col = pat.start.col}),
                                    ((expId, path ++ [pathNum], 1), pat.val, 
                                      {line = pat.end.line, col = pat.end.col},
                                      {line = pat.end.line, col = pat.end.col + 1})
                                    ]
    PBase ws _                    -> [((expId, path ++ [pathNum], 0), pat.val, 
                                      {line = pat.start.line, col = pat.start.col - 1},
                                      {line = pat.start.line, col = pat.start.col}),
                                    ((expId, path ++ [pathNum], 1), pat.val, 
                                      {line = pat.end.line, col = pat.end.col},
                                      {line = pat.end.line, col = pat.end.col + 1})
                                    ]
    PVar ws x _                   -> [((expId, path ++ [pathNum], 0), pat.val, 
                                      {line = pat.start.line, col = pat.start.col - 1},
                                      {line = pat.start.line, col = pat.start.col}),
                                    ((expId, path ++ [pathNum], 1), pat.val, 
                                      {line = pat.end.line, col = pat.end.col},
                                      {line = pat.end.line, col = pat.end.col + 1})
                                    ]
    PList ws1 ps ws2 Nothing ws3  -> [((expId, path ++ [pathNum], 0), pat.val, 
                                      {line = pat.start.line, col = pat.start.col - 1},
                                      {line = pat.start.line, col = pat.start.col}),
                                    ((expId, path ++ [pathNum], 1), pat.val, 
                                      {line = pat.end.line, col = pat.end.col},
                                      {line = pat.end.line, col = pat.end.col + 1})
                                    ] ++ Utils.concatMapi (\(i,p) -> (computePatSpaces expId (path ++ [pathNum]) i p)) ps
    PList ws1 ps ws2 (Just p) ws3 -> [((expId, path ++ [pathNum], 0), pat.val, 
                                      {line = pat.start.line, col = pat.start.col - 1},
                                      {line = pat.start.line, col = pat.start.col}),
                                    ((expId, path ++ [pathNum], 1), pat.val, 
                                      {line = pat.end.line, col = pat.end.col},
                                      {line = pat.end.line, col = pat.end.col + 1})
                                    ] ++ Utils.concatMapi (\(i,p) -> (computePatSpaces expId (path ++ [pathNum]) i p)) (p::ps)
    PAs ws1 x ws2 p               -> [((expId, path ++ [pathNum], 0), pat.val, 
                                        {line = pat.start.line, col = pat.start.col - 1},
                                        {line = pat.start.line, col = pat.start.col}),
                                      ((expId, path ++ [pathNum], 1), pat.val, 
                                        {line = pat.end.line, col = pat.end.col},
                                        {line = pat.end.line, col = pat.end.col + 1})
                                    ] ++ computePatSpaces expId (path ++ [pathNum]) (pathNum + 1) p

findPatSpaces e = 
  let find e acc = 
    case e.val.e__ of 
      EFun _ (p::ps) _ _ -> Utils.concatMapi (\(i,p) -> (computePatSpaces e.val.eid [] i p)) (p::ps) ++ acc
      ETypeCase _ p _ _ -> computePatSpaces e.val.eid [] 1 p ++ acc
      ELet _ _ _ p _ _ _ -> computePatSpaces e.val.eid [] 1 p ++ acc
      ETyp _ p _ _ _ -> computePatSpaces e.val.eid [] 1 p ++ acc
      ETypeAlias _ p _ _ _ -> computePatSpaces e.val.eid [] 1 p ++ acc
      _ -> acc 
  in
  foldExp find [] e 

computeConstantRanges : Exp -> List (EId, Num, P.Pos, P.Pos)
computeConstantRanges e =
  let combine e acc =
    case e.val.e__ of
      EConst _ n _ _ -> (e.val.eid, n, e.start, e.end) :: acc
      _              -> acc
  in
  foldExp combine [] e

-- positions: start, end, start of selection area, end of selection area
computeExpRanges : Exp -> List (EId, Exp, P.Pos, P.Pos, P.Pos, P.Pos)
computeExpRanges e =
  let combine e acc =
    case e.val.e__ of
      EConst _ n _ _ -> (e.val.eid, e, e.start, e.end, e.start, e.end) :: acc
      EBase _ b -> (e.val.eid, e, e.start, e.end, e.start, e.end) :: acc 
      EVar _ i -> (e.val.eid, e, e.start, e.end, e.start, e.end) :: acc 
      EFun _ p e2 _ -> (e.val.eid, e, e.start, e.end, { line = e.start.line, col = e.start.col + 1 }, { line = e.start.line, col = e.start.col + 2 }) :: acc
      ELet _ _ r p e1 e2 _ -> (e.val.eid, e, e.start, e.end, { line = e.start.line, col = e.start.col + 1 }, { line = e.start.line, col = e.start.col + 4 }) :: acc 
      _              -> acc
  in
  foldExp combine [] e

expRangesToHighlights m =
  let maybeHighlight (eid,n,start,end,selectStart, selectEnd) =
    let range =
      { start = { row = selectStart.line, column = selectStart.col }
      , end   = { row = selectEnd.line, column = selectEnd.col  } }
    in
    if Set.member eid m.selectedEIds then
      [ { color = "orange", range = range } ]
    else if m.hoveringCodeBox then
      [ { color = "lightgray", range = range } ]
    else
      []
  in
  if m.deuceMode
    then List.concatMap maybeHighlight (computeExpRanges m.inputExp)
    else []

patRangesToHighlights m = 
  let maybeHighlight ((expId,path),p,start,end,selectEnd) =
    let range =
      { start = { row = start.line, column = start.col }
      , end   = { row = end.line, column = selectEnd.col  } }
    in
    if Set.member ([expId] ++ path) m.selectedPats then
      [ { color = "gold", range = range } ]
    else if not (Set.isEmpty m.selectedPats) then
      []
    else if m.hoveringCodeBox then
      [ { color = "lightyellow", range = range } ]
    else
      []
  in
  if m.deuceMode
    then List.concatMap maybeHighlight (findPats m.inputExp)
    else []

patSpacesToHighlights m = 
  let maybeHighlight ((expId,path,befaft), p,start,end) =
    let range =
      { start = { row = start.line, column = start.col }
      , end   = { row = end.line, column = end.col  } }
    in
    if Set.isEmpty m.selectedPats then 
      []
    else if Set.member ([expId] ++ path ++ [befaft]) m.selectedPatSpaces then
      [ { color = "green", range = range } ]
    else if not (Set.isEmpty m.selectedPatSpaces) then
      []
    else if m.hoveringCodeBox then
      [ { color = "lightgreen", range = range } ]
    else
      []
  in
  if m.deuceMode
    then List.concatMap maybeHighlight (findPatSpaces m.inputExp)
    else []

--------------------------------------------------------------------------------

codeToShow model =
  case model.preview of
     Just (string, _) -> string
     Nothing          -> model.code

--------------------------------------------------------------------------------

prependDescription newPrefix {description, exp} =
  { description = (newPrefix ++ description), exp = exp}

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
    { code          = code
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
                      , fontSize = 14
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
    , synthesisResults = []
    , randomColor   = 100
    , lambdaTools   = (1, [LambdaBounds (eVar "star")])
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
    , selectedEIds  = Set.empty
    , deuceMode = True
    , hoveringCodeBox = False
    , expRanges = []
    , patRanges = []
    , selectedPats = Set.empty
    , selectedPatSpaces = Set.empty
    , scopeGraph = DependenceGraph.compute e
    }

