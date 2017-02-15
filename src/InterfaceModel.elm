module InterfaceModel exposing (..)

import Lang exposing (..)
import Types exposing (AceTypeInfo)
import Eval
import Sync exposing (ZoneKey)
import Utils
import LangSvg exposing (RootedIndexedTree, NodeId, ShapeKind)
import ShapeWidgets exposing (ShapeFeature, SelectedShapeFeature, Zone)
import ExamplesGenerated as Examples
import LangUnparser exposing (unparse, unparsePat)
import OurParser2 as P
import DependenceGraph exposing
  (ScopeGraph, PatternId, PatTargetPosition, ExpTargetPosition)
import Ace
import Either exposing (Either(..))
import Keys 
import Svg
import LangSvg exposing (attr)
import Html.Attributes as Attr
import VirtualDom

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

type alias Html msg = VirtualDom.Node msg

type alias Position = { col : Int, line : Int }

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
  , selectedPats : Set.Set PatternId
  , selectedPatTargets : Set.Set PatTargetPosition
  , selectedExpTargets : Set.Set ExpTargetPosition
  , scopeGraph : ScopeGraph
  , hoveredExp : List (Exp, EId, Position, Position, Position, Float, Float) 
  , hoveredPat : List (Pat, EId, Position, Position, Position, Float, Float) 
  , expSelectionBoxes : List (Exp, EId, Position, Position, Position, Float, Float) 
  , patSelectionBoxes : List (Pat, PatternId, Position, Position, Position, Float, Float) 
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

  | MouseDownInCodebox Mouse.Position

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

computePatRanges expId path addPath pat =
  let baseResult = [(pat, (expId, path ++ addPath), pat.start, pat.end, pat.end)] in 
  case pat.val of
    PConst _ _              -> baseResult
    PBase _ _               -> baseResult
    PVar _ x _              -> baseResult
    PList _ ps _ Nothing _  -> [(pat, (expId, path ++ addPath), pat.start, pat.end, { line = pat.start.line, col = pat.start.col + 1 })] 
                                ++ Utils.concatMapi (\(i,p) -> (computePatRanges expId (path ++ addPath) [i] p)) ps
    PList _ ps _ (Just p) _ -> [(pat, (expId, path ++ addPath), pat.start, pat.end, { line = pat.start.line, col = pat.start.col + 1 })] 
                                ++ Utils.concatMapi (\(i,p) -> (computePatRanges expId (path ++ addPath) [i] p)) (p::ps)
    PAs _ x _ p             -> case addPath of
                                [n] -> baseResult
                                        ++ computePatRanges expId (path ++ addPath) [n + 1] p
                                _   -> baseResult

findPats e = 
  let find e acc = 
    case e.val.e__ of 
      EFun _ (p::ps) _ _    -> List.concatMap (computePatRanges e.val.eid [] []) (p::ps) ++ acc
      ETypeCase _ p _ _     -> (computePatRanges e.val.eid [] [] p) ++ acc
      ELet _ _ _ p _ _ _    -> (computePatRanges e.val.eid [] [] p) ++ acc
      ETyp _ p _ _ _        -> (computePatRanges e.val.eid [] [] p) ++ acc
      ETypeAlias _ p _ _ _  -> (computePatRanges e.val.eid [] [] p) ++ acc
      _                     -> acc 
  in
  foldExp find [] e 

computePatTargets expId path addPath pat =
  let baseBefore = ((0,(expId, path ++ addPath)),
                    {line = pat.start.line, col = pat.start.col - 1},
                    {line = pat.start.line, col = pat.start.col}) in
  let baseAfter =  ((1,(expId, path ++ addPath)),
                    {line = pat.end.line, col = pat.end.col},
                    {line = pat.end.line, col = pat.end.col + 1}) in
  case pat.val of
    PConst ws _                   -> [baseBefore, baseAfter]
    PBase ws _                    -> [baseBefore, baseAfter]
    PVar ws x _                   -> [baseBefore, baseAfter]
    PList ws1 ps ws2 Nothing ws3  -> [baseBefore, baseAfter] 
                                      ++ Utils.concatMapi (\(i,p) -> (computePatTargets expId (path ++ addPath) [i] p)) ps
    PList ws1 ps ws2 (Just p) ws3 -> [baseBefore, baseAfter]
                                      ++ Utils.concatMapi (\(i,p) -> (computePatTargets expId (path ++ addPath) [i] p)) (p::ps)
    PAs ws1 x ws2 p               -> case addPath of
                                      [n] -> [baseBefore, baseAfter]
                                              ++ computePatTargets expId (path ++ addPath) [n + 1] p
                                      _   -> [baseBefore, baseAfter]

findPatTargets e = 
  let find e acc = 
    case e.val.e__ of 
      EFun _ (p::ps) _ _    -> List.concatMap (computePatTargets e.val.eid [] []) (p::ps) ++ acc
      ETypeCase _ p _ _     -> computePatTargets e.val.eid [] [] p ++ acc
      ELet _ _ _ p _ _ _    -> computePatTargets e.val.eid [] [] p ++ acc
      ETyp _ p _ _ _        -> computePatTargets e.val.eid [] [] p ++ acc
      ETypeAlias _ p _ _ _  -> computePatTargets e.val.eid [] [] p ++ acc
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
computeExpRanges : Exp -> List (Exp, EId, P.Pos, P.Pos, P.Pos, P.Pos)
computeExpRanges e =
  let combine e acc =
    let baseValue = (e, e.val.eid, e.start, e.end, e.start, e.end) in
    let parenValue = (e, e.val.eid, e.start, e.end, e.start, { line = e.start.line, col = e.start.col + 1 }) in 
    case e.val.e__ of
      EConst _ n _ _        -> baseValue :: acc
      EBase _ b             -> baseValue :: acc 
      EVar _ i              -> baseValue :: acc 
      EFun _ p e2 _         -> (e, e.val.eid, e.start, e.end, { line = e.start.line, col = e.start.col + 1 }, { line = e.start.line, col = e.start.col + 2 }) :: acc
      -- unique cases for let and def (equations and entire expressions)
      ELet _ _ r p e1 e2 _  -> (e, e.val.eid, e.start, e.end, { line = e.start.line, col = e.start.col + 1 }, { line = e.start.line, col = e.start.col + 4 }) :: acc 
      EApp _ e elist _      -> parenValue :: acc
      EOp _ _ elist _       -> parenValue :: acc 
      EList _ elist _ e _   -> parenValue :: acc 
      EIf _ e1 e2 e3 _      -> parenValue :: acc 
      ECase _ e _ _         -> parenValue :: acc 
      ETypeCase _ p _ _     -> parenValue :: acc 
      _                     -> acc
  in
  foldExp combine [] e

-- positions: start and end of selection area
computeExpTargets : Exp -> List (ExpTargetPosition, P.Pos, P.Pos)
computeExpTargets e =
  let combine e acc =
    let baseTargets =    [((0,e.val.eid), { line = e.start.line, col = e.start.col - 1 }, e.start),
                          ((1,e.val.eid),  e.end, { line = e.end.line, col = e.end.col + 1 })] ++ acc 
    in
    let offsetTargets =  [((0,e.val.eid), e.start, { line = e.start.line, col = e.start.col + 1 }),
                          ((1,e.val.eid), { line = e.end.line, col = e.end.col - 1 }, e.end)] ++ acc 
    in
    case e.val.e__ of 
      EConst _ _ _ _        -> baseTargets
      EBase _ b             -> baseTargets
      EVar _ i              -> baseTargets
      EFun _ p e2 _         -> offsetTargets
      EApp _ _ _ _          -> baseTargets
      EOp _ _ _ _           -> baseTargets
      EList _ _ _ _ _       -> baseTargets
      EIf _ _ _ _ _         -> baseTargets
      ECase _ _ _ _         -> baseTargets
      ETypeCase _ _ _ _     -> baseTargets
      ELet _ _ r p e1 e2 _  -> offsetTargets
      _                     -> acc
  in
  foldExp combine [] e

shiftKeyPressed m = List.member Keys.keyShift m.keysDown
showDeuceWidgets m = shiftKeyPressed m

betweenPos start pixelPos end =
  (start.line <= pixelPos.row + 1) &&
  (start.col <= pixelPos.column + 1) &&
  (end.line >= pixelPos.row + 1) &&
  (end.col > pixelPos.column + 1)

hoveringItem start p end = 
  case p of
    Nothing   -> False 
    Just pos  -> betweenPos start pos end  

leadingTrailingSpaces str start end = 
  let leftTrim = String.trimLeft str in
  let rightTrim = String.trimRight str in 
  let fullLength = String.length str in 
  let leftTrimLength = String.length leftTrim in
  let rightTrimLength = String.length rightTrim in
  let leading = fullLength - leftTrimLength in 
  let trailing = rightTrimLength in 
  (leading, trailing)

lineStartEnd ls currIndex start end results =
  case ls of 
    Just lines -> 
      if currIndex > end.line
      then results
      else 
        if currIndex >= start.line
        then 
          case List.head(lines) of
            Just str -> lineStartEnd (List.tail(lines)) (currIndex + 1) start end 
                                      (Dict.insert currIndex (leadingTrailingSpaces str start end) results)
            _ -> results
        else 
          lineStartEnd (List.tail(lines)) (currIndex+1) start end results
    _ -> results 

leadingNewlines lines total = 
  case lines of
    Just l1 -> 
      case (List.head l1) of
        Just l2 -> if l2 == "" 
                    then leadingNewlines (List.tail l1) (total + 1)
                    else total
        _ -> total 
    Nothing -> total 

expBoundingPolygon exp = 
  let start = exp.start in
  let end = exp.end in 
  let string = unparse exp in 
  let lines = String.lines string in 
  if List.length lines == 1 && start.line == end.line 
  then 
    let s = start.line in 
    (exp, Dict.fromList [(s, (start.col - 1, end.col - 1))]) 
  else 
    let leading = leadingNewlines (Just lines) 0 in 
    let output = lineStartEnd (Just lines) (start.line - leading) start end (Dict.empty) in 
    (exp, output)

patBoundingPolygon pat = 
  let start = pat.start in
  let end = pat.end in 
  let string = unparsePat pat in 
  let lines = String.lines string in 
  if List.length lines == 1 && start.line == end.line 
  then 
    let s = start.line in 
    (pat, Dict.fromList [(s, (start.col - 1, end.col - 1))]) 
  else 
    let leading = leadingNewlines (Just lines) 0 in 
    let output = lineStartEnd (Just lines) (start.line - leading) start end (Dict.empty) in 
    (pat, output)

expRangesToHighlights m pos =
  let maybeHighlight (exp,eid,start,end,selectStart,selectEnd) =
    let range =
      { start = { row = selectStart.line, column = selectStart.col }
      , end   = { row = selectEnd.line, column = selectEnd.col  } }
    in
    let pixelPos = rowColToPixelPos selectStart m in
    let w = getBoxWidth start end m in 
    let h = getBoxHeight start end m in
    if Set.member eid m.selectedEIds then
      [ { color = "orange", range = range } ]
    else if showDeuceWidgets m || hoveringItem selectStart pos selectEnd then
      [ { color = "peachpuff", range = range } ]
    else
      []
  in
  if m.deuceMode
    then List.concatMap maybeHighlight (computeExpRanges m.inputExp)
    else []

expRangeSelections m = 
  let maybeHighlight (exp,eid,start,end,selectStart,selectEnd) =
    let range =
      { start = { row = selectStart.line, column = selectStart.col }
      , end   = { row = selectEnd.line, column = selectEnd.col  } }
    in
    let pixelPos = rowColToPixelPos selectStart m in
    let w = getBoxWidth start end m in 
    let h = getBoxHeight start end m in
    if Set.member eid m.selectedEIds then
      [(exp, eid, selectStart, start, end, w, h)]
    else
      []
  in
  if m.deuceMode
    then List.concatMap maybeHighlight (computeExpRanges m.inputExp)
    else []

patRangeSelections m = 
  let maybeHighlight (pat,pid,start,end,selectEnd) =
    let range =
      { start = { row = start.line, column = start.col }
      , end   = { row = selectEnd.line, column = selectEnd.col  } }
    in
    let pixelPos = rowColToPixelPos start m in
    let w = getBoxWidth start end m in 
    let h = getBoxHeight start end m in
    if Set.member pid m.selectedPats then
      [(pat, pid, start, start, end, w, h)]
    else
      []
  in
  if m.deuceMode
    then List.concatMap maybeHighlight (findPats m.inputExp)
    else []

pixelToRowColPosition pos m = 
  let rowPadding = m.codeBoxInfo.offsetHeight in
  let colPadding = m.codeBoxInfo.offsetLeft +  m.codeBoxInfo.gutterWidth in
  let row = truncate((toFloat(pos.y) + rowPadding) / m.codeBoxInfo.lineHeight - 1) in
  let col = truncate((toFloat(pos.x) - colPadding) / m.codeBoxInfo.characterWidth) in 
    {row = row, column = col}

rowColToPixelPos pos m = 
  let rowPadding = m.codeBoxInfo.offsetHeight in
  let colPadding = m.codeBoxInfo.offsetLeft +  m.codeBoxInfo.gutterWidth in
  let y = toFloat(pos.line) * m.codeBoxInfo.lineHeight - rowPadding in 
  let x = (toFloat(pos.col) - 0.5) * m.codeBoxInfo.characterWidth + colPadding in 
    {x = x, y = y}

getBoxWidth start end m = 
  let offSet = if start.line == end.line then 0 else 1 in 
  let characters = end.col - start.col - offSet in
  toFloat(characters) * m.codeBoxInfo.characterWidth 

getBoxHeight start end m = 
  let lines = end.line - start.line + 1 in 
  toFloat(lines) * m.codeBoxInfo.lineHeight

pixels n = toString n ++ "px"

expRangesToHover m pos =
  let boxes pos (exp,eid,start,end,selectStart,selectEnd) = 
    let pixelPos = rowColToPixelPos selectStart m in
    let w = getBoxWidth start end m in 
    let h = getBoxHeight start end m in 
    if hoveringItem selectStart (Just (pixelToRowColPosition pos m)) selectEnd then 
      [(exp,eid, selectStart, start, end, w, h)]
    else 
      []
  in
  if m.deuceMode
    then List.concatMap (boxes pos) (computeExpRanges m.inputExp)
    else []

patRangesToHover m pos =
  let boxes pos (pat,(eid,ls),start,end,selectEnd) = 
    let pixelPos = rowColToPixelPos start m in
    let w = getBoxWidth start end m in 
    let h = getBoxHeight start end m in 
    if hoveringItem start (Just (pixelToRowColPosition pos m)) selectEnd then 
      [(pat, eid, start, start, end, w, h)]
    else 
      []
  in
  if m.deuceMode
    then List.concatMap (boxes pos) (findPats m.inputExp)
    else []

expTargetsToHighlights m pos =
  let maybeHighlight (expTarget,selectStart,selectEnd) =
    let range =
      { start = { row = selectStart.line, column = selectStart.col }
      , end   = { row = selectEnd.line, column = selectEnd.col  } }
    in
    if Set.member expTarget m.selectedExpTargets then
      [ { color = "green", range = range } ]
    else if showDeuceWidgets m || hoveringItem selectStart pos selectEnd then
      [ { color = "lightgreen", range = range } ]
    else
      []
  in
  if m.deuceMode
    then List.concatMap maybeHighlight (computeExpTargets m.inputExp)
    else []

patRangesToHighlights m pos = 
  let maybeHighlight (pat,pid,start,end,selectEnd) =
    let range =
      { start = { row = start.line, column = start.col }
      , end   = { row = end.line, column = selectEnd.col  } }
    in
    if Set.member pid m.selectedPats then
      [ { color = "gold", range = range } ]
    else if showDeuceWidgets m || hoveringItem start pos selectEnd then
      [ { color = "lightyellow", range = range } ]
    else
      []
  in
  if m.deuceMode
    then List.concatMap maybeHighlight (findPats m.inputExp)
    else []

patTargetsToHighlights m pos = 
  let maybeHighlight (target,start,end) =
    let range =
      { start = { row = start.line, column = start.col }
      , end   = { row = end.line, column = end.col  } }
    in
    if Set.member target m.selectedPatTargets then
      [ { color = "green", range = range } ]
    else if showDeuceWidgets m || hoveringItem start pos end then
      [ { color = "lightgreen", range = range } ]
    else
      []
  in
  if m.deuceMode
    then List.concatMap maybeHighlight (findPatTargets m.inputExp)
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
                      , fontSize = 16
                      , characterWidth = 10.0
                      , lineHeight = 20.0
                      , offsetLeft = 10.0
                      , offsetHeight = 10.0
                      , gutterWidth = 50.0
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
    , selectedPats = Set.empty
    , selectedPatTargets = Set.empty
    , selectedExpTargets = Set.empty
    , scopeGraph = DependenceGraph.compute e
    , hoveredExp = []
    , hoveredPat = [] 
    , expSelectionBoxes = [] 
    , patSelectionBoxes = []
    }

