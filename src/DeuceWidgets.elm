module DeuceWidgets exposing (..) -- TODO

import Lang exposing (..)
import OurParser2 as P
import LangUnparser exposing (unparse, unparsePat)
import Utils
import Keys

import Set exposing (Set)
import Dict exposing (Dict)


--------------------------------------------------------------------------------

type alias DeuceState =
  { selectedWidgets : List DeuceWidget   -- not Set b/c not comparable
  , hoveredWidgets : List DeuceWidget    -- not Set b/c not comparable
  , renameVarTextBox : String
  }

type DeuceWidget
  = DeuceExp EId
  | DeucePat PatternId
  | DeuceEquation ScopeId
  | DeuceExpTarget ExpTargetPosition
  | DeucePatTarget PatTargetPosition

emptyDeuceState : DeuceState
emptyDeuceState =
  { selectedWidgets = []
  , hoveredWidgets = []
  , renameVarTextBox = ""
  }

resetDeuceState m =
  let layoutOffsets = m.layoutOffsets in
  { m | deuceState = emptyDeuceState
      , preview = Nothing
      , layoutOffsets =
          { layoutOffsets |
              deuceToolBox =
                { pinned = layoutOffsets.deuceToolBox.pinned
                , offsets = if layoutOffsets.deuceToolBox.pinned
                            then layoutOffsets.deuceToolBox.offsets
                            else {dx=0, dy=0}
                }
          }
      }

--------------------------------------------------------------------------------


-- Returns list of (pat, patId, startPos, endPos, selectionZoneEndPos)
--
-- For selecting a code element.
findPats : Exp -> List (Pat, PatternId, P.Pos, P.Pos, P.Pos)
findPats e =
  let find e acc =
    case e.val.e__ of
      -- TODO: Support ECase
      EFun _ (p::ps) _ _    -> Utils.concatMapi1 (\(i,p) -> computePatRanges (e.val.eid, 1) [] [i] p) (p::ps) ++ acc
      ETypeCase _ p _ _     -> (computePatRanges (e.val.eid, 1) [] [] p) ++ acc
      ELet _ _ _ p _ _ _    -> (computePatRanges (e.val.eid, 1) [] [] p) ++ acc
      ETyp _ p _ _ _        -> (computePatRanges (e.val.eid, 1) [] [] p) ++ acc
      ETypeAlias _ p _ _ _  -> (computePatRanges (e.val.eid, 1) [] [] p) ++ acc
      _                     -> acc
  in
  foldExp find [] e

computePatRanges scopeId path addPath pat =
  let baseResult = [(pat, (scopeId, path ++ addPath), pat.start, pat.end, pat.end)] in
  case pat.val of
    PConst _ _              -> baseResult
    PBase _ _               -> baseResult
    PVar _ x _              -> baseResult
    PList _ ps _ Nothing _  -> [(pat, (scopeId, path ++ addPath), pat.start, pat.end, { line = pat.start.line, col = pat.start.col + 1 })]
                                ++ Utils.concatMapi1 (\(i,p) -> (computePatRanges scopeId (path ++ addPath) [i] p)) ps
    PList _ ps _ (Just p) _ -> [(pat, (scopeId, path ++ addPath), pat.start, pat.end, { line = pat.start.line, col = pat.start.col + 1 })]
                                ++ Utils.concatMapi1 (\(i,p) -> (computePatRanges scopeId (path ++ addPath) [i] p)) (p::ps)
    PAs _ x _ p             -> case addPath of
                                [n] -> baseResult
                                        ++ computePatRanges scopeId (path ++ addPath) [n + 1] p
                                _   -> baseResult

-- For inserting a code element between other elements
findPatTargets e =
  let find e acc =
    case e.val.e__ of
      EFun _ (p::ps) _ _    -> Utils.concatMapi1 (\(i,p) -> computePatTargets (e.val.eid, 1) [] [i] p) (p::ps) ++ acc
      ETypeCase _ p _ _     -> computePatTargets (e.val.eid, 1) [] [] p ++ acc
      ELet _ _ _ p _ _ _    -> computePatTargets (e.val.eid, 1) [] [] p ++ acc
      ETyp _ p _ _ _        -> computePatTargets (e.val.eid, 1) [] [] p ++ acc
      ETypeAlias _ p _ _ _  -> computePatTargets (e.val.eid, 1) [] [] p ++ acc
      _ -> acc
  in
  foldExp find [] e

computePatTargets scopeId path addPath pat =
  let baseBefore = ((Before, (scopeId, path ++ addPath)),
                    {line = pat.start.line, col = pat.start.col - 1},
                    {line = pat.start.line, col = pat.start.col}) in
  let baseAfter =  ((After, (scopeId, path ++ addPath)),
                    {line = pat.end.line, col = pat.end.col},
                    {line = pat.end.line, col = pat.end.col + 1}) in
  case pat.val of
    PConst ws _                   -> [baseBefore, baseAfter]
    PBase ws _                    -> [baseBefore, baseAfter]
    PVar ws x _                   -> [baseBefore, baseAfter]
    PList ws1 ps ws2 Nothing ws3  -> [baseBefore, baseAfter]
                                      ++ Utils.concatMapi1 (\(i,p) -> (computePatTargets scopeId (path ++ addPath) [i] p)) ps
    PList ws1 ps ws2 (Just p) ws3 -> [baseBefore, baseAfter]
                                      ++ Utils.concatMapi1 (\(i,p) -> (computePatTargets scopeId (path ++ addPath) [i] p)) (p::ps)
    PAs ws1 x ws2 p               -> case addPath of
                                      [n] -> [baseBefore, baseAfter]
                                              ++ computePatTargets scopeId (path ++ addPath) [n + 1] p
                                      _   -> [baseBefore, baseAfter]

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
    let baseTargets =    [((Before, e.val.eid), { line = e.start.line, col = e.start.col - 1 }, e.start),
                          ((After,  e.val.eid),  e.end, { line = e.end.line, col = e.end.col + 1 })] ++ acc
    in
    let offsetTargets =  [((Before, e.val.eid), e.start, { line = e.start.line, col = e.start.col + 1 }),
                          ((After,  e.val.eid), { line = e.end.line, col = e.end.col - 1 }, e.end)] ++ acc
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

showAllDeuceWidgets m = List.member Keys.keyA m.keysDown
shiftKeyPressed m = List.member Keys.keyShift m.keysDown
showDeuceWidgets m = shiftKeyPressed m
needsRun m =
  m.code /= m.lastRunCode
showSelectedDeuceWidgets m =
  not (needsRun m) && m.preview == Nothing

betweenPos start pixelPos end =
  (start.line <= pixelPos.row + 1) &&
  (start.col <= pixelPos.column + 1) &&
  (end.line >= pixelPos.row + 1) &&
  (end.col > pixelPos.column + 1)

pixelToRowColPosition pos m =
  let rowPadding = m.codeBoxInfo.offsetHeight in
  let colPadding = m.codeBoxInfo.offsetLeft +  m.codeBoxInfo.gutterWidth in
  let row = truncate((toFloat(pos.y) - rowPadding + m.codeBoxInfo.marginTopOffset) / m.codeBoxInfo.lineHeight) in
  let col = truncate((toFloat(pos.x) - colPadding - m.codeBoxInfo.marginLeftOffset) / m.codeBoxInfo.characterWidth) in
    {row = row + m.codeBoxInfo.firstVisibleRow, column = col}

rowColToPixelPos pos m =
  let rowPadding = m.codeBoxInfo.offsetHeight in
  let colPadding = m.codeBoxInfo.offsetLeft +  m.codeBoxInfo.gutterWidth in
  let y = (toFloat(pos.line - m.codeBoxInfo.firstVisibleRow) - 1) * m.codeBoxInfo.lineHeight + rowPadding + m.codeBoxInfo.marginTopOffset in
  let x = (toFloat(pos.col) - 0.5) * m.codeBoxInfo.characterWidth + colPadding + m.codeBoxInfo.marginLeftOffset in
    {x = x, y = y}

hoveringItem start p end =
  case p of
    Nothing   -> False
    Just pos  -> betweenPos start pos end

leadingTrailingSpaces str index start end =
  let leftTrim = String.trimLeft str in
  let rightTrim = String.trimRight str in
  let fullLength = String.length str in
  let leftTrimLength = String.length leftTrim in
  let rightTrimLength = String.length rightTrim in
  let leading = fullLength - leftTrimLength in
  let trailing = rightTrimLength in
  if index == start.line 
  then 
    (start.col - 1, (trailing - leading) + (start.col - 1))
  else
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
                                      (Dict.insert currIndex (leadingTrailingSpaces str currIndex start end) results)
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

patBoundingPolygon :
    DeuceWidget
    -> Pat
    -> { a | end : P.Pos }
    -> ( DeuceWidget, Dict Int ( Int, Int ) )
patBoundingPolygon id pat end = boundingPolygon unparsePat id pat end


expBoundingPolygon :
    DeuceWidget
    -> Exp
    -> { a | end : P.Pos }
    -> ( DeuceWidget, Dict Int ( Int, Int ) )
expBoundingPolygon id exp end = boundingPolygon unparse id exp end


boundingPolygon :
    ({ a | start : P.Pos } -> String)
    -> DeuceWidget
    -> { a | start : P.Pos }
    -> { b | end : P.Pos }
    -> ( DeuceWidget, Dict Int ( Int, Int ) )
boundingPolygon unparseExpOrPat id expOrPat endExpOrPat =
  let start = expOrPat.start in
  let end = endExpOrPat.end in
  let string = unparseExpOrPat expOrPat in
  let lines = String.lines string in
  if List.length lines == 1 && start.line == end.line
  then
    let s = start.line in
    case id of
      DeuceEquation _ ->
        (id, Dict.fromList [(s, (start.col, end.col - 1))])
      _ -> 
        (id, Dict.fromList [(s, (start.col - 1, end.col - 1))])
  else
    let leading = leadingNewlines (Just lines) 0 in
    let output = lineStartEnd (Just lines) (start.line - leading) start end (Dict.empty) in
    case id of
      DeuceEquation _ ->
        let parenLine = Dict.get start.line output in
        case parenLine of
            Just (firstLineStart, firstLineEnd) ->  (id, (Dict.insert start.line (firstLineStart + 1, firstLineEnd) output))
            _ -> (id, output)
      _ -> (id, output)
