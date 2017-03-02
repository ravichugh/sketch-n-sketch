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
  }

resetDeuceState m = { m | deuceState = emptyDeuceState }

--------------------------------------------------------------------------------

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

computePatRanges expId path addPath pat =
  let baseResult = [(pat, (expId, path ++ addPath), pat.start, pat.end, pat.end)] in
  case pat.val of
    PConst _ _              -> baseResult
    PBase _ _               -> baseResult
    PVar _ x _              -> baseResult
    PList _ ps _ Nothing _  -> [(pat, (expId, path ++ addPath), pat.start, pat.end, { line = pat.start.line, col = pat.start.col + 1 })]
                                ++ Utils.concatMapi1 (\(i,p) -> (computePatRanges expId (path ++ addPath) [i] p)) ps
    PList _ ps _ (Just p) _ -> [(pat, (expId, path ++ addPath), pat.start, pat.end, { line = pat.start.line, col = pat.start.col + 1 })]
                                ++ Utils.concatMapi1 (\(i,p) -> (computePatRanges expId (path ++ addPath) [i] p)) (p::ps)
    PAs _ x _ p             -> case addPath of
                                [n] -> baseResult
                                        ++ computePatRanges expId (path ++ addPath) [n + 1] p
                                _   -> baseResult

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
                                      ++ Utils.concatMapi1 (\(i,p) -> (computePatTargets expId (path ++ addPath) [i] p)) ps
    PList ws1 ps ws2 (Just p) ws3 -> [baseBefore, baseAfter]
                                      ++ Utils.concatMapi1 (\(i,p) -> (computePatTargets expId (path ++ addPath) [i] p)) (p::ps)
    PAs ws1 x ws2 p               -> case addPath of
                                      [n] -> [baseBefore, baseAfter]
                                              ++ computePatTargets expId (path ++ addPath) [n + 1] p
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

showAllDeuceWidgets m = List.member Keys.keyA m.keysDown
shiftKeyPressed m = List.member Keys.keyShift m.keysDown
showDeuceWidgets m = shiftKeyPressed m
needsRun m =
  m.code /= m.lastRunCode

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
  -- difference from expBoundingPolygon: unparsePat instead of unparse
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
