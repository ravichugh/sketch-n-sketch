module UpdateUtils exposing (..)

import Lang exposing (..)
import LangUtils exposing (..)
import Syntax
import Results exposing (Results(..), ok1)
import LazyList exposing (LazyList(..))
import Dict exposing (Dict)
import Info exposing (WithInfo)
import LangUtils exposing (valToExp, valToExpFull, IndentStyle(..), pruneEnv, pruneEnvPattern, valToString)
import Regex
import Utils
import Set exposing (Set)
import Pos exposing (Pos)
import ValBuilder as Vb
import ValUnbuilder as Vu

bvToString: EBaseVal -> String
bvToString b = Syntax.unparser Syntax.Elm <| withDummyExpInfo <| EBase space0 <| b

splitRegex = Regex.regex "\\b|(?=[-\\]\"'\\[\\)\\(,><\\\\])"

diffExp: Exp -> Exp -> String --Summary in strings
diffExp e1 e2 =
   let s1 = Syntax.unparser Syntax.Elm e1 in
   let s2 = Syntax.unparser Syntax.Elm e2 in
   let before = Regex.split Regex.All splitRegex s1 in
   let after = Regex.split Regex.All splitRegex s2 in
   let difference = diff identity before after in
   displayDiff identity difference

diffExpWithPositions: Exp -> Exp -> (String, List Exp)
diffExpWithPositions e1 e2 =
  let s1 = Syntax.unparser Syntax.Elm e1 in
   let s2 = Syntax.unparser Syntax.Elm e2 in
   let before = Regex.split Regex.All splitRegex s1 in
   let after = Regex.split Regex.All splitRegex s2 in
   let difference = diff identity before after in
   displayDiffPositions identity difference

diffVals: List Val -> List Val -> List (DiffChunk (List Val))
diffVals before after =
  let vToString = Syntax.unparser Syntax.Elm << valToExp (ws "") InlineSpace in
  let beforeStrings = List.map (\v -> (v, vToString v)) before in
  let afterStrings = List.map (\v -> (v, vToString v)) after in
  let diffRaw= diff Tuple.second beforeStrings afterStrings in
  diffRaw |> List.map (diffChunkMap <| List.map Tuple.first)

type DiffChunk a = DiffEqual a | DiffRemoved a | DiffAdded a

diffChunkMap: (a -> b) -> DiffChunk a -> DiffChunk b
diffChunkMap f d = case d of
   DiffEqual a -> DiffEqual (f a)
   DiffRemoved a -> DiffRemoved (f a)
   DiffAdded a -> DiffAdded (f a)

--diff before after =

displayDiff: (a -> String) -> List (DiffChunk (List a)) -> String
displayDiff tos difference =
  difference
     |> List.concatMap (\d ->
       case d of
         DiffEqual l -> []
         DiffRemoved a -> [" -" ++ String.join "" (List.map tos a)]
         DiffAdded a ->   [" +" ++ String.join "" (List.map tos a)]
     )
     |> String.join ","

displayDiffPositions: (a -> String) -> List (DiffChunk (List a)) -> (String, List Exp)
displayDiffPositions tos difference =
  let dummyExp msg row col row2 col2 = WithInfo (Exp_ (EBase space0 <| EString "\"" msg) 0) (Pos row col) (Pos row2 col2) in
  let lToString l = String.join "" (List.map tos l) in
  let newStringRowCol prevRow prevCol l =
    let kept = lToString l in
    let rowAdded = (String.indexes "\n" kept) |> List.length in
    let colAdded = String.length (Regex.replace Regex.All (Regex.regex "(.*\r?\n)*") (\_ -> "") kept) in
    if rowAdded == 0 then (kept, prevRow, prevCol + colAdded) else (kept, prevRow + rowAdded, colAdded)
  in
  let maybeComma s = if s == "" then s else s ++ ", \n" in
  let aux:(Int,     Int) ->  (String, List Exp) -> List (DiffChunk (List a)) -> (String, List Exp)
      aux (prevRow, prevCol) (string, prevAcc)     difference = case difference of
    [] -> (string, prevAcc)
    DiffEqual l::tail ->
     let (sameString, newRow, newCol) = newStringRowCol prevRow prevCol l in
     aux (newRow, newCol) (string, prevAcc) tail
    DiffRemoved l::DiffAdded d::tail ->
     let removed = lToString l in
     let (added, newRow, newCol) = newStringRowCol prevRow prevCol d in
     let e = dummyExp added prevRow prevCol newRow newCol in
     aux (newRow, newCol) (maybeComma string ++ "Line " ++ toString prevRow ++ ": " ++ removed ++ " -> " ++ added, e::prevAcc) tail
    DiffRemoved l::tail ->
     let removed = lToString l in
     let e = dummyExp ("- " ++ removed) prevRow prevCol prevRow (prevCol + List.length l) in
     aux (prevRow, prevCol) (maybeComma string ++ "Line " ++ toString prevRow ++ ": -" ++ removed, e :: prevAcc) tail
    DiffAdded l::tail ->
     let (added, newRow, newCol) = newStringRowCol prevRow prevCol l in
     let e = dummyExp added prevRow prevCol newRow newCol in
     let _ = Debug.log ("Highlighting " ++ toString prevRow ++ "," ++ toString prevCol ++ " -> " ++ toString newRow ++ "," ++ toString newCol) () in
     aux (newRow, newCol) (maybeComma string ++ "Line " ++ toString prevRow ++ ": " ++ added, e::prevAcc) tail
  in aux (0, 0) ("", []) difference

--diff: List a -> List a -> List (() -> List (DiffChunk (List a))) -> List (DiffChunk (List a))

diff: (a -> String) -> List a -> List a -> List (DiffChunk (List a))
diff keyOf before after =
    {- Adapted from https://github.com/paulgb/simplediff/blob/master/javascript/simplediff.js
        Find the differences between two lists. Returns a list of pairs, where the
        first value is in ['+','-','='] and represents an insertion, deletion, or
        no change for that list. The second value of the pair is the list
        of elements.
        Params:
            before  the old list of immutable, comparable values (ie. a list
                    of strings)
            after   the new list of immutable, comparable values
        Returns:
            A list of pairs, with the first part of the pair being one of three
            strings ('-', '+', '=') and the second part being a list of values from
            the original before and/or after lists. The first part of the pair
            corresponds to whether the list of values is a deletion, insertion, or
            unchanged, respectively.
        Examples:
            diff([1,2,3,4],[1,3,4])
            [["=",[1]],["-",[2]],["=",[3,4]]]
            diff([1,2,3,4],[2,3,4,1])
            [["-",[1]],["=",[2,3,4]],["+",[1]]]
            diff('The quick brown fox jumps over the lazy dog'.split(/[ ]+/),
                'The slow blue cheese drips over the lazy carrot'.split(/[ ]+/))
            [["=",["The"]],
             ["-",["quick","brown","fox","jumps"]],
             ["+",["slow","blue","cheese","drips"]],
             ["=",["over","the","lazy"]],
             ["-",["dog"]],
             ["+",["carrot"]]]
    -}

    -- Create a map from before values to their indices
    let oldIndexMapRev =
      List.foldl (\(b,i) d ->
         Dict.update (keyOf b) (\mbv -> case mbv of
          Just v -> Just <| i::v
          Nothing -> Just [i]
        ) d) Dict.empty (Utils.zipWithIndex before)
    in
    let oldIndexMap = Dict.map (\k -> List.reverse) oldIndexMapRev in

    -- Find the largest substring common to before and after.
    -- We use a dynamic programming approach here.
    -- We iterate over each value in the `after` list.
    -- At each iteration, `overlap[inew]` is the
    -- length of the largest substring of `before.slice(0, iold)` equal
    -- to a substring of `after.splice(0, iold)` (or unset when
    -- `before[iold]` != `after[inew]`).
    -- At each stage of iteration, the new `overlap` (called
    -- `_overlap` until the original `overlap` is no longer needed)
    -- is built from the old one.
    -- If the length of overlap exceeds the largest substring
    -- seen so far (`subLength`), we update the largest substring
    -- to the overlapping strings.

    -- `startOld` is the index of the beginning of the largest overlapping
    -- substring in the before list. `startNew` is the index of the beginning
    -- of the same substring in the after list. `subLength` is the length that
    -- overlaps in both.
    -- These track the largest overlapping substring seen so far, so naturally
    -- we start with a 0-length substring.

    let (overlap, startOld, startNew, subLength) =
         List.foldl (\(afterinew, inew) (overlap, startOld, startNew, subLength) ->
           let oldIndexMapAfterInew = Dict.get (keyOf afterinew) oldIndexMap |> Maybe.withDefault [] in
           List.foldl (\iold (overlap_, startOld, startNew, subLength) ->
              -- now we are considering all values of val such that
              -- `before[iold] == after[inew]`
              let newIoldValue = (if iold /= 0 then Dict.get (iold - 1) overlap |> Maybe.withDefault 0 else 0) + 1 in
              let newOverlap_ = Dict.insert iold newIoldValue overlap_ in
              if newIoldValue > subLength then
                   (newOverlap_, iold - newIoldValue + 1, inew - newIoldValue + 1 , newIoldValue)
              else (newOverlap_, startOld,             startNew,              subLength)
           ) (Dict.empty, startOld, startNew, subLength) oldIndexMapAfterInew
         ) (Dict.empty, 0, 0, 0) (Utils.zipWithIndex after)
    in
    if subLength == 0 then
        -- If no common substring is found, we return an insert and delete...
        (if List.isEmpty before then [] else [DiffRemoved before]) ++
        (if List.isEmpty after then [] else [DiffAdded after])
    else
    -- otherwise, the common substring is unchanged and we recursively
    -- diff the text before and after that substring
    diff keyOf (List.take startOld before) (List.take startNew after) ++
    (DiffEqual (List.drop startNew after |> List.take subLength) ::
      diff keyOf  (List.drop (startOld + subLength) before) (List.drop (startNew + subLength) after))

type Diff3Chunk a = Diff3Merged (DiffChunk a) | Diff3Conflict (List (DiffChunk a)) (List (DiffChunk a))

diff3ChunkMap: (a -> b) -> Diff3Chunk a -> Diff3Chunk b
diff3ChunkMap f d = case d of
   Diff3Merged a -> Diff3Merged (diffChunkMap f a)
   Diff3Conflict a b -> Diff3Conflict (List.map (diffChunkMap f) a) (List.map (diffChunkMap f) b)

-- Three way merge. We diff the diffs
diff3: (a -> String) -> List a -> List a -> List a  -> List (Diff3Chunk a)
diff3 keyOf original update1 update2 =
  let modifs1 = diff keyOf original update1 in
  let modifs2 = diff keyOf original update2 in
  -- Now we flatten the diffs and we compare the diffs to merge them !
  let flattenDiff diff = case diff of
    DiffEqual a   -> List.map DiffEqual a
    DiffAdded a   -> List.map DiffAdded a
    DiffRemoved a -> List.map DiffRemoved a
  in
  let newKeyOf d = case d of
    DiffEqual a -> "=" ++ keyOf a
    DiffAdded a -> "+" ++ keyOf a
    DiffRemoved a -> "-" ++ keyOf a
  in
  let diff1: List (DiffChunk a)
      diff1 = List.concatMap flattenDiff modifs1 in -- A list of differences at the element level.
  let diff2: List (DiffChunk a)
      diff2  = List.concatMap flattenDiff modifs2 in
  let diffOfDiffs: List (DiffChunk (List (DiffChunk a)))
      diffOfDiffs = diff newKeyOf diff1 diff2 in
  let convertDiffs: List (Diff3Chunk a) -> List (DiffChunk (List (DiffChunk a))) -> List (Diff3Chunk a)
      convertDiffs acc thediffs = case thediffs of
    [] -> acc
    DiffRemoved leftDiffs:: DiffAdded rightDiffs :: diffTail ->
        -- Conflict here, but only if there is a deletion.
        convertDiffs (acc ++ [Diff3Conflict leftDiffs rightDiffs]) diffTail
    DiffEqual equalDiffs :: diffTail ->
        convertDiffs (acc ++ List.map Diff3Merged equalDiffs) diffTail
    DiffRemoved leftDiffs::diffTail ->
        convertDiffs (acc ++ List.map Diff3Merged leftDiffs) diffTail
    DiffAdded rightDiffs :: diffTail ->
        convertDiffs (acc ++ List.map Diff3Merged rightDiffs) diffTail
  in convertDiffs [] diffOfDiffs

regroupDiffs: List (DiffChunk a) -> List (DiffChunk (List a))
regroupDiffs diffs2 =
  let assembleDiffs acc currentModif thediffs =  case (currentModif, thediffs) of
    (Just (DiffEqual aa),    DiffEqual a   :: diffTail) -> assembleDiffs acc (Just (DiffEqual (aa ++ [a]))) diffTail
    (Just (DiffRemoved aa),  DiffRemoved a :: diffTail) -> assembleDiffs acc (Just (DiffRemoved (aa ++ [a]))) diffTail
    (Just (DiffAdded aa),    DiffAdded a   :: diffTail) -> assembleDiffs acc (Just (DiffAdded (aa ++ [a]))) diffTail
    (Just x, diffTail)                    -> assembleDiffs (acc ++ [x]) Nothing diffTail
    (Nothing, []) -> acc
    (Nothing, DiffEqual a   :: diffTail) -> assembleDiffs acc (Just (DiffEqual [a]))   diffTail
    (Nothing, DiffRemoved a :: diffTail) -> assembleDiffs acc (Just (DiffRemoved [a])) diffTail
    (Nothing, DiffAdded a   :: diffTail) -> assembleDiffs acc (Just (DiffAdded [a]))   diffTail
  in assembleDiffs [] Nothing diffs2

-- When there are edit conflicts, resolves the conflict by keeping only the left modifications.
autodiff3: (a -> String) -> (List a -> List (DiffChunk a) -> List (DiffChunk a) -> List (DiffChunk a)) -> List a -> List a -> List a  -> List (DiffChunk (List a))
autodiff3 keyOf conflictPolicy original update1 update2 =
  let originalDiff = diff3 keyOf original update1 update2 in
  let assembleDiffs acc currentModif thediffs = case (currentModif, thediffs) of
    (Just (DiffEqual aa),    Diff3Merged (DiffEqual a)   :: diffTail) -> assembleDiffs acc (Just (DiffEqual (aa ++ [a]))) diffTail
    (Just (DiffRemoved aa),  Diff3Merged (DiffRemoved a) :: diffTail) -> assembleDiffs acc (Just (DiffRemoved (aa ++ [a]))) diffTail
    (Just (DiffAdded aa),    Diff3Merged (DiffAdded a)   :: diffTail) -> assembleDiffs acc (Just (DiffAdded (aa ++ [a]))) diffTail
    (Just x, diffTail)                    -> assembleDiffs (acc ++ [x]) Nothing diffTail
    (Nothing, []) -> acc
    (Nothing, Diff3Merged (DiffEqual a)   :: diffTail) -> assembleDiffs acc (Just (DiffEqual [a]))   diffTail
    (Nothing, Diff3Merged (DiffRemoved a) :: diffTail) -> assembleDiffs acc (Just (DiffRemoved [a])) diffTail
    (Nothing, Diff3Merged (DiffAdded a)   :: diffTail) -> assembleDiffs acc (Just (DiffAdded [a]))   diffTail
    (Nothing, Diff3Conflict aa bb:: diffTail) ->
        let (accNotRemoved, accRemoved) = case Utils.snocUnapply acc of
             Just (init, DiffRemoved aa) -> (init, aa)
             _ -> (acc, [])
        in
        assembleDiffs (accNotRemoved ++ regroupDiffs (conflictPolicy accRemoved aa bb)) Nothing diffTail
  in assembleDiffs [] Nothing originalDiff

-- Simple conflict policy.
defaultConflictPolicy: List (DiffChunk a) -> List (DiffChunk a) -> List (DiffChunk a)
defaultConflictPolicy a b = a

--------------------------------------------------------------------------------
-- Value builders with dummy ids, brought back from the dead in Lang


-- Combine mupltiple runs on updating the same expression to a single result
{-combineResults: (Env, Exp) -> List (Result String (Env, Exp)) -> Result String (Env, Exp)
combineResults ((envo, expo) as original) results =
  case results of
        [] -> ok1 original
        [head] -> head
        Errs msg::tail ->
          case combineResults original tail of
            Errs msg2 -> Errs <| msg ++ msg2
            anything  -> anything
        (Oks r1 as head)::Errs msg::tail ->
          combineResults original (head::tail)
        Oks LazyNil :: Oks r2 :: tail -> Oks LazyNil
        Oks r1 :: Oks LazyNil :: tail -> Oks LazyNil
        Oks (LazyCons (env1, exp1) lazyTail1):: Oks (LazyCons (env2, exp2) lazyTail2):: tail ->
          let finalEnv = triCombine expo envo env1 env2 in
          let finalExp = mergeExp expo exp1 exp2 in
          Oks (LazyCons (finalEnv, finalExp) (Lazy.lazy
-}
-- Time to merge all possible results. May result in exponential blowup if each result is ambiguous.

recursiveMerge: (a -> a -> ma -> a -> ma -> (a, ma)) -> a -> List (a, ma) -> (a, Maybe ma)
recursiveMerge merge original modifications =
  case modifications of
    [] -> (original, Nothing)
    [(head, headDiff)] -> (head, Just headDiff)
    (head1, head1Diff)::(head2, head2Diff)::tail ->
      recursiveMerge merge original (merge original head1 head1Diff head2 head2Diff:: tail)

recursiveMergeVal: Val -> List (Val, VDiffs) -> (Val, Maybe VDiffs)
recursiveMergeVal = recursiveMerge mergeVal

type alias TupleDiffs a = List (Int, a)

type VListElemDiff = VListElemUpdate VDiffs | VListElemInsert Int | VListElemDelete Int

type VDictElemDiff = VDictElemDelete | VDictElemInsert | VDictElemUpdate VDiffs

type alias EnvDiffs = TupleDiffs VDiffs
-- The environment of a closure if it was modified, the modifications of an environment else.
type VDiffs = VClosureDiffs EnvDiffs (Maybe EDiffs)
            | VListDiffs (List (Int, VListElemDiff))
            | VDictDiffs (Dict (String, String) VDictElemDiff)
            | VRecordDiffs (Dict String VDiffs)
            | VConstDiffs

type EDiffs = EChanged -- TODO: More diffs for expressions there.

type PDiffs = PChanged -- TODO: More diffs for patterns there

type BranchDiffs = BChanged -- TODO: More diffs for branches there

extractors: {
  unapply: String -> (a -> Maybe b) -> a -> (b -> Result String c) -> Result String c,
  unapplySeq: String -> (a -> Maybe b) -> List a -> (List b -> Result String c) -> Result String c
  }
extractors = {
  unapply = \msg unapply e c -> Result.fromMaybe msg (unapply e) |> Result.andThen c,
  unapplySeq = \msg transformer input c -> List.map transformer input |> Utils.projJusts |> Result.fromMaybe msg |> Result.andThen c
  }

maybeToVal: Val -> (a -> Val) -> Maybe a -> Val
maybeToVal v subroutine mba = case mba of
  Just x  -> Vb.constructor v "Just"    [subroutine x]
  Nothing -> Vb.constructor v "Nothing" []

valToMaybe: (Val -> Result String a)  -> Val -> Result String (Maybe a)
valToMaybe subroutine v = case Vu.constructor Ok v of
  Ok ("Just", [x]) -> subroutine x |> Result.map Just
  Ok ("Nothing", []) -> Ok Nothing
  Ok _ -> Err <| "Expected Just or Nothing, got " ++ valToString v
  Err msg -> Err msg

envDiffsToVal: Val -> EnvDiffs -> Val
envDiffsToVal v = (Vb.list v) ((Vb.tuple2 v) (Vb.int v) (vDiffsToVal v))

valToEnvDiffs: Val -> Result String EnvDiffs
valToEnvDiffs = Vu.list (Vu.tuple2 Vu.int valToVDiffs)

vListElemDiffToVal: Val -> VListElemDiff -> Val
vListElemDiffToVal v velem = case velem of
   VListElemUpdate d -> (Vb.constructor v) "VListElemUpdate" [vDiffsToVal v d]
   VListElemInsert i -> (Vb.constructor v) "VListElemInsert" [(Vb.int v) i]
   VListElemDelete i -> (Vb.constructor v) "VListElemDelete" [(Vb.int v) i]

valToVListElemDiff: Val -> Result String VListElemDiff
valToVListElemDiff v = case Vu.constructor Ok v of
  Ok ("VListElemUpdate", [d]) -> valToVDiffs d |> Result.map VListElemUpdate
  Ok ("VListElemInsert", [i]) -> Vu.int i |> Result.map VListElemInsert
  Ok ("VListElemDelete", [i]) -> Vu.int i |> Result.map VListElemDelete
  Ok _ -> Err <| "Expected VListElemUpdate[_], VListElemInsert[_], VListElemDelete[_], got " ++ valToString v
  Err msg -> Err msg

vDictElemDiffToVal: Val -> VDictElemDiff -> Val
vDictElemDiffToVal v velem = case velem of
  VDictElemDelete -> (Vb.constructor v) "VDictElemDelete" []
  VDictElemInsert -> (Vb.constructor v) "VDictElemInsert" []
  VDictElemUpdate u -> (Vb.constructor v) "VDictElemUpdate" [vDiffsToVal v u]

valToVDictElemDiff: Val -> Result String VDictElemDiff
valToVDictElemDiff v = case Vu.constructor Ok v of
  Ok ("VDictElemDelete", []) -> Ok VDictElemDelete
  Ok ("VDictElemInsert", []) -> Ok VDictElemInsert
  Ok ("VDictElemUpdate", [u]) -> Result.map VDictElemUpdate <| valToVDiffs u
  Ok _ -> Err <| "Expected VDictElemDelete[], VDictElemInsert[], VDictElemUpdate[_], got " ++ valToString v
  Err msg -> Err msg

-- Val encoding of VDiffs
vDiffsToVal: Val -> VDiffs -> Val
vDiffsToVal v vdiffs = case vdiffs of
  VClosureDiffs e mbe -> (Vb.constructor v) "VClosureDiffs" [envDiffsToVal v e, maybeToVal v (eDiffsToVal v) mbe]
  VListDiffs list     -> (Vb.constructor v) "VListDiffs"    [(Vb.list v) ((Vb.tuple2 v) (Vb.int v) (vListElemDiffToVal v)) list]
  VConstDiffs         -> (Vb.constructor v) "VConstDiffs"   []
  VDictDiffs d        -> (Vb.constructor v) "VDictDiffs"    [(Vb.dict v) (vDictElemDiffToVal v) d]
  VRecordDiffs d      -> (Vb.constructor v) "VRecordDiffs " [(Vb.record v) (vDiffsToVal v) d]

valToVDiffs: Val -> Result String VDiffs
valToVDiffs v = case Vu.constructor Ok v of
  Ok ("VClosureDiffs", [v1, v2]) -> Result.map2 VClosureDiffs (valToEnvDiffs v1) (valToMaybe valToEDiffs v2)
  Ok ("VListDiffs"   , [l]) -> Vu.list (Vu.tuple2 Vu.int valToVListElemDiff) l |> Result.map VListDiffs
  Ok ("VConstDiffs"  , []) -> Ok VConstDiffs
  Ok ("VDictDiffs"   , [d]) -> Vu.dict valToVDictElemDiff d|> Result.map VDictDiffs
  Ok ("VRecordDiffs ", [d]) -> Vu.record valToVDiffs d |> Result.map VRecordDiffs
  Ok _ -> Err <| "Expected VDictElemDelete[], VDictElemInsert[], VDictElemUpdate[_], got " ++ valToString v
  Err msg -> Err msg

eDiffsToVal: Val -> EDiffs -> Val
eDiffsToVal v ediffs = case ediffs of
  EChanged -> (Vb.constructor v) "EChanged" []

valToEDiffs: Val -> Result String EDiffs
valToEDiffs v = case Vu.constructor Ok v of
  Ok ("EChanged", []) -> Ok EChanged
  Ok _ -> Err <| "Expected EChanged, got " ++ valToString v
  Err msg -> Err msg

{-
pDiffsToVal: ValBuilders -> PDiffs -> Val
pDiffsToVal v pdiffs = case pdiffs of
  PChanged -> (Vb.constructor v) "PChanged" []

bDiffsToVal: ValBuilders ->
             BranchDiffs -> Val
bDiffsToVal bdiffs = case bdiffs of
  BChanged -> (Vb.constructor v) "BChanged" []
-}

offset: Int -> List (Int, a) -> List (Int, a)
offset n defaultVDiffs = List.map (\(i, e) -> (i + n, e)) defaultVDiffs

-- Invoke this only if strictly necessary.
defaultVDiffs: Val -> Val -> Result String (Maybe VDiffs)
defaultVDiffs original modified =
  case (original.v_, modified.v_) of
    (VList originals, VList modified) ->
      defaultListDiff valToString defaultVDiffs originals modified
    (VClosure isRec1 pats1 body1 env1, VClosure isRec2 pats2 body2 env2) ->
      defaultEnvDiffs env1 env2 |> Result.map (\mbEnvDiff ->
        let mbBodyDiff = if Syntax.unparser Syntax.Elm body1 == Syntax.unparser Syntax.Elm body2 then Nothing else (Just EChanged) in
        case mbEnvDiff of
          Nothing ->
            case mbBodyDiff of
              Nothing -> Nothing
              _ -> Just <| VClosureDiffs [] mbBodyDiff
          Just x -> Just <| VClosureDiffs x mbBodyDiff
      )
    (VDict original, VDict modified) ->
      defaultDictDiffs valToString defaultVDiffs original modified
    (VRecord original, VRecord modified) ->
      defaultRecordDiffs valToString defaultVDiffs original modified
    (v1, v2) -> if v1 == v2 then Ok Nothing else Ok (Just VConstDiffs)

defaultListDiff: (a -> String) -> (a -> a -> Result String (Maybe VDiffs)) -> List a -> List a -> Result String (Maybe VDiffs)
defaultListDiff keyOf defaultElemModif elems1 elems2 =
  let difference = diff keyOf elems1 elems2 in
  let aux: Int -> List (Int, VListElemDiff) -> List (DiffChunk (List a)) -> Result String (Maybe VDiffs)
      aux i accDiffs diffs = case diffs of
        [] -> case List.reverse accDiffs of
           [] -> Ok Nothing
           diffs -> Ok <| Just <| VListDiffs diffs
        DiffEqual elems::difftail ->
          aux (i + List.length elems) accDiffs difftail
        DiffRemoved removed::DiffAdded added::difftail ->
          let lengthRemoved = List.length removed in
          let lengthAdded = List.length added in
          let toInsertRes = List.map3 (\i r a -> defaultElemModif r a |> Result.map (\mbv -> mbv |> Maybe.map (\v -> (i, VListElemUpdate v)))) (List.range i (i + lengthAdded - 1)) removed added in
          Utils.projOk toInsertRes |> Result.andThen (\toInsert ->
            let accDiffs1 = maybeReverseInsert toInsert accDiffs in
            let accDiffs2 = if lengthRemoved > lengthAdded then
                  (i + lengthAdded, VListElemDelete (lengthRemoved - lengthAdded))::accDiffs1
                 else accDiffs1
            in
            let accDiffs3 = if lengthAdded > lengthRemoved then
                    (i + lengthRemoved, VListElemInsert (lengthAdded - lengthRemoved))::accDiffs2
                  else accDiffs2
            in
            aux (i + lengthRemoved) accDiffs3 difftail
          )
        DiffRemoved elems::difftail ->
          let removedLength = List.length elems in
          aux (i + removedLength) ((i, VListElemDelete removedLength)::accDiffs) difftail
        DiffAdded elems::difftail ->
          let addedLength = List.length elems in
          aux i ((i, VListElemInsert addedLength)::accDiffs) difftail
  in
  aux 0 [] difference

defaultEnvDiffs: Env -> Env -> Result String (Maybe EnvDiffs) -- lowercase val so that it can be applied to something else?
defaultEnvDiffs elems1 elems2 =
  let aux: Int -> List (Int, VDiffs) -> Env         -> Env -> Result String (Maybe EnvDiffs)
      aux  i      revEnvDiffs          envToCollect1  envToCollect2 =
        case (envToCollect1, envToCollect2) of
          ([], []) ->
            case List.reverse revEnvDiffs of
              [] -> Ok Nothing
              envDiffs -> Ok <| Just envDiffs
          (((k1, v1) as ehd1)::etl1, ((k2, v2) as ehd2)::etl2) ->
            if k1 /= k2 then Err <| "trying to compute a diff on unaligned environments " ++ k1 ++ "," ++ k2 else
            if valEqual v1 v2 then
              aux (i + 1) revEnvDiffs etl1 etl2
            else
              defaultVDiffs v1 v2 |> Result.andThen (\mbv ->
                let newRevEnvDiffs = case mbv of
                  Nothing -> revEnvDiffs
                  Just v -> (i, v)::revEnvDiffs
                in
                aux (i + 1) newRevEnvDiffs etl1 etl2
              )
          _ -> Err <| "Environments do not have the same size: " ++ envToString envToCollect1 ++ ", " ++ envToString envToCollect2
  in aux 0 [] elems1 elems2

defaultTupleDiffs: (a -> String) -> (a -> a -> Result String (Maybe VDiffs)) -> List a -> List a -> Result String (Maybe (TupleDiffs VDiffs)) -- lowercase val so that it can be applied to something else?
defaultTupleDiffs keyOf defaultElemModif elems1 elems2 =
  let aux: Int -> List (Int, VDiffs) -> List a  -> List a -> Result String (Maybe (TupleDiffs VDiffs))
      aux  i      revEnvDiffs          l1         l2 =
        case (l1, l2) of
          ([], []) ->
            case List.reverse revEnvDiffs of
              [] -> Ok Nothing
              tupleDiffs -> Ok (Just tupleDiffs)

          (v1::etl1, v2::etl2) ->
            if keyOf v1 == keyOf v2 then
              aux (i + 1) revEnvDiffs etl1 etl2
            else
              defaultElemModif v1 v2 |> Result.andThen (\mbv ->
                 let newRevEnvDiffs = case mbv of
                   Nothing -> revEnvDiffs
                   Just v -> (i, v)::revEnvDiffs
                 in
                 aux (i + 1) newRevEnvDiffs etl1 etl2)

          _ -> Err <| "Tuples do not have the same size: " ++ toString l1 ++ ", " ++ toString l2
  in aux 0 [] elems1 elems2

defaultDictDiffs: (Val -> String) -> (Val -> Val -> Result String (Maybe VDiffs)) -> Dict (String, String) Val -> Dict (String, String) Val -> Result String (Maybe VDiffs)
defaultDictDiffs keyOf defaultElemModif elems1 elems2 =
  Result.map (\d -> if Dict.isEmpty d then Nothing else Just <| VDictDiffs d) <| Dict.merge
    (\k1 v1 acc -> Result.map (Dict.insert k1 VDictElemDelete) acc)
    (\k v1 v2 acc -> if keyOf v1 == keyOf v2 then acc
       else
         acc |> Result.andThen (\acc ->
           defaultElemModif v1 v2 |> Result.map (\mbv ->
             mbv |> Maybe.map (\v -> Dict.insert k (VDictElemUpdate v) acc) |> Maybe.withDefault acc)))
    (\k2 v2 acc -> Result.map (Dict.insert k2 VDictElemInsert) acc)
    elems1
    elems2
    (Ok Dict.empty)

defaultRecordDiffs: (Val -> String) -> (Val -> Val -> Result String (Maybe VDiffs)) -> Dict String Val -> Dict String Val -> Result String (Maybe VDiffs)
defaultRecordDiffs keyOf defaultElemModif elems1 elems2 =
  Result.map (\d -> if Dict.isEmpty d then Nothing else Just <| VRecordDiffs d) <| Dict.merge
    (\k1 v1 acc -> Err <| "Not allowed to remove a key from record:" ++ k1)
    (\k v1 v2 acc -> if keyOf v1 == keyOf v2 then acc else acc |> Result.andThen (\acc ->
      defaultElemModif v1 v2 |> Result.map (\mbv ->
        mbv |> Maybe.map (\v ->
         Dict.insert k v acc) |> Maybe.withDefault acc)))
    (\k2 v2 acc -> Err <| "Not allowed to insert a key to record:" ++ k2)
    elems1
    elems2
    (Ok Dict.empty)

-- Assume that the lists are always aligned
autoMergeTuple: (o -> a -> a -> a) -> List o -> List a -> List a -> List a
autoMergeTuple submerger original modified1 modified2 =
  List.map3 submerger original modified1 modified2

mergeTuple: (a -> a -> VDiffs -> a -> VDiffs -> (a, VDiffs)) -> List a -> List a -> TupleDiffs VDiffs -> List a -> TupleDiffs VDiffs -> (List a, TupleDiffs VDiffs)
mergeTuple submerger =
  let aux: Int -> List a -> List (Int,  VDiffs) -> List a -> List a -> TupleDiffs VDiffs -> List a -> TupleDiffs VDiffs -> (List a, TupleDiffs VDiffs)
      aux  i      accTuple    accDiffs             origTuple newTup2   modifs2              newTup3   modifs3 =
       case (origTuple, modifs2, newTup2, modifs3, newTup3) of
         ([], [], [], [], []) -> (List.reverse accTuple, List.reverse accDiffs)
         (_, [], _, _, _) ->     (List.reverse accTuple ++ newTup3, List.reverse accDiffs ++ modifs3)
         (_, _, _, [], _) ->     (List.reverse accTuple ++ newTup2, List.reverse accDiffs ++ modifs2)
         (v1::oe, (m2, md2)::m2tail, v2::ne2, (m3, md3)::m3tail, v3::ne3) ->
           if m2 == i && m3 == i then
            let (newVal, newDiffs) = submerger v1 v2 md2 v3 md3 in
            aux (i + 1) (newVal::accTuple) ((i, newDiffs)::accDiffs) oe ne2 m2tail ne3 m3tail
           else if m2 == i then
             aux (i + 1) (v2::accTuple) ((i, md2)::accDiffs) oe ne2 m2tail ne3 modifs3
           else if m3 == i then
             aux (i + 1) (v3::accTuple) ((i, md3)::accDiffs) oe ne2 modifs2 ne3 m3tail
           else
             let countToIgnore = min (m3 - i) (m2 - i) in
             let (toInsert, toRemain) = Utils.split countToIgnore origTuple in
             aux (i + countToIgnore) (reverseInsert toInsert accTuple) accDiffs oe (List.drop countToIgnore ne2) modifs2 (List.drop countToIgnore ne3) modifs3
         _ -> Debug.crash <| "Expected tuples to have the same size, got\n" ++
                       toString origTuple ++ ", " ++ toString newTup2 ++ ", " ++ toString newTup3
    in aux 0 [] []

mergeEnv: Env -> Env -> EnvDiffs -> Env -> EnvDiffs -> (Env, EnvDiffs)
mergeEnv originalEnv_ newEnv2_ modifs2_ newEnv3_ modifs3_ =
  let aux: Int -> Env -> List (Int,  VDiffs) -> Env ->     Env ->  List (Int,  VDiffs) -> Env ->  List (Int,  VDiffs) -> (Env, EnvDiffs)
      aux  i      accEnv accDiffs              originalEnv newEnv2 modifs2                newEnv3 modifs3=
    {-let _ = Debug.log ("aux " ++ toString i ++ "\n" ++
                                      (List.take 5 originalEnv |> List.map Tuple.first |> String.join ",") ++ "...\n" ++
                                      (List.take 5 newEnv2 |> List.map Tuple.first |> String.join ",") ++ "...\n" ++
                                      (List.take 5 newEnv3 |> List.map Tuple.first |> String.join ",") ++ "...\n" ++ "\nModifications:\n" ++
                                      toString modifs2 ++ "\n" ++ toString modifs3) () in-}
    case (originalEnv, newEnv2, modifs2, newEnv3, modifs3) of
       ([], [], [], [], []) -> (List.reverse accEnv, List.reverse accDiffs)
       (_, _, [], _, _) ->     (List.reverse accEnv ++ newEnv3, List.reverse accDiffs ++ modifs3)
       (_, _, _, _, []) ->     (List.reverse accEnv ++ newEnv2, List.reverse accDiffs ++ modifs2)
       (((x, v1) as xv1)::oe, (y, v2)::ne2, (m2, md2)::m2tail, (z, v3)::ne3, (m3, md3)::m3tail) ->
         if x /= y || y /= z || x /= z then
           Debug.crash <| "Expected environments to have the same variables, got\n" ++
            x ++ " = " ++ valToString v1 ++ "\n" ++
            y ++ " = " ++ valToString v2 ++ "\n" ++
            z ++ " = " ++ valToString v3 ++ "\n" ++
             (List.take 5 originalEnv |> List.map Tuple.first |> String.join ",") ++ "\n" ++
             (List.take 5 newEnv2 |> List.map Tuple.first |> String.join ",") ++ "\n" ++
             (List.take 5 newEnv3 |> List.map Tuple.first |> String.join ",") ++ "\n" ++ "\nOriginals:\n" ++
             envToString originalEnv_ ++ "\n" ++ envToString newEnv2_ ++ "\n" ++ envToString newEnv3_++ "\nModifications:\n" ++
             toString modifs2_ ++ "\n" ++ toString modifs3_
         else if m2 == i && m3 == i then
          let (newVal, newDiffs) = mergeVal v1 v2 md2 v3 md3 in
          aux (i + 1) ((x, newVal)::accEnv) ((i, newDiffs)::accDiffs) oe ne2 m2tail ne3 m3tail
         else if m2 == i then
           aux (i + 1) ((x, v2)::accEnv) ((i, md2)::accDiffs) oe ne2 m2tail ne3 modifs3
         else if m3 == i then
           aux (i + 1) ((x, v3)::accEnv) ((i, md3)::accDiffs) oe ne2 modifs2 ne3 m3tail
         else
           let countToIgnore = min (m3 - i) (m2 - i) in
           let (toInsert, toRemain) = Utils.split countToIgnore originalEnv in
           aux (i + countToIgnore) (reverseInsert toInsert accEnv) accDiffs toRemain (List.drop countToIgnore newEnv2) modifs2 (List.drop countToIgnore newEnv3) modifs3
       _ -> Debug.crash <| "Expected environments to have the same size, got\n" ++
                     envToString originalEnv ++ ", " ++ envToString newEnv2 ++ ", " ++ envToString newEnv3 ++ "\nOriginals:\n" ++
                     envToString originalEnv_ ++ ", " ++ envToString newEnv2_ ++ ", " ++ envToString newEnv3_
  in aux 0 [] [] originalEnv_ newEnv2_ modifs2_ newEnv3_ modifs3_

mergeInt: Int -> Int -> Int -> Int
mergeInt original modified1 modified2 =
  if original == modified1 then modified2 else modified1

mergeValMaybe: Val -> Val -> Maybe VDiffs -> Val -> Maybe VDiffs -> (Val, Maybe VDiffs)
mergeValMaybe  original modified1 modifs1 modified2 modifs2 =
  case modifs1 of
    Nothing -> (modified2, modifs2)
    Just m1 -> case modifs2 of
      Nothing -> (modified1, modifs1)
      Just m2 -> mergeVal original modified1 m1 modified2 m2 |> (\(v, vd) -> (v, Just vd))

-- Merges values using a diffing algorithm.
mergeVal: Val ->   Val ->    VDiffs -> Val ->    VDiffs -> (Val, VDiffs)
mergeVal  original modified1 modifs1   modified2 modifs2 =
  --let _ = Debug.log (valToString original ++ "<-(\n" ++ valToString modified1 ++ "("++toString modifs1++")" ++ "\n,\n" ++ valToString modified2++ "("++toString modifs2++")" ++ ")") () in
  --(\x -> let _ = Debug.log (Tuple.first x |> valToString) () in x) <|
  case (original.v_, modified1.v_, modifs1, modified2.v_, modifs2) of    -- TODO: Find multiple elem insertions and deletions
    (VBase (VString originalString), VBase (VString modified1String), VConstDiffs, VBase (VString modified2String), VConstDiffs) ->
      (replaceV_ original <| VBase (VString <| mergeString originalString modified1String modified2String), VConstDiffs)

    (VList originalElems, VList modified1Elems, VListDiffs l1, VList modified2Elems, VListDiffs l2) ->
      let (newList, newDiffs) = mergeList mergeVal originalElems modified1Elems l1 modified2Elems l2 in
      (replaceV_ original <| VList <| newList, newDiffs)

    (VRecord originalDict, VRecord modified1Dict, VRecordDiffs d1, VRecord modified2Dict, VRecordDiffs d2) ->
      let (newDict, newDiffs) = mergeRecord mergeVal originalDict modified1Dict d1 modified2Dict d2 in
      (replaceV_ original <| VRecord <| newDict, VRecordDiffs newDiffs)

    (VDict originalDict, VDict modified1Dict, VDictDiffs d1, VDict modified2Dict, VDictDiffs d2) ->
      let (newDict, newDiffs) = mergeDict mergeVal originalDict modified1Dict d1 modified2Dict d2 in
      (replaceV_ original <| VDict <| newDict, VDictDiffs newDiffs)

    (VClosure mbRec0 pats0 body0 env0, VClosure mbRec1 pats1 body1 env1, VClosureDiffs envmodifs1 bodymodifs1, VClosure mbRec2 pats2 body2 env2, VClosureDiffs envmodifs2 bodymodifs2) ->
      if mbRec0 == mbRec1 && mbRec1 == mbRec2 then
        if patsEqual pats0 pats1 pats2 then
          let (newEnv, newEnvDiffs) = mergeEnv env0 env1 envmodifs1 env2 envmodifs2 in
          let (newBody, newBodyModifs) = case (bodymodifs1, bodymodifs2) of
            (Just emodif1, Just emodif2) ->
              let _ = Debug.log "Merging two VClosures" () in
              (mergeExp body0 body1 body2, Just EChanged)
            (Nothing, _) -> (body2, bodymodifs2)
            (_, Nothing) -> (body1, bodymodifs1)
          in
          (replaceV_ original <| VClosure mbRec0 pats0 newBody newEnv, VClosureDiffs newEnvDiffs newBodyModifs)
        else if valEqual original modified1 then (modified2, modifs2) else (modified1, modifs1)
      --(VRecord originalElems, VRecord modified1Elems, VRecord modified2Elems)->
      --  Dict.keys originalElems
      else if valEqual original modified1 then (modified2, modifs2) else (modified1, modifs1)
    _ ->
      --let _ = Debug.log ("mergeVal" ++ valToString original ++ " "  ++ valToString modified1 ++ " " ++ valToString modified2) " " in
      let result = if valEqual original modified1 then (modified2, modifs2) else (modified1, modifs1)
      in
      --let _ = Debug.log ("mergeVal=" ++ valToString result) "" in
      result

patsEqual: List Pat -> List Pat -> List Pat -> Bool
patsEqual pats1 pats2 pats3 = List.all identity <| List.map3 (\p0 p1 p2 -> patEqual p0 p1 && patEqual p1 p2) pats1 pats2 pats3

mergeWS: WS -> WS -> WS -> WS -- No advanced strategy. No synthesis pushes concurrent changes in whitespace, unless we allow editing of vclosures in the output
mergeWS o e1 e2 = if o.val == e1.val then e2 else e1

mergeString: String -> String -> String -> String
mergeString o s1 s2 =
  let original = Regex.split Regex.All splitRegex o in
  let modified1 = Regex.split Regex.All splitRegex s1 in
  let modified2 = Regex.split Regex.All splitRegex s2 in
  mergeListWithDiffs identity (\ori ss1 ss2 -> if ss1 == ori then ss2 else ss1) original modified1 modified2
  |> String.join ""

mergeInfo: (a -> a -> a -> a) -> WithInfo a ->WithInfo a -> WithInfo a -> WithInfo a
mergeInfo merger w1 w2 w3 = Info.replaceInfo w1 (merger w1.val w2.val w3.val)

mergeExp: Exp -> Exp -> Exp -> Exp
mergeExp o e1 e2 =
  let default () = (if expEqual o e1 then e2 else e1).val.e__ in
  let mergexExpList = mergeListWithDiffs (Syntax.unparser Syntax.Elm) mergeExp in
  let result = case (o.val.e__, e1.val.e__, e2.val.e__) of
       (EFun sp0 pats0 body0 esp0,
        EFun sp1 pats1 body1 esp1,
        EFun sp2 pats2 body2 esp2) ->
          if patsEqual pats0 pats1 pats2 then
            EFun (mergeWS sp0 sp1 sp2) pats0 (mergeExp body0 body1 body2) (mergeWS esp0 esp1 esp2)
          else default ()
       (EApp sp0 fun0 args0 appStyle1 esp0,
        EApp sp1 fun1 args1 appStyle2 esp1,
        EApp sp2 fun2 args2 appStyle3 esp2) ->
         EApp (mergeWS sp0 sp1 sp2) (mergeExp fun0 fun1 fun2) (mergexExpList args0 args1 args2) appStyle1 (mergeWS esp0 esp1 esp2)

       (EOp sp0 op0 args0 esp0,
        EOp sp1 op1 args1 esp1,
        EOp sp2 op2 args2 esp2) ->
         if op0.val == op1.val && op1.val == op2.val then
           EOp (mergeWS sp0 sp1 sp2) op0 (mergexExpList args0 args1 args2) (mergeWS esp0 esp1 esp2)
         else default ()

       (EList sp0 args0 isp0 mTail0 esp0,
        EList sp1 args1 isp1 mTail1 esp1,
        EList sp2 args2 isp2 mTail2 esp2) ->
         EList (mergeWS sp0 sp1 sp2) (mergeListWithDiffs (\(ws, e) -> ws.val ++ Syntax.unparser Syntax.Elm e)
            (\(s0, v0) (s1, v1) (s2, v2) -> (mergeWS s0 s1 s2, mergeExp v0 v1 v2)) args0 args1 args2) (mergeWS isp0 isp1 isp2)
             (mergeMaybe mergeExp mTail0 mTail1 mTail2) (mergeWS esp0 esp1 esp2)
       (EIf spc0 cond0 spt0 then0 spe0 else0 esp0,
        EIf spc1 cond1 spt1 then1 spe1 else1 esp1,
        EIf spc2 cond2 spt2 then2 spe2 else2 esp2) ->
         EIf (mergeWS spc0 spc1 spc2)  (mergeExp cond0 cond1 cond2)
             (mergeWS spt0 spt1 spt2)  (mergeExp then0 then1 then2)
             (mergeWS spe0 spe1 spe2)  (mergeExp else0 else1 else2)
             (mergeWS esp0 esp1 esp2)
       (ECase sp0 input0 branches0 esp0,
        ECase sp1 input1 branches1 esp1,
        ECase sp2 input2 branches2 esp2) ->
         ECase (mergeWS sp0 sp1 sp2) (mergeExp input0 input1 input2)
               (autoMergeTuple mergeBranch branches0 branches1 branches2)
              (mergeWS esp0 esp1 esp2)
       (ETypeCase sp0 input0 tbranches0 esp0,
        ETypeCase sp1 input1 tbranches1 esp1,
        ETypeCase sp2 input2 tbranches2 esp2) ->
         ETypeCase (mergeWS sp0 sp1 sp2) (mergeExp input0 input1 input2)
           (autoMergeTuple mergeTBranch tbranches0 tbranches1 tbranches2)
           (mergeWS esp0 esp1 esp2)
       (ELet sp0 lk0 rec0 pat0 spi0 exp0 spj0 body0 esp0,
        ELet sp1 lk1 rec1 pat1 spi1 exp1 spj1 body1 esp1,
        ELet sp2 lk2 rec2 pat2 spi2 exp2 spj2 body2 esp2) ->
         if lk0 == lk1 && lk1 == lk2 then
           if rec0 == rec1 && rec1 == rec2 then
             if patEqual pat0 pat1 && patEqual pat1 pat2 then
               ELet (mergeWS sp0 sp1 sp2)
                    lk0 rec0 pat0
                    (mergeWS spi0 spi1 spi2)
                    (mergeExp exp0 exp1 exp2)
                    (mergeWS spj0 spj1 spj2)
                    (mergeExp body0 body1 body2)
                    (mergeWS esp0 esp1 esp2)
             else default ()
           else default ()
         else default ()
       (EComment sp0 s0 e0,
        EComment sp1 s1 e1,
        EComment sp2 s2 e2) ->
         EComment (mergeWS sp0 sp1 sp2)
           (mergeString s0 s1 s2)
           (mergeExp e0 e1 e2)
       (EOption sp0 kStr0 spi0 wStr0 exp0,
        EOption sp1 kStr1 spi1 wStr1 exp1,
        EOption sp2 kStr2 spi2 wStr2 exp2) ->
         EOption (mergeWS sp0 sp1 sp2)
                 (mergeInfo mergeString kStr0 kStr1 kStr2)
                 (mergeWS spi0 spi1 spi2)
                 (mergeInfo mergeString wStr0 wStr1 wStr2)
                 (mergeExp exp0 exp1 exp2)
       (ETyp sp0 pat0 t0 e0 esp0,
        ETyp sp1 pat1 t1 e1 esp1,
        ETyp sp2 pat2 t2 e2 esp2) ->
          if (patEqual pat0 pat1 && patEqual pat1 pat2) then
            if typeEqual t0 t1 && typeEqual t1 t2 then
             ETyp (mergeWS sp0 sp1 sp2)
                  pat0
                  t0
                  (mergeExp e0 e1 e2)
                  (mergeWS esp0 esp1 esp2)
            else default()
          else default ()
       (EColonType sp0 e0 spi0 t0 esp0,
        EColonType sp1 e1 spi1 t1 esp1,
        EColonType sp2 e2 spi2 t2 esp2) ->
          if typeEqual t0 t1 && typeEqual t1 t2 then
            EColonType (mergeWS sp0 sp1 sp2) (mergeExp e0 e1 e2) (mergeWS spi0 spi1 spi2) t0 (mergeWS esp0 esp1 esp2)
          else default ()
       (ETypeAlias sp0 pat0 t0 e0 esp0,
        ETypeAlias sp1 pat1 t1 e1 esp1,
        ETypeAlias sp2 pat2 t2 e2 esp2) ->
          if (patEqual pat0 pat1 && patEqual pat1 pat2) then
            if typeEqual t0 t1 && typeEqual t1 t2 then
             ETypeAlias (mergeWS sp0 sp1 sp2)
                  pat0
                  t0
                  (mergeExp e0 e1 e2)
                  (mergeWS esp0 esp1 esp2)
            else default()
          else default ()
       (EParens sp0 e0 pStyle0 esp0,
        EParens sp1 e1 pStyle1 esp1,
        EParens sp2 e2 pStyle2 esp2) ->
         if pStyle0 == pStyle1 && pStyle1 == pStyle2 then
             EParens (mergeWS sp0 sp1 sp2) (mergeExp e0 e1 e2) pStyle0 (mergeWS esp0 esp1 esp2)
         else default ()
       (EHole sp0 (Just v0),
        EHole sp1 (Just v1),
        EHole sp2 (Just v2)) ->
         defaultVDiffs v0 v1 |> Result.andThen (\m1 ->
           defaultVDiffs v0 v2 |> Result.map (\m2 ->
              EHole (mergeWS sp0 sp1 sp2) (Just v0)
           )
         ) |> Result.withDefault (EHole sp0 (Just v0))

       _ -> default ()
  in
  replaceE__ o result

mergeBranch: Branch -> Branch -> Branch -> Branch
mergeBranch o e1 e2 =
  case (o.val, e1.val, e2.val) of
    (Branch_ sp0 pat0 exp0 spe0,
     Branch_ sp1 pat1 exp1 spe1,
     Branch_ sp2 pat2 exp2 spe2) ->
       -- Check that the patterns are the same. If not takes the first pattern change.
       if patEqual pat0 pat1 && patEqual pat1 pat2 then
         {o | val = Branch_ (mergeWS sp0 sp1 sp2) pat0 (mergeExp exp0 exp1 exp2) (mergeWS spe0 spe1 spe2) }
       else if Syntax.patternUnparser Syntax.Elm pat0 == Syntax.patternUnparser Syntax.Elm pat1 then e2 else e1

mergeTBranch: TBranch -> TBranch -> TBranch -> TBranch
mergeTBranch o e1 e2 =
  case (o.val, e1.val, e2.val) of
    (TBranch_ sp0 typ0 exp0 spe0,
     TBranch_ sp1 typ1 exp1 spe1,
     TBranch_ sp2 typ2 exp2 spe2) ->
       -- Check that the patterns are the same. If not takes the first pattern change.
       if typeEqual typ0 typ1 && typeEqual typ1 typ2 then
         {o | val = TBranch_ (mergeWS sp0 sp1 sp2) typ0 (mergeExp exp0 exp1 exp2) (mergeWS sp0 sp1 sp2) }
       else if Syntax.typeUnparser Syntax.Elm typ0 == Syntax.typeUnparser Syntax.Elm typ1 then e2 else e1

mergeMaybe: (a -> a -> a -> a) -> Maybe a -> Maybe a -> Maybe a -> Maybe a
mergeMaybe submerger o e1 e2 =
  case (o, e1, e2) of
    (Nothing, Nothing, _) -> e2
    (Nothing, _, Nothing) -> e1
    (Nothing, Just m1, Just m2) -> Just (submerger m2 m1 m2)
    (Just o1, _, Nothing) -> Nothing
    (Just o1, Nothing, _) -> Nothing
    (Just o1, Just m1, Just m2) ->  Just (submerger o1 m1 m2)

mergeTuple2: (a -> a -> a -> a) ->  (b -> b -> b -> b) -> (a, b) -> (a, b) -> (a, b) -> (a, b)
mergeTuple2 merge1 merge2 (oa, ob) (ma1, mb1) (ma2, mb2) =
  (merge1 oa ma1 ma2, merge2 ob mb1 mb2)

-- This merger ensures that local delete/addition are merged properly.
mergeListWithDiffs: (a -> String) -> (a -> a -> a -> a) -> List a -> List a -> List a -> List a
mergeListWithDiffs keyOf submerger original modified1 modified2 =
  -- We must ensure that the modifications we generate are compatible with the original
  -- Notably, the sum of DiffEqual and DiffRemoved must be the original list
  let maybeRemoved r = List.map DiffRemoved r in
  let conflictPolicy: List a -> List (DiffChunk a) -> List (DiffChunk a) -> List (DiffChunk a)
      conflictPolicy previouslyRemoved ld1 ld2 = case (previouslyRemoved, ld1, ld2) of
       (r, [], ld2) -> maybeRemoved r ++ ld2
       (r, ld1, []) -> maybeRemoved r ++ ld1
       (removed::removedTail, DiffAdded m1 :: m1Tail, DiffAdded m2 :: m2Tail) ->
          DiffRemoved removed :: DiffAdded (submerger removed m1 m2) :: conflictPolicy removedTail m1Tail m2Tail
       (r, DiffRemoved m1 :: m1Tail, DiffRemoved m2 :: m2Tail) -> maybeRemoved r ++ (DiffRemoved m1 :: conflictPolicy [] m1Tail m2Tail)
       (r, DiffRemoved m1 :: m1Tail, m2 :: m2Tail) -> maybeRemoved r ++ m2 :: conflictPolicy [] ld1 m2Tail
       (r, m1 :: m1Tail, DiffRemoved m2 :: m2Tail) -> maybeRemoved r ++ m1 :: conflictPolicy [] m1Tail ld2
       (_, DiffEqual m1::m1Tail, DiffEqual m2::m2Tail) -> DiffEqual m1 :: conflictPolicy [] m1Tail m2Tail
       (_, DiffEqual m1::m1Tail, m2::m2Tail) -> m2 :: conflictPolicy [] ld1 m2Tail
       (_, m1::m1Tail, DiffEqual m2::m2Tail) -> m1 :: conflictPolicy [] m1Tail ld2
       ([], DiffAdded m1 :: m1Tail, DiffAdded m2::m2Tail) -> DiffAdded m1 :: DiffAdded m2 :: conflictPolicy [] m1Tail m2Tail
        -- No previously removed before, so only additions or sames.
  in
  let thediff = autodiff3 keyOf conflictPolicy original modified1 modified2 in
  let aux: List a -> List (DiffChunk (List a)) -> List a
      aux acc thediff = case thediff of
     [] -> acc
     DiffEqual same::diffTail -> aux (acc ++ same) diffTail
     DiffRemoved removed::diffTail -> aux acc diffTail
     DiffAdded added::diffTail -> aux (acc ++ added) diffTail
  in aux [] thediff

reverseInsert: List a -> List a -> List a
reverseInsert elements revAcc =
  case elements of
    [] -> revAcc
    head::tail -> reverseInsert tail (head::revAcc)

maybeReverseInsert: List (Maybe a) -> List a -> List a
maybeReverseInsert elements revAcc =
  case elements of
    [] -> revAcc
    head::tail ->
      case head of
        Nothing -> maybeReverseInsert tail revAcc
        Just h -> maybeReverseInsert tail (h::revAcc)

-- Guarantees that
-- * If updated lists have equal size, elements will be merged aligned.
-- * A list which was not modified makes that the other lists replaces the original list.
-- Would be better to have a real diffing algorithm.
mergeList: (a -> a -> VDiffs -> a -> VDiffs -> (a, VDiffs)) -> List a -> List a -> List (Int, VListElemDiff) -> List a -> List (Int, VListElemDiff) -> (List a, VDiffs)
mergeList submerger =
  let aux: Int -> (List a,    List (Int, VListElemDiff)) -> List a -> List a -> List (Int, VListElemDiff) -> List a -> List (Int, VListElemDiff) -> (List a, VDiffs)
      aux  i      (accMerged, accDiffs)                    originals modified1 modifs1                      modified2 modifs2 =
       case (originals, modifs1, modifs2) of
         (_, [], []) -> (List.reverse accMerged, VListDiffs (List.reverse accDiffs))
         (_, [], _) -> (List.reverse accMerged ++ modified2, VListDiffs (List.reverse accDiffs ++ modifs2))
         (_, _,  []) -> (List.reverse accMerged ++ modified1, VListDiffs (List.reverse accDiffs ++ modifs1))
         ([], (i1, m1)::t1, (i2, m2)::t2) ->
           if i1 /= i || i2 /= i || not (List.isEmpty t1) || not (List.isEmpty t2) then
             Debug.crash <| "Expected only at most one modification at the end of a list, got " ++ toString (i, i1, i2, t1, t2)
           else
             case (m1, m2) of
               (VListElemInsert a, VListElemInsert b) ->
                 (List.reverse accMerged ++ modified1 ++ modified2, VListDiffs (List.reverse accDiffs ++ modifs1 ++ modifs2))
               _ -> Debug.crash <| "Expected two insertions, got " ++ toString (m1, m2)
         (oh::ot, (i1, m1)::t1, (i2, m2)::t2) ->
           if i1 == i && i2 == i then -- Edition conflict
              case (m1, m2) of
                (VListElemUpdate mu1, VListElemUpdate mu2) ->
                  case (modified1, modified2) of
                    (hdModified1::tlModified1, hdModified2::tlModified2) ->
                      let (newHd, newDiffs) = submerger oh hdModified1 mu1 hdModified2 mu2 in
                      aux (i + 1) (newHd::accMerged, (i, VListElemUpdate newDiffs)::accDiffs) ot tlModified1 t1 tlModified2 t2
                    _ -> Debug.crash "Expected non-empty modifications since they were updates"
                (VListElemInsert count1, VListElemInsert count2) ->
                  let (hdModified1, tlModified1) = Utils.split count1 modified1 in
                  let (hdModified2, tlModified2) = Utils.split count2 modified2 in
                  aux i (accMerged |> reverseInsert hdModified1 |> reverseInsert hdModified2, (i, VListElemInsert (count1 + count2))::accDiffs) originals tlModified1 t1 tlModified2 t2
                (VListElemDelete count1, VListElemDelete count2) ->
                  if count1 == count2 then
                    aux i (accMerged, (i, m1)::accDiffs) originals modified1 t1 modified2 t2
                  else if count1 < count2 then
                    aux i (accMerged, accDiffs) originals modified1 modifs1 modified2 ((i, VListElemDelete count1)::(i + count1, VListElemDelete (count2 - count1))::t2)
                  else
                    aux i (accMerged, accDiffs) originals modified1 ((i, VListElemDelete count2)::(i + count2, VListElemDelete (count1 - count2))::t1) modified2 modifs2
                (_, VListElemInsert count2) ->
                  -- The insertion happens before.
                  let (inserted, modified2Tail) = Utils.split count2 modified2 in
                  aux i (reverseInsert inserted accMerged, (i, m2)::accDiffs) originals modified1 modifs2 modified2Tail t2
                (VListElemInsert count1, _) ->
                  -- The insertion happens before.
                  let (inserted, modified1Tail) = Utils.split count1 modified1 in
                  aux i (reverseInsert inserted accMerged, (i, m1)::accDiffs) originals modified1Tail t1 modified2 modifs2
                (VListElemUpdate mu1, VListElemDelete count2) ->
                  if count2 == 1 then -- Just delete it.
                    let originalsTail = List.drop 1 originals in
                    aux (i + 1) (accMerged, (i, m2)::accDiffs) originalsTail (List.drop 1 modified1) t1 modified2 t2
                  else if count2 == 0 then Debug.crash "Unexpected 0 here"
                  else
                    aux i (accMerged, accDiffs) originals modified1 modifs1 modified2 ((i, VListElemDelete 1) :: (i, VListElemDelete 1) :: t2)
                (VListElemDelete count1, VListElemUpdate mu2) ->
                  if count1 == 1 then -- Just delete it.
                    let originalsTail = List.drop 1 originals in
                    aux (i + 1) (accMerged, (i, m1)::accDiffs) originalsTail modified1 t1 (List.drop 1 modified2) t2
                  else if count1 == 0 then Debug.crash "Unexpected 0 here"
                  else
                    aux i (accMerged, accDiffs) originals modified1 ((i, VListElemDelete 1) :: (i, VListElemDelete 1) :: t1) modified2 modifs2

           else if i2 == i then
              case m2 of
                VListElemInsert count ->
                  let (inserted, modified2Tail) = Utils.split count modified2 in
                  aux i (reverseInsert inserted accMerged, (i, m2)::accDiffs) originals modified1 modifs2 modified2Tail t2
                VListElemDelete count -> --  Check that we are not taking over modif2's modifications.
                  let maxCount = i1 - i2 in
                  if count <= maxCount then
                    let originalsTail = List.drop count originals in
                    aux (i + count) (accMerged, (i, m2)::accDiffs) originalsTail modified1 modifs1 modified2 t2
                  else
                    aux i (accMerged, accDiffs) originals modified1 modifs1 modified2 ((i, VListElemDelete maxCount) :: (i + maxCount, VListElemDelete (count - maxCount)) :: t2)
                VListElemUpdate defaultVDiffs ->
                  case modified2 of
                    hdModified2::tlModified2 ->
                      aux (i + 1) (hdModified2::accMerged, (i, m2)::accDiffs) ot modified1 modifs1 tlModified2 t2
                    _ -> Debug.crash "empty modified although it said it was updated"
           else if i1 == i then
             case m1 of
               VListElemInsert count ->
                 let (inserted, modified1Tail) = Utils.split count modified1 in
                 aux i (reverseInsert inserted accMerged, (i, m1)::accDiffs) originals modified1Tail t1 modified2 modifs2
               VListElemDelete count -> --  Check that we are not taking over modif2's modifications.
                 let maxCount = i2 - i1 in
                 if count <= maxCount then
                   let originalsTail = List.drop count originals in
                   aux (i + count) (accMerged, (i, m1)::accDiffs) originalsTail modified1 t1 modified2 modifs2
                 else
                   aux i (accMerged, accDiffs) originals modified1 ((i, VListElemDelete maxCount) :: (i + maxCount, VListElemDelete (count - maxCount)) :: t1) modified2 modifs2
               VListElemUpdate defaultVDiffs ->
                 case modified1 of
                   hdModified1::tlModified1 ->
                     aux (i + 1) (hdModified1::accMerged, (i, m1)::accDiffs) ot tlModified1 t1 modified2 modifs2
                   _ -> Debug.crash "empty modified although it said it was updated"
           else
             let untouched = min (i2 - i) (i1 - i) in
             let (originalUntouched, originalRemaining) = Utils.split untouched originals in
             aux (i+untouched) (reverseInsert originalUntouched accMerged, accDiffs) originalRemaining (List.drop untouched modified1) modifs1 (List.drop untouched modified2) modifs2
  in aux 0 ([], [])

mergeDict: (v -> v -> VDiffs -> v -> VDiffs -> (v, VDiffs)) -> Dict k v -> Dict k v -> Dict k VDictElemDiff -> Dict k v -> Dict k VDictElemDiff -> (Dict k v, Dict k VDictElemDiff)
mergeDict submerger originalDict modified1Dict modifs1 modified2Dict modifs2 =
  let get0 name = Dict.get name originalDict  |> Utils.fromJust_ "mergeDict0" in
  let get1 name = Dict.get name modified1Dict |> Utils.fromJust_ "mergeDict1" in
  let get2 name = Dict.get name modified2Dict |> Utils.fromJust_ "mergeDict2" in
  Dict.merge
       (\kIn1 vIn1 (accDict, accDiffs) ->
          case vIn1 of
            VDictElemDelete ->  (Dict.remove kIn1 accDict, Dict.insert kIn1 vIn1 accDiffs)
            VDictElemInsert ->  (Dict.insert kIn1 (get1 kIn1 ) accDict, Dict.insert kIn1 vIn1 accDiffs)
            VDictElemUpdate mu -> (Dict.insert kIn1 (get1 kIn1) accDict, Dict.insert kIn1 vIn1 accDiffs)
       )
       (\kIn vIn1 vIn2 (accDict, accDiffs) ->
          case (vIn1, vIn2) of
            (_, VDictElemDelete) ->  (Dict.remove kIn accDict, Dict.insert kIn vIn2 accDiffs)
            (VDictElemDelete, _) ->  (Dict.remove kIn accDict, Dict.insert kIn vIn2 accDiffs)
            (VDictElemUpdate mu1, VDictElemUpdate mu2) ->
              let (newV, mergedModif) = submerger (get0 kIn) (get1 kIn) mu1 (get2 kIn) mu2 in
              (Dict.insert kIn newV accDict, Dict.insert kIn (VDictElemUpdate mergedModif) accDiffs)
            (VDictElemInsert, VDictElemInsert) -> -- Insertion conflict ! We just take the first one.
               (Dict.insert kIn (get1 kIn) accDict, Dict.insert kIn vIn1 accDiffs)
            (VDictElemInsert, VDictElemUpdate mu2) ->
              Debug.crash <| "Something pretended that it inserted a key, and the other pretended that it updated the key. This is not possible. Key = " ++ toString kIn
            (VDictElemUpdate mu1, VDictElemInsert) ->
              Debug.crash <| "Something pretended that it inserted a key, and the other pretended that it updated the key. This is not possible. Key = " ++ toString kIn
       )
       (\kIn2 vIn2 (accDict, accDiffs) ->
          case vIn2 of
            VDictElemDelete ->  (Dict.remove kIn2 accDict, Dict.insert kIn2 vIn2 accDiffs)
            VDictElemInsert ->  (Dict.insert kIn2 (get2 kIn2) accDict, Dict.insert kIn2 vIn2 accDiffs)
            VDictElemUpdate mu -> (Dict.insert kIn2 (get2 kIn2) accDict, Dict.insert kIn2 vIn2 accDiffs)
       )
       modifs1
       modifs2
       (originalDict, Dict.empty)

mergeRecord: (v -> v -> VDiffs -> v -> VDiffs -> (v, VDiffs)) -> Dict k v -> Dict k v -> Dict k VDiffs -> Dict k v -> Dict k VDiffs -> (Dict k v, Dict k VDiffs)
mergeRecord submerger originalDict modified1Dict modifs1 modified2Dict modifs2 =
  let get0 name = Dict.get name originalDict  |> Utils.fromJust_ "mergeDict0" in
  let get1 name = Dict.get name modified1Dict |> Utils.fromJust_ "mergeDict1" in
  let get2 name = Dict.get name modified2Dict |> Utils.fromJust_ "mergeDict2" in
  Dict.merge
       (\kIn1 vIn1 (accDict, accDiffs) ->
          (Dict.insert kIn1 (get1 kIn1) accDict, Dict.insert kIn1 vIn1 accDiffs)
       )
       (\kIn vIn1 vIn2 (accDict, accDiffs) ->
          let (newV, mergedModif) = submerger (get0 kIn) (get1 kIn) vIn1 (get2 kIn) vIn2 in
          (Dict.insert kIn newV accDict, Dict.insert kIn mergedModif accDiffs)
       )
       (\kIn2 vIn2 (accDict, accDiffs) ->
          (Dict.insert kIn2 (get2 kIn2) accDict, Dict.insert kIn2 vIn2 accDiffs)
       )
       modifs1
       modifs2
       (originalDict, Dict.empty)