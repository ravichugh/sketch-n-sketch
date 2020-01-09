module UpdateUtils exposing (..)

import Lang exposing (..)
import LangUtils exposing (..)
import Syntax
import Results exposing (Results, ok1, oks)
import LazyList exposing (LazyList(..))
import Dict exposing (Dict)
import Info exposing (WithInfo)
import LangUtils exposing (valToExp, valToExpFull, IndentStyle(..), pruneEnv, pruneEnvPattern, valToString)
import Regex
import Utils exposing (foldLeft, strFoldLeft, foldLeftWithIndex, strFoldLeftWithIndex, reverseInsert, maybeReverseInsert)
import Set exposing (Set)
import Pos exposing (Pos)
import ValBuilder as Vb
import ValUnbuilder as Vu
import LeoUnparser
import ImpureGoodies exposing (nativeDict, nativeIntDict)
import Array
import Char
import String
import LangParserUtils exposing (isSpace)
import ParserUtils

bvToString: EBaseVal -> String
bvToString b = Syntax.unparser Syntax.Leo <| withDummyExpInfo <| EBase space0 <| b

splitRegex = Regex.regex "\\b|(?=[-\\]\"'\\[\\)\\(,><\\\\])"

diffString: String -> String-> String --Summary in strings
diffString s1 s2 =
   let before = Regex.split Regex.All splitRegex s1 in
   let after = Regex.split Regex.All splitRegex s2 in
   let difference = diff identity before after in
   displayDiff identity difference

diffStringPositions: Pos -> String -> String -> (String, List Exp)
diffStringPositions initialPosition s1 s2 =
  --let _ = Debug.log ("diffStringPositions.initialPosition:" ++ toString initialPosition) () in
  let before = Regex.split Regex.All splitRegex s1 in
  let after = Regex.split Regex.All splitRegex s2 in
  let difference = diff identity before after in
  displayDiffPositions identity initialPosition difference

diffExp: Exp -> Exp -> String --Summary in strings
diffExp e1 e2 =
   let s1 = Syntax.unparser Syntax.Leo e1 in
   let s2 = Syntax.unparser Syntax.Leo e2 in
   diffString s1 s2

diffExpWithPositions: Exp -> Exp -> (String, List Exp)
diffExpWithPositions e1 e2 =
   let s1 = Syntax.unparser Syntax.Leo e1 in
   let s2 = Syntax.unparser Syntax.Leo e2 in
   diffStringPositions (Pos 1 1) s1 s2

diffVals: List Val -> List Val -> List (DiffChunk (List Val))
diffVals before after =
  let vToString = Syntax.unparser Syntax.Leo << valToExp (ws "") InlineSpace in
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

deltaLineRow: String -> (Int, Int)
deltaLineRow kept =
  let rowAdded = (String.indexes "\n" kept) |> List.length in
  let colAdded = String.length (Regex.replace Regex.All (Regex.regex "(.*\r?\n)*") (\_ -> "") kept) in
  (rowAdded, colAdded)

-- Rows and cols are zero-based
dummyExp: String -> Int -> Int -> Int -> Int -> Exp
dummyExp msg row col row2 col2 = Expr <| WithInfo (makeExp_ (EBase space0 <| EString "\"" msg) 0) (Pos row col) (Pos row2 col2)

dummyExp1: String -> Int -> Int -> Int -> Int -> Exp
dummyExp1 msg row col row2 col2 = Expr <| WithInfo (makeExp_ (EBase space0 <| EString "\"" msg) 0) (Pos (row - 1) (col - 1)) (Pos (row2 - 1) (col2 - 1))

displayDiffPositions: (a -> String) -> Pos -> List (DiffChunk (List a)) -> (String, List Exp)
displayDiffPositions tos initialPosition difference =
  let lToString l = String.join "" (List.map tos l) in
  let newStringRowCol prevRow prevCol l =
    let kept = lToString l in
    let (rowAdded, colAdded) = deltaLineRow kept in
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
     --let _ = Debug.log ("Removed and added, " ++ toString prevRow ++ "," ++ toString prevCol) () in
     let e = dummyExp added prevRow prevCol newRow newCol in
     aux (newRow, newCol) (maybeComma string ++ "Replaced " ++ removed ++ " by " ++ added, e::prevAcc) tail
    DiffRemoved l::tail ->
     let removed = lToString l in
     let e = dummyExp ("- " ++ removed) prevRow prevCol prevRow (prevCol + List.length l) in
     aux (prevRow, prevCol) (maybeComma string ++ "Removed " ++ {-"Line " ++ toString prevRow ++ ": " ++ -}removed, e :: prevAcc) tail
    DiffAdded l::tail ->
     let (added, newRow, newCol) = newStringRowCol prevRow prevCol l in
     let e = dummyExp added prevRow prevCol newRow newCol in
     --let _ = Debug.log ("Highlighting " ++ toString prevRow ++ "," ++ toString prevCol ++ " -> " ++ toString newRow ++ "," ++ toString newCol) () in
     aux (newRow, newCol) (maybeComma string ++ "Inserted " ++ {-"Line " ++ toString prevRow ++ ": " ++ -} added, e::prevAcc) tail
  in aux (initialPosition.line - 1, initialPosition.col - 1) ("", []) difference

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

combineLengthWeight length weight = 2*length + weight

-- Returns all diffs that are equally probable, i.e. with the smaller number of added/removed chars.
alldiffs: (a -> String) -> List a -> List a -> Results String (List (DiffChunk (List a)))
alldiffs keyOf before after =
    --let _ = ImpureGoodies.log <| "alldiffs " ++ (List.map keyOf before |> String.join ",") ++ " " ++ (List.map keyOf after |> String.join ",") in
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
            diff(   [a,b,3,a,b],[a,b,a,b]) --Here there should not be any ambiguity (if yes, there is duplication of results)
            overlap [1[0:1],0     ,0,1[0:1],0     ] weight = [1, _, _, 2 + 3, _]
            overlap [0     ,2[0:2],0,0     ,2[0:2]] weight = [_, 1, _, _,   3+2]
            overlap [1[2:3],0     ,0,1[2:3],0     ] weight = [2+3, _, _, 1, _]
            overlap [0     ,2[2:3],0,0     ,2[2:3]] weight = [_, 3+2, _, _,  1]
            Minimum weights for maximum coverage gives that [0:2] comes from [0:2] on one side, and [2:4] comes from [3:5].
            We take the first one, because the other one will be found later anyway because it's on the right of [0:2]
            diff(   [a,a,3,a,a],[a,a]) --Here there is ambiguity, do we remove a,a,3 or 3,a,a ?
            overlap [1[0:1],1[0:1],0,1[0:1],1[0:1]] weight = [3, 1+2, _, 3, 4+1]
            overlap [1[0:1],2[0:2],0,1[0:1],2[0:2]] weight = [1+4, 3, _, 2+1, 3]
            Minimum weights for maximum coverage gives that [0:2] comes from [0:2] and [3:5] with equal weights, so there is ambiguity

            How to measure the difference? We should give some weights to these maximum. If they are equal, there is ambiguity
            If not, one is prefered.

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

    -- Create a Dict from before values to the list of every index at which they appears
    -- In this variables, the biggest indices appear first
    let oldIndexMapRev =
      foldLeftWithIndex Dict.empty before <|
              \d indexBeforeValue beforeValue ->
         Dict.update (keyOf beforeValue) (
           Just << (\v -> indexBeforeValue :: v) << Maybe.withDefault []
         ) d
    in
    -- Here the smaller indices appear first.
    let valueToIndexBefore = Dict.map (\k v -> List.reverse v) oldIndexMapRev in

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

    -- `startOldNew` is a list of pairs consisting of the index of the beginning of the largest overlapping
    -- substring in the before list, and the index of the beginning
    -- of the same substring in the after list. `subLength` is the length that
    -- overlaps in both.
    -- These track the largest overlapping substring seen so far, so naturally
    -- we start with a 0-length substring.
    -- overlap is a Dict (old index: Int) (length of greater common substring after index included: Int, weight: Int)
    let afterLength = List.length after in
    let beforeLength = List.length before in
    let (overlap, startOldNew, subLength, weight) =
         foldLeftWithIndex (Dict.empty, [],          0,        -(List.length before + List.length after))                    after <|
                          \(overlap,    startOldNew, subLength, weight) afterIndex afterValue ->
           let oldIndicesWhereAfterAppeared = valueToIndexBefore |> Dict.get (keyOf afterValue) |> Maybe.withDefault [] in
           --let _ = ImpureGoodies.log <| "start of round\noverlap="++toString overlap ++ ", startOldNew=" ++ toString startOldNew ++ ", subLength=" ++ toString subLength ++ ", weight=" ++ toString weight in
           foldLeft (Dict.empty, startOldNew, subLength, weight) oldIndicesWhereAfterAppeared <|
                   \(overlap_,   startOldNew, subLength_, weight_) oldIdxAfterValue ->
              -- now we are considering all values of val such that
              -- `before[oldIdxAfterValue] == after[afterIndex]`
              let newSubLength = (if oldIdxAfterValue == 0 then 0 else Dict.get (oldIdxAfterValue - 1) overlap |>
                   Maybe.map Tuple.first |> Maybe.withDefault 0) + 1 in
              --let _ = ImpureGoodies.log <| "overlap_=" ++ toString overlap_ ++ ", startOldNew=" ++ toString startOldNew ++ ", subLength_=" ++ toString subLength_ ++ ", weight_=" ++ toString weight_ in
              let newWeight = 0 - abs ((afterLength - afterIndex) - (beforeLength - oldIdxAfterValue)) -
                abs (afterIndex - oldIdxAfterValue) in
              let newOverlap_ = Dict.insert oldIdxAfterValue (newSubLength, newWeight) overlap_ in
              if combineLengthWeight newSubLength newWeight > combineLengthWeight subLength_ weight_ then -- No ambiguity, we erase the previous startOldNew
                   (newOverlap_, [(oldIdxAfterValue - newSubLength + 1, afterIndex - newSubLength + 1)] , newSubLength, newWeight)
              else if combineLengthWeight newSubLength newWeight == combineLengthWeight subLength_ weight_ && combineLengthWeight subLength_ weight_ > combineLengthWeight subLength weight then -- Ambiguity
                   (newOverlap_, (oldIdxAfterValue - newSubLength + 1, afterIndex - newSubLength + 1)::startOldNew , newSubLength, newWeight)
              else (newOverlap_, startOldNew, subLength_, weight_)
    in
    --let _ = ImpureGoodies.log <| "End of process\noverlap=" ++ toString overlap ++ ", startOldNew=" ++ toString startOldNew ++ ", subLength=" ++ toString subLength ++ ", weight=" ++ toString weight in
    if subLength == 0 then
        -- If no common substring is found, we return an insert and delete...
        ok1 <| (if List.isEmpty before then  [] else [DiffRemoved before]) ++
               (if List.isEmpty after then [] else [DiffAdded after])
    else
      flip Results.andThen (oks (List.reverse startOldNew)) <| \(startOld, startNew) ->
        -- otherwise, the common substring is unchanged and we recursively
        -- diff the text before and after that substring
        let left = alldiffs keyOf (List.take startOld before) (List.take startNew after) in
        let right = alldiffs keyOf  (List.drop (startOld + subLength) before) (List.drop (startNew + subLength) after) in
        flip Results.andThen left <| \leftDiffs ->
          flip Results.map right <| \rightDiffs ->
            leftDiffs ++ (DiffEqual (List.drop startNew after |> List.take subLength) :: rightDiffs)

longestPrefixSizeBetween before after =
  let lengthBefore = String.length before in
  let aux i =
    if i < lengthBefore && String.slice i (i+1) before == String.slice i (i+1) after then aux (i + 1) else i
  in aux 0

longestSufixSizeBetween before after =
  let lengthBefore = String.length before in
  let lengthAfter = String.length after in
  let aux i =
    if i < lengthBefore &&
       String.slice (lengthBefore - 1 - i) (lengthBefore - i) before ==
       String.slice (lengthAfter - 1 - i) (lengthAfter - i) after then aux (i + 1) else i
  in aux 0

longestSufixSizeBetweenGuard maxIndex before after =
  let lengthBefore = String.length before in
  let lengthAfter = String.length after in
  let aux i =
    if i <= maxIndex && i < lengthBefore &&
       String.slice (lengthBefore - 1 - i) (lengthBefore - i) before ==
       String.slice (lengthAfter - 1 - i) (lengthAfter - i) after then aux (i + 1) else i
  in aux 0

-- Faster implementation of allDiffs for strings
allStringDiffs: String -> String -> Results String (List (DiffChunk String))
allStringDiffs before after =
    --let _ = ImpureGoodies.log <| "allStringDiffs '" ++ before ++ "' '" ++ after ++ "'" in
    -- If at least one half of the string before/after was the same, no need to look for the longest equal sequence, it should be a prefix of this one !
    -- This is true only if the considered strings don't have the same
    let lengthBefore = String.length before in
    let lengthAfter = String.length after in
    let testLeftRight continuation =
      let longestPrefixSize = longestPrefixSizeBetween before after in
      let longestSuffixSize = longestSufixSizeBetweenGuard (min (String.length before) (String.length after) - longestPrefixSize) before after in
      -- Is the longest common substring either the prefix or the suffix?
      -- For that, it's sufficient to prove that |prefix| + |suffix| >= 2/3 |before| and 2/3 |after|
      -- Indeed, if this is the case, then

      let halfBeforePlus1 = (lengthBefore + 2) // 2 in
      let halfAfterPlus1 = (lengthAfter + 2) // 2 in
      let halfCommon = max halfBeforePlus1 halfAfterPlus1 in
      let testLeft continuation =
         let leftPart = String.slice 0 halfCommon before in
         if leftPart == String.slice 0 halfCommon after then
           let longestPrefixSize = longestPrefixSizeBetween before after in
           let longestPrefix = String.left longestPrefixSize before in
           let remainingBefore = String.dropLeft longestPrefixSize before in
           let remainingAfter = String.dropLeft longestPrefixSize after in
           allStringDiffs remainingBefore remainingAfter |> Results.andThen (\l ->
             maybeExpandLongest [] longestPrefix l
           )
         else
           continuation ()
      in
      let testRight continuation =
         let rightPart = String.slice (lengthBefore - halfCommon) lengthBefore before in
         if rightPart == String.slice (lengthAfter - halfCommon) lengthAfter after then
           let longestSuffixSize = longestSufixSizeBetween before after in
           let longestSuffix = String.right longestSuffixSize after in
           let remainingBefore = String.dropRight halfCommon before in
           let remainingAfter = String.dropRight halfCommon after in
           allStringDiffs remainingBefore remainingAfter |> Results.andThen (\l ->
             maybeExpandLongest l longestSuffix []
           )
         else -- Here neither the first half or the last half are in common. We test if the first fourth and the last fourth are simultaneously the same.
           continuation ()
      in
      testLeft <| \_ ->
      testRight continuation
    in
    let testFirstLastFourth continuation =
      let fourthBeforePlus1 = (lengthBefore + 4) // 4 in
      let fourthAfterPlus1 = (lengthAfter + 4) // 4 in
      let fourthCommon = max fourthBeforePlus1 fourthAfterPlus1 in
      let firstFourth = String.slice 0 fourthCommon before
          lastFourth = String.slice (lengthBefore - fourthCommon) lengthBefore before in
      if firstFourth == String.slice 0 fourthCommon after
         && lastFourth == String.slice (lengthAfter - fourthCommon) lengthAfter after then
         let remainingBefore = String.dropLeft fourthCommon <| String.dropRight fourthCommon <| before in
         let remainingAfter = String.dropLeft fourthCommon <| String.dropRight fourthCommon <| after in
         allStringDiffs remainingBefore remainingAfter |> Results.map (\l ->
           let aux l = case l of
             [DiffEqual x] -> [DiffEqual (x ++ lastFourth)]
             head::tail -> head :: aux tail
             [] -> [DiffEqual lastFourth]
          in case aux l of
            DiffEqual x :: tail -> DiffEqual (firstFourth ++ x) :: tail
            ll ->  DiffEqual firstFourth :: l
         )
    else
      continuation ()
    in
    let tests continuation =
      if lengthBefore > 2 && lengthAfter > 2 then
         testLeftRight-- <| \_ ->
         --testFirstLastFourth
         continuation
      else continuation ()
    in
    tests <| \_ ->
   --    allStringDiffs remainingBefore remainingAfter
    -- Create a Dict from before values to the list of every index at which they appears
    -- In this variables, the biggest indices appear first
    --let _ = ImpureGoodies.log <| "allStringDiffs not optimized '" ++ before ++ "' '" ++ after ++ "'" in
    let oldIndexMapRev =
      strFoldLeftWithIndex (nativeDict.empty ()) before <|
              \d indexBeforeValue  beforeChar ->
         nativeDict.update (String.fromChar beforeChar) (
           Just << (\v -> indexBeforeValue :: v) << Maybe.withDefault []
         ) d
    in
    -- Here the smaller indices appear first.
    let valueToIndexBefore = nativeDict.map (\k v -> List.reverse v) oldIndexMapRev in

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

    -- `startOldNew` is a list of pairs consisting of the index of the beginning of the largest overlapping
    -- substring in the before list, and the index of the beginning
    -- of the same substring in the after list. `subLength` is the length that
    -- overlaps in both.
    -- These track the largest overlapping substring seen so far, so naturally
    -- we start with a 0-length substring.
    -- overlap is a Dict (old index: Int) (length of greater common substring after index included: Int, weight: Int)
    let afterLength = String.length after in
    let beforeLength = String.length before in
    -- TODO: Put more  information in overlap so that we can obtain the result without this recursion.
    let (_, startOldNew, subLength, weight) =
         strFoldLeftWithIndex (nativeIntDict.empty (), [],          0,         -(String.length before + String.length after)) after <|
                             \(overlap,                startOldNew, subLength, weight)                             afterIndex afterChar ->
           let oldIndicesWhereAfterAppeared = valueToIndexBefore |> nativeDict.get (String.fromChar afterChar) |> Maybe.withDefault [] in
           -- We look for the longest sequence in a row where after values appeared consecutively in before.
           --let _ = ImpureGoodies.log <| "start of round\noverlap="++toString overlap ++ ", startOldNew=" ++ toString startOldNew ++ ", subLength=" ++ toString subLength ++ ", weight=" ++ toString weight in
           foldLeft    (nativeIntDict.empty (), startOldNew, subLength, weight) oldIndicesWhereAfterAppeared <|
                      \(overlap_,   startOldNew, subLength_, weight_) oldIdxAfterChar ->
              -- now we are considering all values of val such that
              -- `before[oldIdxAfterChar] == after[afterIndex]`
              let newSubLength = (if oldIdxAfterChar == 0 then 0 else nativeIntDict.get (oldIdxAfterChar - 1) overlap |>
                   Maybe.map Tuple.first |> Maybe.withDefault 0) + 1 in
              --let _ = ImpureGoodies.log <| "overlap_=" ++ toString overlap_ ++ ", startOldNew=" ++ toString startOldNew ++ ", subLength_=" ++ toString subLength_ ++ ", weight_=" ++ toString weight_ in
              let newWeight = 0 - abs ((afterLength - afterIndex) - (beforeLength - oldIdxAfterChar)) -
                abs (afterIndex - oldIdxAfterChar) in
              let newOverlap_ = nativeIntDict.insert oldIdxAfterChar (newSubLength, newWeight) overlap_ in
              if combineLengthWeight newSubLength newWeight > combineLengthWeight subLength_ weight_ then
                   (newOverlap_, [(oldIdxAfterChar - newSubLength + 1, afterIndex - newSubLength + 1)] , newSubLength, newWeight)
              else if combineLengthWeight newSubLength newWeight == combineLengthWeight subLength_ weight_ {-&& combineLengthWeight subLength_ weight_ > combineLengthWeight subLength weight-} then -- Ambiguity
                   (newOverlap_, (oldIdxAfterChar - newSubLength + 1, afterIndex - newSubLength + 1)::startOldNew , newSubLength, newWeight)
              else (newOverlap_, startOldNew,                         subLength_, weight_)
    in
    --let _ = ImpureGoodies.log <| "End of process\noverlap=" ++ toString overlap ++ ", startOldNew=" ++ toString startOldNew ++ ", subLength=" ++ toString subLength ++ ", weight=" ++ toString weight in
    if subLength == 0 then
        -- If no common substring is found, we return an insert and delete...
        ok1 <| (if String.isEmpty before then [] else [DiffRemoved before]) ++
               (if String.isEmpty after  then [] else [DiffAdded after])
    else
      let startOldNewPruned =
          --Debug.log ("before overlapping removal\n" ++ toString (List.reverse  startOldNew) ++ "\nAfter:") <|
          onlyOverlappingStartOldNew subLength (List.reverse  startOldNew) in
      --(\x -> let _ = Debug.log ("Computed diffs:" ++ toString (Results.toList x)) () in x) <|
      takeShortest <|
      flip Results.andThen (oks startOldNewPruned) <| \(startOld, startNew) ->
        -- otherwise, the common substring is unchanged and we recursively
        -- diff the text before and after that substring
        let middle = String.slice startNew (startNew + subLength) after in
        --let _ = Debug.log "middle that is the same" middle in
        let left = allStringDiffs (String.left startOld before) (String.left startNew after) in
        let right = allStringDiffs  (String.dropLeft (startOld + subLength) before) (String.dropLeft (startNew + subLength) after) in
        flip Results.andThen left <| \leftDiffs ->
          flip Results.andThen right <| \rightDiffs ->
            -- If leftDiffs ends with an insertion, there could be an ambiguity. Same if rightDiffs starts with an insertion.
            -- Consider:
            -- 'abc Truck' 'abc big Truck' gives DiffEqual "abc", DiffAdded " big", DiffEqual " Truck" because " Truck is the longest substring
            -- However, the first space of the inserted string " big" could as well have been the first space of "Truck".
            -- Therefore, the following diff should also be valid:
            -- DiffEqual "abc ", DiffAdded "big ", DiffEqual "Truck"
            -- Conversely, for 'Truck abc' 'Truck big abc', gives DiffEqual 'Truck ', DiffAdded 'big ', DiffEqual 'abc'
            --   alternatively, we should have DiffEqual 'Truck', DiffAdded ' big', DiffEqual ' abc'
            maybeExpandLongest leftDiffs middle rightDiffs

maybeExpandLongest: List (DiffChunk String) -> String -> List (DiffChunk String) ->  Results String (List (DiffChunk String))
maybeExpandLongest leftDiffs middleEqual rightDiffs =
  let processRight middleEqual rightDiffs =
     let baseResult = ok1 <| leftDiffs ++ (if middleEqual == "" then [] else [DiffEqual middleEqual]) ++ rightDiffs in
     case rightDiffs of
        DiffAdded inserted :: DiffEqual x :: tail ->
          let (initMiddleEqual, lastMiddleEqual) = (String.dropRight 1 middleEqual, String.right 1 middleEqual) in
          let (initInserted, lastInitInserted) = (String.dropRight 1 inserted, String.right 1 inserted) in
          if lastMiddleEqual == lastInitInserted && lastInitInserted /= "" then
            let variant = processRight initMiddleEqual (DiffAdded (lastInitInserted ++ initInserted)::DiffEqual (lastInitInserted++x)::tail) in
            baseResult |> Results.andAlso variant
          else
            baseResult
        _ -> baseResult
  in
  let baseLeftResult = processRight middleEqual rightDiffs  in
  case leftDiffs of
  [DiffEqual x, DiffAdded inserted] -> case (String.uncons inserted, String.uncons middleEqual) of
    (Just (insertedChar, insertedTail), Just (equalChar, equalTail)) ->
      if insertedChar == equalChar then -- There is a variant
        let variant = maybeExpandLongest [DiffEqual (x ++ String.fromChar insertedChar), DiffAdded (insertedTail ++ String.fromChar insertedChar)] equalTail rightDiffs in
        baseLeftResult |> Results.andAlso variant
      else
        baseLeftResult
    _ -> baseLeftResult
  head::((b::c::_) as tail) -> maybeExpandLongest tail middleEqual rightDiffs |> Results.map (\l -> head::l)
  _ -> baseLeftResult



-- Remove differences that, if taken recursively, will produce the same diffs
onlyOverlappingStartOldNew: Int -> List (Int, Int) -> List (Int, Int)
onlyOverlappingStartOldNew  subLength startOldNew =
  case startOldNew of
    [] -> []
    [head] -> [head]
    (oldStart, newStart)::tail ->
  -- Easy: We remove non-overlapping sequences.
      let aux: List (Int, Int) -> List (Int, Int) -> List (Int, Int)
          aux startOldNewTail revAcc  = case startOldNewTail of
        [] -> (oldStart, newStart) :: List.reverse revAcc
        (oldStart2, newStart2)::tail ->
           if oldStart + subLength <= oldStart2 then (oldStart, newStart) :: List.reverse revAcc
           else
             (oldStart2, newStart2)::revAcc |>
             aux tail
      in aux tail []

takeShortest: Results String (List a) -> Results String (List a)
takeShortest x =
  case x of
    Err msg -> Debug.crash "A diff cannot return Err but it did so"
    Ok ll ->
      let l = LazyList.toList ll in
      let sizes = List.map List.length l in
      let minsize = List.minimum sizes |> Maybe.withDefault 0 in
      let finallist = List.filterMap (\l -> if List.length l <= minsize then Just l else Nothing) l in
      --let _ = Debug.log "Ambiguities before pruning" (toString l) in
      --let _ = Debug.log "Ambiguities after pruning" (toString finallist) in
      Ok (LazyList.fromList finallist)


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
        Err msg::tail ->
          case combineResults original tail of
            Err msg2 -> Err <| msg ++ msg2
            anything  -> anything
        (Ok r1 as head)::Err msg::tail ->
          combineResults original (head::tail)
        Ok LazyNil :: Ok r2 :: tail -> Ok LazyNil
        Ok r1 :: Ok LazyNil :: tail -> Ok LazyNil
        Ok (LazyCons (env1, exp1) lazyTail1):: Ok (LazyCons (env2, exp2) lazyTail2):: tail ->
          let finalEnv = triCombine expo envo env1 env2 in
          let finalExp = mergeExp expo exp1 exp2 in
          Ok (LazyCons (finalEnv, finalExp) (Lazy.lazy
-}
-- Time to merge all possible results. May result in exponential blowup if each result is ambiguous.

recursiveMerge: (a -> a -> ma -> a -> ma -> (a, ma)) -> a -> List (a, ma) -> (a, Maybe ma)
recursiveMerge merge original modifications =
  case modifications of
    [] -> (original, Nothing)
    [(head, headDiff)] -> (head, Just headDiff)
    (head1, head1Diff)::(head2, head2Diff)::tail ->
      recursiveMerge merge original (merge original head1 head1Diff head2 head2Diff:: tail)

recursiveMergeVal: Val -> List (Val, Maybe VDiffs) -> (Val, Maybe VDiffs)
recursiveMergeVal original modifications = recursiveMerge mergeVal original (
  List.filterMap (\(a, mbb) -> Maybe.map ((,) a) mbb) modifications)

-- type PDiffs = PChanged -- TODO: More diffs for patterns there?

type alias BranchDiffs = TupleDiffs EDiffs

extractors: {
  unapply: String -> (a -> Maybe b) -> a -> (b -> Result String c) -> Result String c,
  unapplySeq: String -> (a -> Maybe b) -> List a -> (List b -> Result String c) -> Result String c
  }
extractors = {
  unapply = \msg unapply e c -> Result.fromMaybe msg (unapply e) |> Result.andThen c,
  unapplySeq = \msg transformer input c -> List.map transformer input |> Utils.projJusts |> Result.fromMaybe msg |> Result.andThen c
  }

-- Helpers for TupleDiffs and ListDiffs
tupleDiffsToVal: (Vb.Vb -> a -> Val) -> Vb.Vb -> TupleDiffs a -> Val
tupleDiffsToVal subroutine =
  Vb.list (Vb.tuple2 Vb.int subroutine)

valToTupleDiffs: (Val -> Result String a) -> Val -> Result String (TupleDiffs a)
valToTupleDiffs subroutine =
  Vu.list (Vu.tuple2 Vu.int subroutine)

listDiffsToVal: (Vb.Vb -> a -> Val) -> Vb.Vb -> (ListDiffs a) -> Val
listDiffsToVal subroutine= Vb.list (Vb.tuple2 Vb.int (listElemDiffToVal subroutine))

valToListDiffs: (Val -> Result String a) -> Val -> Result String (ListDiffs a)
valToListDiffs subroutine l = Vu.list (Vu.tuple2 Vu.int (valToListElemDiff subroutine)) l

listElemDiffToVal: (Vb.Vb -> a -> Val) -> Vb.Vb -> ListElemDiff a -> Val
listElemDiffToVal subroutine vb velem = case velem of
   ListElemUpdate d -> (Vb.constructor vb) "ListElemUpdate" [subroutine vb d]
   ListElemInsert i -> (Vb.constructor vb) "ListElemInsert" [(Vb.int vb) i]
   ListElemDelete i -> (Vb.constructor vb) "ListElemDelete" [(Vb.int vb) i]

stringDiffsToVal: Vb.Vb -> StringDiffs -> Val
stringDiffsToVal vb sd = case sd of
  StringUpdate start end replaced -> Vb.constructor vb "StringUpdate" [Vb.int vb start, Vb.int vb end, Vb.int vb replaced]

valToStringDiffs: Val -> Result String StringDiffs
valToStringDiffs v = case Vu.constructor Ok v of
  Ok ("StringUpdate", [startV, endV, replacedV]) ->
    Result.map3 StringUpdate (Vu.int startV) (Vu.int endV) (Vu.int replacedV)
  _ -> Err <| "Expected StringUpdate, got " ++ valToString v

valToListElemDiff: (Val -> Result String a) -> Val -> Result String (ListElemDiff a)
valToListElemDiff subroutine v = case Vu.constructor Ok v of
  Ok ("ListElemUpdate", [d]) -> subroutine d |> Result.map ListElemUpdate
  Ok ("ListElemInsert", [i]) -> Vu.int i |> Result.map ListElemInsert
  Ok ("ListElemDelete", [i]) -> Vu.int i |> Result.map ListElemDelete
  Ok _ -> Err <| "Expected ListElemUpdate[_], ListElemInsert[_], ListElemDelete[_], got " ++ valToString v
  Err msg -> Err msg


-- Instantiation of differences
envDiffsToVal: Vb.Vb -> EnvDiffs -> Val
envDiffsToVal = tupleDiffsToVal vDiffsToVal
valToEnvDiffs: Val -> Result String EnvDiffs
valToEnvDiffs = valToTupleDiffs valToVDiffs

vDictElemDiffToVal: Vb.Vb -> VDictElemDiff -> Val
vDictElemDiffToVal vb velem = case velem of
  VDictElemDelete -> (Vb.constructor vb) "VDictElemDelete" []
  VDictElemInsert -> (Vb.constructor vb) "VDictElemInsert" []
  VDictElemUpdate u -> (Vb.constructor vb) "VDictElemUpdate" [vDiffsToVal vb u]

valToVDictElemDiff: Val -> Result String VDictElemDiff
valToVDictElemDiff v = case Vu.constructor Ok v of
  Ok ("VDictElemDelete", []) -> Ok VDictElemDelete
  Ok ("VDictElemInsert", []) -> Ok VDictElemInsert
  Ok ("VDictElemUpdate", [u]) -> Result.map VDictElemUpdate <| valToVDiffs u
  Ok _ -> Err <| "Expected VDictElemDelete[], VDictElemInsert[], VDictElemUpdate[_], got " ++ valToString v
  Err msg -> Err msg

-- Val encoding of VDiffs
vDiffsToVal: Vb.Vb -> VDiffs -> Val
vDiffsToVal vb vdiffs = case vdiffs of
  VClosureDiffs e mbe -> (Vb.constructor vb) "VClosureDiffs" [envDiffsToVal vb e, Vb.maybe eDiffsToVal vb mbe]
  VListDiffs list     -> (Vb.constructor vb) "VListDiffs"    [listDiffsToVal vDiffsToVal vb list]
  VStringDiffs list   -> (Vb.constructor vb) "VStringDiffs"  [Vb.list stringDiffsToVal vb list]
  VConstDiffs         -> (Vb.constructor vb) "VConstDiffs"   []
  VDictDiffs d        -> (Vb.constructor vb) "VDictDiffs"    [Vb.dict vDictElemDiffToVal vb d]
  VRecordDiffs d      -> (Vb.constructor vb) "VRecordDiffs"  [Vb.record vDiffsToVal vb d]

valToVDiffs: Val -> Result String VDiffs
valToVDiffs v = case Vu.constructor Ok v of
  Ok ("VClosureDiffs", [v1, v2]) -> Result.map2 VClosureDiffs (valToEnvDiffs v1) (Vu.maybe valToEDiffs v2)
  Ok ("VListDiffs"   , [l]) -> valToListDiffs valToVDiffs l |> Result.map VListDiffs
  Ok ("VStringDiffs",  [l]) -> Vu.list valToStringDiffs l |> Result.map VStringDiffs
  Ok ("VConstDiffs"  , []) -> Ok VConstDiffs
  Ok ("VDictDiffs"   , [d]) -> Vu.dict valToVDictElemDiff d|> Result.map VDictDiffs
  Ok ("VRecordDiffs", [d]) -> Vu.record valToVDiffs d |> Result.map VRecordDiffs
  Ok (name, args) -> Err <| "Expected VClosureDiffs _ _, VListDiffs _, VStringDiffs, VConstDiffs, VDictDiffs _, VRecordDiffs _, got " ++
    valToString v ++ " (constructor = " ++ name ++ " with " ++ toString (List.length args) ++ "arguments)"
  Err msg -> Err msg

eDiffsToVal: Vb.Vb -> EDiffs -> Val
eDiffsToVal vb ediffs = case ediffs of
  EConstDiffs ws -> Vb.constructor vb "EConstDiffs" [Vb.int vb (if ws == EOnlyWhitespaceDiffs then 0 else 1)]
  EListDiffs list -> Vb.constructor vb "EListDiffs" [listDiffsToVal eDiffsToVal vb list]
  EStringDiffs list -> Vb.constructor vb "EStringDiffs" [Vb.list stringDiffsToVal vb list]
  EChildDiffs children -> Vb.constructor vb "EChildDiffs" [tupleDiffsToVal eDiffsToVal vb children]

valToEDiffs: Val -> Result String EDiffs
valToEDiffs v = case Vu.constructor Ok v of
  Ok ("EConstDiffs", [i]) -> Vu.int i |> Result.map (\i -> EConstDiffs <| if i == 0 then EOnlyWhitespaceDiffs else EAnyDiffs)
  Ok ("EListDiffs", [list]) -> valToListDiffs valToEDiffs list |> Result.map EListDiffs
  Ok ("EStringDiffs", [list]) -> Vu.list valToStringDiffs list |> Result.map EStringDiffs
  Ok ("EChildDiffs", [list]) -> valToTupleDiffs valToEDiffs list |> Result.map EChildDiffs
  Ok _ -> Err <| "Expected EConstDiffs, EListDiffs, EStringDiffs, EChildDiffs, got " ++ valToString v
  Err msg -> Err msg

valToUpdateReturn: Val -> Result String UpdateReturn
valToUpdateReturn v = case Vu.constructor Ok v of
  Ok ("Inputs",          [v]) -> Vu.list Vu.identity v |> Result.map Inputs
  Ok ("InputsWithDiffs", [v]) -> Vu.list (Vu.tuple2 Vu.identity (Vu.maybe valToVDiffs)) v |> Result.map InputsWithDiffs
  Ok (n, args) -> Err <| "Expected Inputs (List Val) or InputsWithDiffs (List (Val, Maybe VDiffs)), got " ++ n ++ " with " ++ toString (List.length args) ++ " arguments"
  Err msg -> Err msg

updateReturnToVal: Vb.Vb -> UpdateReturn -> Val
updateReturnToVal vb updateReturn = case updateReturn of
  Inputs l -> Vb.constructor vb "Inputs" [Vb.list Vb.identity vb l]
  InputsWithDiffs l -> Vb.constructor vb "InputsWithDiffs" [Vb.list (Vb.tuple2 Vb.identity (Vb.maybe vDiffsToVal)) vb l]

{-
pDiffsToVal: ValBuilders -> PDiffs -> Val
pDiffsToVal v pdiffs = case pdiffs of
  PChanged -> (Vb.constructor v) "PChanged" []

bDiffsToVal: ValBuilders ->
             BranchDiffs -> Val
bDiffsToVal bdiffs = case bdiffs of
  BChanged -> (Vb.constructor v) "BChanged" []
-}

tupleDiffsToString: Maybe String -> (String -> b -> b -> a -> String) -> String -> List b -> List b-> TupleDiffs a -> String
tupleDiffsToString mbStructName subroutine indent originalEnv modifiedEnv envDiffs =
  let aux i original modified diffs acc =
    --let _ = Debug.log ("aux " ++ toString i ++ " [" ++ (List.take 5 original |> List.map Tuple.first |> String.join ",") ++ "] [" ++
    --     " [" ++ (List.take 5 modified |> List.map Tuple.first |> String.join ",") ++ "] " ++ toString diffs ++ " '" ++ acc ++ "'") () in
    case diffs of
       [] -> acc
       (j, change)::diffsTail ->
        if j > i then
          aux j (List.drop (j - i) original) (List.drop (j - i) modified) diffs acc
        else if j == i then
          case (original, modified) of
            (b1::tailOriginal, b2::tailModified) ->
              acc ++ subroutine indent b1 b2 change |>
                aux (i + 1) tailOriginal tailModified diffsTail
            _ -> Debug.crash <| "Expcted non-empty " ++ (mbStructName |> Maybe.withDefault "structuro")
        else
          Debug.crash <| "Changes does not match the " ++ (mbStructName |> Maybe.withDefault "structuro")
  in
   (Maybe.map (\structName ->
   "\n" ++ indent ++ structName ++ " = ...") mbStructName |> Maybe.withDefault "") ++
   aux  0 originalEnv modifiedEnv envDiffs ""

envDiffsToString_ = tupleDiffsToString (Just "environment") <| \indent (kOriginal, valueOriginal) (kModified, valueModified) change ->
  (if kOriginal /= kModified then
   "\n" ++ indent ++ "Weird: a name changed from " ++ kOriginal ++ " to " ++ kModified
   else "") ++ "\n" ++ indent ++ "Variable " ++ kOriginal ++" changes to value:" ++
   vDiffsToString_ (indent ++ "  ") valueOriginal valueModified change

envDiffsToString = envDiffsToString_ ""

stringDiffsToString: String -> String -> String -> List StringDiffs -> String
stringDiffsToString  indent    original  modified  diffs =
  let aux acc offset diffs = case diffs of
    [] -> String.join ", " (List.reverse acc)
    StringUpdate start end replacement :: tail ->
       let accInc = if start == end then -- Pure insertion
            "\n" ++ indent ++ "Inserted: '" ++ String.slice (start + offset) (start + offset + replacement) modified ++ "'" ++ " at pos " ++ toString start
         else if replacement == 0 then -- Pure deletion
            "\n" ++ indent ++ "Removed: '" ++ String.slice start end original ++ "'" ++ " at pos " ++ toString start
         else -- Replacement
            "\n" ++ indent ++ "Replaced '" ++ String.slice start end original ++ "' by '"++ String.slice (start + offset) (start + offset + replacement) modified ++"'"
       in
       let newOffset = offset - (end - start) + replacement in
       let newAcc = accInc::acc in
       aux newAcc newOffset tail
  in
  aux [] 0 diffs

vDiffsToString_: String -> Val -> Val -> VDiffs-> String
vDiffsToString_ indent vOriginal vModified vDiffs =
  case vDiffs of
    VListDiffs diffs ->
      case (vOriginal.v_, vModified.v_) of
        (VList originals, VList modified) ->
          "\n" ++ indent ++ "A list was modified:" ++ vListDiffsToString indent originals modified diffs
        _ -> "[Internal error] vDiffsToString " ++ toString vDiffs ++ " expects lists here, got " ++ valToString vOriginal ++ ", " ++ valToString vModified
    VStringDiffs diffs ->
      case (vOriginal.v_, vModified.v_) of
        (VBase (VString original), VBase (VString modified)) ->
          stringDiffsToString indent original modified diffs
        _ -> "[Internal error] vDiffsToString_ " ++ toString vDiffs ++ " expects strings here, got " ++ valToString vOriginal ++ ", " ++ valToString vModified
    VDictDiffs diffs ->
      case (vOriginal.v_, vModified.v_) of
        (VDict originals, VDict modified) ->
          "\n" ++ indent ++ "A dict was modified:" ++ vDictDiffsToString indent originals modified diffs

        _ -> "[Internal error] vDiffsToString_ " ++ toString vDiffs ++ " expects dicts here, got " ++ valToString vOriginal ++ ", " ++ valToString vModified
    VRecordDiffs diffs ->
      case (vOriginal.v_, vModified.v_) of
        (VRecord originals, VRecord modified) ->
          "\n" ++ indent ++ "A record was modified:" ++ vRecordDiffsToString indent originals modified diffs
        _ -> "[Internal error] vDiffsToString_ " ++ toString vDiffs ++ " expects records here, got " ++ valToString vOriginal ++ ", " ++ valToString vModified
    VClosureDiffs envDiffs bodyDiffs ->
      case (vOriginal.v_, vModified.v_) of
        (VClosure _ _ body1 env1, VClosure _ _ body2 env2) ->
          "\n" ++ indent ++ "A closure was modified:" ++ vClosureDiffsToString indent env1 body1 env2 body2 envDiffs bodyDiffs
        _ -> "[Internal error] vDiffsToString_ " ++ toString vDiffs ++ " expects closures here, got " ++ valToString vOriginal ++ ", " ++ valToString vModified
    VConstDiffs ->
      "\n" ++ indent ++ "Was " ++ valToString vOriginal ++ ", now " ++ valToString vModified

vDiffsToString: Val -> Val -> VDiffs -> String
vDiffsToString = vDiffsToString_  ""

listDiffsToString: String -> (String -> b -> b -> a -> String) -> (String -> Int -> Maybe (String, String)) -> (b -> String) -> String -> List b -> List b -> ListDiffs a -> String
listDiffsToString structName subroutine displayElemModif elementDisplay indent originals modifieds diffs =
  if List.isEmpty diffs then "[Internal error: Empty " ++ structName ++ " diff]" else
  let aux i original modifieds diffs acc =
    case diffs of
       [] -> acc
       (j, change)::diffsTail ->
        if j >i then
          aux j (List.drop (j - i) original) (List.drop (j - i) modifieds) diffs acc
        else
          case change of
            ListElemDelete count ->
              let (originalRemoved, originalKept) = Utils.split count original in
              acc ++ "\n" ++ indent ++ "Removed " ++ (List.map elementDisplay originalRemoved |> String.join ",") |>
              aux (i + 1) originalKept modifieds diffsTail
            ListElemInsert count ->
              let (modifiedInserted, modifiedTail) = Utils.split count modifieds in
              acc ++ "\n" ++ indent ++ "Inserted " ++ (List.map elementDisplay modifiedInserted |> String.join ",") |>
              aux i original modifiedTail diffsTail

            ListElemUpdate diff ->
              case (original, modifieds) of
                (ho::to, hm::tm) ->
                  let (incAcc, newIndent) = displayElemModif indent i |> Maybe.withDefault ("", indent) in
                  acc ++ incAcc ++ subroutine newIndent ho hm diff |>
                  aux (i + 1) to tm diffsTail
                _ -> "[Internal error] For diff " ++ toString diffs ++ ", expected non-empty lists, got [" ++ (List.map elementDisplay originals |> String.join ",")  ++ "] and [" ++  (List.map elementDisplay modifieds |> String.join ",") ++ "]"
  in aux 0 originals modifieds diffs ""


vListDiffsToString: String -> List Val -> List Val -> List (Int, ListElemDiff VDiffs) -> String
vListDiffsToString = listDiffsToString "list" vDiffsToString_ (\indent index ->
                                                                  Just ("\n" ++ indent ++ "At index " ++ toString index ++ ", element modified:", indent ++ "  ")
                                                                ) valToString

dictDiffsToString: String -> (String -> k -> Maybe a -> Maybe a -> b -> String) -> Dict k a -> Dict k a -> Dict k b -> String
dictDiffsToString indent subroutine originals modified diffs =
  Dict.foldl (\k diffb accStr ->
    accStr ++ subroutine indent k (Dict.get k originals) (Dict.get k modified) diffb
  ) "" diffs

vDictDiffsToString: String -> Dict (String, String) Val -> Dict (String, String) Val -> Dict (String, String) VDictElemDiff -> String
vDictDiffsToString indent originals modified diffs =
  dictDiffsToString indent (\indent key maybeOriginal maybeModified diff ->
    case (diff, maybeOriginal, maybeModified) of
      (VDictElemInsert, Nothing, Just added) -> "\n" ++ indent ++ "element for '" ++ Tuple.first key ++ "' was inserted: " ++ valToString added
      (VDictElemDelete, Just removed, Nothing) -> "\n" ++ indent ++ "element for '" ++ Tuple.first key ++ "' was removed: " ++ valToString removed
      (VDictElemUpdate d, Just previous, Just after) -> "\n" ++ indent ++ "element for '" ++ Tuple.first key ++ "' was updated: " ++
        vDiffsToString_ (indent ++ "  ") previous after d
      _ -> "[Internal error] Inconsistency between diff and values " ++ toString diff ++ "," ++ (Maybe.map valToString maybeOriginal |> Maybe.withDefault "") ++ "," ++ (Maybe.map valToString maybeModified |> Maybe.withDefault "")
  ) originals modified diffs

vRecordDiffsToString: String -> Dict String Val -> Dict String Val -> Dict String VDiffs -> String
vRecordDiffsToString indent originals modified diffs =
    dictDiffsToString indent (\indent key maybeOriginal maybeModified diff ->
      case (maybeOriginal, maybeModified) of
        (Just original, Just modified) -> "\n" ++ indent ++ "value for '" ++ key ++ "' was updated: " ++ vDiffsToString_ (indent ++ "  ") original modified diff
        _ -> "[Internal error] Inconsistency between diff and values " ++ toString diff ++ "," ++ (Maybe.map valToString maybeOriginal |> Maybe.withDefault "") ++ "," ++ (Maybe.map valToString maybeModified |> Maybe.withDefault "")
    ) originals modified diffs

vClosureDiffsToString: String -> Env -> Exp -> Env -> Exp -> EnvDiffs -> Maybe EDiffs -> String
vClosureDiffsToString indent origEnv origBody modifEnv modifBody diffEnv maybeDiffBody =
  envDiffsToString_ indent origEnv modifEnv diffEnv ++
    (Maybe.map (\ediff ->
      eDiffsToString indent origBody modifBody ediff
    ) maybeDiffBody |> Maybe.withDefault "")

eDiffsToString: String -> Exp -> Exp -> EDiffs -> String
eDiffsToString indent origExp modifExp ediff =
  eDiffsToStringPositions LeoSyntax indent (Pos 1 1, (0, 0)) origExp modifExp ediff |> Tuple.first

-- (p, (r, c)) = On the original position p, it's like if the user pressed r times ENTER ( and added c characters
-- If c is negative, the user pressed -c times the backspace key
-- If r is negative the user removed everything up to the previous newline char and the previous line itself, and added c > 0 characters
type alias LastEdit = (Pos, (Int, Int))

-- Compute a new position given a modification
collapseLastEdit: LastEdit -> Pos
collapseLastEdit (p, (r, c)) =
  if r /= 0 then Pos (p.line + r) c
  else Pos p.line (p.col + c)

-- To a position of an expression, given an insertion of (rowOffset, colOffset) at position (targetRow, targetCol), returns the new position.
offsetPosition: LastEdit ->  Pos ->  Pos
offsetPosition (target, (newlines, newChars)) p  =
  if p.line < target.line || p.line == target.line && p.col < target.col
  then p
  else if p.line == target.line && p.col >= target.col
  then Pos (p.line + newlines) (
    if newlines == 0 then p.col + newChars
    else p.col - target.col + newChars
    )
  else --if p.line > target.line
    { p | line = p.line + newlines }

-- Given that p is the start of the (existing) string, what is the end of it?
addOffsetFromString: Pos -> String -> Pos
addOffsetFromString p s1 =
  let (r1, c1) = deltaLineRow s1 in
  collapseLastEdit (p, (r1, c1))

displayPos: Pos -> String
displayPos p = "L" ++ toString p.line ++ " "

-- Given a last modification, it used to be that at position p we rendered s1. Now we render s2.
-- What would be the new equivalent last modification?
-- And what would be the new ending position?
offsetFromStrings: LastEdit -> Pos -> String -> String -> (LastEdit, Pos)
offsetFromStrings lm startOldReferential s1 s2 =
  let (lmpos, (lmline, lmcol)) = lm in
  let startNewReferential = offsetPosition lm startOldReferential in
  let (r1, c1) = deltaLineRow s1 in
  let (r2, c2) = deltaLineRow s2 in
  let prevEndOldReferential = collapseLastEdit (startOldReferential, (r1, c1)) in
  let prevEndNewReferential = collapseLastEdit (startNewReferential, (r1, c1)) in
  let newEndNewReferential  = collapseLastEdit (startNewReferential, (r2, c2)) in

  let deltaLine = newEndNewReferential.line - prevEndOldReferential.line in
  let deltaCol = if deltaLine == 0 then newEndNewReferential.col - prevEndOldReferential.col else newEndNewReferential.col in
-- Normally, we have that offsetPosition (prevEndOldReferential, (deltaLine, deltaCol)) prevEndOldReferential == newEndNewReferential
  ((prevEndOldReferential, (deltaLine, deltaCol)), newEndNewReferential)

-- Offset difference from what was there originally. The column is after any addition or row (which reset columns to zero)
--offsetFromWhitespace: LastEdit -> Exp -> Exp -> (Int, Int)
--offsetFromWhitespace origExp modifExp =
--  let s1.val = precedingWhitespace origExp in
--  let s2.val = precedingWhitespace modifExp in
--  if s1 == s2 then (0, 0)
--  else offsetFromStrings s1 s2

--combineDiff: LastEdit -> (Pos, String) -> String -> LastEdit
--combineDiff (pos, (lineoffset, coloffset)) (end, sBefore) sAfter =
--  let (lineDelta, colDelta) = offsetFromStrings sBefore sAfter in
--  (end, (lineoffset + lineDelta, if lineDelta >= 0 then colDelta else coloffset + colDelta))

listDiffsToString2: ParensStyle-> String->   (Exp -> String) -> String -> LastEdit -> Pos   -> List (WS, Exp) -> List (WS, Exp) -> ListDiffs EDiffs -> (String, ((LastEdit, Pos), List Exp))
listDiffsToString2 renderingStyle structName elementDisplay     indent    lastEdit    lastPos  originals_        modifieds_        diffs_ =
  if List.isEmpty diffs_ then ("[Internal error]: Empty " ++ structName ++ " diff]", ((lastEdit, lastPos), [])) else
  let displaySpaceComma = case renderingStyle of
    HtmlSyntax -> False
    _ -> True
  in
  let aux: Int -> LastEdit -> Pos ->  List (WS, Exp) -> List (WS, Exp) -> ListDiffs EDiffs -> (String, List Exp) -> (String, ((LastEdit, Pos), List Exp))
      aux  i      lastEdit    lastPos original          modifieds         diffs               (accStr, accList) =
    --let _ = Debug.log ("listDiffsToString.aux: i=" ++ toString i ++ ", lastEdit = " ++ toString lastEdit ++ ", lastPos = " ++ toString lastPos ++
    --   ", original = " ++ Syntax.unparser Syntax.Leo (eListWs original Nothing) ++ ", modifieds = " ++
    --   Syntax.unparser Syntax.Leo (eListWs modifieds Nothing) ++ ", diffs=" ++ toString diffs ++
    --   ", acc=" ++ toString accStr ++ ", accList=" ++ toString accList) () in
    case diffs of
       [] -> (accStr, ((lastEdit, lastPos), accList))
       (j, change)::diffsTail ->
        if j > i then
          let count = j - i in
          let (originalDropped, originalTaken) = Utils.split count original in
          let newLastPos = Utils.maybeLast originalDropped |> flip Utils.maybeWithLazyDefault ( \_ ->
               Debug.crash <| "Inconsistent diffs at index " ++ toString i ++ " in listDiffsToStrings2 " ++ toString renderingStyle ++ " " ++
                toString structName ++ " _ " ++ toString indent ++ " " ++ toString lastEdit ++ " " ++ toString lastPos ++ " " ++
                Syntax.unparser Syntax.Leo (eListWs  originals_ Nothing) ++ " " ++ Syntax.unparser Syntax.Leo (eListWs  modifieds_ Nothing)  ++ " " ++ toString diffs_
            ) |> Tuple.second |> (\(Expr e) -> e.end) in
          aux j lastEdit newLastPos  (List.drop count original) (List.drop count modifieds) diffs (accStr, accList)
        else
          case change of
            ListElemDelete count ->
              let (originalRemoved, originalKept) = Utils.split count original in
              let secondCommaSpace = List.tail originalKept |> Maybe.andThen List.head
                |> Maybe.map (Tuple.first >> .val)
                |> Maybe.map (\x -> x ++  ",")
                |> Maybe.withDefault "" in
              let beforeS = originalRemoved |> List.indexedMap (\k (sp, e) ->
                (if i + k > 0 && displaySpaceComma then sp.val ++ "," else "") ++ elementDisplay e ++
                (if i + k == 0 && displaySpaceComma then secondCommaSpace else "")
                ) |> String.join "" in
              let afterS = "" in
              let (newLastEdit, newEnd) = offsetFromStrings lastEdit lastPos beforeS afterS in
              let newStartPos = offsetPosition lastEdit lastPos in
              let removedExp = dummyExp1 "-" newStartPos.line newStartPos.col newStartPos.line newStartPos.col in
              ( accStr ++ "\n" ++ indent ++ displayPos newEnd ++ "Removed '" ++ beforeS ++ "'",
                removedExp::accList) |>
              aux (i + count) newLastEdit newEnd originalKept modifieds diffsTail
            ListElemInsert count ->
              let (modifiedInserted, modifiedTail) = Utils.split count modifieds in
              let secondCommaSpace = List.tail modifiedInserted |> Maybe.andThen List.head
                |> Maybe.map (Tuple.first >> .val)
                |> Maybe.map (\x -> x ++  ",")
                |> Maybe.withDefault "" in
              let beforeS = "" in
              let afterS = modifiedInserted |> List.indexedMap (\k (sp, e) ->
                (if i + k > 0 && displaySpaceComma then sp.val ++ "," else "") ++ elementDisplay e ++
                (if i + k == 0 && displaySpaceComma then secondCommaSpace else "")) |> String.join "" in
              let (newLastEdit, newEnd) = offsetFromStrings lastEdit lastPos beforeS afterS in
              let newStartPos = offsetPosition lastEdit lastPos in
              let insertedExp = dummyExp1 "+" newStartPos.line newStartPos.col newEnd.line newEnd.col in
              (accStr ++ "\n" ++ indent ++ displayPos newEnd ++ "Inserted '" ++ afterS ++ "'",
                insertedExp::accList) |>
              aux i newLastEdit lastPos original modifiedTail diffsTail

            ListElemUpdate diff ->
              case (original, modifieds) of
                ((spo, Expr ho)::to, (spm, hm)::tm) ->
                  let (incAcc, ((newLastEdit, lastPos2), newHighlights)) =
                    case diff of
                       EConstDiffs EOnlyWhitespaceDiffs ->
                        let (newLastEdit, newEndSpace) = offsetFromStrings lastEdit lastPos spo.val spm.val in
                        let newElemEnd = offsetPosition newLastEdit ho.end in
                        ("", ((newLastEdit, ho.end), []))
                       _ ->
                        eDiffsToStringPositions renderingStyle indent lastEdit (Expr ho) hm diff
                  in
                  (accStr ++ incAcc, newHighlights++accList)  |>
                  aux (i + 1) newLastEdit lastPos2 to tm diffsTail
                _ ->
                  (accStr ++ "[Internal error]2 For diff " ++ toString diff ++ ", expected non-empty lists, got [" ++
                  (List.map elementDisplay (Utils.listValues originals_) |> String.join ",")  ++ "] and [" ++
                  (List.map elementDisplay (Utils.listValues modifieds_) |> String.join ",") ++ "]", ((lastEdit, lastPos), accList))
  in aux 0 lastEdit lastPos originals_ modifieds_ diffs_ ("", [])


stringDiffsToString2: ParensStyle -> String -> LastEdit -> Pos   -> String -> String -> String -> List StringDiffs -> (String, ((LastEdit, Pos), List Exp))
stringDiffsToString2  renderingStyle indent    lastEdit    lastPos  quoteChar original  modified  diffs =
  let renderChars = case renderingStyle of
     LongStringSyntax -> LeoUnparser.unparseLongStringContent
     HtmlSyntax -> LeoUnparser.unparseHtmlTextContent LeoUnparser.Interpolated
     _ -> ParserUtils.unparseStringContent quoteChar
  in
  let initialLine = lastPos.line in
  -- If one-line string, characters \n \t \r and \\ count for double.
  let aux: LastEdit -> Pos -> Int -> Int -> List StringDiffs -> (List String, List Exp) -> (String, ((LastEdit, Pos), List Exp))
      aux lastEdit lastPos lastEnd offset diffs (revAcc, revAccExp) = case diffs of
    [] -> (String.join ", " (List.reverse revAcc), ((lastEdit, lastPos), List.reverse revAccExp))
    StringUpdate start end replacement :: tail ->
       --let _ = Debug.log ("string diffs on display") (diffs) in
       --let _ = Debug.log ("rendering style") (renderingStyle) in
       -- We merge changes if the length of the "same" string is less than the length of the previous change or the next change.
       -- Was useful when the diff was not colored.
       --let mbMergeDiffs = case tail of
       --  StringUpdate start2 end2 replacement2 :: tail2->
       --     if start2 - end <= end - start || start2 - end <= end2 - start2 then
       --       Just (StringUpdate start end2 (replacement + replacement2 + (start2 - end)) :: tail2)
       --     else Nothing
       --  _ -> Nothing
       --in
       --case mbMergeDiffs of
       --   Just newDiffs -> aux lastEdit lastPos lastEnd offset newDiffs (revAcc, revAccExp)
       --   Nothing ->
       let betweenNormalized = renderChars <| String.slice lastEnd start original in
       --let _ = ImpureGoodies.log <| "lastPos = " ++ toString lastPos  in
       let lastPos1 = addOffsetFromString lastPos betweenNormalized in
       --let _ = ImpureGoodies.log <| "lastPos1 = " ++ toString lastPos1  in
       let beforeS = renderChars <| String.slice start end original in
       let afterS = renderChars <| String.slice (start + offset) (start + offset + replacement) modified in
       --let _ = ImpureGoodies.log <| "Computed beforeS from " ++ toString (StringUpdate start end replacement) ++ " and original = '"++original++"', = '" ++ beforeS ++ "'"  in
       --let _ = ImpureGoodies.log <| "Offset = " ++ toString offset  in
       --let _ = ImpureGoodies.log <| "Computed afterS from " ++ toString (StringUpdate start end replacement) ++ " and modified = '"++modified++"', = '" ++ beforeS ++ "'"  in
       let (newLastEdit, newEndPos) = offsetFromStrings lastEdit lastPos1 beforeS afterS in
       let newStartPos = offsetPosition lastEdit lastPos1 in
       let (accInc, title) = if start == end then -- Pure insertion
            ("\n" ++ indent ++ displayPos newEndPos ++ "Inserted [" ++ afterS ++ "]", "+")
         else if replacement == 0 then -- Pure deletion
            ("\n" ++ indent ++ displayPos newEndPos ++ "Removed [" ++ beforeS ++ "]", "-")
         else -- Replacement
            ("\n" ++ indent ++ displayPos newEndPos ++ "Replaced [" ++ beforeS ++ "] by ["++ afterS ++"]", "~")
       in
       --let _ = ImpureGoodies.log <| "newEndPos = " ++ toString newEndPos  in
       let newOffset = offset - (end - start) + replacement in
       let newRevAcc = accInc::revAcc in
       let endCol = if newStartPos.line == newEndPos.line && newStartPos.col == newEndPos.col then newEndPos.col {-+ 1-} else newEndPos.col in
       let duckTapeOffset = if renderingStyle == LongStringSyntax && initialLine < newStartPos.line then 1 else 0 in
       let insertedExp = dummyExp1 title newStartPos.line (newStartPos.col + duckTapeOffset) newEndPos.line (endCol + duckTapeOffset) in
       aux newLastEdit (Tuple.first newLastEdit) end newOffset tail (newRevAcc, insertedExp::revAccExp)
  in
  aux lastEdit lastPos 0 0 diffs ([], [])


tupleDiffsToString2: ParensStyle -> String -> LastEdit -> List Exp ->      List Exp ->      TupleDiffs EDiffs -> (String, (LastEdit, List Exp))
tupleDiffsToString2  renderingStyle indent    lastEdit    originalChildren modifiedChildren childDiffs =
  let aux: Int -> LastEdit -> List Exp -> List Exp -> TupleDiffs EDiffs -> (String, List Exp) -> (String, (LastEdit, List Exp))
      aux i lastEdit original modified diffs (accStr, accList) =
    --let _ = Debug.log ("aux " ++ toString i ++ " [" ++ (List.take 5 original |> List.map Tuple.first |> String.join ",") ++ "] [" ++
    --     " [" ++ (List.take 5 modified |> List.map Tuple.first |> String.join ",") ++ "] " ++ toString diffs ++ " '" ++ acc ++ "'") () in
    case diffs of
       [] -> (accStr, (lastEdit, accList))
       (j, change)::diffsTail ->
        if j > i then
          let count = j - i in
          aux j lastEdit (List.drop count original) (List.drop count modified) diffs (accStr, accList)
        else if j == i then
          case (original, modified) of
            (b1::tailOriginal, b2::tailModified) ->
              let (incAcc, ((newLastEdit, _), newExps)) = eDiffsToStringPositions renderingStyle indent lastEdit b1 b2 change in
              (accStr ++ incAcc,  accList ++ newExps) |>
                aux (i + 1) newLastEdit tailOriginal tailModified diffsTail
            _ -> ("[Internal error] Expcted non-empty " ++ "expression" ++
                ", diffs = " ++ toString childDiffs ++ ", original children = " ++ (List.map (Syntax.unparser Syntax.Leo) originalChildren |> String.join ",") ++
                ", modified children: " ++ (List.map (Syntax.unparser Syntax.Leo) modifiedChildren |> String.join ""), (lastEdit, []))
        else
          Debug.crash <| "Changes does not match the expression"
  in
  let (msg, (newLastEdit, newExps)) =  aux  0 lastEdit originalChildren modifiedChildren childDiffs ("", []) in
  (msg, (newLastEdit, newExps))

-- Returns a summary of changes,
-- which position was affected last
-- the offset in #row/#column that happens before on the last affected row (hence if we are on a row afterward, column does not count)
-- and a list of expressions to highlight in the code
eDiffsToStringPositions: ParensStyle -> String ->  LastEdit -> Exp ->          Exp ->         EDiffs -> (String, ((LastEdit, Pos), List Exp))
eDiffsToStringPositions renderingStyle  indent     lastEdit    (Expr origExp) (Expr modifExp) ediff =
  let renderExp = case renderingStyle of
     --LongStringSyntax -> LeoUnparser.unparseAnyLongString
     HtmlSyntax -> LeoUnparser.unparseAnyHtml
     _ -> Syntax.unparser Syntax.Leo
  in
  --let _ = Debug.log ("eDiffsToStringPositions: lastEdit = " ++ toString lastEdit) () in
  --let _ = Debug.log ("eDiffsToStringPositions: origExp.start = " ++ toString origExp.start) () in
  case ediff of
      EConstDiffs ws ->
        if ws == EOnlyWhitespaceDiffs then
          let newLastEdit_newEnd = offsetFromStrings lastEdit origExp.start (renderExp <| Expr origExp) (renderExp <| Expr modifExp) in
          ("", (newLastEdit_newEnd, []))
        else (
          let prefix = "\n" ++ indent ++ displayPos origExp.start {-++ "C" ++ toString origExp.start.col-} ++ ": " in
          let beforeS = renderExp <| Expr origExp in
          let afterS = renderExp <| Expr modifExp in
          let msg = "Was " ++ beforeS ++ ", now " ++ afterS in
          let newStart = offsetPosition lastEdit origExp.start  in
          let (newLastEdit, newEnd) = offsetFromStrings lastEdit origExp.start beforeS afterS in
          let diffExp = dummyExp1 "~" newStart.line newStart.col newEnd.line newEnd.col in
          (prefix ++ msg, ((newLastEdit, newEnd), [diffExp]))
         )
      EListDiffs diffs ->
        case (unwrapExp <| Expr origExp, unwrapExp <| Expr modifExp) of
          (EList _ originals _ _ _, EList _ modified _ _ _) ->
            --"\n" ++ indent ++ "Line " ++ toString origExp.start.line ++ " col " ++ toString origExp.start.col ++ " Change in a list:" ++
            let theStart =  origExp.start in
            let lastPos = case renderingStyle of
              HtmlSyntax -> theStart
              _ -> { theStart | col = theStart.col + 1 } in
            listDiffsToString2 renderingStyle "list" renderExp indent lastEdit lastPos originals modified diffs
          _ -> ("[Internal error] eDiffsToString " ++ toString ediff ++ " expects lists here, got " ++ (renderExp <| Expr origExp) ++ ", " ++ (renderExp <| Expr modifExp),
            ((lastEdit, offsetPosition lastEdit origExp.end), []))
      EStringDiffs diffs ->
        case (unwrapExp <| Expr origExp, unwrapExp <| Expr modifExp) of
          (EBase _ (EString quoteChar original), EBase _ (EString _ modified)) ->
             let theStart =  origExp.start in
             let lastPos = case renderingStyle of
               LongStringSyntax -> theStart
               HtmlSyntax -> theStart
               _ -> { theStart | col = theStart.col + 1 }
             in
             stringDiffsToString2 renderingStyle indent lastEdit lastPos quoteChar original modified diffs
          _ -> ("[Internal error] eDiffsToString " ++ toString ediff ++ " expects strings here, got " ++ (renderExp <| Expr origExp) ++ ", " ++ (renderExp <| Expr modifExp),
            ((lastEdit, offsetPosition lastEdit origExp.end), []))
      EChildDiffs diffs ->
        -- If you want the trace of what was modified, just input here something
        let childIndent = "" in
        let newRenderingStyle = case unwrapExp <| Expr origExp of
          EParens _ _ r _ -> r
          _ -> renderingStyle
        in
        --let _ = Debug.log ("current renderingStyle:" ++ toString renderingStyle ++ ", new renderingStyle : " ++ toString newRenderingStyle ++ ", currentExpression:" ++ toString origExp) () in
        let (msg, (newLastEdit, newExps)) = tupleDiffsToString2 newRenderingStyle childIndent lastEdit (childExps <| Expr origExp) (childExps <| Expr modifExp) diffs in
        let newLastPos = offsetPosition newLastEdit origExp.end in
        (msg, ((newLastEdit, newLastPos), newExps))

offsetConcStr: Int -> List (Int, Int, String) -> List (Int, Int, String)
offsetConcStr n diffs =
  List.map (\(start, end, str) -> (start + n, end + n, str)) diffs

-- When f x y k z w was changed to (f x y k) z w (realElementNumber = 3 here), how to recover initial differences
flattenFirstEChildDiffs: Int -> EDiffs -> EDiffs
flattenFirstEChildDiffs realElementNumber ediffs =
  case ediffs of
    EChildDiffs ((0, EChildDiffs l)::tail) -> EChildDiffs (l ++ offset realElementNumber tail)
    EChildDiffs diffs -> EChildDiffs (offset realElementNumber diffs)
    _ -> ediffs

strDiffToConcreteDiff: String -> List StringDiffs -> List (Int, Int, String)
strDiffToConcreteDiff newString d =
  let aux offset d revAcc = case d of
    [] -> List.reverse revAcc
    StringUpdate start end replaced :: tail ->
       (start, end, String.slice (start + offset) (start + replaced + offset) newString)::revAcc |>
       aux (offset + replaced - (end - start)) tail
  in aux 0 d []


pruneConcreteDiffs: List (Int, Int, String) -> List (Int, Int, String)
pruneConcreteDiffs l =
  let aux: List (Int, Int, String) -> List (Int, Int, String)
      aux l = case l of
    ((s1, e1, ss1) as head)::(((s2, e2, ss2)::tail2) as tail1) ->
       --if e1 == s2 then
       --  aux ((s1, e2, ss1 ++ ss2)::tail2)
       --else
       if e1 <= s2 then
         (s1, e1, ss1) :: aux tail1
       else aux (head::tail2)
    _ -> l
  in aux (List.sortBy (\(a, b, c) -> a) l)

applyConcreteDiffs: String ->  List (Int, Int, String)  -> (String, List StringDiffs)
applyConcreteDiffs string diffs =
  let aux l (string, diffs) = case l of
    [] -> (string, diffs)
    (s, e, ss)::tail ->
       (String.left s string ++ ss ++ String.dropLeft e string, StringUpdate s e (String.length ss)::diffs) |>
       aux tail
  in aux (List.reverse diffs) (string, [])

composeStringDiffs: List StringDiffs -> List StringDiffs -> List StringDiffs
composeStringDiffs oldStringDiffs newStringDiffs =
  let aux: Int ->     Int ->    Int ->         List StringDiffs -> List StringDiffs -> List StringDiffs -> List StringDiffs
      aux offsetStart offsetEnd offsetReplaced oldStringDiffs newStringDiffs revStrDiffs =
    --let _ = Debug.log "composeStringDiffs.aux" (offsetStart, offsetEnd, offsetReplaced, oldStringDiffs, newStringDiffs, revStrDiffs) in
    let makeOldReplacement newStrDiff = case newStrDiff of
         StringUpdate newStart newEnd newReplaced ->
          StringUpdate (newStart - offsetStart) (newEnd - offsetEnd) newReplaced
    in
    case (oldStringDiffs, newStringDiffs) of
       ([], []) -> List.reverse revStrDiffs
       ([], newStrDiff  :: newTail) ->
         makeOldReplacement newStrDiff :: revStrDiffs |>
          -- This is not a copy-paste typo: We consumed the offsetStart, now everything else is on the offsetEnd
         aux offsetEnd offsetEnd 0 [] newTail
       (((StringUpdate start end replaced) as oldStrDiff) :: oldTail, []) ->
         StringUpdate start end (replaced + offsetReplaced) ::  revStrDiffs |>
         aux offsetEnd offsetEnd 0 oldTail []
       (((StringUpdate start end replaced) as oldStrDiff) :: oldTail, ((StringUpdate repStart repEnd repCount) as newStrDiff)  :: newTail)  ->
         let newStart = offsetEnd + start
             newEnd = offsetEnd + start + replaced
         in
         if newStart >= repEnd then -- repStart repEnd ... newStart newEnd
           makeOldReplacement newStrDiff :: revStrDiffs |>
           -- This is not a copy-paste typo: We consumed the offsetStart, now everything else is on the offsetEnd
           aux offsetEnd offsetEnd 0 oldStringDiffs newTail
         else if newEnd <= repStart then -- newStart newEnd ... repStart repEnd
           let newOffsetEnd = offsetEnd + replaced - (end - start) in
           (StringUpdate start end (replaced + offsetReplaced)) :: revStrDiffs |>
           -- This is not a copy-paste typo: Everything else runs on the newOffset
           aux newOffsetEnd newOffsetEnd 0 oldTail newStringDiffs
         else if newStart <= repStart && repEnd <= newEnd then  -- newStart repStart repEnd ...  newEnd .
           aux offsetStart offsetEnd (offsetReplaced + repCount - (repEnd - repStart)) (StringUpdate start end replaced :: oldTail)  newTail revStrDiffs
         else -- repStart < newEnd  && newStart < repEnd && (newStart > repStart || newEnd < replacmentEnd)
           if newStart > repStart then
             if newEnd < repEnd then  -- repStart newStart newEnd ... repEnd
               let offsetIncrease = replaced - (end - start) in
               let newOffsetEnd = offsetEnd + offsetIncrease in
               aux offsetStart newOffsetEnd 0 oldTail newStringDiffs revStrDiffs
             else  -- repStart newStart repEnd ... newEnd
               let deltaReplaced = repCount - (repEnd - repStart) in
               aux offsetStart offsetEnd deltaReplaced (StringUpdate (repStart - offsetStart) end (replaced + newStart - repStart) :: oldTail) newTail revStrDiffs
           else -- newStart repStart newEnd ... repEnd
           let offsetIncrease = replaced - (end - start) in
           let newOffsetEnd = offsetEnd + offsetIncrease in
           aux offsetStart newOffsetEnd 0 oldTail (StringUpdate newStart repEnd (repCount + repStart - newStart) :: newTail) revStrDiffs
  in aux 0 0 0 oldStringDiffs newStringDiffs []

-- Wraps a change to a change in the outer expression at the given index
offset: Int -> List (Int, a) -> List (Int, a)
offset n diffs = List.map (\(i, e) -> (i + n, e)) diffs

wrap: Int -> Maybe EDiffs -> Maybe EDiffs
wrap i mbd =
  mbd |> Maybe.map (\d -> EChildDiffs [(i, d)])

-- Add offset to ETupleDiffs, useful for rebuilding ELets
shift: Int -> Maybe EDiffs -> Maybe EDiffs
shift i mbd =
  mbd |> Maybe.map (\d -> case d of
    EChildDiffs ds -> EChildDiffs (offset i ds)
    d -> d)

replace: Int -> a -> TupleDiffs a -> TupleDiffs a
replace n a td = case td of
  [] -> td
  ((i, k) as head)::t ->
    if i == n then (i, a)::t
    else if i > n then (n, a)::td
    else head :: replace n a t

diffsAt: Int -> TupleDiffs a -> Maybe a
diffsAt n td = case td of
  [] -> Nothing
  (i, a)::t ->
    if i == n then Just a
    else if i > n then Nothing
    else diffsAt n t

mapDiffs: (a -> Maybe b -> (aa, Maybe bb)) ->  (List a, TupleDiffs b) -> (List aa, TupleDiffs bb)
mapDiffs f (l, diffs) =
  let aux: Int -> List a -> TupleDiffs b -> (List aa, TupleDiffs bb) -> (List aa, TupleDiffs bb)
      aux i l diffs (revLa, revDb)= case l of
        [] -> (List.reverse revLa, List.reverse revDb)
        a::aTail->
          let (thisDiff, tailDiffs) = case diffs of
            [] -> (Nothing, diffs)
            (j, d)::dTail ->
               if i < j then (Nothing, diffs)
               else if i == j then (Just d, dTail)
               else Debug.crash "Malformed diffs" (diffs, i)
          in
          let (newA, newDiff) = f a thisDiff in
           (newA::revLa, (newDiff |> Maybe.map ((,) i >> List.singleton) |> Maybe.withDefault []) ++ revDb) |>
           aux (i + 1) aTail tailDiffs
  in aux 0 l diffs ([], [])

zipDiffs: List a -> TupleDiffs b -> List (a, Maybe b)
zipDiffs l diffs =
  let aux: Int -> List a -> TupleDiffs b -> List (a, Maybe b) -> List (a, Maybe b)
      aux i l diffs revAcc = case l of
        [] -> List.reverse revAcc
        a::aTail ->
          let (thisDiff, tailDiffs) = case diffs of
            [] -> (Nothing, diffs)
            (j, d)::dTail ->
               if i < j then (Nothing, diffs)
               else if i == j then (Just d, dTail)
               else Debug.crash "Malformed diffs" (diffs, i)
          in
          (a, thisDiff) :: revAcc |>
          aux (i + 1) aTail tailDiffs
  in aux 0 l diffs []

changeAt: List Int -> Maybe EDiffs -> Maybe EDiffs
changeAt path mbd = case path of
  [] -> mbd
  head::tail ->
    case mbd of
      Nothing -> Nothing
      Just (EChildDiffs d) -> changeAt tail (diffsAt head d)
      _ -> Nothing

dropDiffs: Int -> TupleDiffs a -> TupleDiffs a
dropDiffs n td = case td of
  [] -> []
  (i, a)::t ->
    if i < n then dropDiffs n t
    else offset (0 - n) td

toTupleDiffs: ListDiffs a -> Maybe (TupleDiffs a)
toTupleDiffs l = case l of
  [] -> Just []
  (i, ListElemUpdate d)::tail -> toTupleDiffs tail |> Maybe.map (\td -> (i, d)::td)
  _ -> Nothing

-- Returns true if the index i was modified
eDiffModifiedIndex: Int -> EDiffs -> Bool
eDiffModifiedIndex i ediffs =
  case ediffs of
    EChildDiffs ((j, _)::tail) -> if j < i then  eDiffModifiedIndex i <| EChildDiffs tail else (i == j)
    _ -> False

modifiedIndices: TupleDiffs a -> List Int
modifiedIndices = List.map Tuple.first


combineEChildDiffs: List (Int, Maybe EDiffs) -> Maybe EDiffs
combineEChildDiffs l =
  combineTupleDiffs l |> Maybe.map EChildDiffs

combineTupleDiffs: List (Int, Maybe a) -> Maybe (TupleDiffs a)
combineTupleDiffs l =
  let aux revAcc l = case l of
    [] -> case List.reverse revAcc of
       [] -> Nothing
       acc -> Just <| acc
    (i, Nothing)::tail -> aux revAcc tail
    (i, Just e)::tail -> aux ((i, e)::revAcc) tail
  in aux [] l


-- Combines function and argument changes into one single change
combineAppChanges: Maybe EDiffs -> Maybe (TupleDiffs EDiffs) -> Maybe EDiffs
combineAppChanges newE1Changes newE2Changes =
  let e1Changes = newE1Changes |> Maybe.map (\x -> [(0, x)]) |> Maybe.withDefault [] in
  let e2Changes = newE2Changes |> Maybe.map (offset 1) |> Maybe.withDefault [] in
  case e1Changes ++ e2Changes of
    [] -> Nothing
    eChanges -> Just <| EChildDiffs eChanges

tupleDiffsToDiffs: TupleDiffs a -> Results String (Maybe (TupleDiffs a))
tupleDiffsToDiffs t = case t of
  []-> ok1 Nothing
  l ->ok1 <| Just l

-- Invoke this only if strictly necessary.
defaultVDiffs: Val -> Val -> Results String (Maybe VDiffs)
defaultVDiffs original modified =
  --let _ = Debug.log ("defaultVDiffs " ++ valToString original ++ "\n" ++ valToString modified) () in
  {--
   (\x ->
    let diffs=  Results.toList x in
    let _ = if List.length diffs > 1 then
       Debug.log ("There was a diff ambiguity here : " ++ toString diffs ++ "\n" ++ valToString original ++ "\n" ++ valToString modified) ()
       else
          Debug.log ("A diff was recomputed here : " ++ (diffs |> List.filterMap identity |> List.map (vDiffsToString original modified) |> String.join "\n") ++ "\n" ++ valToString original ++ "\n" ++ valToString modified) <|
          ()
    in
    x) <|
  --}
  defaultVDiffsRec True defaultVDiffs original modified

defaultVDiffsRec: Bool -> (Val -> Val -> Results String (Maybe VDiffs)) -> Val -> Val -> Results String (Maybe VDiffs)
defaultVDiffsRec testEquality recurse original modified =
  {-(\result ->
    case result of
      Err msg -> result
      Ok ll ->
        let x = ll |> LazyList.toList |> List.length in
        let _ = if x > 1 then
          Debug.log ("Number of diffs for " ++ valToString original ++ "\nvs.\n" ++ valToString modified) x else x in
        result
  ) <|-}
  case (original.v_, modified.v_) of
    (VList originals, VList modifieds) ->
      defaultListDiffs valToString (\v -> valToMaybeCtorName v |> Utils.maybeOrElseLazy (\() -> getViewDatatypeName v))
        recurse originals modifieds |> Results.map (Maybe.map VListDiffs)
    (VBase (VString originalStr), VBase (VString modifiedStr)) ->
      defaultStringDiffs originalStr modifiedStr |> Results.map (Maybe.map VStringDiffs)
    (VClosure _ pats1 body1 env1, VClosure _ pats2 body2 env2) ->
      let ids = Set.union (Set.diff (Lang.identifiersSet body1) (Lang.identifiersSetInPats pats1))
               (Set.diff (Lang.identifiersSet body2) (Lang.identifiersSetInPats pats2))
      in
      defaultEnvDiffsRec testEquality recurse ids env1 env2 |> Results.andThen (\mbEnvDiff ->
         defaultEDiffs body1 body2 |> Results.map (\mbEDiff ->
          case mbEnvDiff of
            Nothing ->
              case mbEDiff of
                Nothing -> Nothing
                _ -> Just <| VClosureDiffs [] mbEDiff
            Just x -> Just <| VClosureDiffs x mbEDiff
      ))
    (VDict original, VDict modified) ->
      defaultDictDiffs valToString recurse original modified
    (VRecord original, VRecord modified) ->
      case (Dict.get Lang.ctorDataType original, Dict.get ctorDataType modified) of
        (Just x, Just  y) -> case (x.v_, y.v_) of
           (VBase (VString sx), VBase (VString sy)) -> if sx == sy
             then defaultRecordDiffs valToString recurse original modified
             else ok1 (Just VConstDiffs)
           _ -> defaultRecordDiffs valToString recurse original modified
        (Just x, Nothing) -> ok1 (Just VConstDiffs)
        (Nothing, Just x) -> ok1 (Just VConstDiffs)
        (Nothing, Nothing) -> defaultRecordDiffs valToString recurse original modified
    (v1, v2) -> if valEqual original modified then ok1 Nothing else ok1 (Just VConstDiffs)

defaultStringDiffs: String -> String -> Results String (Maybe (List StringDiffs))
defaultStringDiffs before after =
  if before == after then ok1 Nothing else
  allStringDiffs before after |> Results.andThen (\difference ->
    let aux: Int -> List (DiffChunk String) -> List StringDiffs -> Results String (Maybe (List StringDiffs))
        aux  i      diffs                      revAccDiffs = case diffs of
          [] -> case List.reverse revAccDiffs of
            [] -> ok1 Nothing
            diffs -> ok1 <| Just <| diffs
          DiffEqual elems::diffTail ->
            aux (i + String.length elems) diffTail revAccDiffs
          DiffRemoved removed::DiffAdded added::diffTail ->
            let lengthRemoved = String.length removed in
            let lengthAdded = String.length added in
            StringUpdate i (i + lengthRemoved) lengthAdded :: revAccDiffs |>
              aux (i + lengthRemoved) diffTail
          DiffRemoved removed::diffTail ->
            let lengthRemoved = String.length removed in
            StringUpdate i (i + lengthRemoved) 0 :: revAccDiffs  |>
              aux (i + lengthRemoved) diffTail
          DiffAdded added::diffTail ->
            let lengthAdded = String.length added in
            StringUpdate i i lengthAdded ::revAccDiffs |>
              aux i diffTail
    in aux 0 difference []
  )


-- Given a way to get the name of datatypes-encoded values, try to perform an alignment on the datatype names instead
-- If all the datatypes returned are the same, return Nothing.
alignRecordDatatypes: (a -> Maybe String) -> List a -> List a -> List (DiffChunk (List a)) -> Maybe (Results String (List (DiffChunk (List a))))
alignRecordDatatypes datatypeNameOf removed added difftail =
  let withDatatypes l = List.map (\elem -> (elem, datatypeNameOf elem)) l in
  let resDataTypeDifferences = alldiffs (\(_, datatypename) -> Maybe.withDefault "" datatypename) (withDatatypes removed) (withDatatypes added) in
  case resDataTypeDifferences of
    Ok (LazyList.Cons [DiffEqual _] _) -> Nothing
    _ ->
      resDataTypeDifferences |> Results.map (\datatypedifferences ->
  let aux: List (DiffChunk (List (a, Maybe String))) -> List a -> List a -> List (DiffChunk (List a)) -> List (DiffChunk (List a))
      aux diffs removed added revAccDiffs = case diffs of
     [] -> List.reverse revAccDiffs
     DiffEqual elems::difftail -> -- Same datatypes here, so the alignment is good.
       let count = List.length elems in
       let (removedTaken, removedRemaining) = Utils.split count removed in
       let (addedTaken, addedRemaining) = Utils.split count added in
       (DiffEqual [] :: DiffAdded addedTaken :: DiffRemoved removedTaken :: revAccDiffs) |>
       aux difftail removedRemaining addedRemaining
     DiffRemoved elems::difftail -> -- tags were deleted
       let count = List.length elems in
       let (removedTaken, removedRemaining) = Utils.split count removed in
       (DiffEqual [] :: DiffRemoved removedTaken :: revAccDiffs) |>
       aux difftail removedRemaining added
     DiffAdded elems :: difftail -> -- tags were inserted
       let count = List.length elems in
       let (addedTaken, addedRemaining) = Utils.split count added in
       -- Prevent these insertions to be part of a replacement
       (DiffEqual [] :: DiffAdded addedTaken :: revAccDiffs) |>
       aux difftail removed addedRemaining
  in
  aux datatypedifferences removed added []
  ) |> Just


defaultListDiffs: (a -> String) -> (a -> Maybe String) -> (a -> a -> Results String (Maybe b)) -> List a -> List a -> Results String (Maybe (ListDiffs b))
defaultListDiffs keyOf datatypeNameOf defaultElemModif elems1 elems2 =
  alldiffs keyOf elems1 elems2 |> Results.andThen (\difference ->
  {-let _ = Debug.log ("Processing list difference: " ++ String.join "," (List.map (\d -> case d of
    DiffEqual elems -> "DiffEqual " ++ toString (List.length elems)
    DiffRemoved elems -> "DiffRemoved " ++ toString (List.length elems)
    DiffAdded elems -> "DiffAdded " ++ toString (List.length elems)
      ) difference)) () in-}
  let aux: Int -> List (Int, ListElemDiff b) -> List (DiffChunk (List a)) -> Results String (Maybe (ListDiffs b))
      aux i accDiffs diffs = case diffs of
        [] -> case List.reverse accDiffs of
           [] -> ok1 Nothing
           diffs -> ok1 <| Just <| diffs
        DiffEqual elems::difftail ->
          aux (i + List.length elems) accDiffs difftail
        DiffRemoved removed::DiffAdded added::difftail ->
          -- Better align elements based on datatype.
          case alignRecordDatatypes datatypeNameOf removed added difftail of
            Just resNewDiffs -> resNewDiffs |> Results.andThen (aux i accDiffs)
            Nothing ->
          -- No alignment possible, hence we just diff all elements and then insert/delete the remaining elements.
          let lengthRemoved = List.length removed in
          let lengthAdded = List.length added in
          let toInsertRes = List.map3 (\i r a -> defaultElemModif r a |>
             Results.map (Maybe.map (\v -> (i, ListElemUpdate v)))) (List.range i (i + lengthAdded - 1)) removed added in
          Results.projOk toInsertRes |> Results.andThen ((\i lengthRemoved difftail toInsert ->
             let accDiffs1 = maybeReverseInsert toInsert accDiffs in
             let accDiffs2 = if lengthRemoved > lengthAdded then
                  (i + lengthAdded, ListElemDelete (lengthRemoved - lengthAdded))::accDiffs1
                  else accDiffs1
             in
             let accDiffs3 = if lengthAdded > lengthRemoved then
                  (i + lengthRemoved, ListElemInsert (lengthAdded - lengthRemoved))::accDiffs2
                  else accDiffs2
             in
             aux (i + lengthRemoved) accDiffs3 difftail
             ) i lengthRemoved difftail)
        DiffRemoved elems::difftail ->
          let removedLength = List.length elems in
          aux (i + removedLength) ((i, ListElemDelete removedLength)::accDiffs) difftail
        DiffAdded elems::difftail ->
          let addedLength = List.length elems in
          aux i ((i, ListElemInsert addedLength)::accDiffs) difftail
  in aux 0 [] difference
  )

valEqualDiff: Val -> Val -> Bool
valEqualDiff original modified =
  case defaultVDiffs original modified of
    Ok (LazyList.Cons Nothing tail) -> True
    _ -> False


defaultEnvDiffs: Set Ident -> Env -> Env -> Results String (Maybe EnvDiffs)
defaultEnvDiffs = defaultEnvDiffsRec True defaultVDiffs

defaultEnvDiffsRec: Bool     -> (Val -> Val -> Results String (Maybe VDiffs)) -> Set Ident -> Env -> Env -> Results String (Maybe EnvDiffs)
defaultEnvDiffsRec testEquality recurse identsToCompare elems1 elems2 =
  let aux: Int -> Set Ident    -> List (Int, VDiffs) -> Env         -> Env -> Results String (Maybe EnvDiffs)
      aux  i      identsToCompare revEnvDiffs           envToCollect1  envToCollect2 =
        if Set.isEmpty identsToCompare then
          case List.reverse revEnvDiffs of
            [] -> ok1 Nothing
            envDiffs -> ok1 <| Just envDiffs
        else
        case (envToCollect1, envToCollect2) of
          ([], []) ->
            case List.reverse revEnvDiffs of
              [] -> ok1 Nothing
              envDiffs -> ok1 <| Just envDiffs
          (((k1, v1) as ehd1)::etl1, ((k2, v2) as ehd2)::etl2) ->
            if k1 /= k2 then Err <| "trying to compute a diff on unaligned environments " ++ k1 ++ "," ++ k2 else
            if not (Set.member k1 identsToCompare) then
              aux (i + 1) identsToCompare revEnvDiffs etl1 etl2
            else if testEquality && valEqualDiff v1 v2 then
              aux (i + 1) (Set.remove k1 identsToCompare) revEnvDiffs etl1 etl2
            else
              recurse v1 v2 |> Results.andThen ((\i k1 identsToCompare etl1 etl2 revEnvDiffs mbv ->
                let newRevEnvDiffs = case mbv of
                  Nothing -> revEnvDiffs
                  Just v -> (i, v)::revEnvDiffs
                in
                aux (i + 1) (Set.remove k1 identsToCompare) newRevEnvDiffs etl1 etl2
              ) i k1 identsToCompare etl1 etl2 revEnvDiffs)
          _ -> Err <| "Environments do not have the same size: " ++ envToString envToCollect1 ++ ", " ++ envToString envToCollect2
  in aux 0 identsToCompare  [] elems1 elems2

defaultTupleDiffs: (a -> String) -> (a -> a -> Results String (Maybe b)) -> List a -> List a -> Results String (Maybe (TupleDiffs b)) -- lowercase val so that it can be applied to something else?
defaultTupleDiffs keyOf defaultElemModif elems1 elems2 =
  let aux: Int -> List (Int, b) -> List a  -> List a -> Results String (Maybe (TupleDiffs b))
      aux  i      revEnvDiffs          l1         l2 =
        case (l1, l2) of
          ([], []) ->
            case List.reverse revEnvDiffs of
              [] -> ok1 Nothing
              tupleDiffs -> ok1 <| Just tupleDiffs

          (v1::etl1, v2::etl2) ->
            if keyOf v1 == keyOf v2 then
              aux (i + 1) revEnvDiffs etl1 etl2
            else
              defaultElemModif v1 v2 |> Results.andThen ((\i etl1 etl2 revEnvDiffs mbv ->
                 let newRevEnvDiffs = case mbv of
                   Nothing -> revEnvDiffs
                   Just v -> (i, v)::revEnvDiffs
                 in
                 aux (i + 1) newRevEnvDiffs etl1 etl2) i etl1 etl2 revEnvDiffs)
          _ -> Err <| "Tuples do not have the same size: " ++ toString l1 ++ ", " ++ toString l2
  in aux 0 [] elems1 elems2

defaultDictDiffs: (Val -> String) -> (Val -> Val -> Results String (Maybe VDiffs)) -> Dict (String, String) Val -> Dict (String, String) Val -> Results String (Maybe VDiffs)
defaultDictDiffs keyOf defaultElemModif elems1 elems2 =
  Results.map (\d -> if Dict.isEmpty d then Nothing else Just <| VDictDiffs d) <| Dict.merge
    (\k1 v1 acc -> Results.map (Dict.insert k1 VDictElemDelete) acc)
    (\k v1 v2 acc -> if keyOf v1 == keyOf v2 then acc
       else
         acc |> Results.andThen (\acc ->
           defaultElemModif v1 v2 |> Results.map (\mbv ->
             mbv |> Maybe.map (\v -> Dict.insert k (VDictElemUpdate v) acc) |> Maybe.withDefault acc)))
    (\k2 v2 acc -> Results.map (Dict.insert k2 VDictElemInsert) acc)
    elems1
    elems2
    (ok1 Dict.empty)

defaultRecordDiffs: (Val -> String) -> (Val -> Val -> Results String (Maybe VDiffs)) -> Dict String Val -> Dict String Val -> Results String (Maybe VDiffs)
defaultRecordDiffs keyOf defaultElemModif elems1 elems2 =
  Results.map (\d -> if Dict.isEmpty d then Nothing else Just <| VRecordDiffs d) <| Dict.merge
    (\k1 v1 acc -> Err <| "Not allowed to remove a key from record:" ++ k1)
    (\k v1 v2 acc -> if keyOf v1 == keyOf v2 then acc else acc |> Results.andThen (\acc ->
      defaultElemModif v1 v2 |> Results.map (\mbv ->
        mbv |> Maybe.map (\v ->
         Dict.insert k v acc) |> Maybe.withDefault acc)))
    (\k2 v2 acc -> Err <| "Not allowed to insert a key to record:" ++ k2 ++ " " ++ valToString v2)
    elems1
    elems2
    (ok1 Dict.empty)

defaultEDiffs: Exp -> Exp -> Results String (Maybe EDiffs)
defaultEDiffs e1 e2 =
  case ((unwrapExp e1), (unwrapExp e2)) of
    (EConst _ n1 _ _, EConst _ n2 _ _) -> if n1 == n2 then ok1 Nothing else ok1 <| Just <| EConstDiffs EAnyDiffs
    (EBase _ (EString _ s1), EBase _ (EString _ s2)) ->
       defaultStringDiffs s1 s2 |> Results.map (Maybe.map EStringDiffs)
    (EBase _ x, EBase _ y) -> if x == y then ok1 <| Just <| EConstDiffs EAnyDiffs else ok1 <| Nothing
    (EList _ e1s _ Nothing _, EList _ e2s _ Nothing _) ->
      defaultListDiffs (Syntax.unparser Syntax.Leo) (\_ -> Nothing) defaultEDiffs (Utils.listValues e1s) (Utils.listValues e2s) |> Results.map (Maybe.map EListDiffs)
    (_, _) ->
      defaultTupleDiffs (Syntax.unparser Syntax.Leo) defaultEDiffs (childExps e1) (childExps e2) |> Results.map (Maybe.map EChildDiffs)

-- Assume that the lists are always aligned
autoMergeTuple: (o -> a -> a -> a) -> List o -> List a -> List a -> List a
autoMergeTuple submerger original modified1 modified2 =
  List.map3 submerger original modified1 modified2

mergeTuple: (a -> a -> vDiffs -> a -> vDiffs -> (a, vDiffs)) -> List a -> List a -> TupleDiffs vDiffs -> List a -> TupleDiffs vDiffs -> (List a, TupleDiffs vDiffs)
mergeTuple submerger =
  let aux: Int -> List a -> List (Int,  vDiffs) -> List a -> List a -> TupleDiffs vDiffs -> List a -> TupleDiffs vDiffs -> (List a, TupleDiffs vDiffs)
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
             aux (i + countToIgnore) (reverseInsert toInsert accTuple) accDiffs (List.drop countToIgnore origTuple)
                 (List.drop countToIgnore newTup2) modifs2 (List.drop countToIgnore newTup3) modifs3
         _ -> Debug.crash <| "Expected tuples to have the same size, got\n" ++
                       toString origTuple ++ ", " ++ toString newTup2 ++ ", " ++ toString newTup3
    in aux 0 [] []

mergeEnv: Env -> Env -> EnvDiffs -> Env -> EnvDiffs -> (Env, EnvDiffs)
mergeEnv originalEnv_ newEnv2_ modifs2_ newEnv3_ modifs3_ =
  let aux: Int -> Env -> List (Int,  VDiffs) -> Env ->     Env ->  List (Int,  VDiffs) -> Env ->  List (Int,  VDiffs) -> (Env, EnvDiffs)
      aux  i      accEnv accDiffs              originalEnv newEnv2 modifs2                newEnv3 modifs3=
    --let _ = Debug.log ("aux " ++ toString i ++ "\n" ++
    --                                  (List.take 5 originalEnv |> List.map Tuple.first |> String.join ",") ++ "...\n" ++
    --                                  (List.take 5 newEnv2 |> List.map Tuple.first |> String.join ",") ++ "...\n" ++
    --                                  (List.take 5 newEnv3 |> List.map Tuple.first |> String.join ",") ++ "...\n" ++ "\nModifications:\n" ++
    --                                  toString modifs2 ++ "\n" ++ toString modifs3) () in-}
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

defaultMerge: Val ->   Val ->    VDiffs -> Val ->    VDiffs -> (Val, VDiffs)
defaultMerge  original modified1 modifs1   modified2 modifs2 =
  if valEqual original modified2 then (modified1, modifs1) else (modified2, modifs2)

-- Merges values using a diffing algorithm.
mergeVal: Val ->   Val ->    VDiffs -> Val ->    VDiffs -> (Val, VDiffs)
mergeVal  original modified1 modifs1   modified2 modifs2 =
  --let _ = Debug.log (valToString original ++ "<-(\n" ++ valToString modified1 ++ "("++toString modifs1++")" ++ "\n,\n" ++ valToString modified2++ "("++toString modifs2++")" ++ ")") () in
  --(\x -> let _ = Debug.log (Tuple.first x |> valToString) () in x) <|
  case (original.v_, modified1.v_, modifs1, modified2.v_, modifs2) of    -- TODO: Find multiple elem insertions and deletions
    (VBase (VString originalString), VBase (VString modified1String), VConstDiffs, VBase (VString modified2String), VConstDiffs) ->
      (replaceV_ original <| VBase (VString <| mergeStringHeuristic originalString modified1String modified2String), VConstDiffs)
    (VBase (VString originalString), VBase (VString modified1String), VStringDiffs diffs1, VBase (VString modified2String), VStringDiffs diffs2) ->
      let (newString, newDiffs) = mergeString originalString modified1String diffs1 modified2String diffs2 in
       (replaceV_ original <| VBase <| VString <| newString, VStringDiffs newDiffs)
    (VList originalElems, VList modified1Elems, VListDiffs l1, VList modified2Elems, VListDiffs l2) ->
      let (newList, newDiffs) = mergeList mergeVal originalElems modified1Elems l1 modified2Elems l2 in
      --let _ = Debug.log ("mergeList " ++ "[" ++ (List.map valToString originalElems |> String.join ",") ++ "]" ++ " " ++
      --      "[" ++ (List.map valToString modified1Elems |> String.join ",") ++ "]" ++ toString l1 ++
      --      "[" ++ (List.map valToString modified2Elems |> String.join ",") ++ "]" ++ toString l2
      --     ) ()
      --in
      --let _ = Debug.log ("=" ++ "[" ++ (List.map valToString newList |> String.join ",") ++ "], " ++ toString newDiffs) () in
      (replaceV_ original <| VList <| newList, VListDiffs newDiffs)

    (VRecord originalDict, VRecord modified1Dict, VRecordDiffs d1, VRecord modified2Dict, VRecordDiffs d2) ->
      let (newDict, newDiffs) = mergeRecord mergeVal originalDict modified1Dict d1 modified2Dict d2 in
      (replaceV_ original <| VRecord <| newDict, VRecordDiffs newDiffs)

    (VDict originalDict, VDict modified1Dict, VDictDiffs d1, VDict modified2Dict, VDictDiffs d2) ->
      let (newDict, newDiffs) = mergeDict mergeVal originalDict modified1Dict d1 modified2Dict d2 in
      (replaceV_ original <| VDict <| newDict, VDictDiffs newDiffs)

    (VClosure recNames0 pats0 body0 env0, VClosure recNames1 pats1 body1 env1, VClosureDiffs envmodifs1 bodymodifs1, VClosure recNames2 pats2 body2 env2, VClosureDiffs envmodifs2 bodymodifs2) ->
      if recNames0 == recNames1 && recNames1 == recNames2 then
        if patsEqual pats0 pats1 pats2 then
          let (newEnv, newEnvDiffs) = mergeEnv env0 env1 envmodifs1 env2 envmodifs2 in
          let (newBody, newBodyModifs) = case (bodymodifs1, bodymodifs2) of
            (Just emodif1, Just emodif2) ->
               --let _ = Debug.log "Merging two VClosures' bodies, it's unusual enough to be noticed" () in
               (mergeExp body0 body1 emodif1 body2 emodif2) |> Tuple.mapSecond Just
            (Nothing, _) -> (body2, bodymodifs2)
            (_, Nothing) -> (body1, bodymodifs1)
          in
          (replaceV_ original <| VClosure recNames0 pats0 newBody newEnv, VClosureDiffs newEnvDiffs newBodyModifs)
        else defaultMerge original modified1 modifs1   modified2 modifs2
      --(VRecord originalElems, VRecord modified1Elems, VRecord modified2Elems)->
      --  Dict.keys originalElems
      else defaultMerge original modified1 modifs1   modified2 modifs2
    _ ->
      --let _ = Debug.log ("mergeVal" ++ valToString original ++ " "  ++ valToString modified1 ++ " " ++ valToString modified2) " " in
      defaultMerge original modified1 modifs1   modified2 modifs2
      --let _ = Debug.log ("mergeVal=" ++ valToString result) "" in

patsEqual: List Pat -> List Pat -> List Pat -> Bool
patsEqual pats1 pats2 pats3 = List.all identity <| List.map3 (\p0 p1 p2 -> patEqual p0 p1 && patEqual p1 p2) pats1 pats2 pats3

mergeWS: WS -> WS -> WS -> WS -- No advanced strategy. No synthesis pushes concurrent changes in whitespace, unless we allow editing of vclosures in the output
mergeWS o e1 e2 = if o.val == e1.val then e2 else e1

mergeStringHeuristic: String -> String -> String -> String
mergeStringHeuristic o s1 s2 =
  let original = Regex.split Regex.All splitRegex o in
  let modified1 = Regex.split Regex.All splitRegex s1 in
  let modified2 = Regex.split Regex.All splitRegex s2 in
  mergeListWithDiffs identity (\ori ss1 ss2 -> if ss1 == ori then ss2 else ss1) original modified1 modified2
  |> String.join ""

mergeInfo: (a -> a -> a -> a) -> WithInfo a ->WithInfo a -> WithInfo a -> WithInfo a
mergeInfo merger w1 w2 w3 = Info.replaceInfo w1 (merger w1.val w2.val w3.val)

mergeTupleDiffs: (a -> a -> a) -> TupleDiffs a -> TupleDiffs a -> TupleDiffs a
mergeTupleDiffs submerger l1_ l2_ =
  let aux i revAcc l1 l2 =
    case (l1, l2) of
       ([], _) -> List.reverse revAcc ++ l2
       (_, []) -> List.reverse revAcc ++ l1
       ((j, m1)::t1, (k, m2)::t2) ->
        if j > i && k > i then
          aux (min j k) revAcc l1 l2
        else if j == i && k == i then
          aux (j + 1) ((i, submerger m1 m2)::revAcc) t1 t2
        else if j == i then
          aux (j + 1) ((j, m1)::revAcc) t1 l2
        else if k == i then
          aux (k + 1) ((k, m2)::revAcc) l1 t2
        else
          Debug.crash <| "Malformed tuple diffs:" ++ toString l1_ ++ ", " ++ toString l2_
  in aux 0 [] l1_ l2_

mergeListDiffs: (a -> a -> a) -> ListDiffs a -> ListDiffs a -> ListDiffs a
mergeListDiffs submerger l1_ l2_ =
  let aux i revAcc l1 l2 =
       case (l1, l2) of
         ([], _) -> List.reverse revAcc ++ l2
         (_, []) -> List.reverse revAcc ++ l1
         ((j, m1)::t1, (k, m2)::t2) ->
          if j > i && k > i then
            aux (min j k) revAcc l1 l2
          else if j == i && k == i then
            case (m1, m2) of
              (ListElemUpdate md1, ListElemUpdate md2) ->
                 aux (j + 1) ((i, ListElemUpdate (submerger md1 md2))::revAcc) t1 t2
              (ListElemDelete c1, ListElemDelete c2) ->
                if c1 == c2 then
                  aux (i + c1) ((i, ListElemDelete (min c1 c2))::revAcc) t1 t2
                else if c1 < c2 then
                  aux (i + c1) ((i, ListElemDelete (min c1 c2))::revAcc) t1 ((i, ListElemDelete <| c2 - c1)::t2)
                else
                  aux (i + c1) ((i, ListElemDelete (min c1 c2))::revAcc) ((i, ListElemDelete <| c1 - c2)::t1) t2
              _ -> Debug.crash <| "Inconsistent modifications to merge: " ++ toString m1 ++ ", " ++ toString m2
          else if j == i then
            aux (j + 1) ((j, m1)::revAcc) t1 l2
          else if k == i then
            aux (k + 1) ((k, m2)::revAcc) l1 t2
          else
            Debug.crash <| "Malformed list diffs:" ++ toString l1_ ++ ", " ++ toString l2_
  in aux 0 [] l1_ l2_

mergeExp: Exp -> Exp -> EDiffs -> Exp -> EDiffs -> (Exp, EDiffs)
mergeExp o e1 ediff1 e2 ediff2 =
  --let _ = Debug.log ("mergeExp "  ++ Syntax.unparser Syntax.Leo o ++ " " ++ Syntax.unparser Syntax.Leo e1 ++ " " ++ toString ediff1 ++
  --   " " ++ Syntax.unparser Syntax.Leo e2 ++ " " ++ toString ediff2) () in
  case (ediff1, ediff2) of
    (EConstDiffs EAnyDiffs, EConstDiffs EOnlyWhitespaceDiffs) ->
      (e1, ediff1)
    (_, EConstDiffs _) ->
      (e2, ediff2)
    (EConstDiffs _, _) ->
       (e2, ediff2)
    (EStringDiffs ss1, EStringDiffs ss2) ->
      case ((unwrapExp o), eStrUnapply e1, eStrUnapply e2) of
        (EBase sp0 (EString quote x), Just s1, Just s2) ->
          let (finalStr, finalDiffs) = mergeString x s1 ss1 s2 ss2 in
          (replaceE__ o <| EBase sp0 <| EString quote finalStr, EStringDiffs finalDiffs)
        _ -> (e2, ediff2)
    (EListDiffs ld1, EListDiffs ld2) ->
      case ((unwrapExp o), eListUnapplyWS e1, eListUnapplyWS e2) of
        (EList sp0 x sp1 Nothing sp2, Just l1, Just l2) ->
           let (finalelems, finaldiff) = mergeList (\(wso, eo) (ws1, e1) d1 (ws2, e2) d2 ->
             let (finale, finald) = mergeExp eo e1 d1 e2 d2 in
             ((wso, finale), finald)) x l1 ld1 l2 ld2
           in
           (replaceE__ o <| EList sp0 finalelems sp1 Nothing sp2, EListDiffs finaldiff)
        _ -> Debug.crash <| "unexpected EListDiffs without lists: " ++ Syntax.unparser Syntax.Leo o ++ ", " ++
             Syntax.unparser Syntax.Leo e1 ++ ", " ++ Syntax.unparser Syntax.Leo e2
    (EChildDiffs cd1, EChildDiffs cd2) ->
      case (childExpsExtractors o, childExpsExtractors e1, childExpsExtractors e2) of
        ((os, oBuild), (e1s, e1sBuild), (e2s, e2sBuild)) ->
          let (finalelems, finalDiffs) = mergeTuple mergeExp os e1s cd1 e2s cd2 in
          (oBuild finalelems, EChildDiffs finalDiffs)
    _ -> (e2, ediff2)

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

type StringMerge  = DoLeft Int Int Int (List StringDiffs)
                  | DoRight Int Int Int (List StringDiffs)
                  | DoMerge Int Int Int (List StringDiffs) Int Int Int (List StringDiffs)

mergeString: String -> String -> List StringDiffs -> String -> List StringDiffs -> (String, List StringDiffs)
mergeString original modified1 diffs1 modified2 diffs2 =
  let aux: Int -> Int -> Int -> List StringDiffs -> List StringDiffs -> (String, List StringDiffs) -> (String, List StringDiffs)
      aux originalOffset modified1Offset modified2Offset diffs1 diffs2 (str, revAccDiffs) =
       case (diffs1, diffs2) of
    ([], []) -> (str, List.reverse revAccDiffs)
    _ -> let mergeWay = case (diffs1, diffs2) of
            ([], []) -> Debug.crash "impossible"
            ([], StringUpdate start end replaced :: tail) -> DoRight start end replaced tail
            (StringUpdate start end replaced :: tail, []) -> DoLeft start end replaced tail
            (StringUpdate start1 end1 replaced1 :: tail1, StringUpdate start2 end2 replaced2 :: tail2) ->
              if end1 <= start2 then DoLeft start1 end1 replaced1 tail1
              else if end2 <= start1 then DoRight start2 end2 replaced2 tail2
              else DoMerge start1 end1 replaced1 tail1 start2 end2 replaced2 tail2
         in
         case mergeWay of
           DoLeft start1 end1 replaced1 tail1 ->
             let insertedString = String.slice (start1 + modified1Offset) (start1 + modified1Offset + replaced1) modified1 in
             let offsetIncrease1 = replaced1 - (end1 - start1) in
             (String.left (start1 + originalOffset) str ++ insertedString ++ String.dropLeft (end1 + originalOffset) str,
              StringUpdate start1 end1 replaced1 :: revAccDiffs) |>
             aux (originalOffset + offsetIncrease1) (modified1Offset + offsetIncrease1) modified2Offset tail1 diffs2
           DoRight start2 end2 replaced2 tail2 ->
             let insertedString = String.slice (start2 + modified2Offset) (start2 + modified2Offset + replaced2) modified2 in
             let offsetIncrease2 = replaced2 - (end2 - start2) in
             (String.left (start2 + originalOffset) str ++ insertedString ++ String.dropLeft (end2 + originalOffset) str,
              StringUpdate start2 end2 replaced2 :: revAccDiffs) |>
             aux (originalOffset + offsetIncrease2) modified1Offset (modified2Offset + offsetIncrease2) diffs1 tail2
           DoMerge start1 end1 replaced1 tail1 start2 end2 replaced2 tail2 ->
             -- Here the second takes predecence.
             -- start2 start1 end2 end1  or start2 start1 end1 end2
             -- start1 start2 end1 end2  or start1 start2 end2 end1
             let offsetIncrease1 = replaced1 - (end1 - start1) in
             (str, revAccDiffs) |>
             aux originalOffset  (modified1Offset + offsetIncrease1) modified2Offset tail1 diffs2
  in aux 0 0 0 diffs1 diffs2 (original, [])

-- Guarantees that
-- * If updated lists have equal size, elements will be merged aligned.
-- * A list which was not modified makes that the other lists replaces the original list.
-- Would be better to have a real diffing algorithm.
mergeList: (a -> a -> vDiffs -> a -> vDiffs -> (a, vDiffs)) -> List a -> List a -> ListDiffs  vDiffs-> List a -> ListDiffs vDiffs -> (List a, ListDiffs vDiffs)
mergeList submerger =
  let aux: Int -> (List a,    ListDiffs vDiffs) -> List a -> List a -> ListDiffs vDiffs -> List a -> ListDiffs vDiffs -> (List a, ListDiffs vDiffs)
      aux  i      (accMerged, accDiffs)                    originals modified1 modifs1                      modified2 modifs2 =
       case (originals, modifs1, modifs2) of
         (_, [], []) -> (List.reverse accMerged, (List.reverse accDiffs))
         (_, [], _) -> (List.reverse accMerged ++ modified2, (List.reverse accDiffs ++ modifs2))
         (_, _,  []) -> (List.reverse accMerged ++ modified1, (List.reverse accDiffs ++ modifs1))
         ([], (i1, m1)::t1, (i2, m2)::t2) ->
           if i1 /= i || i2 /= i then
             Debug.crash <| "Expected terminal modifications to only occur at the end of the list, got " ++ toString (i, i1, i2)
           else
             case (m1, m2) of
               (ListElemInsert count1, ListElemInsert count2) ->
                 let (hdModified1, tlModified1) = Utils.split count1 modified1 in
                 let (hdModified2, tlModified2) = Utils.split count2 modified2 in
                 aux i (accMerged |> reverseInsert hdModified1 |> reverseInsert hdModified2, (i, ListElemInsert (count1 + count2))::accDiffs) originals tlModified1 t1 tlModified2 t2
               _ -> Debug.crash <| "Expected two insertions at the end of the list, got " ++ toString (m1, m2)
         (oh::ot, (i1, m1)::t1, (i2, m2)::t2) ->
           if i1 == i && i2 == i then -- Edition conflict
              case (m1, m2) of
                (ListElemUpdate mu1, ListElemUpdate mu2) ->
                  case (modified1, modified2) of
                    (hdModified1::tlModified1, hdModified2::tlModified2) ->
                      let (newHd, newDiffs) = submerger oh hdModified1 mu1 hdModified2 mu2 in
                      aux (i + 1) (newHd::accMerged, (i, ListElemUpdate newDiffs)::accDiffs) ot tlModified1 t1 tlModified2 t2
                    _ -> Debug.crash "Expected non-empty modifications since they were updates"
                (ListElemInsert count1, ListElemInsert count2) ->
                  let (hdModified1, tlModified1) = Utils.split count1 modified1 in
                  let (hdModified2, tlModified2) = Utils.split count2 modified2 in
                  aux i (accMerged |> reverseInsert hdModified1 |> reverseInsert hdModified2, (i, ListElemInsert (count1 + count2))::accDiffs) originals tlModified1 t1 tlModified2 t2
                (ListElemDelete count1, ListElemDelete count2) ->
                  if count1 == count2 then
                    aux i (accMerged, (i, m1)::accDiffs) originals modified1 t1 modified2 t2
                  else if count1 < count2 then
                    aux i (accMerged, accDiffs) originals modified1 modifs1 modified2 ((i, ListElemDelete count1)::(i + count1, ListElemDelete (count2 - count1))::t2)
                  else
                    aux i (accMerged, accDiffs) originals modified1 ((i, ListElemDelete count2)::(i + count2, ListElemDelete (count1 - count2))::t1) modified2 modifs2
                (_, ListElemInsert count2) ->
                  -- The insertion happens before.
                  let (inserted, modified2Tail) = Utils.split count2 modified2 in
                  aux i (reverseInsert inserted accMerged, (i, m2)::accDiffs) originals modified1 modifs2 modified2Tail t2
                (ListElemInsert count1, _) ->
                  -- The insertion happens before.
                  let (inserted, modified1Tail) = Utils.split count1 modified1 in
                  aux i (reverseInsert inserted accMerged, (i, m1)::accDiffs) originals modified1Tail t1 modified2 modifs2
                (ListElemUpdate mu1, ListElemDelete count2) ->
                  if count2 == 1 then -- Just delete it.
                    let originalsTail = List.drop 1 originals in
                    aux (i + 1) (accMerged, (i, m2)::accDiffs) originalsTail (List.drop 1 modified1) t1 modified2 t2
                  else if count2 == 0 then Debug.crash "Unexpected 0 here"
                  else
                    aux i (accMerged, accDiffs) originals modified1 modifs1 modified2 ((i, ListElemDelete 1) :: (i, ListElemDelete 1) :: t2)
                (ListElemDelete count1, ListElemUpdate mu2) ->
                  if count1 == 1 then -- Just delete it.
                    let originalsTail = List.drop 1 originals in
                    aux (i + 1) (accMerged, (i, m1)::accDiffs) originalsTail modified1 t1 (List.drop 1 modified2) t2
                  else if count1 == 0 then Debug.crash "Unexpected 0 here"
                  else
                    aux i (accMerged, accDiffs) originals modified1 ((i, ListElemDelete 1) :: (i, ListElemDelete 1) :: t1) modified2 modifs2

           else if i2 == i then
              case m2 of
                ListElemInsert count ->
                  let (inserted, modified2Tail) = Utils.split count modified2 in
                  aux i (reverseInsert inserted accMerged, (i, m2)::accDiffs) originals modified1 modifs2 modified2Tail t2
                ListElemDelete count -> --  Check that we are not taking over modif2's modifications.
                  let maxCount = i1 - i2 in
                  if count <= maxCount then
                    let originalsTail = List.drop count originals in
                    aux (i + count) (accMerged, (i, m2)::accDiffs) originalsTail modified1 modifs1 modified2 t2
                  else
                    aux i (accMerged, accDiffs) originals modified1 modifs1 modified2 ((i, ListElemDelete maxCount) :: (i + maxCount, ListElemDelete (count - maxCount)) :: t2)
                ListElemUpdate defaultVDiffs ->
                  case modified2 of
                    hdModified2::tlModified2 ->
                      aux (i + 1) (hdModified2::accMerged, (i, m2)::accDiffs) ot (List.drop 1 modified1) modifs1 tlModified2 t2
                    _ -> Debug.crash "empty modified although it said it was updated"
           else if i1 == i then
             case m1 of
               ListElemInsert count ->
                 let (inserted, modified1Tail) = Utils.split count modified1 in
                 aux i (reverseInsert inserted accMerged, (i, m1)::accDiffs) originals modified1Tail t1 modified2 modifs2
               ListElemDelete count -> --  Check that we are not taking over modif2's modifications.
                 let maxCount = i2 - i1 in
                 if count <= maxCount then
                   let originalsTail = List.drop count originals in
                   aux (i + count) (accMerged, (i, m1)::accDiffs) originalsTail modified1 t1 modified2 modifs2
                 else
                   aux i (accMerged, accDiffs) originals modified1 ((i, ListElemDelete maxCount) :: (i + maxCount, ListElemDelete (count - maxCount)) :: t1) modified2 modifs2
               ListElemUpdate defaultVDiffs ->
                 case modified1 of
                   hdModified1::tlModified1 ->
                     aux (i + 1) (hdModified1::accMerged, (i, m1)::accDiffs) ot tlModified1 t1 (List.drop 1 modified2) modifs2
                   _ -> Debug.crash "empty modified although it said it was updated"
           else
             let untouched = min (i2 - i) (i1 - i) in
             let (originalUntouched, originalRemaining) = Utils.split untouched originals in
             aux (i+untouched) (reverseInsert originalUntouched accMerged, accDiffs) originalRemaining (List.drop untouched modified1) modifs1 (List.drop untouched modified2) modifs2
  in aux 0 ([], [])

mergeDict: (v -> v -> VDiffs -> v -> VDiffs -> (v, VDiffs)) -> Dict k v -> Dict k v -> Dict k VDictElemDiff -> Dict k v -> Dict k VDictElemDiff -> (Dict k v, Dict k VDictElemDiff)
mergeDict submerger originalDict modified1Dict modifs1 modified2Dict modifs2 =
  let get0 name = Dict.get name originalDict  |> Utils.fromJust "mergeDict0" in
  let get1 name = Dict.get name modified1Dict |> Utils.fromJust "mergeDict1" in
  let get2 name = Dict.get name modified2Dict |> Utils.fromJust "mergeDict2" in
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
  let get0 name = Dict.get name originalDict  |> Utils.fromJust "mergeDict0" in
  let get1 name = Dict.get name modified1Dict |> Utils.fromJust "mergeDict1" in
  let get2 name = Dict.get name modified2Dict |> Utils.fromJust "mergeDict2" in
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

affinityArray = Array.fromList <| [
                 Array.fromList [19, 9, 6, 11, 14]
               , Array.fromList [5, 0, 3, 12, 16]
               , Array.fromList [1, 18, 24, 21, 22]
               , Array.fromList [8, 2, 17, 7, 20]
               , Array.fromList [15, 4, 23, 10, 13]]

classOf c =
  if Char.isDigit c then 0
  else if c == '.' then 1
  else if Char.isLower c || Char.isUpper c || c == '_' then 2
  else if not (isSpace c) then 3
  else 4

affinityChar c1 c2 =
  Array.get (classOf c1) affinityArray |> Maybe.andThen (\row ->
    Array.get (classOf c2) row
  ) |> Maybe.withDefault 0

-- The affinity between a string and an empty string should be the greatest
affinity s1 s2 =
   case String.uncons (String.slice (-1) (String.length s1) s1) of
     Just (s1Last, _) ->
       case String.uncons s2 of
         Just (s2First, _) -> affinityChar s1Last s2First
         Nothing -> 25
     Nothing -> 25

-- The list of strings must be non-empty
reverseStringConcatenationMultiple: List String -> String -> List StringDiffs -> Results String (List String, List (List StringDiffs))
reverseStringConcatenationMultiple strings newOut strDiffs =
  case strings of
    [] -> ok1 ([], [])
    [a] -> ok1 ([newOut], [strDiffs])
    head::tail ->
      reverseStringConcatenation head (String.join "" tail) newOut strDiffs
      |> Results.andThen ((\tail (newHead, headDiffs, newTailStr, tailStrDiffs) ->
        reverseStringConcatenationMultiple tail newTailStr tailStrDiffs
        |> Results.map ((\newHead headDiffs (newTailVals, tailDiffs) ->
            (newHead::newTailVals, headDiffs::tailDiffs)
          ) newHead headDiffs)
        ) tail)

reverseStringConcatenation: String -> String -> String -> List StringDiffs -> Results String (String, List StringDiffs, String, List StringDiffs)
reverseStringConcatenation sa sb newOut strDiffs =
  let saLength = String.length sa in
  let aux: Int -> List StringDiffs -> List StringDiffs -> Results String (String, List StringDiffs, String, List StringDiffs)
      aux  offset revForSa            diffs               =
        --let _ = ImpureGoodies.log <| "offset=" ++ toString offset ++ ", revForSa=" ++ toString revForSa ++", diffs=" ++ toString diffs in
        case diffs of
        [] ->
          let indexCut = offset + saLength in
          let newSa = String.left     indexCut newOut in
          let newSb = String.dropLeft indexCut newOut in
          ok1 (newSa, List.reverse revForSa, newSb, [])
        ((StringUpdate start end replacement) as su) :: diffsTail ->
          if start < saLength && end > saLength then -- We need to split the diffs, it cannot encompass two positions
            [aux offset revForSa (StringUpdate start saLength 0 ::StringUpdate saLength end replacement ::diffsTail),
             aux offset revForSa (StringUpdate start saLength replacement ::StringUpdate saLength end 0 ::diffsTail)] |>
               Results.projOk |> Results.andThen (LazyList.fromList >> Ok)
          else if start > saLength || start == saLength && end > start then -- The diff happens to the right of saLength
            let indexCutNew = saLength + offset in
            let newSa = String.left     indexCutNew newOut in
            let newSb = String.dropLeft indexCutNew newOut in
            ok1 (newSa, List.reverse revForSa, newSb, offsetStr (0 - saLength) diffs)
           else if start == saLength && end == saLength then -- Ambiguity here. We return both solutions with some preferences
            let aLeft = String.slice (max (start - 1) 0) start sa in
            let bRight = String.slice (end - saLength) (end - saLength + 1) sb in
            let inserted = String.slice (start + offset) (start + offset + replacement) newOut in
            let replacements = [
               (offset + start + replacement, List.reverse (su :: revForSa), offsetStr (0 - saLength) diffsTail),
               (offset + start, List.reverse revForSa, offsetStr (0 - saLength) (su :: diffsTail))
               ]
            in
            let orderedReplacementst =
                 if affinity aLeft inserted >= affinity inserted bRight || end - start >= 1 && end <= saLength then
                  oks replacements
                 else
                  oks  <| List.reverse replacements
            in
            orderedReplacementst |> Results.andThen (\(indexCut, forSa, forSb) ->
              let newSa = String.left     indexCut newOut in
              let newSb = String.dropLeft indexCut newOut in
              ok1 (newSa, forSa, newSb, forSb))
           else -- end < saLength
              aux (offset - (end - start) + replacement) (su::revForSa) diffsTail
  in aux 0 [] strDiffs