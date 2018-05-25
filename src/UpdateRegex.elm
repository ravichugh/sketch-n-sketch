module UpdateRegex exposing (
    replaceFirstByIn
  , replaceAllByIn
  , evalRegexExtractFirstIn
  , updateRegexExtractFirstIn
  , join
  , escapeSlashDollar
  , unescapeSlashDollar
  , nth
  , updateReplace
  , allInterleavingsIn)

import UpdateStack exposing (UpdateStack(..), Output)
import Results exposing
  ( Results
  , ok1, oks, okLazy )
import LazyList exposing (LazyList(..))
import Lang exposing (..)
import Regex exposing (..)
import Lazy
import Utils
import ImpureGoodies
import GroupStartMap
import Syntax
import Dict
import UpdateUtils exposing (..)
import LangUtils exposing (valToString)
import UpdateStack exposing (..)
import UpdatedEnv exposing (UpdatedEnv)
import ValBuilder as Vb
import ValUnbuilder as Vu
--import LangTools exposing (valToString)

join: Val
join = let
    substring = String.slice
    take = String.left
    drop = String.dropLeft
    length = String.length
    preferStringInsertionToLeft_ s1 inserted s2 = affinity s1 inserted > affinity inserted s2
  in
  builtinVal "UpdateRegex.join" <| VFun "__joinEmpty__" ["list"] (\args ->
    case args of
      [list] -> case list.v_ of
        VList exps ->
          let strExps = exps |> List.map (\v -> case v.v_ of
            VBase (VString s) -> Ok s
            _ -> Err <| "join expects a list of strings, got " ++ valToString v) |> Utils.projOk
          in
          flip Result.map strExps <| \strList ->
            (replaceV_ list <| VBase (VString (String.join "" strList)), [])
        _ -> Err <| "join Expectes a list of strings, got " ++ valToString list
      _ -> Err <| "join expects two arguments, got " ++ toString (List.length args)
  ) <| Just (\args oldVal newVal diffs ->
    case args of
      [list] -> case (list.v_, oldVal.v_, newVal.v_, diffs) of
        (VList exps, VBase (VString oldOutput), VBase (VString newOutput), VStringDiffs l) ->
           let strExps = exps |> List.map (\v -> case v.v_ of
             VBase (VString s) -> Ok s
             _ -> Err <| "join expects a list of strings, got " ++ valToString v) |>
               Utils.projOk in
           strExps |> Results.fromResult |> Results.andThen (\input ->
             let lastIndex = List.length input - 1 in
             -- startHead is the real ending position in the oldOutput of the last element considered, hence the start position of the head of input.
             -- Since the first element might be modified recursively, the length of head might be affected, but the end position should still be the original one.
             -- Hence we need to offset startHead + length head by deltaLengthHeadInput to compute endHead.
             let gather:Int ->     List String -> Int ->  Int ->    Int ->               Int ->       List StringDiffs -> Results String (List String, ListDiffs VDiffs)
                 gather lastIndexDeleted input indexInput startHead deltaLengthHeadInput offsetOutput diffs =
               --let _ = Debug.log ("gather@" ++ ":"  ++ toString lastIndexDeleted ++ " " ++ toString input++ " " ++ toString indexInput++ " " ++ toString startHead++ " " ++ toString deltaLengthHeadInput++ " " ++ toString offsetOutput++ " " ++ toString diffs) [] in
               --(\x -> let _ = Debug.log ("gather" ++ ":" ++ " " ++ toString lastIndexDeleted++ " " ++ toString input++ " " ++ toString indexInput++ " " ++ toString startHead++ " " ++ toString deltaLengthHeadInput++ " " ++ toString offsetOutput++ " " ++ toString diffs) (Results.toList x) in x) <|
               case diffs of
               [] -> ok1 (input, [])
               ((StringUpdate start end replaced) :: diffTail) ->
                  case input of
                   [] -> -- We do not allow insertion of new content here
                     let inserted = substring (start + offsetOutput) (end + offsetOutput + replaced) newOutput in
                     ok1 ([inserted], [(indexInput, ListElemInsert 1)])
                   (head::tail) ->
                     let endHead = startHead + length head + deltaLengthHeadInput in
                     --We assume that start >= startHead (invariant)
                     if start < endHead && end > endHead then
                       let firstReplaced =
                         gather lastIndexDeleted input indexInput startHead deltaLengthHeadInput offsetOutput ((StringUpdate start endHead 0)::(StringUpdate endHead end replaced)::diffTail)
                       in
                       if replaced == 0 then firstReplaced else firstReplaced |> Results.andElse (
                         gather lastIndexDeleted input indexInput startHead deltaLengthHeadInput offsetOutput ((StringUpdate start endHead replaced)::(StringUpdate endHead end 0)::diffTail)
                       )
                     else
                       -- Now either start >= endHead || end <= endHead
                       -- Here we postpone the diff to afterwards if not the end and the diff happens strictly after
                     if (start > endHead || start == endHead && end > start) && indexInput /= lastIndex then
                       gather lastIndexDeleted tail (indexInput + 1) endHead 0 offsetOutput diffs |> Results.andThen (\(tail, diffTail) ->
                         ok1 (head::tail, diffTail)
                       )
                     else
                       -- Now: (start >= endHead || end <= endHead) && start <= endHead && (start < endHead || (end == start && indexInput == lastIndex))
                     if start == endHead && end == start then
                       --let _ = Debug.log "Insertion at the end of the current input" () in
                       -- Now: end == endHead && start == endHead && indexInput != lastIndex
                       let inserted = substring (start + offsetOutput) (end + offsetOutput + replaced) newOutput in
                       let sa = substring 0 start oldOutput in
                       let sb = drop end oldOutput in
                       let newHead = head ++ inserted in
                       let newOffsetOutput = offsetOutput + replaced - (end - start) in

                       let appendNow = gather -1 tail (indexInput + 1) endHead 0 newOffsetOutput diffTail |>
                         Results.andThen (\(newTail, newDiffTail) ->
                         ok1 (newHead :: newTail, (indexInput, ListElemUpdate (VStringDiffs [StringUpdate (start - startHead) (end - startHead) replaced])) :: newDiffTail)
                         )
                       in
                       --let _ = Debug.log "indexInput == lastIndex" (indexInput == lastIndex) in
                       if indexInput == lastIndex then
                         appendNow
                       else
                        let appendLater = gather -1 tail (indexInput + 1) endHead 0 offsetOutput diffs |>
                             Results.andThen (\(newTail, newDiffTail) ->
                             ok1 (head::newTail, newDiffTail)
                           )
                        in
                       if preferStringInsertionToLeft_ sa inserted sb
                       then appendNow |> Results.andElse appendLater
                       else appendLater |> Results.andElse appendNow
                     else
                       let offsetChange = replaced - (end - start) in
                       let newOffsetOutput = offsetOutput + offsetChange in
                       if start == startHead && end == endHead && replaced == 0 then
                       -- If the entire string was deleted, one option is to delete the element alltogether. We display only this option if chars from the left and from the right were deleted as well.
                       let resultsWithDelete = gather end tail (indexInput + 1) endHead 0 newOffsetOutput diffTail |> Results.andThen (\(newTail, newDiffTail) ->
                         ok1 (newTail, (indexInput, ListElemDelete 1)::newDiffTail)
                         )
                       in
                       let deleteAnyway = lastIndexDeleted == start && (case diffTail of
                         ((StringUpdate start2 end2 0) :: _) -> start2 == endHead && end2 > start2
                         _ -> indexInput == lastIndex )
                       in
                       let resultsWithEmptyString = gather end tail (indexInput + 1) endHead 0 newOffsetOutput diffTail |> Results.andThen (\(newTail, newDiffTail) ->
                         ok1 ("" :: newTail, (indexInput, ListElemUpdate (VStringDiffs [StringUpdate 0 (endHead - startHead) 0]))::newDiffTail)
                       )
                       in
                       if deleteAnyway then
                         resultsWithDelete
                       else
                         resultsWithDelete |> Results.andElse resultsWithEmptyString
                     else -- start
                       let inserted = substring (start + offsetOutput) (start + offsetOutput + replaced) newOutput in
                       let newHead = substring 0 (start - startHead - deltaLengthHeadInput) head ++
                         inserted ++ drop (end - startHead - deltaLengthHeadInput) head in
                       let newDeltaLengthHeadInput = deltaLengthHeadInput - offsetChange in
                       let thisDiff = StringUpdate (start - startHead) (end - startHead) replaced in
                       gather (if replaced == 0 then end else -1) (newHead::tail) indexInput startHead newDeltaLengthHeadInput newOffsetOutput diffTail |> Results.andThen (\(newList, newDiffTail) ->
                         let finalDiffs =
                           case newDiffTail of
                             ((i, ListElemUpdate (VStringDiffs l))::tail) ->
                               if i == indexInput then
                                 (i, ListElemUpdate (VStringDiffs (thisDiff::l)))::tail
                               else
                                  (indexInput, ListElemUpdate (VStringDiffs [thisDiff]))::newDiffTail
                             _ -> (indexInput, ListElemUpdate (VStringDiffs [thisDiff]))::newDiffTail
                         in
                         ok1 (newList, finalDiffs)
                       )
             in
             gather -1 input 0 0 0 0 l |> Results.map (\(value, strdiffs) ->
               ([value |> List.map (replaceV_ oldVal << VBase << VString) |> VList |> replaceV_ list]
               , if strdiffs == [] then [] else [(0, VListDiffs strdiffs)])
             )
           )
        _ -> Err <| "join expects a list of strings, got " ++ valToString list ++ " updated with " ++ valToString newVal ++ " and " ++ toString diffs
      _ -> Err <| "join expects two arguments, got " ++ toString (List.length args)
  )


nth = builtinVal "UpdateRegex.nth" <| VFun "nth" ["list" ,"n"] (\args ->
    case args of
      [list, nv] -> case (list.v_, nv.v_) of
        (VList exps, VConst _ (n, _))->
          Utils.nth exps (floor n) |> Result.map (\v -> (v, []))
        _ -> Err <| "nth Expected a list and an integer, got " ++ valToString list ++ " " ++ valToString nv
      _ -> Err <| "nth expects two arguments, got " ++ toString (List.length args)
  ) <| Just <| \args oldVal newVal diffs ->
    case args of
      [list, nv] -> case (list.v_, nv.v_) of
        (VList exps, VConst _ (n, _)) ->
          let nInt: Int
              nInt = floor n
          in
          ok1 ([replaceV_ list <| VList <| List.take nInt exps ++ [newVal] ++ List.drop (nInt + 1) exps],
              [(0, VListDiffs [(nInt, ListElemUpdate diffs)])])
        _ -> Err <| "nth Expected a list and an integer, got " ++ valToString list ++ " " ++ valToString nv
      _ -> Err <| "nth expects two arguments, got " ++ toString (List.length args)

replaceDollarOrSlash= builtinVal "UpdateRegex.replaceDollarOrSlash" <| VFun "replaceDollarOrSlash" ["m"] (\margs ->
  case margs of
    [m] -> case m.v_ of
      VRecord dm ->
        case Dict.get "match" dm |> Maybe.andThen vStrUnapply of
          Just "\\$" -> Ok (replaceV_ m <| VBase <| VString "$", [])
          _ -> Ok (replaceV_ m <| VBase <| VString "\\", [])
      _ -> Err <| "replaceDollarOrSlash expected a record, got " ++ valToString m
    _ -> Err <| "replaceDollarOrSlash expected 1 argument, got " ++ toString (List.length margs)
  ) <| Just <| \margs oldval newval diffs ->
  case margs of
    [m] -> case (m.v_, oldval.v_, newval.v_, diffs) of
      (VRecord dm, VBase (VString oldStr), VBase (VString newStr), VStringDiffs ldiffs) -> -- Because the output only had one char, there are only three differences possibles
        if ldiffs == [] then ok1 (margs, [])
        else
        let aux: Int -> Int -> List StringDiffs -> (String, List StringDiffs) -> (String, List StringDiffs)
            aux offset lastEnd ldiffs (strAcc, revAcc) = case ldiffs of
          [] -> (strAcc ++ String.dropLeft lastEnd oldStr, List.reverse revAcc)
          StringUpdate a b r :: tail ->
             let stringReplaced = String.slice (a + offset) (a + r + offset) newStr in
             let stringReplacedEscaped = escapeSlashDollar stringReplaced in
             let newR = String.length stringReplacedEscaped in
             (strAcc ++ String.slice lastEnd a oldStr ++ stringReplacedEscaped, StringUpdate (if a == 0 then 0 else 2) (if b == 0 then 0 else 2) newR :: revAcc) |>
             aux (offset + b - a + r) b tail
        in
        let (newStr, newStrDiffs) = aux 0 0 ldiffs ("", []) in
        let newRecord = replaceV_ m <| VRecord (Dict.insert "match" (replaceV_ oldval <| VBase <| VString newStr) dm) in
        let newRecordDiff = VRecordDiffs <| Dict.fromList [("match", VStringDiffs newStrDiffs)] in
        ok1 ([newRecord], [(0, newRecordDiff)])
      _ -> Err <| "replaceDollarOrSlash update expected a record, a string and a stringdiffs, got " ++ valToString m ++ " " ++ valToString newval ++ " " ++ toString diffs
    _ -> Err <| "replaceDollarOrSlash expected 1 argument, got " ++ toString (List.length margs)

unescapeSlashDollar: (Env -> Exp -> Result String (Val, Widgets)) ->
                     (UpdateStack -> Results String (UpdatedEnv, UpdatedExp)) -> Val
unescapeSlashDollar eval update =
  let localEnv args = [("replace", replaceAllByIn eval update), ("replacement", replaceDollarOrSlash)] ++ List.map (\arg -> ("string", arg)) args in
  let localExp = (eApp (eVar "replace") [eStr "\\\\\\$|\\\\\\\\", eVar "replacement", eVar "string"]) in
  builtinVal "UpdateRegex.unescapeSlashDollar" <| VFun "unescapeSlashDollar" ["s"] (\args ->
    eval (localEnv args) localExp
  ) <| Just <| \args oldVal newVal diffs ->
    let prevEnv = localEnv args in
    update (updateContext "unescapeSlashDollar" prevEnv localExp [] oldVal newVal diffs) |> Results.andThen (\(newEnv, newExp) ->
      case newEnv.val of
        _::_::(_, newArg)::_ ->
          case newEnv.changes of
            (2, argChanges)::_ ->
                 ok1 ([newArg], [(0, argChanges)])
            _ -> ok1 ([newArg], [])
        l -> Err <| "The environment should have contained at least 3 variables, got " ++ toString (List.length newEnv.val)
    )

-- Performs replacements on a string with differences but also return those differences along with the old ones.
updateReplace eval update =  builtinVal "UpdateRegex.updateReplace" <| VFun "updateReplace" ["regex", "replacement", "string", "diffs"] (\margs ->
    case margs of
      [regexV, replacementV, stringV, stringDiffsV] ->
        case (regexV.v_, stringV.v_, valToVDiffs stringDiffsV) of
          (VBase (VString regex), VBase (VString string), Ok (VStringDiffs vdiffs)) ->
            let closure = case replacementV.v_ of
              VBase (VString replacement) -> stringToLambda eval update nth join (replaceV_ replacementV) replacement |> Tuple.first
              _ -> replacementV
            in
            let matches = GroupStartMap.find Regex.All regex string in
            let replacements = List.map (evalReplacement eval closure) matches in
            let (initStrings, lastString) = interleavingStrings string matches in
            List.map2 (\m evalResult ->
              case evalResult of
                Err msg -> Err msg
                Ok v -> case v.v_ of
                  VBase (VString mReplacement) ->
                    Ok (StringUpdate m.index (m.index + String.length m.match) (String.length mReplacement), mReplacement)
                  _ -> Err <| "the regex callback did not return a string, got " ++ valToString v
            ) matches replacements |> Utils.projOk |> (\x -> case Result.map List.unzip x of
              Err msg -> Err msg
              Ok (newStringDiffs, replacements) ->
                let finalStr = (List.map2 (\s t -> s ++ t) initStrings replacements |> String.join "") ++ lastString in
                -- offsetStart is the offset for the start of the first element of newStringDiffs
                -- offsetEnd is the offset for the end of the first element of newStringDiffs
                -- offsetEnd is the one that is usueally called "offset" and contains all the offsets before oldStringDiffs
                let finalStrDiffs =  composeStringDiffs vdiffs newStringDiffs in
                Ok ((Vb.tuple2 Vb.string vDiffsToVal (Vb.fromVal stringV) (finalStr, VStringDiffs <| finalStrDiffs)), [])
            )
          _ -> Err <| "updateReplace expected a regex, a replacement string/closure, a string, original diffs made to the string, got " ++ valToString regexV ++ " " ++ valToString replacementV ++ " " ++ valToString stringV ++ " " ++ valToString stringDiffsV
      _ -> Err <| "updateReplace expected 4 arguments, got " ++ toString (List.length margs)
  ) Nothing

escapeSlashDollar s = Regex.replace Regex.All (Regex.regex "\\$") (\_ -> "\\\\\\$") s

lambdaToString: List Int -> Val -> VDiffs -> Results String (List (Int, Int, String))
lambdaToString oldConcatenationStarts valAfter vdiffs =
  case (valAfter.v_, vdiffs)  of
    (VClosure Nothing [_] lambdaBody2 env2, VClosureDiffs _ Nothing) ->
      ok1 []
    (VClosure Nothing [_] lambdaBody2 env2, VClosureDiffs _ mbediffs) ->
      flip Results.andThen (unconcat lambdaBody2 mbediffs |> Results.fromResult) <| \(newConcatenation, listElemDiffs) ->
        let recoverSubExpressionStringDiffs e = ok1 [] in
        let recoverSubStringsDiffs e = case eAppUnapply1 e of
          Just (_, eStr) -> case eStrUnapply eStr of
            Just s -> Just (\sd -> case sd of
              EChildDiffs [(1, EStringDiffs l)] -> Just <| strDiffToConcreteDiff s l
              _ -> Nothing)
            _ -> Nothing
          _ -> Nothing
        in
        recoverStringDiffs recoverSubExpressionStringDiffs recoverSubStringsDiffs oldConcatenationStarts newConcatenation listElemDiffs
    _ -> Debug.crash "Trying to call lambdaToString with something else than a closure or closurediffs"

-- nth should be a reversible function of the type 'List a -> Int -> a'  and raises an error else.
stringToLambda: (Env -> Exp -> Result String (Val, Widgets)) ->
                (UpdateStack -> Results String (UpdatedEnv, UpdatedExp)) -> Val -> Val -> (Val_ -> Val) -> String -> (Val, List Int)
stringToLambda eval update nth join toVal s =
  let l = find All (regex "(\\\\\\$|\\\\\\\\|\\$(\\d))") s |> List.map (\m -> (m, m.index, String.length m.match + m.index)) in
  let (finalLastStart, lWithPrevStart) = List.foldl (\(mat, start, end) (lastEnd, acc) -> (end, acc ++ [(mat, lastEnd, start, end)])) (0, []) l in
  let oldConcatenationStarts = (0 :: (lWithPrevStart |> List.concatMap (\(_, _, start, end) -> [start, end]))) ++ [String.length s] in
  let m = eVar "m" in
  let tmp = List.concatMap (\(mat, lastEnd, start, end) ->
       let groupIndexMaybe = case mat.submatches of
         [_, Nothing] -> Nothing
         [_, Just submatch] ->
            case String.toInt submatch of
              Ok i -> Just i
              Err msg -> ImpureGoodies.throw <| EvaluationError <| mat.match ++ " is not a valid group match: " ++ msg
         _ -> ImpureGoodies.throw <| EvaluationError <| mat.match ++ " is not a valid group match"
       in
       case groupIndexMaybe of
         Nothing -> [] --It was just a regular escaped dollar, or an escaped backslash
         Just groupIndex ->
           [ eApp (eVar "unescapeSlashDollar") [eStr <| String.slice lastEnd start s],
             eApp (eVar "nth") [eSelect m "group", eConstDummyLoc <| toFloat groupIndex]
           ]
       ) lWithPrevStart
  in
  let lambdaBody = concat "join" (tmp ++ [eApp (eVar "unescapeSlashDollar") [eStr <| String.dropLeft finalLastStart s]]) in
  (toVal<| VClosure Nothing [pVar "m"] lambdaBody [
    ("nth", nth), ("join", join), ("unescapeSlashDollar", unescapeSlashDollar eval update)], oldConcatenationStarts)

type EvaluationError = EvaluationError String

-- API for this function
type alias RegexMatch = {match: String, submatches: List String, group: List String, start: List Int, index: Int, number: Int}

gsmMatchToRegexMatch: GroupStartMap.Match -> RegexMatch
gsmMatchToRegexMatch m =
  let submatches = m.submatches |> List.map (\{match} -> match |> Maybe.withDefault "") in
  {
  match = m.match,
  submatches = submatches,
  group = m.match :: submatches,
  start =  m.index :: (m.submatches |> List.map (\{start} -> start)),
  index = m.index,
  number = m.number
  }

matchToVal: RegexMatch -> Val
matchToVal m =
  let vb = builtinVal "matchToVal" in
  Vb.record Vb.identity vb <| Dict.fromList [
    ("match", Vb.string vb m.match),
    ("submatches", Vb.list Vb.string vb m.submatches),
    ("group",      Vb.list Vb.string vb m.group),
    ("start",      Vb.list Vb.int vb m.start),
    ("index",      Vb.int vb m.index),
    ("number",     Vb.int vb m.number)
  ]

-- Builds the expression consisting of calling the replacement callback to the argument
matchApp: String -> String -> Exp
matchApp replacementName argumentName =
  eOp ToStrExceptStr [ eApp (eVar replacementName) [eVar argumentName]]

-- Returns the name of the application in this match application
appMatchArg: Exp -> Result String String
appMatchArg e = case eOpUnapply1 ToStrExceptStr e of
   Nothing -> Err <| "Expected ToStrExceptStr, got " ++ Syntax.unparser Syntax.Elm e
   Just inside -> case eAppUnapply1 inside of
     Nothing  -> Err <| "In ToStrExceptStr, expected f x, got " ++ Syntax.unparser Syntax.Elm inside
     Just (r, a) -> case eVarUnapply a of
       Nothing -> Err <| "In ToStrExceptStr (f x), expected x to be a variable name, got " ++ Syntax.unparser Syntax.Elm a
       Just s -> Ok s

extractHeadTail: String -> List a -> ((a, List a) -> Result String e) -> Result String e
extractHeadTail msg list continuation =
   case list of
     [] -> Err msg
     head::tail -> continuation (head, tail)

valToMatch: Val -> (RegexMatch -> Results String b) -> Results String b
valToMatch v continuation =
  let try: Result String a -> (a -> Results String b) -> Results String b
      try a b = Results.andThen b (Results.fromResult a) in
  let get: Dict.Dict String a -> String -> (a -> Results String b) -> Results String b
      get d k = try (Dict.get k d |> Result.fromMaybe ("Key " ++ k ++ " not found")) in
  try (Vu.record Vu.identity v) <| \rd -> let g = get rd in
    g "match" <| \vmatch -> g "submatches" <| \vsubmatches -> g "group"  <| \vgroup ->
    g "start" <| \vstart    -> g "index"   <| \vindex ->      g "number" <| \vnumber ->
    try (Vu.string vmatch) <| \match ->
    try (Vu.int vindex) <| \index ->
    try (Vu.int vnumber) <| \number ->
    try (Vu.list Vu.string vsubmatches) <| \submatches ->
    try (Vu.list Vu.string vgroup) <| \groups ->
    try (Vu.list Vu.int vstart) <| \start ->
    continuation (RegexMatch match submatches groups start index number)

-- Reverse operation of matchToVal. Computes a list of transformations
recoverMatchedStringDiffs : RegexMatch -> Val -> (Maybe VDiffs) -> Results String (List (Int, Int, String)) {- The matched string -}
recoverMatchedStringDiffs  oldRegexMatch newV mbdiffs =
   let oldMatch      = oldRegexMatch.match in
   case mbdiffs of
      Nothing -> ok1 []
      Just vd ->
   case vd of
       VRecordDiffs d ->
         valToMatch newV <| \newRegexMatch ->
           let newMatch      = newRegexMatch.match
               newSubmatches = newRegexMatch.submatches
               newGroups     = newRegexMatch.group in
        case Dict.get "match" d of
          Just (VStringDiffs matchDiff) -> ok1 (strDiffToConcreteDiff newMatch matchDiff)
          Just ds -> Err <| "Expected a VStringDiffs for match, got " ++ toString ds
          Nothing ->
            case Dict.get "index" d of
              Just _ -> Err "Cannot update the .index of of a regex match, only the string itself !"
              Nothing ->
                case Dict.get "number" d of
                  Just _ -> Err "Cannot update the .number of of a regex match, only the string itself !"
                  Nothing ->
                    case Dict.get "start" d of
                      Just _ -> Err "Cannot update the group .start of a regex match, only the string itself !"
                      Nothing ->
                        let makeTupleStringDiffs oldStringList newStringList a =
                          (case a of
                             VListDiffs l -> Just l
                             _ -> Nothing
                          ) |>
                          Maybe.andThen toTupleDiffs |>
                          Result.fromMaybe "No insertion/deletion allowed in the .submatches of a regex match" |>
                          Result.andThen (\tuplediffs ->
                             List.map (\d -> case d of
                              (i, VStringDiffs l) -> Ok (i, l)
                              (i, VUnoptimizedDiffs) ->
                                case defaultStringDiffs (Utils.nth oldStringList i |> Utils.fromOk "UpdateRegex.nth1") (Utils.nth newStringList i |> Utils.fromOk "UpdateRegex.nth2") of
                                  Ok (LazyList.Cons (Just l) _) -> Ok (i, l)
                                  Ok (LazyList.Cons Nothing _) -> Ok (i, [])
                                  Err msg -> Err msg
                                  d -> Err <| "Expected vStringDiffs, got " ++ toString d
                              _ -> Err <| "Expected vStringDiffs, got " ++ toString d
                             ) tuplediffs |> Utils.projOk
                          ) |>
                          Results.fromResult  in
                        let finalGroupAndDiffs =
                             let submatchesTupleDiffs = case Dict.get "submatches" d of
                               Nothing -> Nothing
                               Just submatchesDiff ->
                                  makeTupleStringDiffs oldRegexMatch.submatches oldRegexMatch.submatches submatchesDiff |>
                                  Results.map (\gTupleDiff -> (newMatch::newRegexMatch.submatches, offset 1 gTupleDiff)) |>
                                  Just
                             in
                             let groupTupleDiffs = case Dict.get "group" d of
                               Nothing -> Nothing
                               Just groupDiff ->
                                  makeTupleStringDiffs oldRegexMatch.group oldRegexMatch.group groupDiff |>
                                  Results.map (\gTupleDiff -> (newRegexMatch.group, gTupleDiff)) |>
                                  Just
                             in
                             --let oldGroups = oldRegexMatch.group |> List.map (replaceV_ newV << VBase << VString) in
                             case (submatchesTupleDiffs, groupTupleDiffs) of
                               (Nothing, Nothing) -> ok1 (oldRegexMatch.group, [])
                               (Just x, Nothing) -> x
                               (Nothing, Just x) -> x
                               (Just x, Just y)  ->
                                  Results.map2 (\(newGroups1, newGroupDiffs1) (newGroups2, newGroupDiffs2) ->
                                      mergeTuple mergeString oldRegexMatch.group newGroups1 newGroupDiffs1 newGroups2 newGroupDiffs2
                                 ) x y
                        in
                        flip Results.andThen finalGroupAndDiffs <| \(finalGroups, finalGroupsTupleDiffs) ->
                          let diffByGroupIndex =  Dict.fromList finalGroupsTupleDiffs in
                          --let _ = Debug.log "oldRegexMatch: " (oldRegexMatch) in
                          List.map2 (\(group, i) groupSTart ->
                            case Dict.get i diffByGroupIndex of
                              Nothing ->
                                Ok []
                              Just manydiffs ->
                                let aux offset diffs revAcc=
                                  case diffs of
                                     [] -> List.reverse revAcc
                                     StringUpdate start end replaced :: diffsTail->
                                       (groupSTart + start, groupSTart + end, String.slice (start + offset) (start + replaced + offset) group)::revAcc |>
                                       aux (offset + replaced - (end - start)) diffsTail
                                in
                                Ok <| aux 0 manydiffs []
                            ) (Utils.zipWithIndex finalGroups) oldRegexMatch.start |>
                          Utils.projOk |> Result.map (\listlist ->
                            List.concatMap identity listlist
                          ) |> Results.fromResult
       _ -> Err <| "Expected VRecordDiffs, got " ++ toString vd

mergeTransformations: String -> List (Int, Int, String) -> String
mergeTransformations originalString replacements =
  let aux maxReplacementIndex string transformations = case transformations of
    [] -> string
    (start, end, newGroup)::remainingTransformations ->
       if start >= 0 && end <= maxReplacementIndex then
         let updatedString = (String.left start string) ++ newGroup ++ String.dropLeft end string in
         aux start updatedString remainingTransformations
       else
         aux maxReplacementIndex string remainingTransformations
  in aux (String.length originalString) originalString (List.sortBy (\(a, b, c) -> 0- a) replacements)


replaceAllByIn eval update = replaceByIn Regex.All "replaceAllByIn" eval update
replaceFirstByIn eval update = replaceByIn (Regex.AtMost 1) "replaceFirstByIn" eval update

replaceByIn: Regex.HowMany {- -> Val-} -> String ->
    (Env -> Exp -> Result String (Val, Widgets)) ->
    (UpdateStack -> Results String (UpdatedEnv, UpdatedExp)) -> Val
replaceByIn howmany {-stringjoin-} name evaluate update = builtinVal "UpdateRegex.replaceByIn" <| VFun name ["regex", "replacement", "string"] (
    \args ->
      case args of
        [regexpV, replacementV, stringV] ->
          evalRegexReplaceByIn howmany evaluate regexpV replacementV stringV
        _ -> Err <| "regex replacement expects three arguments, got " ++ toString (List.length args)
  ) (Just (\args oldVal newVal diffs ->
      case args of
        [regexpV, replacementV, stringV] ->
          updateRegexReplaceByIn howmany evaluate update regexpV replacementV stringV oldVal newVal diffs
        _ -> Err <| "regex replacement expects three arguments, got " ++ toString (List.length args)
  ))

evalReplacement eval closureReplacementV gsmMatch =
  let replacementName = "user_callback" in
  let argumentName = "x" in
  let matchVal = matchToVal (gsmMatchToRegexMatch gsmMatch) in
  let localEnv = [(replacementName, closureReplacementV), (argumentName, matchVal)] in
  let localExp = matchApp replacementName argumentName in
  --let _ = Debug.log ("The new env is" ++ LangUtils.envToString localEnv) () in
  --let _ = Debug.log ("The replacement body is" ++ Syntax.unparser Syntax.Elm localExp) () in
  eval localEnv localExp |> Result.map Tuple.first

evalRegexReplaceByIn: Regex.HowMany -> (Env -> Exp -> Result String (Val, Widgets))-> Val -> Val -> Val -> Result String (Val, Widgets)
evalRegexReplaceByIn  howmany eval regexpV replacementV stringV =
   case (regexpV.v_, replacementV.v_, stringV.v_) of
     (VBase (VString regexp), VBase (VString replacement), VBase (VString string)) ->
        let (lambdaReplacement, _) = stringToLambda eval dummyUpdate nth join (replaceV_ replacementV) replacement
        in evalRegexReplaceByIn howmany eval regexpV lambdaReplacement stringV
       -- Conver the string to a lambda with string concatenation
     (VBase (VString regexp), _, VBase (VString string)) ->
        ImpureGoodies.tryCatch "EvaluationError" (\() ->
          let newString = GroupStartMap.replace howmany regexp (\m ->
               case evalReplacement eval replacementV m of
                 Err msg -> ImpureGoodies.throw (EvaluationError msg)
                 Ok v ->
                    case v.v_ of
                      VBase (VString s) -> s
                      _ -> ImpureGoodies.throw (EvaluationError "[internal error] The function ToStrExceptStr returned something else than a string")
               ) string
          in
          Ok (replaceV_ replacementV <| VBase (VString newString), [])
        ) (\(EvaluationError msg) -> Err msg)
     _ -> Err <| "replaceAllIn expects a regex (String), a replacement (string/lambda), and the text. Got instead  " ++ valToString regexpV ++ ", " ++ valToString replacementV ++ ", " ++ valToString stringV
           -- Commented out because dependency cycle
           -- "Got " ++  valToString regexpV ++ ", " ++ valToString replacementV ++ ", " ++ valToString stringV

concat: String -> List Exp -> Exp
concat joinName l =
  eApp (eVar joinName) [eList l Nothing]

-- Use only within UpdateRegex. Supposes that it was a join [....]
unconcat: Exp -> Maybe EDiffs -> Result String (List Exp, ListDiffs EDiffs)
unconcat e mbdiffs = case eAppUnapply1 e of
  Nothing -> Err <| "[internal error] Not a join [], instead " ++ Syntax.unparser Syntax.Elm e
  Just (_, l) -> case eListUnapply l of
    Nothing -> Err <| "[internal error] Not a join [], instead " ++ Syntax.unparser Syntax.Elm e
    Just ls -> case mbdiffs of
      Just (EChildDiffs [(1, EListDiffs ld)]) -> Ok (ls, ld)
      Nothing -> Ok (ls, [])
      _ -> Err <| "[Internal error] not a Nothing or Just EChildDiffs [(1, EListDiffs l)], instead " ++ toString mbdiffs
{-
case UpdateRegex.updateRegexReplaceAllByIn
                           env eRec update regexpV replacementV stringV oldVal newVal diffs of
                         Err msg -> UpdateCriticalError msg
                         Ok ll -> updateOpMultiple "replaceAllIn" env opArgs (\newOpArgs -> replaceE__ e <| EOp sp1 op newOpArgs sp2) vs
                           (LazyList.map (\(a, b, c) ->
                             let outputVs = [a, b, c] in
                             (outputVs, UpdateUtils.defaultTupleDiffs valToString UpdateUtils.defaultVDiffs vs outputVs)
                             ) ll)
-}
dummyUpdate: UpdateStack -> Results String (UpdatedEnv, UpdatedExp)
dummyUpdate updateStack = Err "[internal error] Should not call dummyUpdate"

-- Recover all unchanged substrings from a string and a list of matches. Returns all but the last as a separate tuple element
interleavingStrings string matchList =
  let (lastEnd, initStrings) = List.foldl (\m (lastEnd, strings) ->
       (m.index + String.length m.match, strings ++ [String.slice lastEnd m.index string])
       ) (0, []) matchList
  in
  let lastString = String.dropLeft lastEnd string in
  (initStrings, lastString)

-- We need the environment just to make use of the function "nth" for lists, so we don't need to return it !!
updateRegexReplaceByIn: Regex.HowMany ->
   (Env -> Exp -> Result String (Val, Widgets)) ->
   (UpdateStack -> Results String (UpdatedEnv, UpdatedExp)) ->
   Val -> Val -> Val ->   PrevOutput -> Output -> VDiffs -> Results String (List Val, TupleDiffs VDiffs)
updateRegexReplaceByIn howmany eval update regexpV replacementV stringV oldOutV newOutV diffs =
   case (regexpV.v_, replacementV.v_, stringV.v_, newOutV.v_) of
     (VBase (VString regexp), VBase (VString replacement), VBase (VString string), _) ->
        -- let _ = Debug.log "regex1" () in
        let (lambdaReplacementV, oldConcatenationStarts) = stringToLambda eval update nth join (replaceV_ replacementV) replacement in
        updateRegexReplaceByIn howmany eval update regexpV lambdaReplacementV stringV oldOutV newOutV diffs |> Results.andThen (
          \(newArgs, newDiffs) ->
            case newArgs of
              [newRegexpV, newLambdaReplacementV, newStringV] ->
                let replacementDiffs = case UpdateUtils.diffsAt 1 newDiffs of
                  Nothing -> ok1 (replacementV, newDiffs)
                  Just vdiff ->
                     flip Results.andThen (lambdaToString oldConcatenationStarts newLambdaReplacementV vdiff) <| \newReplacementConcreteDiffs ->
                     let (newReplacementStr, newReplacementDiffs) =
                       applyConcreteDiffs replacement (pruneConcreteDiffs newReplacementConcreteDiffs) in
                     ok1 (replaceV_ replacementV <| VBase <| VString newReplacementStr
                         ,newDiffs |> UpdateUtils.replace 1 (VStringDiffs newReplacementDiffs))
                in
                flip Results.map replacementDiffs <| \(newReplacementV, newArgDiffs) ->
                ([newRegexpV, newReplacementV, newStringV], newArgDiffs)
              _ -> Err <| "Expected 3 arguments, got " ++ toString (List.length newArgs)
        )
       -- Conver the string to a lambda with string concatenation
     (VBase (VString regexp), _ {- closure or VFun, whatever -}, VBase (VString string), VBase (VString out)) ->
       -- If possible, we are going to parse the result to recover each changed
       -- replaceAllIn (A|B) (case of "A" -> "a"; "B" -> "b") "A,B"  gives "a,b". What if it is now replaced by x,y ? Can we change "a" and "b" in the lambda?
       -- Now yes because we can merge all results.
       -- let _ = Debug.log "regex2" () in
       let matches = GroupStartMap.find howmany regexp string in
       let (initStrings, lastString) = interleavingStrings string matches in
       let replacementName = "user_callback" in
       -- let _ = Debug.log "regex3" () in
       case evalRegexReplaceByIn Regex.All eval regexpV replacementV stringV of
         Err msg -> Err msg
         Ok (oldVal, _) ->
            let argName i = "match" ++ toString i in
            let concatenation = (List.concatMap identity <|
                         List.map2 (\(m, i) s ->
                             [eStr s, matchApp replacementName (argName i)]) (Utils.zipWithIndex matches) initStrings) ++ [eStr lastString] in
            let oldConcatenationStarts =
              (0:: List.concatMap (\{index, match} -> [index, index + String.length match]) matches) ++ [String.length string]
            in
            let expressionReplacement = concat "join" concatenation in
            -- let _ = Debug.log "regex4" () in
            let argumentsMatches = Utils.zipWithIndex matches |> List.map (\(m, i) -> (argName i, gsmMatchToRegexMatch m)) in
            let argumentsEnv = argumentsMatches |> List.map (\(name, m) -> (name, matchToVal m)) in
            let argNameToIndex = argumentsMatches |> List.indexedMap (\i (name, _) -> (name, i)) |> Dict.fromList in
            let argumentsMatchesDict = Dict.fromList argumentsMatches in
            let envWithReplacement= (replacementName, replacementV)::("join", join)::argumentsEnv in
            update (updateContext "regex replace" envWithReplacement expressionReplacement [] oldVal newOutV diffs) |> Results.andThen (
               \(newEnvWithReplacement, newUpdatedExp) ->
                 --let _ = Debug.log "newEnvWithReplacement.changes " newEnvWithReplacement.changes in
              case newEnvWithReplacement.val of
                (_, newReplacementV)::_::newArguments ->
                  let newRemplacementVChanges = case newEnvWithReplacement.changes of
                    (0, c) :: tail -> [(1, c)]
                    _ -> []
                  in
                  let argChangeByIndex = UpdateUtils.dropDiffs 2 newEnvWithReplacement.changes |> Dict.fromList in
                  let getArgChange: String -> Maybe VDiffs
                      getArgChange name =
                    Dict.get name argNameToIndex |> Maybe.andThen (flip Dict.get argChangeByIndex)
                  in
                  -- TODO: When unconaatenating, compute the newStringVChanges
                  -- let _ = Debug.log "regex6" () in
                  flip Results.andThen (Results.fromResult <| unconcat newUpdatedExp.val newUpdatedExp.changes) <| \(newConcatenation, listElemDiffs) ->
                  let newArgumentsDicts = Dict.fromList newArguments in
                  let recoverSubExpressionStringDiffs e =
                    case appMatchArg e of
                     Err s -> Err s
                     Ok argname -> case Dict.get argname argumentsMatchesDict of
                       Nothing -> Err <| "Could not find " ++ argname ++ " in " ++ toString argumentsMatchesDict
                       Just oldMatch -> case Dict.get argname newArgumentsDicts of
                         Nothing -> Err <| "Could not find " ++ argname ++ " in new environment"
                         Just newVal ->
                           recoverMatchedStringDiffs oldMatch newVal (getArgChange argname)
                  in
                  let recoverSubStringsDiffs e =
                     case eStrUnapply e of
                       Just s -> Just (\sd ->
                         case sd of
                           EStringDiffs l -> Just <| strDiffToConcreteDiff s l
                           _ -> Nothing
                         )
                       _ -> Nothing
                  in
                  -- let _ = Debug.log "regex7" () in
                  recoverStringDiffs recoverSubExpressionStringDiffs recoverSubStringsDiffs
                    oldConcatenationStarts newConcatenation listElemDiffs
                  |> Results.andThen (\newStringDiffs ->
                      let (newString, finalStringDiffs) = applyConcreteDiffs string (pruneConcreteDiffs newStringDiffs) in
                      let newStringVChanges = case finalStringDiffs of
                        [] -> []
                        l -> [(2, VStringDiffs l)]
                      in
                      let newChanges = newRemplacementVChanges ++ newStringVChanges in
                      ok1 ([regexpV, newReplacementV, Vb.string (Vb.fromVal stringV) newString], newChanges)
                    )
                _ -> Debug.crash "A variable disappeared from the environment"
              )
     _ -> Err <| "replaceAllIn expects a regex (String), a replacement (string/lambda), and the text. Got instead  " ++ valToString regexpV ++ ", " ++ valToString replacementV ++ ", " ++ valToString stringV ++ " updated by " ++ valToString newOutV
           -- Commented out because dependency cycle
           -- "Got " ++  valToString regexpV ++ ", " ++ valToString replacementV ++ ", " ++ valToString stringV

-- Given a list of interleaved expresssions, typically applications of lambdas to variables, and constant strings,
-- this function computes the resulting concrete string diff.
recoverStringDiffs: (Exp -> Results String (List (Int, Int, String))) ->
                    (Exp -> Maybe (EDiffs -> Maybe (List (Int, Int, String)))) ->
                       List Int ->            List Exp ->      ListDiffs EDiffs -> Results String (List (Int, Int, String))
recoverStringDiffs  recoverSubExpressionStringDiffs
                    recoverSubStringsDiffs
                       oldConcatenationStarts newConcatenation diffs =
  let aux: Int -> List Int ->            List Exp ->      ListDiffs EDiffs -> Results String (List (Int, Int, String)) -> Results String (List (Int, Int, String))
      aux  i      oldConcatenationStarts newConcatenation diffs               accRes =
    --let _ = Debug.log ("recoverStringDiffs.aux " ++ toString i ++ " " ++ toString oldConcatenationStarts ++ " " ++
     -- (newConcatenation |> List.map (Syntax.unparser Syntax.Elm) |> String.join "") ++ " " ++ toString diffs ++ " " ++ toString accRes) () in
    let currentDiff = case diffs of
       [] -> Nothing
       (j, d) :: tail -> if j > i then Nothing else Just (d, tail)
    in
    case currentDiff of
       Nothing ->
        case (oldConcatenationStarts, newConcatenation) of
          ([], _) -> Err "[Internal error] oldConcatenationStarts should never be empty"
          ([last], []) -> accRes
          (remaining::_, []) -> accRes |> Results.map (\acc -> (remaining, Utils.last "UpdateRegex.recoverStringDiffs" oldConcatenationStarts - remaining, "") :: acc)
          (startOld::tlOld, hdNew::tlNew) ->
           let newRes = case recoverSubStringsDiffs hdNew of
             Just _ -> accRes -- We now the string did not change, as there was no diff
             Nothing -> 
                recoverSubExpressionStringDiffs hdNew |>
                Results.map2 (\l1 l2 -> l1 ++ l2) accRes
           in
           aux (i + 1) tlOld tlNew diffs newRes
       Just (thediff, diffTail) -> -- It has to be a string which changed.
         case thediff of
            ListElemInsert count ->
              let (inserted, remainingNewConcatenation) = Utils.split count newConcatenation in
              let insertedStr = inserted |> List.map (eStrUnapply >> Result.fromMaybe "insertion of something else than strings in regex joins not supported") |>
                Utils.projOk |> Result.map (String.join "")
              in
              case (oldConcatenationStarts, insertedStr) of
                (_, Err msg) -> Err msg
                ([], _) -> Err "[Internal error] oldConcatenationStarts should never be empty"
                (startOld::tlOld, Ok insertedS) ->
                  Results.map (\acc -> (startOld, startOld, insertedS)::acc) accRes |>
                  aux i (List.drop count oldConcatenationStarts) remainingNewConcatenation diffTail
            ListElemDelete count ->
              let (deleted, remainingOldConcatenationStarts) = Utils.split count oldConcatenationStarts in
              case (oldConcatenationStarts, remainingOldConcatenationStarts) of
                (_, [])  -> Err "[Internal error] remainingOldConcatenationStarts should never be empty"
                ([], _)  -> Err "[Internal error] oldConcatenationStarts should never be empty"
                (startOld::_, endOld::_) ->
                  Results.map (\acc -> (startOld, endOld, "")::acc) accRes |>
                  aux (i + count) remainingOldConcatenationStarts newConcatenation diffTail
            ListElemUpdate d ->
              case (oldConcatenationStarts, newConcatenation) of
                ([], _) -> Err "[Internal error] oldConcatenationStarts should never be empty"
                (_, []) -> Err "[Internal error] newConcatenation was empty"
                (startOld::tlOld, updatedElem::tlNew) ->
                   case recoverSubStringsDiffs updatedElem |> Maybe.andThen (\k -> k d) of
                     Nothing ->  Err <| "[Internal error] Cannot update something else than a string in regex join, got " ++ Syntax.unparser Syntax.Elm updatedElem
                     Just concreteDiffs ->
                       Results.map (\acc -> acc ++ (concreteDiffs  |> offsetConcStr startOld)) accRes |>
                       aux (i + 1) tlOld tlNew diffTail
  in aux 0 oldConcatenationStarts newConcatenation diffs (ok1 [])

--updateRegexReplaceFirstIn

-- We can code extractFirstIn once we have records and record pattern matching.
evalRegexExtractFirstIn: Val -> Val -> Result String Val
evalRegexExtractFirstIn regexpV stringV =
  case (regexpV.v_, stringV.v_) of
    (VBase (VString regexp), VBase (VString string)) ->
      let matches = GroupStartMap.find (Regex.AtMost 1) regexp string in
      case matches of
        [] -> Ok (Vb.constructor (Vb.fromVal stringV) "Nothing" [])
        m::_ ->
          Ok (Vb.constructor (Vb.fromVal stringV) "Just"
               [Vb.list (\v {match} -> Vb.string v <| Maybe.withDefault "" match) (Vb.fromVal stringV) m.submatches])
    _ -> Err "Expected two strings, a regex and a string, got something else"

updateRegexExtractFirstIn: Val -> Val -> Val -> Val -> Results String Val
updateRegexExtractFirstIn regexpV stringV oldVal newVal =
  case (regexpV.v_, stringV.v_) of
    (VBase (VString regexp), VBase (VString string)) ->
      let matches = GroupStartMap.find (Regex.AtMost 1) regexp string in
      case matches of
        [] ->
--          let _ = Debug.log "No match, just return original string" () in
          ok1 stringV
        m::_ ->
          case Vu.constructor Ok newVal of
            Ok ("Just", [v]) ->
              Vu.list Vu.string v |> Results.fromResult |> Results.andThen (\newGroups ->
                let transformations =
                     List.map2 (\{match,start} newValue ->
                       match |> Maybe.andThen (\match -> if match == newValue then Nothing else Just (start, start + String.length match, newValue) )) m.submatches newGroups
                     |> List.filterMap identity
                in
                --let _ = Debug.log "transformations to the string" transformations in
                ok1 (Vb.string (Vb.fromVal stringV) <| mergeTransformations string transformations)
              )
            _ -> Err "Updating from not a just"
    _ -> Err "Internal error: Expected two strings, a regex, a string, the old value and the new value, got something else"


-- Given a list of strings, computes all the possible ways they split the string.
-- Each result is a list of size (List.length separators) + 1, such that interleaving this string
-- With separators results in the original string
allInterleavingsIn: List String -> String -> LazyList (List String)
allInterleavingsIn separators string =
  if List.length separators == 0 then LazyList.Cons [string] (Lazy.lazy <| \_ -> LazyList.Nil)
  else
  let aux remainingSeparators numrepetitions = case (remainingSeparators, numrepetitions) of
    ([], []) -> "(.*?)"
    (head::tail, numRep::tailNumRep) ->
       let escapedHead = escape head in
       "(.*?" ++ String.repeat numRep ("(?:(?=" ++ escapedHead ++ ").).*?") ++ ")" ++ escapedHead ++ aux tail tailNumRep
    _ -> Debug.crash "internal error: the two lists of allinterleavings should have the same size"
  in
  let lastIndex = List.length separators - 1 in
  -- We add 1 to numReps at index index, and everything afterwards should be set to zero again.
  let next index numReps =
    case numReps of
        [] -> Debug.crash "Internal error: empty numRep"
        head::tail ->
          if index == 0 then
            (head + 1) :: List.map (\_ -> 0) tail
          else
            head :: next (index - 1) tail
  in
  let initNumRep = List.map (\_ -> 0) separators in
  let findMatchFor indexRepToModify numRep =
    let r = regex <| "^" ++ aux separators numRep ++ "$" in
    let default () =
       if indexRepToModify == 0 then LazyList.Nil
       else findMatchFor (indexRepToModify - 1) <| next (indexRepToModify - 1) numRep in
    case find (AtMost 1) r string of
       [{ match, submatches, index, number }] ->
         case Utils.projJusts submatches of
           Just m ->
             LazyList.Cons m <| Lazy.lazy <| \_ ->
             findMatchFor lastIndex <| next lastIndex numRep
           Nothing ->
             default ()
       _ -> default ()
  in
  findMatchFor (List.length separators - 1) initNumRep


