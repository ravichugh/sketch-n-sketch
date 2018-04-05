module UpdateRegex exposing (
    evalRegexReplaceAllByIn
  , updateRegexReplaceAllByIn

  , evalRegexReplaceFirstByIn
  , updateRegexReplaceFirstByIn

  , evalRegexExtractFirstIn
  , updateRegexExtractFirstIn

  , allInterleavingsIn)

import UpdateStack exposing (NextAction(..), UpdateStack(..), Output)
import Results exposing
  ( Results(..)
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

removeSlashDollar s = replace All (regex "\\\\\\$") (\_ -> "\\$") s
addSlashDollar s = replace All (regex "\\$") (\_ -> "\\\\\\$") s

lambdaBodyToString:Exp -> String
lambdaBodyToString e =
  case e.val.e__ of
    EFun _ _ body _ -> lambdaBodyToString body
    EApp space1 _ [_, pos] _ _ -> lambdaBodyToString pos
    EConst _ groupIndex _ _  -> "$" ++ toString groupIndex
    EOp space0 _ {-ToStrExceptStr-} [a] _-> lambdaBodyToString a
    EOp space1 _ {-Plus -} [a, b] space2 -> lambdaBodyToString a ++ lambdaBodyToString b
    EParens _ e _ _ -> lambdaBodyToString e
    ESelect _ e _ _ _ -> lambdaBodyToString e
    EBase _ (EString _ s) -> addSlashDollar s
    _ -> Debug.crash "[internal error] I should not have encountered a different case for UpdateRegex.lambdaBodyToString"

lambdaToString: Val -> String
lambdaToString v =
  case v.v_ of
    VClosure Nothing [_] lambdaBody env ->
      lambdaBodyToString lambdaBody
    _ -> Debug.crash "Trying to call lambdaToString with something else than a closure"

stringToLambda: Env -> Val -> String -> Val
stringToLambda env vs s =
  let l = find All (regex "(\\\\\\$|\\$(\\d))") s |> List.map (\m -> (m, m.index, String.length m.match + m.index)) in
  let (finalLastStart, lWithPrevStart) = List.foldl (\(mat, start, end) (lastEnd, acc) -> (end, acc ++ [(mat, lastEnd, start, end)])) (0, []) l in
  let m = EVar space0 "m" in
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
         Nothing -> [] --It was just a regular escaped dollar
         Just groupIndex ->
           let res = [ withDummyExpInfo <| EBase space1 (EString "\"" (removeSlashDollar (String.slice lastEnd start s))),
                       withDummyExpInfo <|
                          EApp space1 (withDummyExpInfo <| EVar space1 "nth") [
                           withDummyExpInfo <| ESelect space1 (withDummyExpInfo m) space0 space0 "group",
                            withDummyExpInfo <| EConst space1 (toFloat groupIndex) dummyLoc noWidgetDecl] SpaceApp space0
                        ] in
           res
       ) lWithPrevStart
  in
  let lambdaBody = List.foldr (\a b ->
       case a.val.e__ of
         EBase _ (EString _ _) -> eOp Plus [a, b]
         _ -> eOp Plus [eOp ToStrExceptStr [a], b]
       ) (withDummyExpInfo <| EBase space1 (EString "\"" <| removeSlashDollar(String.dropLeft finalLastStart s))) tmp
  in
  replaceV_ vs <| VClosure Nothing [pVar "m"] lambdaBody env

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

valToMatch: Val -> (RegexMatch -> Result String b) -> Result String b
valToMatch v continuation =
  let try: Result String a -> (a -> Result String b) -> Result String b
      try = flip Result.andThen in
  let get: Dict.Dict String a -> String -> (a -> Result String b) -> Result String b
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

-- Reverse operation of matchToVal
recoverMatchedString : RegexMatch -> Val -> Result String String {- The matched string -}
recoverMatchedString  oldRegexMatch newV =
  valToMatch newV <| \newRegexMatch ->
    let oldMatch      = oldRegexMatch.match
        oldIndex      = oldRegexMatch.index
        oldNumber     = oldRegexMatch.number
        oldSubmatches = oldRegexMatch.submatches
        oldGroups     = oldRegexMatch.group
        oldStarts     = oldRegexMatch.start in
    let newMatch      = newRegexMatch.match
        newIndex      = newRegexMatch.index
        newNumber     = newRegexMatch.number
        newSubmatches = newRegexMatch.submatches
        newGroups     = newRegexMatch.group
        newStarts     = newRegexMatch.start in
      -- Now we need to look for the updated match.
    if newMatch /= oldMatch then
      Ok newMatch
    else if newIndex /= oldIndex then
      Err "Cannot update the index of a matched regular expression, only the string itself !"
    else if newNumber /= oldNumber then
      Err "Cannot update the index of a matched regular expression, only the string itself !"
    else if List.length newSubmatches /= List.length oldSubmatches then
      Err "Cannot change the length of the submatches, only the strings"
    else if oldStarts /= newStarts then
      Err "Cannot change the start positions of the subgroup, only the strings"
    else if List.length newGroups /= List.length oldGroups then
      Err "Cannot change the length of the group list, only the strings"
    else -- Maybe a subgroup has been changed ?
      let defaultStringDiff o s = Ok (if o == s then Nothing else Just VConstDiffs) in
      UpdateUtils.defaultTupleDiffs identity defaultStringDiff oldGroups newGroups |> Result.andThen (\mb1 ->
      UpdateUtils.defaultTupleDiffs identity defaultStringDiff oldGroups (newMatch::newSubmatches) |> Result.andThen (\mb2 ->

      let allsubgroups = case (mb1, mb2) of
        (_, Nothing) -> newGroups
        (Nothing, _) -> newMatch::newSubmatches
        (Just m1, Just m2) ->
           Tuple.first <| UpdateUtils.mergeTuple (\o s1 m1 s2 m2 -> (UpdateUtils.mergeString o s1 s2, m1)) oldGroups newGroups m1 (newMatch::newSubmatches) m2
      in
      --let _ = Debug.log "allsbugroups" allsubgroups in
      let startMatch = case oldStarts of
        head::tail -> head
        _ -> 0
      in
      -- We recover all changes and push only the biggest ones if there are conflicts.
      List.map3 (\mo so m2 ->
       if mo == m2 then Ok Nothing
       else Ok <| Just (so - startMatch, so - startMatch + String.length mo, m2)
      ) oldGroups oldStarts allsubgroups
      |> Utils.projOk |> Result.map (\transformations ->
        let finalMatch = List.filterMap identity transformations |> mergeTransformations oldMatch in
        finalMatch
      )
      ))

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

evalRegexReplaceAllByIn = evalRegexReplaceByIn Regex.All
evalRegexReplaceFirstByIn = evalRegexReplaceByIn <| Regex.AtMost 1

evalRegexReplaceByIn: Regex.HowMany -> Env -> (Env -> Exp -> Result String Val)-> Val -> Val -> Val -> Result String Val
evalRegexReplaceByIn  howmany env eval regexpV replacementV stringV =
   case (regexpV.v_, replacementV.v_, stringV.v_) of
     (VBase (VString regexp), VBase (VString replacement), VBase (VString string)) ->
        evalRegexReplaceByIn howmany env eval regexpV (stringToLambda env replacementV replacement) stringV
       -- Conver the string to a lambda with string concatenation
     (VBase (VString regexp), VClosure _ _ _ _, VBase (VString string)) ->
        ImpureGoodies.tryCatch "EvaluationError" (\() ->
          let newString = GroupStartMap.replace howmany regexp (\m ->
               let replacementName = "UpdateRegex.replaceAll" in
               let argumentName = "x" in
               let matchVal = matchToVal (gsmMatchToRegexMatch m) in
               let localEnv = [(replacementName, replacementV), (argumentName, matchVal)] in
               let localExp = matchApp replacementName argumentName in
               --let _ = Debug.log ("The new env is" ++ LangUtils.envToString localEnv) () in
               --let _ = Debug.log ("The replacement body is" ++ Syntax.unparser Syntax.Elm localExp) () in
               case eval localEnv localExp of
                 Err msg -> ImpureGoodies.throw (EvaluationError msg)
                 Ok v ->
                    case v.v_ of
                      VBase (VString s) -> s
                      _ -> ImpureGoodies.throw (EvaluationError "[internal error] The function ToStrExceptStr returned something else than a string")
               ) string
          in
          Ok (replaceV_ replacementV <| VBase (VString newString))
        ) (\(EvaluationError msg) -> Err msg)
     _ -> Err <| "replaceAllIn expects a regex (String), a replacement (string/lambda), and the text. Got instead  " ++ valToString regexpV ++ ", " ++ valToString replacementV ++ ", " ++ valToString stringV
           -- Commented out because dependency cycle
           -- "Got " ++  valToString regexpV ++ ", " ++ valToString replacementV ++ ", " ++ valToString stringV

concat: List Exp -> Exp
concat l = case l of
  [head] -> head
  head::tail -> eOp Plus [head, concat tail]
  _ -> Debug.crash "Trying to call concat with an empty list"

unconcat: Exp -> List Exp
unconcat e = case e.val.e__ of
  EOp _ p [head, tail] _ -> case p.val of
    Plus -> unconcat head ++ unconcat tail
    _ -> [e]
  _ -> [e]

updateRegexReplaceAllByIn  = updateRegexReplaceByIn Regex.All
updateRegexReplaceFirstByIn = updateRegexReplaceByIn (Regex.AtMost 1)

-- We need the environment just to make use of the function "nth" for lists, so we don't need to return it !!
updateRegexReplaceByIn: Regex.HowMany -> Env -> (Env -> Exp -> Result String Val)-> (Env -> Exp -> Val -> Val -> Results String (UpdatedEnv, UpdatedExp)) -> Val -> Val -> Val -> Val -> Val -> Results String (Val, Val, Val)
updateRegexReplaceByIn howmany env eval updateRoutine regexpV replacementV stringV oldOutV outV =
   case (regexpV.v_, replacementV.v_, stringV.v_, outV.v_) of
     (VBase (VString regexp), VBase (VString replacement), VBase (VString string), _) ->
        -- let _ = Debug.log "regex1" () in
        updateRegexReplaceByIn howmany env eval updateRoutine regexpV (stringToLambda env replacementV replacement) stringV oldOutV outV
        |> Results.map (\(newRegexpV, newReplacementV, newStringV) ->
          (newRegexpV, replaceV_ replacementV <| VBase <| VString <| lambdaToString <| newReplacementV, newStringV)
        )
       -- Conver the string to a lambda with string concatenation
     (VBase (VString regexp), VClosure _ _ _ _ , VBase (VString string), VBase (VString out)) ->
       -- If possible, we are going to parse the result to recover each changed
       -- replaceAllIn (A|B) (case of "A" -> "a"; "B" -> "b") "A,B"  gives "a,b". What if it is now replaced by x,y ? Can we change "a" and "b" in the lambda?
       -- Now yes because we can merge all results.
       -- let _ = Debug.log "regex2" () in
       let matches = GroupStartMap.find howmany regexp string in
       let (lastEnd, initStrings) = List.foldl (\m (lastEnd, strings) ->
            (m.index + String.length m.match, strings ++ [String.slice lastEnd m.index string])
            ) (0, []) matches
       in
       let lastString = String.dropLeft lastEnd string in -- Of size >= 2
       let replacementName = "UpdateRegex.replaceAll" in
       -- let _ = Debug.log "regex3" () in
       case evalRegexReplaceAllByIn env eval regexpV replacementV stringV of
         Err msg -> Errs msg
         Ok olvVal ->
            let argName i = "match" ++ toString i in
            let expressionReplacement = concat <| (
              List.concatMap identity <|
                 List.map2 (\(m, i) s ->
                   [eStr s, matchApp replacementName (argName i)]) (Utils.zipWithIndex matches) initStrings) ++ [eStr lastString] in
            -- let _ = Debug.log "regex4" () in
            let argumentsMatches = Utils.zipWithIndex matches |> List.map (\(m, i) -> (argName i, gsmMatchToRegexMatch m)) in
            let argumentsEnv = argumentsMatches |> List.map (\(name, m) -> (name, matchToVal m)) in
            let argumentsMatchesDict = Dict.fromList argumentsMatches in
            let envWithReplacement= (replacementName, replacementV)::argumentsEnv in
            updateRoutine envWithReplacement expressionReplacement olvVal outV
            |> Results.andThen (\(newEnvWithReplacement, newUpdatedExp) ->
              case newEnvWithReplacement.val of
                (_, newReplacementV)::newArguments ->
                  -- let _ = Debug.log "regex6" () in
                  let stringsAndLambdas = unconcat newUpdatedExp.val in
                  let newArgumentsDicts = Dict.fromList newArguments in
                  -- let _ = Debug.log "regex7" () in
                  stringsAndLambdas |> List.map (\e ->
                    case eStrUnapply e of
                      Just s -> Ok s
                      Nothing -> case appMatchArg e of
                        Err s -> Err s
                        Ok argname -> case Dict.get argname argumentsMatchesDict of
                          Nothing -> Err <| "Could not find " ++ argname ++ " in " ++ toString argumentsMatchesDict
                          Just oldMatch -> case Dict.get argname newArgumentsDicts of
                            Just newVal -> recoverMatchedString oldMatch newVal
                            Nothing -> Err <| "Could not find " ++ argname ++ " in new environment"
                    )
                  |> Utils.projOk
                  |> Results.fromResult
                  |> Results.andThen (\newStrings ->
                      let newString = String.join "" newStrings in
                      ok1 (regexpV, newReplacementV, Vb.string (Vb.fromVal stringV) newString)
                    )
                _ -> Debug.crash "A variable disappeared from the environment"
             )
     _ -> Errs <| "replaceAllIn expects a regex (String), a replacement (string/lambda), and the text. Got instead  " ++ valToString regexpV ++ ", " ++ valToString replacementV ++ ", " ++ valToString stringV
           -- Commented out because dependency cycle
           -- "Got " ++  valToString regexpV ++ ", " ++ valToString replacementV ++ ", " ++ valToString stringV

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
            _ -> Errs "Updating from not a just"
    _ -> Errs "Internal error: Expected two strings, a regex, a string, the old value and the new value, got something else"


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


