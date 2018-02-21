module UpdateRegex exposing (updateRegexReplaceAllByIn, evalRegexReplaceAllByIn, allInterleavingsIn)

import UpdateStack exposing (NextAction(..), UpdateStack(..), Output(..))
import Results exposing
  ( Results(..)
  , ok1, oks, okLazy
  , LazyList(..)
  , appendLazy, appendLazyLazy, mapLazy, andThenLazy, isLazyNil
  , lazyFromList
  , lazyCons2)
import Lang exposing (..)
import Regex exposing (..)
import Lazy
import Utils
import ImpureGoodies
import GroupStartMap
import Syntax
import Dict
--import LangTools exposing (valToString)

removeSlashDollar s = replace All (regex "\\\\\\$") (\_ -> "\\$") s
addSlashDollar s = replace All (regex "\\$") (\_ -> "\\\\\\$") s

lambdaToString:Exp -> String
lambdaToString e =
  case e.val.e__ of
    EFun _ _ body _ -> lambdaToString body
    EApp space1 _ [_, pos] _ _ -> lambdaToString pos
    EConst _ groupIndex _ _  -> "$" ++ toString groupIndex
    EOp space0 _ {-ToStrExceptStr-} [a] _-> lambdaToString a
    EOp space1 _ {-Plus -} [a, b] space2 -> lambdaToString a ++ lambdaToString b
    EBase _ (EString _ s) -> addSlashDollar s
    _ -> Debug.crash "[internal error] I should not have encountered a different case for UpdateRegex.lambdaToString"

stringToLambda: Env -> Val -> String -> Val
stringToLambda env vs s =
  let l = find All (regex "(\\\\\\$|\\$(\\d))") s |> List.map (\m -> (m, m.index, String.length m.match + m.index)) in
  let m = EVar space1 "m" in
  let lastStart = { value = 0 } in
  let tmp = List.concatMap (\(mat, start, end) ->
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
          let res = [ withDummyExpInfo <| EBase space1 (EString "\"" (removeSlashDollar (String.slice lastStart.value start s))),
                      withDummyExpInfo <|
                       EApp space1 (withDummyExpInfo <| EVar space1 "nth") [
                         withDummyExpInfo <| ESelect (withDummyExpInfo m) space0 space0 "group",
                         withDummyExpInfo <| EConst space1 (toFloat groupIndex) dummyLoc noWidgetDecl] SpaceApp space0 ] in
          let _ = ImpureGoodies.mutateRecordField lastStart "value" end in
          res
      ) l
  in
  let lambdaBody = List.foldr (\a b ->
      case a.val.e__ of
        EBase _ (EString _ _) -> eOp Plus [a, b]
        _ -> eOp Plus [eOp ToStrExceptStr [a], b]
      ) (withDummyExpInfo <| EBase space1 (EString "\"" <| removeSlashDollar(String.dropLeft lastStart.value s))) tmp
  in replaceV_ vs <| VClosure Nothing [withDummyPatInfo <| PVar space0 "m" noWidgetDecl] lambdaBody env

type EvaluationError = EvaluationError String

evalRegexReplaceAllByIn: Env -> (Env -> Exp -> Result String Val)-> Val -> Val -> Val -> Result String Val
evalRegexReplaceAllByIn  env eval regexpV replacementV stringV =
   case (regexpV.v_, replacementV.v_, stringV.v_) of
     (VBase (VString regexp), VBase (VString replacement), VBase (VString string)) ->
        evalRegexReplaceAllByIn env eval regexpV (stringToLambda env replacementV replacement) stringV
       -- Conver the string to a lambda with string concatenation
     (VBase (VString regexp), VClosure _ _ body _, VBase (VString string)) ->
        ImpureGoodies.tryCatch "EvaluationError" (\() ->
          let newString = Regex.replace Regex.All (Regex.regex regexp) (\m ->
              let mainMatch = replaceV_ replacementV <| VBase (VString m.match) in
              let subMatches = List.map (replaceV_ replacementV << VBase << VString << Maybe.withDefault "") m.submatches in
              let argument = replaceV_ replacementV <| VRecord ["match", "submatches", "group", "index", "number"] <|
                Dict.fromList [(("match", 1),      mainMatch),
                               (("submatches", 1), replaceV_ replacementV <| VList subMatches),
                               (("group", 1),      replaceV_ replacementV <| VList <| mainMatch :: subMatches),
                               (("index", 1),      replaceV_ replacementV <| VConst Nothing (toFloat m.index, dummyTrace)),
                               (("number", 1),     replaceV_ replacementV <| VConst Nothing (toFloat m.number, dummyTrace))]
              in
              --let _ = Debug.log "The replacement body is" (Syntax.unparser Syntax.Elm body) in
              let res = eval (("UpdateRegex.replaceAll", replacementV)::("UpdateRegex.argument", argument)::env) (eOp ToStrExceptStr [withDummyExpInfo <|
                  EApp space0 (withDummyExpInfo <| EVar space1 "UpdateRegex.replaceAll") [withDummyExpInfo <| EVar space1 "UpdateRegex.argument"] SpaceApp space0])
              in
              case res of
                Err msg -> ImpureGoodies.throw (EvaluationError msg)
                Ok v ->
                  case v.v_ of
                    VBase (VString s) -> s
                    _ -> ImpureGoodies.throw (EvaluationError "[internal error] The function ToStrExceptStr returned something else than a string")
              ) string
          in
          Ok (replaceV_ replacementV <| VBase (VString newString))
        ) (\(EvaluationError msg) -> Err msg)
     _ -> Err <| "Expected a string for the regexp, a string or a lambda returning a string for replacement, and the text to perform the replace on. "
           -- Commented out because dependency cycle
           -- "Got " ++  valToString regexpV ++ ", " ++ valToString replacementV ++ ", " ++ valToString stringV


updateRegexReplaceAllByIn: Val -> Val -> Val -> String -> Results String (Val, Val, Val)
updateRegexReplaceAllByIn regexpV replacementV stringV out =
  Debug.crash "??"
{-  case (regexpV, replacementV, stringV) of
    (VString regexp, VString replacement, VString string) ->
      -- Conver the string to a lambda with string concatenation
    (VString regexp, VClosure replacement, VString string) ->
-}


--updateRegexReplaceFirstIn

-- We can code extractFirstIn once we have records and record pattern matching.
--updateRegexExtractFirstIn: String ->




-- Given a list of strings, computes all the possible ways they split the string.
-- Each result is a list of size (List.length separators) + 1, such that interleaving this string
-- With separators results in the original string
allInterleavingsIn: List String -> String -> LazyList (List String)
allInterleavingsIn separators string =
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
      if indexRepToModify == 0 then LazyNil
      else findMatchFor (indexRepToModify - 1) <| next (indexRepToModify - 1) numRep in
    case find (AtMost 1) r string of
      [{ match, submatches, index, number }] ->
        case Utils.projJusts submatches of
          Just m ->
            LazyCons m <| Lazy.lazy <| \_ ->
            findMatchFor lastIndex <| next lastIndex numRep
          Nothing ->
            default ()
      _ -> default ()
  in
  findMatchFor (List.length separators - 1) initNumRep


