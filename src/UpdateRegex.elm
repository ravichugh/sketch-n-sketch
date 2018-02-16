module UpdateRegex exposing (..)

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

updateRegexReplaceAllByIn: Val_ -> Val_ -> Val_ -> Results String (Val_, Val_, Val_)
updateRegexReplaceAllByIn regexpStr replacement string = Debug.crash ""


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


