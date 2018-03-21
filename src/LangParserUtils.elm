module LangParserUtils exposing
  ( space
  , spaces
  , nospace
  , keywordWithSpace
  , symbolWithSpace
  , spaceSaverKeyword
  , paddedBefore
  , isSpace
  , isOnlySpaces
  , mapPat_
  , mapExp_
  , SpacePolicy
  , spacesDefault
  , spacesWithoutIndentation
  , spacesNotBetweenDefs
  , spacesWithoutNewline
  )

import Parser exposing (..)
import Parser.LowLevel as LL
import Parser.LanguageKit as LK

import ParserUtils exposing (..)

import Lang exposing (..)
import Info exposing (..)
import Regex
import ImpureGoodies

--------------------------------------------------------------------------------
-- Whitespace
--------------------------------------------------------------------------------

isSpace : Char -> Bool
isSpace c =
  c == ' ' || c == '\n' || c == '\t' || c == '\r' || c == '\xa0'

isOnlySpaces : String -> Bool
isOnlySpaces =
  String.all isSpace

space : Parser WS
space =
  trackInfo <|
    keep (Exactly 1) isSpace

{-
numTries = { value = 1 }

failNthTime: Int -> String -> (() -> Parser a) -> Parser a
failNthTime n msg p =
  if numTries.value >= n then fail msg else (
    let _ = ImpureGoodies.mutateRecordField numTries "value" (numTries.value + 1) in
    p ()
  )
-}

-- Failed attempt at adding line comments anywhere and multiline comments
{-spacesDefaultHelper: String -> Parser String -> Parser String
spacesDefaultHelper parsed whitespaceParser =
  lazy <| \_ ->
    oneOf [
      whitespaceParser |> andThen (\result ->
         if result == "" -- If there is no space, we still have to test for line commentts, or return the empty space.
         then succeed parsed
         else spacesDefaultHelper (parsed ++ result) whitespaceParser
        )
      {-, lineComment |> andThen (\result ->
          let _ = Debug.log ("Before: '" ++ parsed ++ "', after: '" ++ result ++ "'") () in
          spacesDefaultHelper (parsed ++ result) whitespaceParser)-}

      {-, (source <| nestableComment "{-" "-}") |> andThen (\result ->
         spacesDefaultHelper (parsed ++ result) whitespaceParser)-}
      , succeed parsed
    ]-}

spacesDefault: Parser String -> Parser WS
spacesDefault whitespaceParser = trackInfo <| whitespaceParser
  --trackInfo <| spacesDefaultHelper "" whitespaceParser


spaces : Parser WS
spaces = spacesDefault (keep zeroOrMore isSpace)

regexSpaceWithoutIndentation = Regex.regex "^(?:\\s*\n)*|(?:\\s*\n)"

spacesWithoutIndentation: Parser WS
spacesWithoutIndentation = spacesDefault (ParserUtils.keepRegex regexSpaceWithoutIndentation)

regexSpaceStoppingIfTwoLinesOrNoIndentation = Regex.regex "((?!\n\n|\n\\S)\\s)*"

spacesNotBetweenDefs: Parser WS
spacesNotBetweenDefs = spacesDefault (ParserUtils.keepRegex regexSpaceStoppingIfTwoLinesOrNoIndentation)

regexSpaceWithoutNewline = Regex.regex "((?!\n)\\s)*"

spacesWithoutNewline: Parser WS
spacesWithoutNewline = spacesDefault (ParserUtils.keepRegex regexSpaceWithoutNewline)

lineComment = source <|
  delayedCommitMap (\a b -> ())
    (ignore zeroOrMore isSpace)
    (symbol "--"
     |. ignore zeroOrMore (\c -> c /= '\n')
     |. oneOf [ symbol "\n", end ])

nestableIgnore : Parser ignore -> Parser keep -> Parser keep
nestableIgnore ignoreParser keepParser =
  map2 (\a b -> b) ignoreParser keepParser

nestableComment : String -> String -> Parser ()
nestableComment start end =
  case (String.uncons start, String.uncons end) of
    (Nothing, _) ->
      fail "Trying to parse a multi-line comment, but the start token cannot be the empty string!"

    (_, Nothing) ->
      fail "Trying to parse a multi-line comment, but the end token cannot be the empty string!"

    ( Just (startChar, _), Just (endChar, _) ) ->
      let
        isNotRelevant char =
          char /= startChar && char /= endChar
      in
        symbol start
          |. nestableCommentHelp isNotRelevant start end 1


nestableCommentHelp : (Char -> Bool) -> String -> String -> Int -> Parser ()
nestableCommentHelp isNotRelevant start end nestLevel =
  lazy <| \_ ->
    nestableIgnore (Parser.ignore zeroOrMore isNotRelevant) <|
      oneOf
        [ nestableIgnore (symbol end) <|
            if nestLevel == 1 then
              succeed ()
            else
              nestableCommentHelp isNotRelevant start end (nestLevel - 1)
        , nestableIgnore (symbol start) <|
            nestableCommentHelp isNotRelevant start end (nestLevel + 1)
        , nestableIgnore (Parser.ignore (Exactly 1) (\_ -> True)) <|
            nestableCommentHelp isNotRelevant start end nestLevel
        ]

nospace : Parser WS
nospace =
  trackInfo <| succeed ""

guardSpace : ParserI ()
guardSpace =
  trackInfo
    ( ( succeed (,)
        |= LL.getOffset
        |= LL.getSource
      )
      |> andThen
      ( \(offset, source) ->
          guard "expecting space" <|
            isOnlySpaces <| String.slice offset (offset + 1) source
      )
    )

keywordWithSpace : String -> ParserI ()
keywordWithSpace kword =
  trackInfo <|
    succeed ()
      |. keyword kword
      |. guardSpace

symbolWithSpace : String -> ParserI ()
symbolWithSpace sym =
  trackInfo <|
    succeed ()
      |. symbol sym
      |. guardSpace

spaceSaverKeyword : Parser WS -> String -> (WS -> a) -> ParserI a
spaceSaverKeyword sp kword combiner =
  delayedCommitMap
    ( \ws _ ->
        withInfo (combiner ws) ws.start ws.end
    )
    ( sp )
    ( keyword kword )


paddedBefore : (WS -> a -> b) -> Parser WS -> ParserI a -> ParserI b
paddedBefore combiner sp p =
  delayedCommitMap
    ( \wsBefore x ->
        withInfo (combiner wsBefore x.val) x.start x.end
    )
    sp
    p

--------------------------------------------------------------------------------
-- Patterns
--------------------------------------------------------------------------------

mapPat_ : ParserI Pat__ -> Parser Pat
mapPat_ = (map << mapInfo) pat_

--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------

mapExp_ : ParserI Exp__ -> Parser Exp
mapExp_ = (map << mapInfo) exp_

-- This is useful to get rid of semicolon or colons in the top-level language.
-- app   how spaces before applications argument can be parsed (e.g. if they can go one line)
-- any   how any other top-level space is parsed (e.g.
type alias SpacePolicy = { first: Parser WS, apparg: Parser WS }
-- Expressions at top-level cannot consume a newline that is followed by an identifier, or two newlines except if they are parsed inside parentheses.