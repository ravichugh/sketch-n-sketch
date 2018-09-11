module LangParserUtils exposing
  ( space
  , spaces
  , oldSpaces
  , spacesCustom
  , SpaceCheck
  , nospace
  , keywordWithSpace
  , spaceSaverKeyword
  , paddedBefore
  , transferInfo
  , unwrapInfo
  , isSpace
  , isOnlySpaces
  , mapPat_
  , mapWSPat_
  , mapType_
  , mapWSType_
  , mapExp_
  , mapWSExp_
  , mapWSInfo
  , SpacePolicy
  , spacesWithoutIndentation
  , spacesNotBetweenDefs
  , spacesWithoutNewline
  , explodeStyleValue
  , implodeStyleValue
  , isFirstChar
  , isRestChar
  , MinStartCol
  , SpaceConstraint(..)
  , spaceSameLineOrNextAfter
  , spaceSameLineOrNextAfterOrTwoLines
  )

import Parser exposing (..)
import Parser.LowLevel as LL
import Parser.LanguageKit as LK

import ParserUtils exposing (..)

import Lang exposing (..)
import Info exposing (..)
import Regex
import ImpureGoodies
import Pos exposing (Pos)
import Char

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

--withNewline: Bool, minIndentation: Maybe Int, maxIndentation: Maybe Int, differentIndentation: Maybe Int
type alias SpaceCheck = {spaceCheck: Pos -> Pos -> Bool, msgBuilder: () -> String}

minIndentation: String -> Int -> SpaceCheck
minIndentation forwhat i =
  SpaceCheck (\start end -> end.col - 1 >= i) <| \() -> "I need an indentation of at least " ++ toString i ++ " spaces " ++ forwhat

maxIndentation: String -> Int -> SpaceCheck
maxIndentation forwhat i =
  SpaceCheck (\start end -> end.col - 1 <= i) <| \() -> "I need an indentation of at most " ++ toString i ++ " spaces " ++ forwhat

differentIndentation: String -> Int -> SpaceCheck
differentIndentation forwhat i =
  SpaceCheck (\start end -> end.col - 1 /= i) <| \() -> "I need an indentation of not " ++ toString i ++ " spaces " ++ forwhat

withoutNewline: String -> SpaceCheck
withoutNewline forwhat =
  SpaceCheck (\start end -> start.line == end.line) <| \() -> "I need a space not containing a newline for " ++ forwhat

maxOneLine: String -> SpaceCheck
maxOneLine forwhat =
  SpaceCheck (\start end -> start.line + 1 >= end.line) <| \() -> "Cannot have more than one newline for " ++ forwhat

spacesCustom: SpaceCheck -> Parser WS
spacesCustom {spaceCheck, msgBuilder} =
  spaces |> map (\ws ->
    if spaceCheck ws.start ws.end then
      succeed ws
    else
      fail <| msgBuilder ()
  ) |>
  andThen identity

spacesRaw: Parser ()
spacesRaw =
  ignore zeroOrMore isSpace |>
    andThen (\_ ->
      oneOf [
        lineComment |> andThen (\_ -> spacesRaw),
        nestableComment "{-" "-}" |> andThen (\_ -> spacesRaw),
        succeed ()
      ])

type alias MinStartCol = Int
type SpaceConstraint = NoSpace | MinIndentSpace

spaceSameLineOrNextAfter: MinStartCol -> SpaceConstraint -> Parser WS
spaceSameLineOrNextAfter minStartCol spConstraint =
  if spConstraint == NoSpace then nospace else
  spacesCustom <| SpaceCheck
  (\start end -> end.line <= start.line + 1 && (if end.line == start.line + 1 then minStartCol <= end.col else True)) <|
    \() -> "Expected a min indentation of " ++ toString minStartCol ++ " if on the next line"

spaceSameLineOrNextAfterOrTwoLines: MinStartCol -> Parser WS
spaceSameLineOrNextAfterOrTwoLines minStartCol = spacesCustom <| SpaceCheck
  (\start end -> start.line + 1 < end.line ||
    end.line <= start.line + 1 && (if start.line + 1 == end.line then minStartCol <= end.col else True)) <|
    \() -> "Expected a min indentation of " ++ toString minStartCol ++ " if on the next line"

spaces : Parser WS
spaces = trackInfo <| source <| spacesRaw

spacesWithoutIndentation: Parser WS
spacesWithoutIndentation = spacesCustom <| maxIndentation "at this place" 0

spacesNotBetweenDefs: Parser WS
spacesNotBetweenDefs =  spacesCustom <| minIndentation "at this place" 1

spacesWithoutNewline: Parser WS
spacesWithoutNewline = spacesCustom <| withoutNewline "at this place"

lineComment: Parser ()
lineComment =
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

-- For compatibility with FastParser
oldSpacesRaw: Parser ()
oldSpacesRaw =
  ignore zeroOrMore isSpace |>
    andThen (\_ ->
      oneOf [
        oldLineComment |> andThen (\_ -> oldSpacesRaw),
        succeed ()
      ])

oldSpaces : Parser WS
oldSpaces = trackInfo <| source <| oldSpacesRaw
oldLineComment: Parser ()
oldLineComment =
  (symbol ";"
   |. ignore zeroOrMore (\c -> c /= '\n')
   |. oneOf [ symbol "\n", end ])


nospace : Parser WS
nospace =
  trackInfo <| succeed ""

isFirstChar : Char -> Bool
isFirstChar char =
  Char.isLower char ||
  Char.isUpper char ||
  char == '_' ||
  char == '$'

isRestChar : Char -> Bool
isRestChar char =
  Char.isLower char ||
  Char.isUpper char ||
  Char.isDigit char ||
  char == '_' ||
  char == '$'

guardSpace : ParserI ()
guardSpace =
  trackInfo
    ( ( succeed (,)
        |= LL.getOffset
        |= LL.getSource
      )
      |> andThen
      ( \(offset, source) ->
           guard "expecting space or an opening parenthese" <|
             (String.all (not << isRestChar)) <| String.slice offset (offset + 1) source
      )
    )

keywordWithSpace : String -> ParserI ()
keywordWithSpace kword =
  trackInfo <|
    succeed ()
      |. keyword kword
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

transferInfo : (a -> b) -> ParserI a -> ParserI b
transferInfo combiner p =
  map ( \x -> withInfo (combiner x.val) x.start x.end) p

unwrapInfo: ParserI (a -> b) -> Parser (a -> WithInfo b)
unwrapInfo = map (\{val, start, end} -> \a -> withInfo (val a) start end)

--------------------------------------------------------------------------------
-- Patterns
--------------------------------------------------------------------------------

mapPat_ : ParserI Pat__ -> Parser Pat
mapPat_ = (map << mapInfo) pat_

mapWSPat_ : ParserI (WS -> Pat__) -> Parser (WS -> Pat)
mapWSPat_ = (map << mapInfoWS) pat_

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

mapType_ : ParserI Type__ -> Parser Type
mapType_ = (map << mapInfo) type_

mapWSType_ : ParserI (WS -> Type__) -> Parser (WS -> Type)
mapWSType_ = (map << mapInfoWS) type_

--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------

mapExp_ : ParserI Exp__ -> ParserI Exp_
mapExp_ = (map << mapInfo) exp_

mapWSExp_ : ParserI (WS -> Exp__) -> Parser (WS -> WithInfo Exp_)
mapWSExp_ = (map << mapInfoWS) exp_

mapWSInfo : ParserI (WS -> a) -> Parser (WS -> (WithInfo a))
mapWSInfo = (map << mapInfoWS) identity

-- This is useful to get rid of semicolon or colons in the top-level language.
-- app   how spaces before applications argument can be parsed (e.g. if they can go one line)
-- any   how any other top-level space is parsed (e.g.
type alias SpacePolicy = { first: Parser WS, apparg: Parser WS }
-- Expressions at top-level cannot consume a newline that is followed by an identifier, or two newlines except if they are parsed inside parentheses.

styleSplitRegex = Regex.regex "(?=;\\s*\\S)"

-- Returns a complete split of the style (pre-name, name, colon, value, post-name)
explodeStyleValue: String -> List (String, String, String, String, String)
explodeStyleValue content =
  Regex.split Regex.All styleSplitRegex content
    |> List.filterMap (\s ->
         case Regex.find (Regex.AtMost 1) (Regex.regex "^(;?)([\\s\\S]*)(:)([\\s\\S]*)(;?\\s*)$") s of
           [m] -> case m.submatches of
             [Just prename, Just name, Just colon, Just value, Just postvalue] -> Just (prename, name, colon, value, postvalue)
             _ ->Nothing
           _ ->Nothing
       )

implodeStyleValue: List (String, String) -> String
implodeStyleValue content =
  content |> List.map (\(name, value) -> name ++ ":" ++ value) |> String.join ";"
