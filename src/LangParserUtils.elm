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
  )

import Parser exposing (..)
import Parser.LowLevel as LL

import ParserUtils exposing (..)

import Lang exposing (..)
import Info exposing (..)

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

spaces : Parser WS
spaces =
  trackInfo <|
    keep zeroOrMore isSpace

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