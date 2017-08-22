module ParserUtils exposing
  ( try
  , token
  , char
  , spaces
  , trackRange
  , spacesBefore
  )

import Parser exposing (..)
import Parser.LowLevel

import Whitespace exposing (Whitespace, whitespace_)
import Position exposing (Position)
import Range exposing (Ranged)
import Padding exposing (Padded)

--------------------------------------------------------------------------------
-- General
--------------------------------------------------------------------------------

try : Parser a -> Parser a
try parser =
  delayedCommitMap always parser (succeed ())

token : String -> a -> Parser a
token text val =
  map (\_ -> val) (keyword text)

char : Parser Char
char =
  map
    ( String.uncons >>
      Maybe.withDefault ('_', "") >>
      Tuple.first
    )
    ( keep (Exactly 1) (always True)
    )

--------------------------------------------------------------------------------
-- Whitespace
--------------------------------------------------------------------------------

isSpace : Char -> Bool
isSpace c =
  c == ' ' || c == '\n'

isOnlySpaces : String -> Bool
isOnlySpaces =
  String.all isSpace

spaces : Parser Whitespace
spaces =
  trackRange <|
    map whitespace_ <|
      keep zeroOrMore isSpace

--------------------------------------------------------------------------------
-- Positions
--------------------------------------------------------------------------------

getPosition : Parser Position
getPosition =
  map
    Position.fromRowCol
    Parser.LowLevel.getPosition

--------------------------------------------------------------------------------
-- Ranges
--------------------------------------------------------------------------------

trackRange : Parser (Ranged a) -> Parser (Ranged a)
trackRange parser =
  delayedCommitMap
    ( \start (x, end) ->
        { x
            | start = start
            , end = end
        }
    )
    getPosition
    ( succeed (,)
        |= parser
        |= getPosition
    )

--------------------------------------------------------------------------------
-- Paddings
--------------------------------------------------------------------------------

spacesBefore : Parser (Padded a) -> Parser (Padded a)
spacesBefore parser =
  delayedCommitMap
    ( \before x ->
        { x
            | before = before
        }
    )
    spaces
    parser
