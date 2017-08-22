module ParserUtils exposing
  ( try
  , token
  , keepUntil
  , inside
  , char
  , whitespace
  , guardWhitespace
  , keywordWithWhitespace
  , trackRange
  , whitespaceBefore
  )

import Parser exposing (..)
import Parser.LowLevel as LL

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

guard : String -> Bool -> Parser ()
guard failReason pred =
  if pred then (succeed ()) else (fail failReason)

token : String -> a -> Parser a
token text val =
  map (\_ -> val) (keyword text)

keepUntil : String -> Parser String
keepUntil end =
  let
    endLength =
      String.length end
  in
    ignoreUntil end
      |> source
      |> map (String.dropRight endLength)

inside : String -> Parser String
inside delimiter =
  succeed identity
    |. symbol delimiter
    |= keepUntil delimiter

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

isWhitespace : Char -> Bool
isWhitespace c =
  c == ' ' || c == '\n'

isOnlyWhitespace : String -> Bool
isOnlyWhitespace =
  String.all isWhitespace

whitespace : Parser Whitespace
whitespace =
  trackRange <|
    map whitespace_ <|
      keep zeroOrMore isWhitespace

guardWhitespace : Parser ()
guardWhitespace =
  ( succeed (,)
    |= LL.getOffset
    |= LL.getSource
  )
  |> andThen
  ( \(offset, source) ->
      guard "expecting space" <|
        isOnlyWhitespace <|
          String.slice offset (offset + 1) source
  )

keywordWithWhitespace : String -> Parser ()
keywordWithWhitespace kword =
  succeed ()
    |. keyword kword
    |. guardWhitespace

--------------------------------------------------------------------------------
-- Positions
--------------------------------------------------------------------------------

getPosition : Parser Position
getPosition =
  map
    Position.fromRowCol
    LL.getPosition

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

whitespaceBefore : Parser (Padded a) -> Parser (Padded a)
whitespaceBefore parser =
  delayedCommitMap
    ( \before x ->
        { x
            | before = before
        }
    )
    whitespace
    parser
