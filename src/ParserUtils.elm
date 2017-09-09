module ParserUtils exposing
  ( try
  , optional
  , chainLeft
  , token
  , inside
  , char
  , whitespace
  , keywordWithWhitespace
  , paddedBefore
  , paddedAfter
  , padded
  , eTermify
  , pTermify
  , sTermify
  )

import Parser exposing (..)
import Parser.LowLevel as LL

import Whitespace exposing (Whitespace, whitespace_)
import Position exposing (Position)
import Range exposing (Ranged)
import Padding exposing (Padded)

import ElmLang exposing (..)

--------------------------------------------------------------------------------
-- General
--------------------------------------------------------------------------------

try : Parser a -> Parser a
try parser =
  delayedCommitMap always parser (succeed ())

optional : Parser a -> Parser (Maybe a)
optional parser =
  oneOf
    [ map Just parser
    , succeed Nothing
    ]

-- Like Parsec's chainl1
chainLeft : (a -> a -> a) -> Parser sep -> Parser a -> Parser a
chainLeft combiner sep term =
  let
    separatedTerm =
      succeed identity
        |. sep
        |= term
  in
    succeed (List.foldl (flip combiner))
      |= term
      |= repeat zeroOrMore separatedTerm

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

paddedBefore : Parser (Padded a) -> Parser (Padded a)
paddedBefore parser =
  delayedCommitMap
    ( \before x ->
        { x | before = before }
    )
    whitespace
    parser

paddedAfter : Parser (Padded a) -> Parser (Padded a)
paddedAfter parser =
  succeed
    ( \x after ->
        { x | after = after }
    )
    |= parser
    |= whitespace

padded : Parser (Padded a) -> Parser (Padded a)
padded =
  paddedBefore >> paddedAfter

--------------------------------------------------------------------------------
-- Terms
--------------------------------------------------------------------------------

termify
  : (a -> Ranged (Padded b)) -> String -> Parser a -> Parser (Ranged (Padded b))
termify term_ context =
  map term_
    >> trackRange
    >> inContext context

eTermify : String -> Parser Expression -> Parser ETerm
eTermify =
  termify eTerm_

pTermify : String -> Parser Pattern -> Parser PTerm
pTermify =
  termify pTerm_

sTermify : String -> Parser Statement -> Parser STerm
sTermify =
  termify sTerm_
