module ElmParser exposing
  (parse)

import ElmLang exposing (..)

import Parser as P exposing (..)

import ParserUtils exposing (..)

--------------------------------------------------------------------------------
-- Bools
--------------------------------------------------------------------------------

bool : Parser Term
bool =
  spacesBefore << trackRange << map term_ <|
    oneOf
      [ token "True" <|
          EBool { bool = True }
      , token "False" <|
          EBool { bool = False }
      ]

--------------------------------------------------------------------------------
-- Ints
--------------------------------------------------------------------------------

int : Parser Term
int =
  spacesBefore << trackRange << map term_ <|
    map (\x -> EInt { int = x })
      P.int

--------------------------------------------------------------------------------
-- Floats
--------------------------------------------------------------------------------

--float : Parser Term
--float =
--  spacesBefore << trackRange << map term_ <|
--    map (\x -> EFloat { float = x }) <|
--      try P.float

--------------------------------------------------------------------------------
-- Chars
--------------------------------------------------------------------------------

char : Parser Term
char =
  spacesBefore << trackRange << map term_ <|
    succeed (\c -> EChar { char = c })
      |. symbol "'"
      |= ParserUtils.char
      |. symbol "'"

--------------------------------------------------------------------------------
-- Strings
--------------------------------------------------------------------------------

string : Parser Term
string =
  spacesBefore << trackRange << map term_ <|
    succeed (\s -> EString { string = s })
      |. symbol "\""
      |= keep zeroOrMore (\c -> c /= '\"')
      |. symbol "\""

--------------------------------------------------------------------------------
-- General
--------------------------------------------------------------------------------

term : Parser Term
term =
  oneOf
    [ bool
    , int
    , char
    , string
    ]

program : Parser Term
program =
  succeed identity
    |= term
    |. end

--------------------------------------------------------------------------------
-- Exports
--------------------------------------------------------------------------------

parse : String -> Result P.Error Term
parse =
  run program
