module ElmParser exposing
  (parse)

import Parser as P exposing (..)
import Parser.LanguageKit as LK

import ParserUtils exposing (..)
import ElmLang exposing (..)

--------------------------------------------------------------------------------
-- Bools
--------------------------------------------------------------------------------

bool : Parser Term
bool =
  inContext "bool" << whitespaceBefore << trackRange << map term_ <|
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
  inContext "int" << whitespaceBefore << trackRange << map term_ <|
    map (\x -> EInt { int = x })
      P.int

--------------------------------------------------------------------------------
-- Floats
--------------------------------------------------------------------------------

--float : Parser Term
--float =
--  whitespaceBefore << trackRange << map term_ <|
--    map (\x -> EFloat { float = x }) <|
--      try P.float

--------------------------------------------------------------------------------
-- Chars
--------------------------------------------------------------------------------

char : Parser Term
char =
  inContext "char" << whitespaceBefore << trackRange << map term_ <|
    succeed (\c -> EChar { char = c })
      |. symbol "'"
      |= ParserUtils.char
      |. symbol "'"

--------------------------------------------------------------------------------
-- Strings
--------------------------------------------------------------------------------

string : Parser Term
string =
  inContext "string" << whitespaceBefore << trackRange << map term_ <|
    succeed (\s -> EString { string = s })
      |. symbol "\""
      |= keep zeroOrMore (\c -> c /= '\"')
      |. symbol "\""

multiLineString : Parser Term
multiLineString =
  inContext "multi-line string" << whitespaceBefore << trackRange << map term_ <|
    map (\s -> EMultiLineString { string = s }) <|
      inside "\"\"\""

--------------------------------------------------------------------------------
-- Lists
--------------------------------------------------------------------------------

list : Parser Term
list =
  let
    member =
      inContext "list member" <|
        succeed
          ( \t after ->
              { t | after = after }
          )
          |= term
          |= whitespace
  in
    inContext "list" << whitespaceBefore << trackRange << map term_ <|
      map (\members -> EList { members = members }) <|
        LK.sequence
          { start = "["
          , separator = ","
          , end = "]"
            -- We'll handle spaces on our own
          , spaces = succeed ()
          , item = member
          , trailing = LK.Forbidden
          }

--------------------------------------------------------------------------------
-- Conditionals
--------------------------------------------------------------------------------

conditional : Parser Term
conditional =
  inContext "conditional" << whitespaceBefore << trackRange << map term_ <|
    succeed
      ( \c cAfter t tAfter f ->
          EConditional
          { condition =
              { c | after = cAfter }
          , trueBranch =
              { t | after = tAfter }
          , falseBranch =
              f
          }
      )
      |. keywordWithWhitespace "if"
      |= term
      |= whitespace
      |. keywordWithWhitespace "then"
      |= term
      |= whitespace
      |. keywordWithWhitespace "else"
      |= term


--------------------------------------------------------------------------------
-- General
--------------------------------------------------------------------------------

term : Parser Term
term =
  oneOf
    [ bool
    , int
    , char
    , multiLineString
    , string
    , lazy <| \_ -> list
    , lazy <| \_ -> conditional
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
