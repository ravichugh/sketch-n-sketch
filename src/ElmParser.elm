module ElmParser exposing
  ( parse
  , showError
  )

import Char
import Set exposing (Set)

import Parser as P exposing (..)
import Parser.LanguageKit as LK

import ParserUtils exposing (..)
import ElmLang exposing (..)

--==============================================================================
--= General
--==============================================================================

keywords : Set String
keywords =
  Set.fromList
    [ "let"
    , "in"
    , "case"
    , "of"
    ]

isRestChar : Char -> Bool
isRestChar char =
  Char.isLower char ||
  Char.isUpper char ||
  Char.isDigit char ||
  char == '_'

littleIdentifier : Parser String
littleIdentifier =
  LK.variable
    Char.isLower
    isRestChar
    keywords

bigIdentifier : Parser String
bigIdentifier =
  LK.variable
    Char.isUpper
    isRestChar
    keywords

--==============================================================================
--= Patterns
--==============================================================================

--------------------------------------------------------------------------------
-- Named
--------------------------------------------------------------------------------

named : Parser PTerm
named =
  ptermify "named" <|
    map (\name -> PNamed { name = name })
      littleIdentifier

--------------------------------------------------------------------------------
-- General
--------------------------------------------------------------------------------

pterm : Parser PTerm
pterm =
  inContext "pattern" <|
    oneOf
      [ named
      ]

--==============================================================================
-- Expressions
--==============================================================================

--------------------------------------------------------------------------------
-- Line Comments
--------------------------------------------------------------------------------

lineComment : Parser ETerm
lineComment =
  etermify "line comment" <|
    succeed
      ( \text termAfter ->
          ELineComment { text = text, termAfter = termAfter }
      )
      |. symbol "--"
      |= keep zeroOrMore (\c -> c /= '\n')
      |. oneOf
           [ symbol "\n"
           , end
           ]
      |= optional (padded eterm)

--------------------------------------------------------------------------------
-- Variables
--------------------------------------------------------------------------------

variable : Parser ETerm
variable =
  etermify "variable" <|
    map (\identifier -> EVariable { identifier = identifier })
      littleIdentifier

--------------------------------------------------------------------------------
-- Bools
--------------------------------------------------------------------------------

bool : Parser ETerm
bool =
  etermify "bool" <|
    oneOf
      [ token "True" <|
          EBool { bool = True }
      , token "False" <|
          EBool { bool = False }
      ]

--------------------------------------------------------------------------------
-- Ints
--------------------------------------------------------------------------------

int : Parser ETerm
int =
  etermify "int" <|
    map (\x -> EInt { int = x })
      P.int

--------------------------------------------------------------------------------
-- Floats
--------------------------------------------------------------------------------

--float : Parser ETerm
--float =
--  whitespaceBefore << trackRange << map term_ <|
--    map (\x -> EFloat { float = x }) <|
--      try P.float

--------------------------------------------------------------------------------
-- Chars
--------------------------------------------------------------------------------

char : Parser ETerm
char =
  etermify "char" <|
    succeed (\c -> EChar { char = c })
      |. symbol "'"
      |= ParserUtils.char
      |. symbol "'"

--------------------------------------------------------------------------------
-- Strings
--------------------------------------------------------------------------------

string : Parser ETerm
string =
  etermify "string" <|
    succeed (\s -> EString { string = s })
      |. symbol "\""
      |= keep zeroOrMore (\c -> c /= '\"')
      |. symbol "\""

multiLineString : Parser ETerm
multiLineString =
  etermify "multi-line string" <|
    map (\s -> EMultiLineString { string = s }) <|
      inside "\"\"\""

--------------------------------------------------------------------------------
-- Lists
--------------------------------------------------------------------------------

emptyList : Parser ETerm
emptyList =
  etermify "empty list" <|
    succeed (\space -> EEmptyList { space = space })
      |. symbol "["
      |= whitespace
      |. symbol "]"

list : Parser ETerm
list =
  lazy <| \_ ->
    let
      member =
        inContext "list member" <|
          padded eterm
    in
      etermify "list" <|
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
-- Records
--------------------------------------------------------------------------------

emptyRecord : Parser ETerm
emptyRecord =
  etermify "empty record" <|
    succeed (\space -> EEmptyRecord { space = space })
      |. symbol "{"
      |= whitespace
      |. symbol "}"

record : Parser ETerm
record =
  lazy <| \_ ->
    let
      entry =
        inContext "record entry" <|
          succeed (,)
            |= padded pterm
            |. symbol "="
            |= padded eterm
      entries =
        inContext "record entries" <|
          LK.sequence
            { start = ""
            , separator = ","
            , end = "}"
              -- We'll handle spaces on our own
            , spaces = succeed ()
            , item = entry
            , trailing = LK.Forbidden
            }
      base =
        inContext "record base" <|
          succeed identity
            |. symbol "{"
            |= optional
                 ( delayedCommitMap
                     (\t _ -> t)
                     (padded eterm)
                     (symbol "|")
                 )
    in
      etermify "record" <|
        succeed
          ( \b es ->
              ERecord
                { base = b
                , entries = es
                }
          )
          |= base
          |= entries

--------------------------------------------------------------------------------
-- Conditionals
--------------------------------------------------------------------------------

conditional : Parser ETerm
conditional =
  lazy <| \_ ->
    etermify "conditional" <|
      succeed
        ( \c t f ->
            EConditional
              { condition = c
              , trueBranch = t
              , falseBranch = f
              }
        )
        |. keywordWithWhitespace "if"
        |= padded eterm
        |. keywordWithWhitespace "then"
        |= padded eterm
        |. keywordWithWhitespace "else"
        |= padded eterm

--------------------------------------------------------------------------------
-- General
--------------------------------------------------------------------------------

etermBase : Parser ETerm
etermBase =
  oneOf
    [ lazy <| \_ -> lineComment
    , bool
    , int
    , char
    , multiLineString
    , string
    , try emptyList
    , lazy <| \_ -> list
    , try emptyRecord
    , lazy <| \_ -> record
    , lazy <| \_ -> conditional
    , variable
    ]

etermWithPrecedence : Int -> Parser ETerm
etermWithPrecedence precedence =
  let
    binop op =
      chainLeft
        (spannedBinaryOperator op)
        (symbol op)
        (padded << etermWithPrecedence <| precedence + 1)
    binops ops =
      oneOf <|
        List.map binop ops
  in
    lazy <| \_ ->
      case precedence of
        9 ->
          etermBase
        8 ->
          binops ["^"]
        7 ->
          binops ["*", "/", "//", "%"]
        6 ->
          binops ["+", "-"]
        5 ->
          etermWithPrecedence 6
        4 ->
          etermWithPrecedence 5
        3 ->
          etermWithPrecedence 4
        2 ->
          etermWithPrecedence 3
        1 ->
          etermWithPrecedence 2
        0 ->
          etermWithPrecedence 1
        _ ->
          fail <|
            "trying to parse expression with invalid precedence '" ++
              (toString precedence) ++
              "'"

eterm : Parser ETerm
eterm =
  let
    -- Start parsing the lowest precedence first.
    ezero =
      lazy <| \_ ->
        etermWithPrecedence 0
    -- If there are multiple expressions in a row, we must have a function
    -- application. Otherwise, if there is only one expression in a row, simply
    -- return that expression.
    combiner head tail =
      case tail of
        [] ->
          head
        _ ->
          spannedFunctionApplication head tail
  in
    succeed combiner
      |= padded ezero
      |= repeat zeroOrMore (padded ezero)

program : Parser ElmLang.Program
program =
  succeed identity
    |= oneOf
         [ map Nonempty <| padded eterm
         , map Empty whitespace
         ]
    |. end

--==============================================================================
--= Exports
--==============================================================================

--------------------------------------------------------------------------------
-- Parser Runners
--------------------------------------------------------------------------------

parse : String -> Result P.Error ElmLang.Program
parse =
  run program

--------------------------------------------------------------------------------
-- Error Handling
--------------------------------------------------------------------------------

showIndentedProblem : Int -> Problem -> String
showIndentedProblem n prob =
  let
    indent =
      String.repeat (2 * n) " "
  in
    case prob of
      BadOneOf probs ->
        indent ++ "One of:\n" ++
          String.concat (List.map (showIndentedProblem (n + 1)) probs)
      BadInt ->
        indent ++ "Bad integer value\n"
      BadFloat ->
        indent ++ "Bad float value\n"
      BadRepeat ->
        indent ++ "Parse of zero-length input indefinitely\n"
      ExpectingEnd ->
        indent ++ "Expecting end\n"
      ExpectingSymbol s ->
        indent ++ "Expecting symbol '" ++ s ++ "'\n"
      ExpectingKeyword s ->
        indent ++ "Expecting keyword '" ++ s ++ "'\n"
      ExpectingVariable ->
        indent ++ "Expecting variable\n"
      ExpectingClosing s ->
        indent ++ "Expecting closing string '" ++ s ++ "'\n"
      Fail s ->
        indent ++ "Parser failure: " ++ s ++ "\n"

showError : Error -> String
showError err =
  let
    prettyError =
      let
        sourceLines =
          String.lines err.source
        problemLine =
          List.head (List.drop (err.row - 1) sourceLines)
        arrow =
          (String.repeat (err.col - 1) " ") ++ "^"
      in
        case problemLine of
          Just line ->
            line ++ "\n" ++ arrow ++ "\n\n"
          Nothing ->
            ""
    showContext c =
      "  (row: " ++ (toString c.row) ++", col: " ++ (toString c.col)
      ++ ") Error while parsing '" ++ c.description ++ "'\n"
    deepestContext =
      case List.head err.context of
        Just c ->
          "Error while parsing '" ++ c.description ++ "':\n"
        Nothing ->
          ""
  in
    "[Parser Error]\n\n" ++
      deepestContext ++ "\n" ++
      prettyError ++
    "Position\n" ++
    "========\n" ++
    "  Row: " ++ (toString err.row) ++ "\n" ++
    "  Col: " ++ (toString err.col) ++ "\n\n" ++
    "Problem\n" ++
    "=======\n" ++
      (showIndentedProblem 1 err.problem) ++ "\n" ++
    "Context Stack\n" ++
    "=============\n" ++
      (String.concat <| List.map showContext err.context) ++ "\n\n"
