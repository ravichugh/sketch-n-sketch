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
    , "if"
    , "then"
    , "else"
    ]

-- See: https://groups.google.com/forum/#!msg/elm-dev/0AHSnDdkSkQ/E0SVU70JEQAJ
symbols : Set Char
symbols =
  Set.fromList
    [ '+'
    , '-'
    , '/'
    , '*'
    , '='
    , '.'
    , '<'
    , '>'
    , ':'
    , '&'
    , '|'
    , '^'
    , '?'
    , '%'
    , '#'
    , '~'
    , '!'
    ]

isRestChar : Char -> Bool
isRestChar char =
  Char.isLower char ||
  Char.isUpper char ||
  Char.isDigit char ||
  char == '_'

isSymbol : Char -> Bool
isSymbol char =
  Set.member char symbols

littleIdentifier : Parser Identifier
littleIdentifier =
  LK.variable
    Char.isLower
    isRestChar
    keywords

bigIdentifier : Parser Identifier
bigIdentifier =
  LK.variable
    Char.isUpper
    isRestChar
    keywords

operatorIdentifier : Parser Identifier
operatorIdentifier =
  keep oneOrMore isSymbol

--==============================================================================
--= Patterns
--==============================================================================

--------------------------------------------------------------------------------
-- Named
--------------------------------------------------------------------------------

named : Parser PTerm
named =
  pTermify "named" <|
    map (\name -> PNamed { name = name })
      littleIdentifier

--------------------------------------------------------------------------------
-- General P-Terms
--------------------------------------------------------------------------------

pTerm : Parser PTerm
pTerm =
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
  eTermify "line comment" <|
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
      |= optional (padded eTerm)

--------------------------------------------------------------------------------
-- Variables
--------------------------------------------------------------------------------

variable : Parser ETerm
variable =
  eTermify "variable" <|
    map (\identifier -> EVariable { identifier = identifier })
      littleIdentifier

--------------------------------------------------------------------------------
-- Bools
--------------------------------------------------------------------------------

bool : Parser ETerm
bool =
  eTermify "bool" <|
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
  eTermify "int" <|
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
  eTermify "char" <|
    succeed (\c -> EChar { char = c })
      |. symbol "'"
      |= ParserUtils.char
      |. symbol "'"

--------------------------------------------------------------------------------
-- Strings
--------------------------------------------------------------------------------

string : Parser ETerm
string =
  eTermify "string" <|
    succeed (\s -> EString { string = s })
      |. symbol "\""
      |= keep zeroOrMore (\c -> c /= '\"')
      |. symbol "\""

multiLineString : Parser ETerm
multiLineString =
  eTermify "multi-line string" <|
    map (\s -> EMultiLineString { string = s }) <|
      inside "\"\"\""

--------------------------------------------------------------------------------
-- Lists
--------------------------------------------------------------------------------

emptyList : Parser ETerm
emptyList =
  eTermify "empty list" <|
    succeed (\ws -> EEmptyList { whitespace = ws })
      |. symbol "["
      |= whitespace
      |. symbol "]"

list : Parser ETerm
list =
  lazy <| \_ ->
    let
      member =
        inContext "list member" <|
          padded eTerm
    in
      eTermify "list" <|
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
  eTermify "empty record" <|
    succeed (\ws -> EEmptyRecord { whitespace = ws })
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
            |= padded pTerm
            |. symbol "="
            |= padded eTerm
      entries =
        inContext "record entries"
          ( LK.sequence
              { start = ""
              , separator = ","
              , end = "}"
                -- We'll handle spaces on our own
              , spaces = succeed ()
              , item = entry
              , trailing = LK.Forbidden
              }
              -- andThen force at least one entry
              |> andThen
                   ( \es ->
                       if List.isEmpty es then
                         fail "trying to parse non-empty records, but no entries found"
                       else
                         succeed es
                   )
          )

      base =
        inContext "record base" <|
          succeed identity
            |. symbol "{"
            |= optional
                 ( delayedCommitMap
                     (\t _ -> t)
                     (padded eTerm)
                     (symbol "|")
                 )
    in
      eTermify "record" <|
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
-- Lambdas
--------------------------------------------------------------------------------
-- The expression
--   \x y -> e
-- desugars to
--   \x -> (\y -> e)

lambda : Parser ETerm
lambda =
  lazy <| \_ ->
    eTermify "lambda" << map .expression <|
      succeed
        ( \parameters body ->
            List.foldr
              ( \parameter bodyAcc ->
                  eTerm_ <|
                    ELambda
                      { parameter =
                          parameter
                      , body =
                          bodyAcc
                      }
              )
              body
              parameters
        )
        |. symbol "\\"
        |= repeat oneOrMore (padded pTerm)
        |. symbol "->"
        |= padded eTerm

--------------------------------------------------------------------------------
-- Parentheses (grouping)
--------------------------------------------------------------------------------

paren : Parser ETerm
paren =
  lazy <| \_ ->
    eTermify "parentheses" <|
      succeed ( \inside -> EParen { inside = inside } )
        |. symbol "("
        |= padded eTerm
        |. symbol ")"

--------------------------------------------------------------------------------
-- Conditionals
--------------------------------------------------------------------------------

conditional : Parser ETerm
conditional =
  lazy <| \_ ->
    eTermify "conditional" <|
      succeed
        ( \c t f ->
            EConditional
              { condition = c
              , trueBranch = t
              , falseBranch = f
              }
        )
        |. keywordWithWhitespace "if"
        |= padded eTerm
        |. keywordWithWhitespace "then"
        |= padded eTerm
        |. keywordWithWhitespace "else"
        |= padded eTerm

--------------------------------------------------------------------------------
-- General E-Terms
--------------------------------------------------------------------------------

eTermBase : Parser ETerm
eTermBase =
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
    , lazy <| \_ -> lambda
    , lazy <| \_ -> paren
    , lazy <| \_ -> conditional
    , variable
    ]

-- For handling binary operators
eTermWithPrecedence : Int -> Parser ETerm
eTermWithPrecedence precedence =
  let
    binaryOperator operator =
      chainLeft
        ( \left right ->
            eTerm_ <|
              EBinaryOperator
                { operator = operator
                , left = left
                , right = right
                }
        )
        (symbol operator)
        (padded << eTermWithPrecedence <| precedence + 1)

    binaryOperators =
      oneOf << List.map binaryOperator
  in
    lazy <| \_ ->
      case precedence of
        9 ->
          eTermBase
        8 ->
          binaryOperators ["^"]
        7 ->
          binaryOperators ["*", "/", "//", "%"]
        6 ->
          binaryOperators ["+", "-"]
        5 ->
          eTermWithPrecedence 6
        4 ->
          eTermWithPrecedence 5
        3 ->
          eTermWithPrecedence 4
        2 ->
          eTermWithPrecedence 3
        1 ->
          eTermWithPrecedence 2
        0 ->
          eTermWithPrecedence 1
        _ ->
          fail <|
            "trying to parse expression with invalid precedence '" ++
              (toString precedence) ++
              "'"

-- The expression
--   f x y
-- desugars to
--   (f x) y

eTerm : Parser ETerm
eTerm =
  let
    ezero =
      lazy <| \_ ->
        eTermWithPrecedence 0

    combiner function arguments =
      if List.isEmpty arguments then
        function -- not an actual function
      else
        ( List.foldl
            ( \argument functionAcc ->
                eTerm_ <|
                  EFunctionApplication
                    { function =
                        functionAcc
                    , argument =
                        argument
                    }
            )
            function
            arguments
        )
  in
    eTermify "expression" << map .expression <|
      succeed combiner
        |= padded ezero
        |= repeat zeroOrMore (padded ezero) -- handles function application

--------------------------------------------------------------------------------
-- Programs
--------------------------------------------------------------------------------

program : Parser ElmLang.Program
program =
  succeed identity
    |= oneOf
         [ map Nonempty <| padded eTerm
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
