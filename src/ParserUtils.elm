module ParserUtils exposing
  ( negativeLookAhead
  , lookAhead
  , try
  , optional
  , guard
  , token
  , inside
  , char
  -- , oneOfBacktracking
  , ParserI
  , getPos
  , trackInfo
  , untrackInfo
  , showError
  , keepUntilRegex
  , ignoreRegex
  , keepRegex
  , singleLineString
  )

import Pos exposing (..)
import Info exposing (..)
-- import Utils

import Parser exposing (..)
import Parser.LowLevel as LL
import Regex exposing (Regex, HowMany(..), find)

--------------------------------------------------------------------------------
-- General
--------------------------------------------------------------------------------

lookAhead : Parser a -> Parser a
lookAhead parser =
  let
    getResult =
      succeed
        ( \offset source ->
            let
              remainingCode =
                String.dropLeft offset source
            in
              run parser remainingCode
        )
        |= LL.getOffset
        |= LL.getSource
  in
    getResult
      |> andThen
           ( \result ->
               case result of
                 Ok x ->
                   -- Return the result without consuming input
                   succeed x

                 Err _ ->
                   -- Consume input and fail (we know it will fail)
                   parser
           )


negativeLookAhead : Parser a -> Parser ()
negativeLookAhead parser =
  let
    getResult =
      succeed
        ( \offset source ->
            let
              remainingCode =
                String.dropLeft offset source
            in
              run parser remainingCode
        )
        |= LL.getOffset
        |= LL.getSource
  in
    getResult
      |> andThen
           ( \result ->
               case result of
                 Ok _ ->
                   fail "Don't want to parse this."
                   -- Return the result without consuming input

                 Err _ ->
                   -- Consume input and fail (we know it will fail)
                   succeed ()
           )



try : Parser a -> Parser a
try parser =
  delayedCommitMap always parser (succeed ())

optional : Parser a -> Parser (Maybe a)
optional parser =
  oneOf
    [ map Just parser
    , succeed Nothing
    ]

guard : String -> Bool -> Parser ()
guard failReason pred =
  if pred then (succeed ()) else (fail failReason)

token : String -> a -> Parser a
token text val =
  map (\_ -> val) (keyword text)

keepUntil : String -> Parser String
keepUntil endString =
  let
    endLength =
      String.length endString
  in
    oneOf
      [ ignoreUntil endString
          |> source
          |> map (String.dropRight endLength)
      , succeed identity
          |. keep zeroOrMore (\_ -> True)
          |= fail ("expecting closing string '" ++ endString ++ "'")
      ]

-- Stop parsing the string until it hits an ending string or a string escape char.
keepUntilRegex: Regex -> Parser String
keepUntilRegex reg =
  oneOf
    [ ignoreUntilRegex reg
        |> source
    , succeed identity
        |. keep zeroOrMore (\_ -> True)
        |= fail ("expecting closing string '" ++ toString reg ++ "'")
    ]

-- A variant of ignoreUntil that ignores everything until the given regex appears
ignoreUntilRegex : Regex -> Parser ()
ignoreUntilRegex reg =
   (succeed (,)
   |= LL.getOffset
   |= LL.getSource)
   |> andThen (\(offset,source) ->
     let sourceFromOffset = String.slice offset (String.length source) source in
     let regexMatches = find (AtMost 1) reg sourceFromOffset in
     case regexMatches of
       {index}::_ ->
         ignore (Exactly index) (\_ -> True)
       _ -> succeed identity
            |. keep zeroOrMore (\_ -> True)
            |= fail ("expecting regex '" ++ toString reg ++ "'")
   )

-- Stop parsing the string until it hits an ending string or a string escape char.
keepRegex: Regex -> Parser String
keepRegex reg =
  oneOf
    [ ignoreRegex reg
        |> source
    , fail ("'" ++ toString reg ++ "' did not match")
    ]

-- A variant of ignoreUntil that ignores the first match at position zero of this regex.
ignoreRegex : Regex -> Parser ()
ignoreRegex reg =
   (succeed (,)
   |= LL.getOffset -- Because it is not delayed commit, this cannot fail
   |= LL.getSource)
   |> andThen (\(offset,source) ->
        let sourceFromOffset = String.slice offset (String.length source) source in
        let finding = find (AtMost 1) reg sourceFromOffset in
        -- let _ = Debug.log ("Trying to ignore regex" ++ toString reg ++ " at pos " ++ toString (offset, source)) () in
        case finding of
          {index, match}::_ ->
            if index == 0 then
              -- let _ = Debug.log ("Found at index 0, length:") (String.length match) in
              ignore (Exactly <| String.length match) (\_ -> True)
            else
               -- let _ = Debug.log ("Found after index 0") () in
               fail ("expecting regex '" ++ toString reg ++ "' immediately but appeared only after " ++ toString index ++ " characters")
          _ ->
            -- let _ = Debug.log ("Not found") () in
            fail ("expecting regex '" ++ toString reg ++ "'")
   )

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

-- Remove when confident in Reduce response parsing
--
-- oneOfBacktracking : String -> List (Parser a) -> Parser a
-- oneOfBacktracking failReason parsers =
--   let
--     getParser =
--       succeed
--         ( \offset source ->
--             let
--               remainingCode =
--                 String.dropLeft offset source
--             in
--               parsers
--               |> Utils.findFirst (\parser -> run parser remainingCode |> Result.toMaybe |> (/=) Nothing)
--               |> Maybe.withDefault (fail failReason)
--
--         )
--         |= LL.getOffset
--         |= LL.getSource
--   in
--     getParser
--       |> andThen (\parser -> parser)


--------------------------------------------------------------------------------
-- Parser With Info
--------------------------------------------------------------------------------

type alias ParserI a = Parser (WithInfo a)

getPos : Parser Pos
getPos =
  map posFromRowCol LL.getPosition

trackInfo : Parser a -> ParserI a
trackInfo p =
  delayedCommitMap
    ( \start (a, end) ->
        withInfo a start end
    )
    getPos
    ( succeed (,)
        |= p
        |= getPos
    )

untrackInfo : ParserI a -> Parser a
untrackInfo =
  map (.val)

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

-- returns the quote string and the string content itself.
singleLineString : Parser (String, String)
singleLineString =
  let
    stringHelper quoteChar =
      let
        quoteString = String.fromChar quoteChar
      in
      let
        quoteEscapeRegex = Regex.regex <| "\n|\r|\t|\\\\|\\" ++ quoteString ++ "|" ++ quoteString
      in
        succeed (\x -> (quoteString, x))
          |. symbol quoteString
          |= map String.concat (
              repeat zeroOrMore <|
                oneOf [
                  map (\_ -> quoteString) <| symbol <| "\\" ++ quoteString,
                  map (\_ -> "\n") <| symbol <| "\\n",
                  map (\_ -> "\r") <| symbol <| "\\r",
                  map (\_ -> "\t") <| symbol <| "\\t",
                  map (\_ -> "\\") <| symbol <| "\\\\",
                  succeed (\a b -> a ++ b)
                  |= keep (Exactly 1) (\c -> c /= quoteChar && c /= '\\' && c /= '\n')
                  |= keepUntilRegex quoteEscapeRegex
                ])
          |. symbol quoteString
  in
    oneOf <| List.map stringHelper ['\'', '"']
