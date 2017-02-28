module OurParser2 exposing (..)

import String
import Debug
import Lazy exposing (Lazy, lazy, force)

import Ace
import Utils

type alias Pos         = { line : Int, col : Int}
type alias WithPos a   = { val : a, pos : Pos }
type alias WithInfo a  = { val : a, start : Pos, end : Pos }
  -- TODO rename to WithRange

startPos               = { line = 1, col = 1}
dummyPos               = { line = -1, col = -1 }

strPos p =
  let (i,j) = (toString p.line, toString p.col) in
  "(Line:" ++ i ++ " Col:" ++ j ++ ")"

offsetBy : Pos -> String -> Pos
offsetBy start s =
  List.foldl addOneChar start (String.toList s)

addOneChar c start =
  if c == '\n'
  then { line = 1 + start.line, col = 1 }
  else { start | col = 1 + start.col }

type alias Parser_ a = WithPos String -> List (WithInfo a, WithPos String)

type Parser a
  = P     (Parser_ a)
  | LazyP (Lazy (Parser_ a))

-- copied from Dandandan/parser
recursively : (() -> Parser a) -> Parser a
recursively thunk = LazyP (lazy (\_ -> runParser (thunk ())))

runParser : Parser a -> WithPos String -> List (WithInfo a, WithPos String)
runParser p =
  case p of
    P f         -> f
    LazyP thunk -> force thunk

{-
parse : Parser a -> String -> Result String (WithInfo a)
parse p s =
  case runParser p (WithPos s startPos) of
    [(s,{val})] -> if val == ""
                     then Ok s
                     else Err <| "incomplete parse, next unparsed: \n" ++ (Utils.takeNLines 10 val)
    [] -> Err ("no parse\n\n" ++ s)
    l  -> Err ("ambiguous parse\n\n" ++ toString (List.map (.val << fst) l))
-}

type alias ParseError = (String, Ace.Annotation)

fst (a, b) = a
snd (a, b) = b

formatError : ParseError -> String
formatError p =
  " row " ++ toString (snd p).row ++ ": " ++ (fst p)

parse : Parser a -> String -> Result ParseError (WithInfo a)
parse p s =
  case runParser p (WithPos s startPos) of
    [(a,{val})] ->
      if val == "" then Ok a
      else
        let previewSuffix = Utils.takeNLines 10 val in
        let err = "incomplete parse, next unparsed: \n" ++ previewSuffix in
        let text = Utils.lines <|
          [ "Successfully parsed up until: " ++ strPos a.end ++ "\n"
          , "Here's where things start to go wrong:\n"
          , previewSuffix
          , "\n..."
          ]
        in
        Err (err, { row = a.end.line - 1, type_ = "error", text = text })
    [] ->
      let err = "no parse\n\n" ++ s in
      Err (err, { row = 0, type_ = "error", text = "Parse Error..." })
    l  ->
      let err = "ambiguous parse\n\n" ++ toString (List.map (.val << Tuple.first) l) in
      Err (err, { row = 0, type_ = "error", text = "Parse Error... ambiguous..." })


satisfy : (Char -> Bool) -> Parser Char
satisfy f = P <| \s ->
  case String.uncons s.val of
    Just (c,s_) ->
      if not (f c)
        then []
        else
          let start = s.pos in
          let end   = offsetBy start (String.fromChar c) in
          [(WithInfo c start end, WithPos s_ end)]
    Nothing -> []

char : Char -> Parser Char
char c = satisfy ((==) c)

-- Satisfy given parser but don't shorten the remaining string to parse.
lookahead : Parser a -> Parser a
lookahead looker = P <| \s ->
  let parses = runParser looker s in
  List.map (\(parsed, restStr) -> (parsed, s)) parses

-- Statisfy first parser, lookahead satisfy second parser, return first parser result
lookafter : Parser a -> Parser b -> Parser a
lookafter p looker = P <| \s ->
  runParser p s
  |> List.filter (\(parsed, restStr) -> not <| List.isEmpty <| runParser looker restStr)

string : String -> Parser String
string str = P <| \s ->
  if not (String.startsWith str s.val) then []
  else
    let n     = String.length str in
    let start = s.pos in
    let end   = offsetBy start str in
    [(WithInfo str start end, WithPos (String.dropLeft n s.val) end)]

token : String -> Parser ()
token = map (always ()) << string

end : Parser ()
end = token ""

fail : Parser a
fail = P (always [])

munch : (Char -> Bool) -> Parser String
munch f = P <| \s ->
  let walk acc s =
    case String.uncons s of
      Nothing     -> (String.reverse acc, s)
      Just (c,s_) -> if f c
                       then walk (String.cons c acc) s_
                       else (String.reverse acc, s)
  in
  let (pre,suf) = walk "" s.val in
  let start     = s.pos in
  let end       = offsetBy start pre in
  [(WithInfo pre start end, WithPos suf end)]

munch1 : (Char -> Bool) -> Parser String
munch1 f = P <| \s ->
  case runParser (munch f) s of
    [(pre,suf)] ->
      if s == suf
        then []
        else [(pre,suf)]
    _ ->
      Debug.crash "munch1"

choice : List (Parser a) -> Parser a
choice ps =
  case ps of
    []     -> fail
    p::ps_ -> or p (choice ps_)

between : Parser open_ -> Parser close_ -> Parser a -> Parser a
between p1 p2 p =
  p1 >>= \a ->
  p  >>= \x ->
  p2 >>= \b ->
    returnWithInfo x.val a.start b.end

-- NOTES:
--  - the following are "munching" versions (<++ instead of +++)
--  - lists of things are wrapped "deeply"

option : a -> Parser a -> Parser a
option default p = p <++ return default

optional : Parser a -> Parser ()
optional p = (p >>> return ()) <++ return ()

many : Parser a -> Parser (List (WithInfo a))
many p = some p <++ return []

some : Parser a -> Parser (List (WithInfo a))
some p =
  p      >>= \x  ->
  many p >>= \xs ->
    returnWithInfo (x :: xs.val) x.start xs.end

sepBy : Parser a -> Parser sep -> Parser (List (WithInfo a))
sepBy p sep = sepBy1 p sep <++ return []

sepBy1 : Parser a -> Parser sep -> Parser (List (WithInfo a))
sepBy1 p sep =
  p                >>= \x ->
  many (sep >>> p) >>= \xs ->
    returnWithInfo (x :: xs.val) x.start xs.end

return : a -> Parser a
return x = P (\s -> [(WithInfo x s.pos s.pos, s)])

returnWithInfo x start end = P (\s -> [(WithInfo x start end, s)])

bind : Parser a -> (WithInfo a -> Parser b) -> Parser b
bind pa f = P <| \s ->
  List.concatMap (\(a,s_) -> runParser (f a) s_) (runParser pa s)

sequence : Parser a -> Parser b -> Parser b
sequence p1 p2 = bind p1 (always p2)

or : Parser a -> Parser a -> Parser a
or p1 p2 = P <| \s -> (runParser p1 s) ++ (runParser p2 s)

left_or : Parser a -> Parser a -> Parser a
left_or p1 p2 = P <| \s ->
  case runParser p1 s of
    []  -> runParser p2 s
    res -> res

map : (a -> b) -> Parser a -> Parser b
map f p = P <| \s ->
  List.map (\(x,s_) -> (WithInfo (f x.val) x.start x.end, s_)) (runParser p s)

(>>=) = bind
(>>>) = sequence
(+++) = or
(<++) = left_or
(<<|) = map

-- functions from Text.ParserCombinators.ReadP not implemented:
--
--  count,      -- :: Int -> ReadP a -> ReadP [a]
--  skipMany,   -- :: ReadP a -> ReadP ()
--  skipMany1,  -- :: ReadP a -> ReadP ()
--  endBy,      -- :: ReadP a -> ReadP sep -> ReadP [a]
--  endBy1,     -- :: ReadP a -> ReadP sep -> ReadP [a]
--  chainr,     -- :: ReadP a -> ReadP (a -> a -> a) -> a -> ReadP a
--  chainl,     -- :: ReadP a -> ReadP (a -> a -> a) -> a -> ReadP a
--  chainl1,    -- :: ReadP a -> ReadP (a -> a -> a) -> ReadP a
--  chainr1,    -- :: ReadP a -> ReadP (a -> a -> a) -> ReadP a
--  manyTill,   -- :: ReadP a -> ReadP end -> ReadP [a]

