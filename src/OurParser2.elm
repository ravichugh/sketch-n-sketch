module OurParser2 where

import String
import Debug
import Lazy exposing (Lazy, lazy, force)

type alias Pos         = { line : Int, col : Int}
type alias WithPos a   = { val : a, pos : Pos }
type alias WithInfo a  = { val : a, start : Pos, end : Pos }

startPos               = { line = 1, col = 1}
withPos val pos        = { val = val, pos = pos }
withInfo val start end = { val = val, start = start, end = end }

offsetBy : Pos -> String -> Pos
offsetBy start s =
  List.foldl addOneChar start (String.toList s)

addOneChar c start =
  if | c == '\n' -> { line = 1 + start.line, col = 1 }
     | otherwise -> { start | col <- 1 + start.col }

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

parse : Parser a -> String -> Result String (WithInfo a)
parse p s =
  case runParser p (withPos s startPos) of
    [(s,{val})] -> if
      | val == "" -> Ok s
      | otherwise -> Err "incomplete parse"
    [] -> Err ("no parse\n\n" ++ s)
    l  -> Err ("ambiguous parse\n\n" ++ toString l)

return : a -> Parser a
return x = P (\s -> [(WithInfo x s.pos s.pos, s)])

returnWithInfo x start end = P (\s -> [(WithInfo x start end, s)])

bind : Parser a -> (WithInfo a -> Parser b) -> Parser b
bind pa f = P <| \s ->
  List.concat (List.map (\(a,s') -> runParser (f a) s') (runParser pa s))

sequence : Parser a -> Parser b -> Parser b
sequence p1 p2 = bind p1 (always p2)

(>>=) = bind
(>>>) = sequence

satisfy : (Char -> Bool) -> Parser Char
satisfy f = P <| \s ->
  case String.uncons s.val of
    Just (c,s') -> if
      | not (f c) -> []
      | otherwise ->
          let start = s.pos in
          let end   = start `offsetBy` String.fromChar c in
          [(withInfo c start end, withPos s' end)]
    Nothing -> []

char : Char -> Parser Char
char c = satisfy ((==) c)

string : String -> Parser String
string str = P <| \s ->
  if not (String.startsWith str s.val) then []
  else
    let n     = String.length str in
    let start = s.pos in
    let end   = start `offsetBy` str in
    [(withInfo str start end, withPos (String.dropLeft n s.val) end)]

map : (a -> b) -> Parser a -> Parser b
map f p = P <| \s ->
  List.map (\(x,s') -> (withInfo (f x.val) x.start x.end, s')) (runParser p s)

token : String -> Parser ()
token = map (always ()) << string

fail : Parser a
fail = P (always [])

or : Parser a -> Parser a -> Parser a
or p1 p2 = P <| \s -> (runParser p1 s) ++ (runParser p2 s)

left_or : Parser a -> Parser a -> Parser a
left_or p1 p2 = P <| \s ->
  case runParser p1 s of
    []  -> runParser p2 s
    res -> res

munch : (Char -> Bool) -> Parser String
munch f = P <| \s ->
  let walk acc s =
    case String.uncons s of
      Nothing     -> (String.reverse acc, s)
      Just (c,s') -> if | f c       -> walk (String.cons c acc) s'
                        | otherwise -> (String.reverse acc, s)
  in
  let (pre,suf) = walk "" s.val in
  let start     = s.pos in
  let end       = start `offsetBy` pre in
  [(withInfo pre start end, withPos suf end)]

munch1 : (Char -> Bool) -> Parser String
munch1 f = P <| \s ->
  let [(pre,suf)] = runParser (munch f) s in
  if | s == suf  -> []
     | otherwise -> [(pre,suf)]

skipSpaces : Parser ()
skipSpaces = map (always ()) (munch ((==) ' '))

choice : List (Parser a) -> Parser a
choice ps =
  case ps of
    []     -> fail
    p::ps' -> p `or` choice ps'

between : Parser open_ -> Parser close_ -> Parser a -> Parser a
between p1 p2 p =
  p1 >>= \a ->
  p  >>= \x ->
  p2 >>= \b ->
    returnWithInfo x.val a.start b.end

option : a -> Parser a -> Parser a
option default p = p +++ return default

optional : Parser a -> Parser ()
optional p = (p >>> return ()) +++ return ()

many : Parser a -> Parser (List a)
many p = return [] +++ some p

some : Parser a -> Parser (List a)
some p =
  p      >>= \x  ->
  many p >>= \xs ->
    returnWithInfo (x.val :: xs.val) x.start xs.end

sepBy : Parser a -> Parser sep -> Parser (List a)
sepBy p sep = return [] +++ sepBy1 p sep

sepBy1 : Parser a -> Parser sep -> Parser (List a)
sepBy1 p sep =
  p                >>= \x ->
  many (sep >>> p) >>= \xs ->
    returnWithInfo (x.val :: xs.val) x.start xs.end
 
(+++) = or
(<++) = left_or
(<$>) = map

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

