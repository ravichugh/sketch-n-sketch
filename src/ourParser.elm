module OurParser where

import String
import Debug
import Lazy exposing (..)

type alias Parser_ a = String -> List (a, String)

type Parser a
  = P     (Parser_ a)
  | LazyP (Lazy (Parser_ a))

-- copied from Dandandan/parser
recursively : (() -> Parser a) -> Parser a
recursively thunk = LazyP (lazy (\_ -> runParser (thunk ())))

runParser : Parser a -> String -> List (a, String)
runParser p =
  case p of
    P f         -> f
    LazyP thunk -> force thunk

parse : Parser a -> String -> a
parse p s =
  let crash err = Debug.crash <| "OurParser.parse: " ++ err in
  case runParser p s of
    [(s,"")] -> s
    [(_,_)]  -> crash "incomplete parse"
    []       -> crash ("no parse\n" ++ toString (String.toList s))
    l        -> crash ("ambiguous parse\n" ++ toString l)

return : a -> Parser a
return x = P (\s -> [(x,s)])

bind : Parser a -> (a -> Parser b) -> Parser b
bind pa f = P <| \s ->
  List.concat (List.map (\(a,s') -> runParser (f a) s') (runParser pa s))

sequence : Parser a -> Parser b -> Parser b
sequence p1 p2 = bind p1 (always p2)

(>>=) = bind
(>>>) = sequence

satisfy : (Char -> Bool) -> Parser Char
satisfy f = P <| \s ->
  case String.uncons s of
    Just (c,s') -> if | f c       -> [(c,s')]
                      | otherwise -> []
    Nothing     -> []

char : Char -> Parser Char
char c = satisfy ((==) c)

string : String -> Parser String
string str = P <| \s ->
  if String.startsWith str s
  then let n = String.length str in [(str, String.dropLeft n s)]
  else []

map : (a -> b) -> Parser a -> Parser b
map f p = P <| \s ->
  List.map (\(x,s') -> (f x, s')) (runParser p s)

token : String -> Parser ()
token = map (always ()) << string

end : Parser ()
end = P <| \s -> if | s == "" -> [((),"")] | otherwise -> []

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
      Nothing     -> (acc, s)
      Just (c,s') -> if | f c       -> walk (String.cons c acc) s'
                        | otherwise -> (acc, s)
  in
  [walk "" s]

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
  p1 >>>
  p  >>= \x ->
  p2 >>>
    return x

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
    return (x::xs)

sepBy : Parser a -> Parser sep -> Parser (List a)
sepBy p sep = return [] +++ sepBy1 p sep

sepBy1 : Parser a -> Parser sep -> Parser (List a)
sepBy1 p sep =
  p                >>= \x ->
  many (sep >>> p) >>= \xs ->
    return (x::xs)
  
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

