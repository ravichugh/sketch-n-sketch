module LangParser where

import List ((::))
import List
import String
import Dict
import Char
import Debug

import Lang (..)
import OurParser ((>>=),(>>>),(<$>),(+++),(<++))
import OurParser as P

------------------------------------------------------------------------------

-- this will be done while parsing eventually...

freshen : Int -> Exp -> (Exp, Int)
freshen k e = case e of
  EConst i _ -> (EConst i k, k + 1)
  EVar x     -> (EVar x, k)
  EFun x e   -> let (e',k') = freshen k e in (EFun x e', k')
  EApp e1 e2 -> let ([e1',e2'],k') = freshenExps k [e1,e2] in (EApp e1' e2', k')
  EOp op es  -> let (es',k') = freshenExps k es in (EOp op es', k')
  EList es   -> let (es',k') = freshenExps k es in (EList es', k')
  ELet x e1 e2 -> let ([e1',e2'],k') = freshenExps k [e1,e2] in (ELet x e1' e2', k')

freshenExps k es =
  List.foldr (\e (es',k') ->
    let (e1,k1) = freshen k' e in
    (e1::es', k1)) ([],k) es

-- this will be done while parsing eventually...

substOf_ s e = case e of
  EConst i l -> case Dict.get l s of
                  Nothing -> Dict.insert l i s
                  Just j  -> if | i == j -> s
  EVar _     -> s 
  EFun _ _   -> s   -- not recursing into lambdas
  EApp e1 e2 -> substOfExps_ s [e1,e2]
  EOp op es  -> substOfExps_ s es
  EList es   -> substOfExps_ s es
  ELet x e1 e2 -> substOfExps_ s [e1,e2]  -- TODO

substOfExps_ s es = case es of
  []     -> s
  e::es' -> substOfExps_ (substOf_ s e) es'

substOf : Exp -> Subst
substOf = substOf_ Dict.empty


------------------------------------------------------------------------------

isAlpha c        = Char.isLower c || Char.isUpper c
isAlphaNumeric c = Char.isLower c || Char.isUpper c || Char.isDigit c
isWhitespace c   = c == ' ' || c == '\n'

unsafeToInt s =
  case String.toInt s of
    Ok i    -> i
    Err err -> Debug.crash err

parseInt : P.Parser Int
parseInt = (unsafeToInt << String.fromList) <$> P.some (P.satisfy Char.isDigit)
             -- TODO negative

parseIdent : P.Parser String
parseIdent =
  P.satisfy isAlpha                 >>= \c ->
  P.many (P.satisfy isAlphaNumeric) >>= \cs ->
    P.return (String.fromList (c::cs))

oneWhite : P.Parser ()
oneWhite = always () <$> P.satisfy isWhitespace

manySpaces : P.Parser ()
manySpaces = always () <$> P.many oneWhite

someSpaces : P.Parser ()
someSpaces = always () <$> P.some oneWhite

white : P.Parser a -> P.Parser a
white p = manySpaces >>> p

token_ = white << P.token

delimit a b = P.between (token_ a) (token_ b)
parens      = delimit "(" ")"

parseIntV = flip VConst dummyTrace <$> parseInt
parseIntE = flip EConst dummyLoc   <$> parseInt

parseList p f =
  token_ "["              >>>
  P.sepBy p (P.token " ") >>= \xs ->
  token_ "]"              >>>
    P.return (f xs)

parseV = P.parse <|
  parseVal    >>= \v ->
  white P.end >>>
    P.return v

parseVal : P.Parser Val
parseVal = P.recursively <| \_ ->
      white parseIntV
  +++ parseValList0
  <++ parseValList

parseValList = parseList parseVal VList

parseValList0 =
  always (VList []) <$> P.token "[]"

parseE = P.parse <|
  parseExp    >>= \e ->
  white P.end >>>
    P.return e

parseVar = EVar <$> (white parseIdent)

parseExp : P.Parser Exp
parseExp = P.recursively <| \_ ->
      white parseIntE
  +++ parseVar
  +++ parseFun
  +++ parseApp
  +++ parseExpList
  +++ parseLet
  +++ parseBinop

parseFun =
  parens <|
    token_ "fn"      >>>
    white parseIdent >>= \x ->
    parseExp         >>= \e ->
      P.return (EFun x e)

parseApp =
  parens <|
    parseExp >>= \e1 ->
    oneWhite >>>
    parseExp >>= \e2 ->
      P.return (EApp e1 e2)

parseExpList = parseList parseExp EList

parseLet =
  parens <|
    token_ "let"     >>>
    white parseIdent >>= \x ->
    parseExp         >>= \e1 ->
    oneWhite         >>>
    parseExp         >>= \e2 ->
      P.return (ELet x e1 e2)

parseBinop =
  parens <|
    token_ "+" >>>
    parseExp   >>= \e1 ->
    oneWhite   >>>
    parseExp   >>= \e2 ->
      P.return (EOp Plus [e1,e2])

