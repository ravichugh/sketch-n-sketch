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
import Utils

------------------------------------------------------------------------------

-- this will be done while parsing eventually...

freshen : Int -> Exp -> (Exp, Int)
freshen k e = case e of
  EConst i _ -> (EConst i k, k + 1)
  EBase v    -> (EBase v, k)
  EVar x     -> (EVar x, k)
  EFun ps e  -> let (e',k') = freshen k e in (EFun ps e', k')
  EApp f es  -> let (f'::es',k') = freshenExps k (f::es) in (EApp f' es', k')
  EOp op es  -> let (es',k') = freshenExps k es in (EOp op es', k')
  EList es m -> let (es',k') = freshenExps k es in
                case m of
                  Nothing -> (EList es' Nothing, k')
                  Just e  -> let (e',k'') = freshen k' e in
                             (EList es' (Just e'), k'')
  EIf e1 e2 e3 -> let ([e1',e2',e3'],k') = freshenExps k [e1,e2,e3] in
                  (EIf e1' e2' e3', k')
  ELet b x e1 e2 ->
    let ([e1',e2'],k') = freshenExps k [e1,e2] in
    (ELet b x e1' e2', k')
  ECase e l ->
    let es = List.map snd l in
    let (e'::es', k') = freshenExps k (e::es) in
    (ECase e' (Utils.zip (List.map fst l) es'), k')

freshenExps k es =
  List.foldr (\e (es',k') ->
    let (e1,k1) = freshen k' e in
    (e1::es', k1)) ([],k) es

-- this will be done while parsing eventually...

substOf_ s e = case e of
  EConst i l -> case Dict.get l s of
                  Nothing -> Dict.insert l i s
                  Just j  -> if | i == j -> s
  EBase _    -> s
  EVar _     -> s 
  EFun _ e'  -> substOf_ s e'
  EApp f es  -> substOfExps_ s (f::es)
  EOp op es  -> substOfExps_ s es
  EList es m -> case m of
                  Nothing -> substOfExps_ s es
                  Just e  -> substOfExps_ s (e::es)
  EIf e1 e2 e3 -> substOfExps_ s [e1,e2,e3]
  ECase e1 l   -> substOfExps_ s (e1 :: List.map snd l)
  ELet _ _ e1 e2 -> substOfExps_ s [e1,e2]  -- TODO

substOfExps_ s es = case es of
  []     -> s
  e::es' -> substOfExps_ (substOf_ s e) es'

substOf : Exp -> Subst
substOf = substOf_ Dict.empty


------------------------------------------------------------------------------

single    x =  [x]
unsingle [x] =  x

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

-- TODO allow '_', disambiguate from wildcard in parsePat
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

parseEBase =
      (always eTrue  <$> P.token "true")
  <++ (always eFalse <$> P.token "false")

parseVBase =
      (always vTrue  <$> P.token "true")
  <++ (always vFalse <$> P.token "false")

parseList_ sepBy start sep end p f =
  token_ start          >>>
  sepBy p (P.token sep) >>= \xs ->
  token_ end            >>>
    P.return (f xs)

parseList :
  String -> String -> String -> P.Parser a -> (List a -> b) -> P.Parser b

parseList  = parseList_ P.sepBy
parseList1 = parseList_ P.sepBy1

parseListLiteral p f = parseList "[" " " "]" p f

parseMultiCons p f =
  parseList1 "[" " " "|" p identity >>= \xs ->
  p                                 >>= \y ->
  token_ "]"                        >>>
    P.return (f xs y)

parseListLiteralOrMultiCons p f g = P.recursively <| \_ ->
      (parseListLiteral p f)
  <++ (parseMultiCons p g)

parseV = P.parse <|
  parseVal    >>= \v ->
  white P.end >>>
    P.return v

parseVal : P.Parser Val
parseVal = P.recursively <| \_ ->
      white parseIntV
  <++ white parseVBase
  <++ parseValList

-- parseValList = parseList "[" " " "]" parseVal VList
parseValList = parseListLiteral parseVal VList

parseE = P.parse <|
  parseExp    >>= \e ->
  white P.end >>>
    P.return e

parseVar = EVar <$> (white parseIdent)

parseExp : P.Parser Exp
parseExp = P.recursively <| \_ ->
      white parseIntE
  <++ white parseEBase
  <++ parseVar
  <++ parseFun
  <++ parseBinop
  <++ parseIf
  <++ parseCase
  <++ parseExpList
  <++ parseLet
  <++ parseApp

parseFun =
  parens <|
    token_ "\\" >>>
    parsePats   >>= \ps ->
    parseExp    >>= \e ->
      P.return (EFun ps e)

parseWildcard = token_ "_" >>> P.return (PVar "_")

parsePat = P.recursively <| \_ ->
      (white parseIdent >>= (PVar >> P.return))
  <++ parseWildcard
  <++ parsePatList

parsePatList =
  parseListLiteralOrMultiCons
    parsePat (\xs -> PList xs Nothing) (\xs y -> PList xs (Just y))

parsePats =
      (parsePat >>= (single >> P.return))
  <++ (parseList1 "(" " " ")" parsePat identity)

parseApp =
  parens <|
    parseExp     >>= \f ->
    oneWhite     >>>
    parseExpArgs >>= \es ->
      P.return (EApp f es)

parseExpArgs = parseList1 "" " " "" parseExp identity

parseExpList =
  parseListLiteralOrMultiCons
    parseExp (\xs -> EList xs Nothing) (\xs y -> EList xs (Just y))

parseRec =
      (always True  <$> token_ "letrec")
  <++ (always False <$> token_ "let")

parseLet =
  parens <|
    parseRec >>= \b ->
    parsePat >>= \p ->
    parseExp >>= \e1 ->
    oneWhite >>>
    parseExp >>= \e2 ->
      P.return (ELet b p e1 e2)

parseBinop =
  parens <|
    parseOp  >>= \op ->
    parseExp >>= \e1 ->
    oneWhite >>>
    parseExp >>= \e2 ->
      P.return (EOp op [e1,e2])

parseOp =
      (always Plus  <$> token_ "+")
  <++ (always Minus <$> token_ "-")
  <++ (always Mult  <$> token_ "*")
  <++ (always Lt    <$> token_ "<")

parseIf =
  parens <|
    token_ "if" >>>
    oneWhite    >>>
    parseExp    >>= \e1 ->
    oneWhite    >>>
    parseExp    >>= \e2 ->
    oneWhite    >>>
    parseExp    >>= \e3 ->
      P.return (EIf e1 e2 e3)

parseCase =
  parens <|
    token_ "case" >>>
    oneWhite      >>>
    parseExp      >>= \e ->
    oneWhite      >>>
    parseBranches >>= \l ->
      P.return (ECase e l)

parseBranches = P.recursively <| \_ ->
  parseList1 "" " " "" parseBranch identity

parseBranch =
  parens <|
    parsePat >>= \p -> oneWhite >>> parseExp >>= \e -> P.return (p,e)

