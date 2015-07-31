module LangParser (prelude, isPreludeLoc, substOf, parseE, parseV) where

import String
import Dict
import Char
import Debug

import Lang exposing (..)
import OurParser exposing ((>>=),(>>>),(<$>),(+++),(<++))
import OurParser as P
import Utils
import PreludeGenerated as Prelude

------------------------------------------------------------------------------

(prelude, initK) = freshen_ 1 <| Utils.fromOk_ <| parseE_ identity Prelude.src

isPreludeLoc : Loc -> Bool
isPreludeLoc (k,_,_) = k < initK

------------------------------------------------------------------------------

-- these top-level freshen and substOf definitions are ugly...

freshen : Exp -> Exp
freshen e = fst (freshen_ initK e)

substOf : Exp -> Subst
substOf e = substOfExps_ Dict.empty [prelude, e]

-- this will be done while parsing eventually...

freshen_ : Int -> Exp -> (Exp, Int)
freshen_ k e = case e of
  EConst i l -> let (0,b,"") = l in (EConst i (k, b, ""), k + 1)
  EBase v    -> (EBase v, k)
  EVar x     -> (EVar x, k)
  EFun ps e  -> let (e',k') = freshen_ k e in (EFun ps e', k')
  EApp f es  -> let (f'::es',k') = freshenExps k (f::es) in (EApp f' es', k')
  EOp op es  -> let (es',k') = freshenExps k es in (EOp op es', k')
  EList es m -> let (es',k') = freshenExps k es in
                case m of
                  Nothing -> (EList es' Nothing, k')
                  Just e  -> let (e',k'') = freshen_ k' e in
                             (EList es' (Just e'), k'')
  EIndList rs -> let (rs', k') = freshenRanges k rs
                 in (EIndList rs', k')
  EIf e1 e2 e3 -> let ([e1',e2',e3'],k') = freshenExps k [e1,e2,e3] in
                  (EIf e1' e2' e3', k')
  ELet kind b p e1 e2 ->
    let ([e1',e2'],k') = freshenExps k [e1,e2] in
    let e1'' = addBreadCrumbs (p, e1') in
    (ELet kind b p e1'' e2', k')
  ECase e l ->
    let es = List.map snd l in
    let (e'::es', k') = freshenExps k (e::es) in
    (ECase e' (Utils.zip (List.map fst l) es'), k')
  EComment s e1 ->
    let (e1',k') = freshen_ k e1 in
    (EComment s e1', k')

freshenExps k es =
  List.foldr (\e (es',k') ->
    let (e1,k1) = freshen_ k' e in
    (e1::es', k1)) ([],k) es

freshenRanges : Int -> List ERange -> (List ERange, Int)
freshenRanges k rs =  
  List.foldr (\(l,u) (rs',k') ->
    let (l1,k1) = freshen_ k' l
        (u1,k2) = freshen_ k1 u
    in ((l1,u1) :: rs', k2)
  ) ([],k) rs

addBreadCrumbs pe = case pe of
  (PVar x, EConst n (k, b, "")) -> EConst n (k, b, x)
  (PList ps mp, EList es me) ->
    case Utils.maybeZip ps es of
      Nothing  -> EList es me
      Just pes -> let es' = List.map addBreadCrumbs pes in
                  let me' =
                    case (mp, me) of
                      (Just p, Just e) -> Just (addBreadCrumbs (p,e))
                      _                -> me in
                  EList es' me'
  (_, e) -> e

-- this will be done while parsing eventually...

substOf_ s e = case e of
  EConst i l ->
    let (k,_,_) = l in
    case Dict.get k s of
      Nothing -> Dict.insert k i s
      Just j  -> if | i == j -> s
  EBase _    -> s
  EVar _     -> s 
  EFun _ e'  -> substOf_ s e'
  EApp f es  -> substOfExps_ s (f::es)
  EOp op es  -> substOfExps_ s es
  EList es m -> case m of
                  Nothing -> substOfExps_ s es
                  Just e  -> substOfExps_ s (e::es)
  EIndList rs -> substOfRanges_ s rs
  EIf e1 e2 e3 -> substOfExps_ s [e1,e2,e3]
  ECase e1 l   -> substOfExps_ s (e1 :: List.map snd l)
  ELet _ _ _ e1 e2 -> substOfExps_ s [e1,e2]  -- TODO
  EComment _ e1 -> substOf_ s e1

substOfExps_ s es = case es of
  []     -> s
  e::es' -> substOfExps_ (substOf_ s e) es'

substOfRanges_ s rs = case rs of
  [] -> s
  (l,u) :: rs' -> substOfRanges_ (substOf_ (substOf_ s l) u) rs'

------------------------------------------------------------------------------

single    x =  [x]
unsingle [x] =  x

isAlpha c        = Char.isLower c || Char.isUpper c
isAlphaNumeric c = Char.isLower c || Char.isUpper c || Char.isDigit c
isWhitespace c   = c == ' ' || c == '\n'

parseInt : P.Parser Int
parseInt =
  P.some (P.satisfy Char.isDigit) >>= \cs ->
    P.return <|
      Utils.fromOk "LangParser.parseInt" <|
        String.toInt (String.fromList cs)

parseFloat =
  P.some (P.satisfy Char.isDigit) >>= \cs1 ->
  P.satisfy ((==) '.')            >>= \c   ->
  P.some (P.satisfy Char.isDigit) >>= \cs2 ->
    P.return <|
      Utils.fromOk "LangParser.parseFloat" <|
        String.toFloat (String.fromList (cs1 ++ (c::cs2)))

parseSign =
  P.option 1 (P.satisfy ((==) '-') >>> P.return (-1))

parseFrozen =
  string_ frozen <++ string_ thawed <++ string_ unann

string_ s = always s <$> P.token s

parseNum : P.Parser (Num, Frozen)
parseNum =
  parseSign                             >>= \i ->
  parseFloat <++ (toFloat <$> parseInt) >>= \n ->
  parseFrozen                           >>= \b ->
    P.return (i * n, b)

-- TODO allow '_', disambiguate from wildcard in parsePat
parseIdent : P.Parser String
parseIdent =
  let pred c = isAlphaNumeric c || c == '_' in
  P.satisfy isAlpha                 >>= \c ->
  P.many (P.satisfy pred)           >>= \cs ->
    P.return (String.fromList (c::cs))

parseStrLit =
  let pred c = isAlphaNumeric c || List.member c (String.toList "#., -():=%") in
  P.between        -- NOTE: not calling delimit...
    (token_ "'")   --   okay to chew up whitespace here,
    (P.token "'")  --   but _not_ here!
    (String.fromList <$> P.many (P.satisfy pred))

oneWhite : P.Parser ()
oneWhite = always () <$> P.satisfy isWhitespace

manySpaces : P.Parser ()
manySpaces = always () <$> P.munch isWhitespace

someSpaces : P.Parser ()
someSpaces = always () <$> P.munch1 isWhitespace

white : P.Parser a -> P.Parser a
white p = manySpaces >>> p

token_ = white << P.token

delimit a b = P.between (token_ a) (token_ b)
parens      = delimit "(" ")"

parseNumV = parseNum >>= \(n,b) -> P.return (VConst (n, dummyTrace_ b))
parseNumE = parseNum >>= \(n,b) -> P.return (EConst n (dummyLoc_ b))

parseEBase =
      (always eTrue  <$> P.token "true")
  <++ (always eFalse <$> P.token "false")
  <++ ((EBase << String) <$> parseStrLit)

parseVBase =
      (always vTrue  <$> P.token "true")
  <++ (always vFalse <$> P.token "false")
  <++ ((VBase << String) <$> parseStrLit)

parseList_ sepBy start sep end p f =
  token_ start          >>>
  sepBy p sep           >>= \xs ->
  token_ end            >>>
    P.return (f xs)

parseList :
  String -> P.Parser sep -> String -> P.Parser a -> (List a -> b) -> P.Parser b

parseList  = parseList_ P.sepBy
parseList1 = parseList_ P.sepBy1

parseListLiteral p f = parseList "[" listSep "]" p f

listSep = P.token " " <++ P.token "\n" -- duplicating isWhitespace...

parseMultiCons p f =
  parseList1 "[" listSep "|" p identity >>= \xs ->
  p                                     >>= \y ->
  token_ "]"                            >>>
    P.return (f xs y)

parseListLiteralOrMultiCons p f g = P.recursively <| \_ ->
      (parseListLiteral p f)
  <++ (parseMultiCons p g)

parseIndListLiteral p f = parseList "[|" listSep "|]" p f

parseV = P.parse <|
  parseVal    >>= \v ->
  white P.end >>>
    P.return v

parseVal : P.Parser Val
parseVal = P.recursively <| \_ ->
      white parseNumV
  <++ white parseVBase
  <++ parseValList

-- parseValList = parseList "[" " " "]" parseVal VList
parseValList = parseListLiteral parseVal VList

parseE_ f = P.parse <|
  parseExp    >>= \e ->
  white P.end >>>
    P.return (f e)

parseE : String -> Result String Exp
parseE = parseE_ freshen

parseVar = EVar <$> (white parseIdent)

parseExp : P.Parser Exp
parseExp = P.recursively <| \_ ->
      white parseNumE
  <++ white parseEBase
  <++ parseVar
  <++ parseFun
  <++ parseConst
  <++ parseUnop
  <++ parseBinop
  <++ parseIf
  <++ parseCase
  <++ parseExpList
  <++ parseExpIndList
  <++ parseLet
  <++ parseDef
  <++ parseApp
  <++ parseCommentExp

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
  <++ (parseList1 "(" listSep ")" parsePat identity)

parseApp =
  parens <|
    parseExp     >>= \f ->
    oneWhite     >>>
    parseExpArgs >>= \es ->
      P.return (EApp f es)

parseExpArgs = parseList1 "" listSep "" parseExp identity

parseExpList =
  parseListLiteralOrMultiCons
    parseExp (\xs -> EList xs Nothing) (\xs y -> EList xs (Just y))

--Like parseExpList but with parseIndListLIteral instead of pLLOMC
parseExpIndList = parseIndListLiteral parseERange EIndList

-- Only want to allow Number Literals at the moment
parseERange =
  ( white parseNumE >>= \l ->
    white (token_ "..") >>>
    white parseNumE >>= \u ->
        P.return (l, u) )
  <++
  ( white parseNumE >>= \l ->
        P.return (l,l) )

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
      P.return (ELet Let b p e1 e2)

parseDefRec =
      (always True  <$> token_ "defrec")
  <++ (always False <$> token_ "def")

parseDef =
  parens (
    parseDefRec >>= \b ->
    parsePat >>= \p ->
    parseExp >>= \e1 -> P.return (b,p,e1)
  ) >>= \(b,p,e1) ->
  oneWhite >>>
  parseExp >>= \e2 ->
    P.return (ELet Def b p e1 e2)

parseBinop =
  parens <|
    parseBOp >>= \op ->
    parseExp >>= \e1 ->
    oneWhite >>>
    parseExp >>= \e2 ->
      P.return (EOp op [e1,e2])

parseBOp =
      (always Plus  <$> token_ "+")
  <++ (always Minus <$> token_ "-")
  <++ (always Mult  <$> token_ "*")
  <++ (always Div   <$> token_ "/")
  <++ (always Lt    <$> token_ "<")
  <++ (always Eq    <$> token_ "=")

parseUnop =
  parens <|
    parseUOp >>= \op ->
    parseExp >>= \e1 ->
      P.return (EOp op [e1])

parseUOp =
      (always Cos     <$> token_ "cos")
  <++ (always Sin     <$> token_ "sin")
  <++ (always ArcCos  <$> token_ "arccos")
  <++ (always ArcSin  <$> token_ "arcsin")
  <++ (always Floor   <$> token_ "floor")
  <++ (always Ceil    <$> token_ "ceiling")
  <++ (always Round   <$> token_ "round")
  <++ (always ToStr   <$> token_ "toString")

parseConst =
  parens <|
    parseNullOp >>= \op ->
      P.return (EOp op [])

parseNullOp =
      (always Pi      <$> token_ "pi")

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
  parseList1 "" listSep "" parseBranch identity

parseBranch =
  parens <|
    parsePat >>= \p -> oneWhite >>> parseExp >>= \e -> P.return (p,e)

parseCommentExp =
  token_ ";" >>>
  P.many (P.satisfy ((/=) '\n')) >>= \cs ->
  P.satisfy ((==) '\n') >>>
  parseExp >>= \e ->
    P.return (EComment (String.fromList cs) e)

