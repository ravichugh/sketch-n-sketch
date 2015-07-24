module LangParser2 (prelude, isPreludeLoc, substOf, parseE, parseV) where

import String
import Dict
import Char
import Debug

import Lang exposing (..)
import OurParser2 exposing ((>>=),(>>>),(<$>),(+++),(<++))
import OurParser2 as P
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
freshen_ k e = (\(e_,k') -> (P.WithInfo e_ e.start e.end, k')) <| case e.val of
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
  EIf e1 e2 e3 -> let ([e1',e2',e3'],k') = freshenExps k [e1,e2,e3] in
                  (EIf e1' e2' e3', k')
  ELet kind b p e1 e2 ->
    let ([e1',e2'],k') = freshenExps k [e1,e2] in
    let e1'' = addBreadCrumbs (p, e1') in
    (ELet kind b p e1'' e2', k')
  ECase e l ->
    let es = List.map (snd << .val) l in
    let (e'::es', k') = freshenExps k (e::es) in
    let foo blah newE = { blah | val <- (fst blah.val, newE) } in
    (ECase e' (List.map2 foo l es'), k')
  EComment s e1 ->
    let (e1',k') = freshen_ k e1 in
    (EComment s e1', k')

freshenExps k es =
  List.foldr (\e (es',k') ->
    let (e1,k1) = freshen_ k' e in
    (e1::es', k1)) ([],k) es

addBreadCrumbs (p,e) =
 let ret e_ = P.WithInfo e_ e.start e.end in
 case (p.val, e.val) of
  (PVar x, EConst n (k, b, "")) -> ret <| EConst n (k, b, x)
  (PList ps mp, EList es me) ->
    case Utils.maybeZip ps es of
      Nothing  -> ret <| EList es me
      Just pes -> let es' = List.map addBreadCrumbs pes in
                  let me' =
                    case (mp, me) of
                      (Just p1, Just e1) -> Just (addBreadCrumbs (p1,e1))
                      _                  -> me in
                  ret <| EList es' me'
  (_, e_) -> ret e_

-- this will be done while parsing eventually...

substOf_ s e = case e.val of
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
  EIf e1 e2 e3 -> substOfExps_ s [e1,e2,e3]
  ECase e1 l   -> substOfExps_ s (e1 :: List.map (snd << .val) l)
  ELet _ _ _ e1 e2 -> substOfExps_ s [e1,e2]  -- TODO
  EComment _ e1 -> substOf_ s e1

substOfExps_ s es = case es of
  []     -> s
  e::es' -> substOfExps_ (substOf_ s e) es'


------------------------------------------------------------------------------

-- single    x  =  [x]
-- unsingle [x] =  x

unwrapChars : P.WithInfo (List (P.WithInfo Char)) -> List Char
unwrapChars = List.map .val << .val

isAlpha c        = Char.isLower c || Char.isUpper c
isAlphaNumeric c = Char.isLower c || Char.isUpper c || Char.isDigit c
isWhitespace c   = c == ' ' || c == '\n'

parseInt : P.Parser Int
parseInt =
  P.some (P.satisfy Char.isDigit) >>= \cs ->
    let i =
      unwrapChars cs
        |> String.fromList
        |> String.toInt
        |> Utils.fromOk "Parser.parseInt"
    in
    P.returnWithInfo i cs.start cs.end

parseFloat : P.Parser Float
parseFloat =
  P.some (P.satisfy Char.isDigit) >>= \cs1 ->
  P.satisfy ((==) '.')            >>= \c   ->
  P.some (P.satisfy Char.isDigit) >>= \cs2 ->
    let n =
      unwrapChars cs1 ++ [c.val] ++ unwrapChars cs2
        |> String.fromList
        |> String.toFloat
        |> Utils.fromOk "Parser.parseFloat"
    in
    P.returnWithInfo n cs1.start cs2.end

parseSign =
  P.option 1 (P.char '-' >>= \c -> P.returnWithInfo (-1) c.start c.end)

parseFrozen =
  string_ frozen <++ string_ thawed <++ string_ unann

string_ s = always s <$> P.token s

parseNum : P.Parser (Num, Frozen)
parseNum =
  parseSign                             >>= \i ->
  parseFloat <++ (toFloat <$> parseInt) >>= \n ->
  parseFrozen                           >>= \b ->
    P.returnWithInfo (i.val * n.val, b.val) i.start b.end

-- TODO allow '_', disambiguate from wildcard in parsePat
parseIdent : P.Parser String
parseIdent =
  let pred c = isAlphaNumeric c || c == '_' in
  P.satisfy isAlpha                 >>= \c ->
  P.many (P.satisfy pred)           >>= \cs ->
    let x = String.fromList (c.val :: unwrapChars cs) in
    P.returnWithInfo x c.start cs.end

parseStrLit =
  let pred c = isAlphaNumeric c || List.member c (String.toList "#., -():=%") in
  P.between        -- NOTE: not calling delimit...
    (token_ "'")   --   okay to chew up whitespace here,
    (P.token "'")  --   but _not_ here!
    ((String.fromList << List.map .val) <$> P.many (P.satisfy pred))

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

parseNumV = (\(n,b) -> VConst (n, dummyTrace_ b)) <$> parseNum
parseNumE = (\(n,b) -> EConst n (dummyLoc_ b)) <$> parseNum

parseEBase =
      (always (EBase (Bool True)) <$> P.token "true")
  <++ (always (EBase (Bool False)) <$> P.token "false")
  <++ ((EBase << String) <$> parseStrLit)

parseVBase =
      (always vTrue  <$> P.token "true")
  <++ (always vFalse <$> P.token "false")
  <++ ((VBase << String) <$> parseStrLit)

parseList_
   : (P.Parser a -> P.Parser sep -> P.Parser (List (P.WithInfo a)))
  -> String -> P.Parser sep -> String -> P.Parser a -> (List (P.WithInfo a) -> b)
  -> P.Parser b
parseList_ sepBy start sep end p f =
  token_ start          >>= \a ->
  sepBy p sep           >>= \xs ->
  token_ end            >>= \b ->
    P.returnWithInfo (f xs.val) a.start b.end

parseList
   : String -> P.Parser sep -> String -> P.Parser a -> (List (P.WithInfo a) -> b)
  -> P.Parser b

parseList  = parseList_ P.sepBy
parseList1 = parseList_ P.sepBy1

parseListLiteral p f = parseList "[" listSep "]" p f

listSep = P.token " " <++ P.token "\n" -- duplicating isWhitespace...

parseMultiCons
   : P.Parser a -> (List (P.WithInfo a) -> P.WithInfo a -> b)
  -> P.Parser b
parseMultiCons p f =
  parseList1 "[" listSep "|" p identity >>= \xs ->
  p                                     >>= \y ->
  token_ "]"                            >>= \blah ->
    P.returnWithInfo (f xs.val y) xs.start blah.end

parseListLiteralOrMultiCons
   : P.Parser a
  -> (List (P.WithInfo a) -> b)
  -> (List (P.WithInfo a) -> P.WithInfo a -> b)
  -> P.Parser b
parseListLiteralOrMultiCons p f g = P.recursively <| \_ ->
      (parseListLiteral p f)
  <++ (parseMultiCons p g)

parseV = P.parse <|
  parseVal    >>= \v ->
  white P.end >>>
    P.return v -- not tracking v.pos

parseVal : P.Parser Val
parseVal = P.recursively <| \_ ->
      white parseNumV
  <++ white parseVBase
  <++ parseValList

parseValList = parseListLiteral parseVal (VList << List.map .val)

parseE_ : (Exp -> Exp) -> String -> Result String Exp
parseE_ f = P.parse <|
  parseExp    >>= \e ->
  white P.end >>>
    P.returnWithInfo (f e).val e.start e.end

parseE : String -> Result String Exp
parseE = parseE_ freshen

parseVar = EVar <$> (white parseIdent)

parseExp : P.Parser Exp_
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
  <++ parseLet
  <++ parseDef
  <++ parseApp
  <++ parseCommentExp

parseFun =
  parens <|
    token_ "\\" >>>
    parsePats   >>= \ps ->
    parseExp    >>= \e ->
      P.return (EFun ps.val e)

parseWildcard : P.Parser Pat_
parseWildcard = token_ "_" >>> P.return (PVar "_")

parsePat : P.Parser Pat_
parsePat = P.recursively <| \_ ->
      (PVar <$> white parseIdent)
  <++ parseWildcard
  <++ parsePatList

parsePatList : P.Parser Pat_
parsePatList =
  parseListLiteralOrMultiCons
    parsePat (\xs -> PList xs Nothing) (\xs y -> PList xs (Just y))

parsePats : P.Parser (List Pat)
parsePats =
      (parsePat >>= \p -> P.returnWithInfo [p] p.start p.end)
  <++ (parseList1 "(" listSep ")" parsePat identity)

parseApp =
  parens <|
    parseExp     >>= \f ->
    oneWhite     >>>
    parseExpArgs >>= \es ->
      P.return (EApp f es.val)

parseExpArgs = parseList1 "" listSep "" parseExp identity

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
      P.return (ELet Let b.val p e1 e2)

parseDefRec =
      (always True  <$> token_ "defrec")
  <++ (always False <$> token_ "def")

parseDef =
  parens (
    parseDefRec >>= \b ->
    parsePat >>= \p ->
    parseExp >>= \e1 -> P.return (b,p,e1)
  ) >>= \def ->
  let (b,p,e1) = def.val in
  oneWhite >>>
  parseExp >>= \e2 ->
    P.returnWithInfo (ELet Def b.val p e1 e2) def.start def.end

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
      P.return (ECase e l.val)

parseBranches : P.Parser (List (P.WithInfo (Pat, Exp)))
parseBranches = P.recursively <| \_ ->
  parseList1 "" listSep "" parseBranch identity

parseBranch : P.Parser (Pat, Exp)
parseBranch =
  parens <|
    parsePat >>= \p -> oneWhite >>> parseExp >>= \e -> P.return (p,e)

parseCommentExp =
  token_ ";"                     >>= \semi ->
  P.many (P.satisfy ((/=) '\n')) >>= \cs ->
  P.satisfy ((==) '\n')          >>= \newline ->
  parseExp                       >>= \e ->
    P.returnWithInfo
      (EComment (String.fromList (unwrapChars cs)) e)
      semi.start newline.end
