module LangParser2 (prelude, isPreludeLoc, substOf, parseE, parseV,
                    substPlusOf) where

import String
import Dict
import Char
import Debug

import Lang exposing (..)
import OurParser2 exposing ((>>=),(>>>),(<$>),(+++),(<++))
import OurParser2 as P
import Utils as U
import PreludeGenerated as Prelude

------------------------------------------------------------------------------

(prelude, initK) = freshen_ 1 <| U.fromOk_ <| parseE_ identity Prelude.src

isPreludeLoc : Loc -> Bool
isPreludeLoc (k,_,_) = k < initK

------------------------------------------------------------------------------

-- these top-level freshen and substOf definitions are ugly...

freshen : Exp -> Exp
freshen e = fst (freshen_ initK e)

substPlusOf : Exp -> SubstPlus
substPlusOf e = substOfExps_ Dict.empty [prelude, e]

substOf : Exp -> Subst
substOf = Dict.map (always .val) << substPlusOf

-- this will be done while parsing eventually...

freshen_ : Int -> Exp -> (Exp, Int)
freshen_ k e = (\(e_,k') -> (P.WithInfo e_ e.start e.end, k')) <| case e.val of
  EConst i l wd -> case l of
                     -- (0,b,"") -> (EConst i (k, b, "") wd, k + 1)
                     (0,b,x)  -> (EConst i (k, b, x) wd, k + 1)
                     _        -> Debug.crash "freshen_"
  EBase v    -> (EBase v, k)
  EVar x     -> (EVar x, k)
  EFun ps e  -> let (e',k') = freshen_ k e in (EFun ps e', k')
  EApp f es  ->
    let ((f',es'),k') = U.mapFst U.uncons <| freshenExps k (f::es) in
    (EApp f' es', k')
  EOp op es  -> let (es',k') = freshenExps k es in (EOp op es', k')
  EList es m -> let (es',k') = freshenExps k es in
                case m of
                  Nothing -> (EList es' Nothing, k')
                  Just e  -> let (e',k'') = freshen_ k' e in
                             (EList es' (Just e'), k'')
  EIndList rs -> let (rs', k') = freshenRanges k rs
                 in (EIndList rs', k')
  EIf e1 e2 e3 ->
    let ((e1',e2',e3'),k') = U.mapFst U.unwrap3 <| freshenExps k [e1,e2,e3] in
    (EIf e1' e2' e3', k')
  ELet kind b p e1 e2 ->
    let ((e1',e2'),k') = U.mapFst U.unwrap2 <| freshenExps k [e1,e2] in
    let e1'' = addBreadCrumbs (p, e1') in
    (ELet kind b p e1'' e2', k')
  ECase e l ->
    let es = List.map (snd << .val) l in
    let ((e',es'), k') = U.mapFst U.uncons <| freshenExps k (e::es) in
    let foo blah newE = { blah | val = (fst blah.val, newE) } in
    (ECase e' (List.map2 foo l es'), k')
  EComment s e1 ->
    let (e1',k') = freshen_ k e1 in
    (EComment s e1', k')
  EOption s1 s2 e1 ->
    let (e1',k') = freshen_ k e1 in
    (EOption s1 s2 e1', k')

freshenExps k es =
  List.foldr (\e (es',k') ->
    let (e1,k1) = freshen_ k' e in
    (e1::es', k1)) ([],k) es

freshenRanges : Int -> List ERange -> (List ERange, Int)
freshenRanges k rs =
  List.foldr (\r (rs',k') ->
    let (l,u) = r.val
        (l1,k1) = freshen_ k' l
        (u1,k2) = freshen_ k1 u
    in ({r | val = (l1,u1)} :: rs', k2)
  ) ([],k) rs


addBreadCrumbs (p,e) =
 let ret e_ = P.WithInfo e_ e.start e.end in
 case (p.val, e.val) of
  -- (PVar x _, EConst n (k, b, "") wd) -> ret <| EConst n (k, b, x) wd
  (PVar x _, EConst n (k, b, _) wd) -> ret <| EConst n (k, b, x) wd
  (PList ps mp, EList es me) ->
    case U.maybeZip ps es of
      Nothing  -> ret <| EList es me
      Just pes -> let es' = List.map addBreadCrumbs pes in
                  let me' =
                    case (mp, me) of
                      (Just p1, Just e1) -> Just (addBreadCrumbs (p1,e1))
                      _                  -> me in
                  ret <| EList es' me'
  (_, e_) -> ret e_

-- this will be done while parsing eventually...

substOf_ : SubstPlus -> Exp -> SubstPlus
substOf_ s e = case e.val of
  EConst i l _ ->
    let (k,_,_) = l in
    case Dict.get k s of
      Nothing -> Dict.insert k { e | val = i } s
      Just j  -> if i == j.val then s else Debug.crash "substOf_"
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
  ECase e1 l   -> substOfExps_ s (e1 :: List.map (snd << .val) l)
  ELet _ _ _ e1 e2 -> substOfExps_ s [e1,e2]  -- TODO
  EComment _ e1 -> substOf_ s e1
  EOption _ _ e1 -> substOf_ s e1

substOfExps_ : SubstPlus -> List Exp -> SubstPlus
substOfExps_ s es = case es of
  []     -> s
  e::es' -> substOfExps_ (substOf_ s e) es'

substOfRanges_ s rs = case rs of
  [] -> s
  r :: rs' ->
      let (l,u) = r.val
      in
        substOfRanges_ (substOf_ (substOf_ s l) u) rs'

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
        |> U.fromOk "Parser.parseInt"
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
        |> U.fromOk "Parser.parseFloat"
    in
    P.returnWithInfo n cs1.start cs2.end

parseSign =
  P.option 1 (P.char '-' >>= \c -> P.returnWithInfo (-1) c.start c.end)

parseFrozen =
  string_ frozen <++ string_ thawed <++ string_ assignOnlyOnce <++ string_ unann

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
  let pred c = isAlphaNumeric c || List.member c (String.toList "#., -():=%;[]") in
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
saveToken = white << string_

delimit a b = P.between (token_ a) (token_ b)
parens      = delimit "(" ")"

parseNumV = (\(n,b) -> VConst (n, dummyTrace_ b)) <$> parseNum
-- parseNumE = (\(n,b) -> EConst n (dummyLoc_ b) noWidgetDecl) <$> parseNum

dummyLocWithDebugInfo b n = (0, b, "literal" ++ toString n)

parseNumE =
  parseNum                     >>= \nb ->
  parseMaybeWidgetDecl Nothing >>= \wd ->
    let (n,b) = nb.val in
    -- see other comments about NoWidgetDecl
    case wd.val of
      NoWidgetDecl ->
        P.returnWithInfo (EConst n (dummyLocWithDebugInfo b n) wd) nb.start nb.end
      _ ->
        P.returnWithInfo (EConst n (dummyLocWithDebugInfo b n) wd) nb.start wd.end
{-
        let _ =
          if b == unann then ()
          else () -- could throw parse error here
        in
        P.returnWithInfo (EConst n (dummyLoc_ frozen) wd) nb.start wd.end
-}

    -- let end = case wd.val of {NoWidgetDecl -> nb.end ; _ -> wd.end} in
    -- P.returnWithInfo (EConst n (dummyLoc_ b) wd) nb.start end

parseEBase =
      (always (EBase (Bool True)) <$> P.token "true")
  <++ (always (EBase (Bool False)) <$> P.token "false")
  <++ ((EBase << String) <$> parseStrLit)

parseVBase =
      (always vTrue  <$> P.token "true")
  <++ (always vFalse <$> P.token "false")
  <++ ((VBase << String) <$> parseStrLit)

parsePBase =
      ((PConst << fst) <$> parseNum) -- allowing but ignoring frozen annotation
  <++ (always (PBase (Bool True)) <$> P.token "true")
  <++ (always (PBase (Bool False)) <$> P.token "false")
  <++ ((PBase << String) <$> parseStrLit)

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

parseIndListLiteral p f = parseList "[|" listSep "|]" p f

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
  -- <++ parseExpIndList
  <++ parseLet
  <++ parseDef
  <++ parseApp
  <++ parseCommentExp
  <++ parseLangOption

parseFun =
  parens <|
    token_ "\\" >>>
    parsePats   >>= \ps ->
    parseExp    >>= \e ->
      P.return (EFun ps.val e)

parseWildcard : P.Parser Pat_
parseWildcard = token_ "_" >>> P.return (PVar "_" noWidgetDecl)

parsePVar : P.Parser Pat_
parsePVar =
  (flip PVar noWidgetDecl) <$> (white parseIdent)

-- not using this feature downstream, so turning this off
{-
  white parseIdent              >>= \x ->
  parseMaybeWidgetDecl (Just x) >>= \wd ->
    -- see other comments about NoWidgetDecl
    let end = case wd.val of {NoWidgetDecl -> x.end ; _ -> wd.end } in
    P.returnWithInfo (PVar x.val wd) x.start end
-}

parsePat : P.Parser Pat_
parsePat = P.recursively <| \_ ->
      parsePVar
  <++ parsePBase
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

parseMaybeWidgetDecl : Caption -> P.Parser WidgetDecl_
parseMaybeWidgetDecl cap = P.option NoWidgetDecl (parseWidgetDecl cap)
  -- this would be nicer if/when P.Parser is refactored so that
  -- it doesn't have to wrap everything with WithInfo

parseWidgetDecl : Caption -> P.Parser WidgetDecl_
parseWidgetDecl cap =
  P.token "{"    >>= \open ->  -- P.token, so no leading whitespace
  white parseNum >>= \min ->
  saveToken "-"  >>= \tok ->
  white parseNum >>= \max ->
  token_ "}"     >>= \close ->
  -- for now, not optionally parsing a caption here
    let a = { min | val = fst min.val } in
    let b = { max | val = fst max.val } in
    let wd =
      if List.all isInt [a.val, b.val] then
        let a' = { a | val = floor a.val } in
        let b' = { b | val = floor b.val } in
        IntSlider a' tok b' cap
      else
        NumSlider a tok b cap
    in
    P.returnWithInfo wd open.start close.end

isInt : Float -> Bool
isInt n = n == toFloat (floor n)

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

--Like parseExpList but with parseIndListLIteral instead of pLLOMC
parseExpIndList = parseIndListLiteral parseERange EIndList

-- Only want to allow Number Literals at the moment
parseERange =
  ( white parseNumE >>= \l ->
        token_ ".." >>>
    white parseNumE >>= \u ->
        P.returnWithInfo (l, u) l.start u.end)
  <++
  ( white parseNumE >>= \l ->
        P.returnWithInfo (l,l) l.start l.end)

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
      (always Plus    <$> token_ "+")
  <++ (always Minus   <$> token_ "-")
  <++ (always Mult    <$> token_ "*")
  <++ (always Div     <$> token_ "/")
  <++ (always Lt      <$> token_ "<")
  <++ (always Eq      <$> token_ "=")
  <++ (always Mod     <$> token_ "mod")
  <++ (always Pow     <$> token_ "pow")
  <++ (always ArcTan2 <$> token_ "arctan2")

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
  <++ (always Sqrt    <$> token_ "sqrt")

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
      semi.start e.end

parseLangOption =
  let p = white (P.munch1 (\c -> c /= '\n' && c /= ' ' && c /= ':')) in
  token_ "#"                    >>= \pound ->
  p                             >>= \s1 ->
  token_ ":"                    >>>
  p                             >>= \s2 ->
  P.many (P.satisfy ((==) ' ')) >>>
  P.satisfy ((==) '\n')         >>>
  parseExp                      >>= \e ->
    P.returnWithInfo (EOption s1 s2 e) pound.start e.end
