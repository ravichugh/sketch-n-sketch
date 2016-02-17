module Lang where

import String
import Debug
import Dict
import Set
import Debug

import OurParser2 as P
import Utils

------------------------------------------------------------------------------

type alias Loc = (LocId, Frozen, Ident)  -- "" rather than Nothing b/c comparable
type alias LocId = Int
type alias Ident = String
type alias Num = Float
  -- may want to preserve decimal point for whole floats,
  -- so that parse/unparse are inverses and for WidgetDecls

type alias Frozen = String -- b/c comparable
(frozen, unann, thawed, assignOnlyOnce) = ("!", "", "?", "~")

type alias LocSet = Set.Set Loc

type alias Pat    = P.WithInfo Pat_
type alias Exp    = P.WithInfo Exp_
type alias Op     = P.WithInfo Op_
type alias Branch = P.WithInfo Branch_
type alias Range  = P.WithInfo Range_

-- TODO add constant literals to patterns, and match 'svg'
type Pat_
  = PVar WS Ident WidgetDecl
  | PConst WS Num
  | PBase WS BaseVal
  | PList WS (List Pat) WS (Maybe Pat) WS

type Op_
  -- nullary ops
  = Pi
  -- unary ops
  | Cos | Sin | ArcCos | ArcSin
  | Floor | Ceil | Round
  | ToStr
  | Sqrt
  -- binary ops
  | Plus | Minus | Mult | Div
  | Lt | Eq
  | Mod | Pow
  | ArcTan2
  -- internal ops
  | RangeOffset Int

type alias EId  = Int
type alias Exp_ = { e__ : Exp__, eid : EId }

type Exp__
  = EConst WS Num Loc WidgetDecl
  | EBase WS BaseVal
  | EVar WS Ident
  | EFun WS (List Pat) Exp WS -- WS: before (, before )
  | EApp WS Exp (List Exp) WS
  | EOp WS Op (List Exp) WS
  | EList WS (List Exp) WS (Maybe Exp) WS
  | EIndList WS (List Range) WS
  | EIf WS Exp Exp Exp WS
  | ECase WS Exp (List Branch) WS
  | ELet WS LetKind Rec Pat Exp Exp WS
  | EComment WS String Exp
  | EOption WS (P.WithInfo String) WS (P.WithInfo String) Exp

    -- EFun [] e     impossible
    -- EFun [p] e    (\p. e)
    -- EFun ps e     (\(p1 ... pn) e) === (\p1 (\p2 (... (\pn e) ...)))

    -- EApp f []     impossible
    -- EApp f [x]    (f x)
    -- EApp f xs     (f x1 ... xn) === ((... ((f x1) x2) ...) xn)

type alias WS = String

type Branch_ = Branch_ WS Pat Exp WS

type LetKind = Let | Def
type alias Rec = Bool

type Range_ -- right now, Exps are always EConsts
  = Interval Exp WS Exp
  | Point Exp

type alias WidgetDecl = P.WithInfo WidgetDecl_

type WidgetDecl_
  = IntSlider (P.WithInfo Int) Token (P.WithInfo Int) Caption
  | NumSlider (P.WithInfo Num) Token (P.WithInfo Num) Caption
  | NoWidgetDecl -- rather than Nothing, to work around parser types

type Widget
  = WIntSlider Int Int String Int Loc
  | WNumSlider Num Num String Num Loc

type alias Widgets = List Widget

type alias Token = P.WithInfo String

type alias Caption = Maybe (P.WithInfo String)

type alias VTrace = List EId
type alias Val    = { v_ : Val_, vtrace : VTrace }

type Val_
  = VConst NumTr
  | VBase BaseVal
  | VClosure (Maybe Ident) Pat Exp Env
  | VList (List Val)
  | VHole Int

type alias NumTr = (Num, Trace)

-- TODO combine all base exps/vals into PBase/EBase/VBase
type BaseVal -- unlike Ints, these cannot be changed by Sync
  = Bool Bool
  | String String
  | Star -- placeholder used by sync

type Trace = TrLoc Loc | TrOp Op_ (List Trace)

type FeatureEquation
  = EqnVal Val
  | EqnOp Op_ (List FeatureEquation)

type alias Env = List (Ident, Val)


------------------------------------------------------------------------------
-- Unparsing

strBaseVal v = case v of
  Bool True  -> "true"
  Bool False -> "false"
  String s   -> "\'" ++ s ++ "\'"
  Star       -> "X"

strRange : Bool -> Int -> Range -> String
strRange showLocs k r = case r.val of
  Point e           -> sExp_ showLocs k e
  Interval e1 ws e2 -> sExp_ showLocs k e1 ++ ".." ++ sExp_ showLocs k e2

strVal     = strVal_ False
strValLocs = strVal_ True

strNum     = toString
-- strNumDot  = strNum >> (\s -> if String.contains "[.]" s then s else s ++ ".0")

strNumTrunc k =
  strNum >> (\s -> if String.length s > k then String.left k s ++ ".." else s)

strVal_ : Bool -> Val -> String
strVal_ showTraces v =
  let foo = strVal_ showTraces in
  let sTrace = if showTraces then Utils.braces (toString v.vtrace) else "" in
  sTrace ++
  case v.v_ of
    VConst (i,tr)    -> strNum i ++ if showTraces then Utils.braces (strTrace tr) else ""
    VBase b          -> strBaseVal b
    VClosure _ _ _ _ -> "<fun>"
    VList vs         -> Utils.bracks (String.join " " (List.map foo vs))
    VHole i          -> "HOLE_" ++ toString i

strOp op = case op of
  Plus          -> "+"
  Minus         -> "-"
  Mult          -> "*"
  Div           -> "/"
  Lt            -> "<"
  Eq            -> "="
  Pi            -> "pi"
  Cos           -> "cos"
  Sin           -> "sin"
  ArcCos        -> "arccos"
  ArcSin        -> "arcsin"
  ArcTan2       -> "arctan2"
  Floor         -> "floor"
  Ceil          -> "ceiling"
  Round         -> "round"
  ToStr         -> "toString"
  Sqrt          -> "sqrt"
  Mod           -> "mod"
  Pow           -> "pow"
  RangeOffset i -> "[[rangeOffset " ++ toString i ++ "]]"

strLoc (k, b, mx) =
  "k" ++ toString k ++ (if mx == "" then "" else "_" ++ mx) ++ b

strTrace tr = case tr of
  TrLoc l   -> strLoc l
  TrOp op l ->
    Utils.parens (String.concat
      [strOp op, " ", String.join " " (List.map strTrace l)])

strPat p = case p.val of
  PVar _ x _       -> x -- TODO strWidgetDecl wd, but not called anyway
  PList _ ps _ m _ -> let s = Utils.spaces (List.map strPat ps) in
                case m of
                  Nothing   -> Utils.bracks s
                  Just rest -> Utils.bracks (s ++ " | " ++ strPat rest)
  _ -> Debug.crash "strPat"

tab k = String.repeat k "  "

sExpK k     = (++) (tab k) << sExp_ False k
sExpLocsK k = (++) (tab k) << sExp_ True k
sExp        = sExpK 0
sExpLocs    = sExpLocsK 0

sExp_ : Bool -> Int -> Exp -> String
sExp_ showLocs k e =
  let foo = sExp_ showLocs in
  let indent = maybeIndent showLocs k in
  let sTrace = if showLocs then Utils.braces (toString e.val.eid) else "" in
  sTrace ++
  case e.val.e__ of
    EBase _ v -> strBaseVal v
    EConst _ i l _ -> -- TODO
      let (_,b,_) = l in
      toString i
        ++ b
        ++ if showLocs then Utils.braces (strLoc l) else ""
    EVar _ x -> x
    EFun _ [p] e _ ->
      Utils.parens <| "\\" ++ strPat p ++ indent e
    EFun _ ps e _ ->
      let args = Utils.spaces (List.map strPat ps) in
      Utils.parens <| "\\" ++ Utils.parens args ++ indent e
    EApp _ e1 [e2] _ ->
      Utils.parens <| foo k e1 ++ " " ++ indent e2
    EApp _ e1 es _ ->
      Utils.parens <|
        let s1 = foo k e1
            ss = List.map (foo (k+1)) es
            s2 = Utils.spaces ss in
        if fitsOnLine s2
        then s1 ++ " " ++ s2
        else String.join ("\n" ++ tab (k+1)) (s1::ss)
    EOp _ op es _ ->
      Utils.parens <| String.join " " (strOp op.val :: List.map (foo k) es)
    EIf _ e1 e2 e3 _ ->
      let s =
        Utils.parens <| Utils.spaces [ "if", foo k e1, foo k e2, foo k e3 ] in
      if fitsOnLine s then s
      else
      Utils.parens <|
        "if " ++ foo k e1 ++ "\n" ++
          tab (k+1) ++ foo (k+1) e2 ++ "\n" ++
          tab (k+1) ++ foo (k+1) e3
    EList _ es _ mrest _ ->
      Utils.bracks <|
        let ss = List.map (foo k) es
            s  = Utils.spaces ss in
        if fitsOnLine s then
          case mrest of
            Nothing -> s
            Just e  -> s ++ " | " ++ foo k e
        else
          let s = String.join ("\n" ++ tab k ++ " ") ss in
          case mrest of
            Nothing -> s
            Just e  -> s ++ "\n" ++ tab k ++ "|" ++ foo k e
    EIndList _ rs _ ->
      Utils.ibracks <|
        let rstrs = List.map (strRange showLocs k) rs
            totstr = Utils.spaces rstrs
        in if fitsOnLine totstr then
          totstr
        else String.join ("\n" ++ tab k ++ " ") rstrs
    ELet _ Let b p e1 e2 _ ->
      Utils.parens <|
        let k' = if isLet e2 then k else k + 1 in
        (if b then "letrec " else "let ") ++ strPat p ++
          indent e1 ++ "\n" ++
          tab k' ++ foo k' e2
    ELet _ Def b p e1 e2 _ ->
      let s = if b then "defrec " else "def " in
      Utils.parens (s ++ strPat p ++ indent e1) ++ "\n" ++
      tab k ++ foo k e2
    ECase _ e1 l _ ->
      let bar (Branch_ ws1 pi ei ws2) =
        tab (k+1) ++ Utils.parens (strPat pi ++ " " ++ foo (k+1) ei) in
      Utils.parens <|
        "case " ++ foo k e1 ++ "\n" ++ Utils.lines (List.map (bar << .val) l)
    EComment _ s e1 ->
      ";" ++ s ++ "\n" ++
      tab k ++ foo k e1
    EOption _ s1 _ s2 e1 ->
      "# " ++ s1.val ++ ": " ++ s2.val ++ "\n" ++
      tab k ++ foo k e1

maybeIndent showLocs k e =
  let s = sExp_ showLocs (k+1) e in
  if fitsOnLine s
    then " " ++ s
    else "\n" ++ tab (k+1) ++ s

-- TODO take into account indent and other prefix of current line
fitsOnLine s =
  if String.length s > 70 then False
  else if List.member '\n' (String.toList s) then False
  else True

isLet e = case e.val.e__ of
  ELet _ _ _ _ _ _ _ -> True
  EComment _ _ e1    -> isLet e1
  _                  -> False


------------------------------------------------------------------------------
-- Mapping WithInfo/WithPos

mapValField f r = { r | val = f r.val }


------------------------------------------------------------------------------
-- Mapping

mapExp : (Exp -> Exp) -> Exp -> Exp
mapExp f e =
  let recurse = mapExp f in
  let wrap e__ = P.WithInfo (Exp_ e__ e.val.eid) e.start e.end in
  let wrapAndMap = f << wrap in
  case e.val.e__ of
    EConst _ _ _ _         -> f e
    EBase _ _              -> f e
    EVar _ _               -> f e
    EFun ws1 ps e' ws2     -> wrapAndMap (EFun ws1 ps (recurse e') ws2)
    EApp ws1 e1 es ws2     -> wrapAndMap (EApp ws1 (recurse e1) (List.map recurse es) ws2)
    EOp ws1 op es ws2      -> wrapAndMap (EOp ws1 op (List.map recurse es) ws2)
    EList ws1 es ws2 m ws3 -> wrapAndMap (EList ws1 (List.map recurse es) ws2 (Utils.mapMaybe recurse m) ws3)
    EIndList ws1 rs ws2    ->
      let rangeRecurse r_ = case r_ of
        Interval e1 ws e2 -> Interval (recurse e1) ws (recurse e2)
        Point e1          -> Point (recurse e1)
      in
      wrapAndMap (EIndList ws1 (List.map (mapValField rangeRecurse) rs) ws2)
    EIf ws1 e1 e2 e3 ws2      -> wrapAndMap (EIf ws1 (recurse e1) (recurse e2) (recurse e3) ws2)
    ECase ws1 e1 branches ws2 ->
      let newE1 = recurse e1 in
      let newBranches =
        List.map
            (mapValField (\(Branch_ bws1 p ei bws2) -> Branch_ bws1 p (recurse ei) bws2))
            branches
      in
      wrapAndMap (ECase ws1 newE1 newBranches ws2)
    EComment ws s e1         -> wrapAndMap (EComment ws s (recurse e1))
    EOption ws1 s1 ws2 s2 e1 -> wrapAndMap (EOption ws1 s1 ws2 s2 (recurse e1))
    ELet ws1 k b p e1 e2 ws2 -> wrapAndMap (ELet ws1 k b p (recurse e1) (recurse e2) ws2)

mapExpViaExp__ : (Exp__ -> Exp__) -> Exp -> Exp
mapExpViaExp__ f e =
  let wrap e__ = P.WithInfo (Exp_ e__ e.val.eid) e.start e.end in
  let f' exp = wrap (f exp.val.e__) in
  mapExp f' e

mapVal : (Val -> Val) -> Val -> Val
mapVal f v = case v.v_ of
  VList vs         -> f { v | v_ = VList (List.map (mapVal f) vs) }
  VConst _         -> f v
  VBase _          -> f v
  VClosure _ _ _ _ -> f v
  VHole _          -> f v

foldVal : (Val -> a -> a) -> Val -> a -> a
foldVal f v a = case v.v_ of
  VList vs         -> f v (List.foldl (foldVal f) a vs)
  VConst _         -> f v a
  VBase _          -> f v a
  VClosure _ _ _ _ -> f v a
  VHole _          -> f v a

-- Fold through preorder traversal
foldExp : (Exp -> a -> a) -> a -> Exp -> a
foldExp f acc exp =
  List.foldl f acc (flattenExpTree exp)

foldExpViaE__ : (Exp__ -> a -> a) -> a -> Exp -> a
foldExpViaE__ f acc exp =
  let f' exp = f exp.val.e__ in
  foldExp f' acc exp

replaceExpNode : Exp -> Exp -> Exp -> Exp
replaceExpNode oldNode newNode root =
  let esubst = Dict.singleton oldNode.val.eid newNode.val.e__ in
  applyESubst esubst root

------------------------------------------------------------------------------
-- Traversing

-- Returns pre-order list of expressions
-- O(n^2) memory
flattenExpTree : Exp -> List Exp
flattenExpTree exp =
  exp :: List.concatMap flattenExpTree (childExps exp)

-- For each node for which `predicate` returns True, return it and its ancestors
findAllWithAncestors : (Exp -> Bool) -> Exp -> List (List Exp)
findAllWithAncestors predicate exp =
  findAllWithAncestorsRec predicate [] exp

findAllWithAncestorsRec : (Exp -> Bool) -> List Exp -> Exp -> List (List Exp)
findAllWithAncestorsRec predicate ancestors exp =
  let ancestorsAndThis = ancestors ++ [exp] in
  let thisResult       = if predicate exp then [ancestorsAndThis] else [] in
  let recurse exp      = findAllWithAncestorsRec predicate ancestorsAndThis exp in
  thisResult ++ List.concatMap recurse (childExps exp)

childExps : Exp -> List Exp
childExps e =
  case e.val.e__ of
    EConst _ _ _ _          -> []
    EBase _ _               -> []
    EVar _ _                -> []
    EFun ws1 ps e' ws2      -> [e']
    EOp ws1 op es ws2       -> es
    EList ws1 es ws2 m ws3  ->
      case m of
        Just e  -> es ++ [e]
        Nothing -> es
    EIndList ws1 ranges ws2 ->
      List.concatMap
        (\range -> case range.val of
          Interval e1 ws e2 -> [e1, e2]
          Point e1          -> [e1]
        )
        ranges
    EApp ws1 f es ws2        -> f :: es
    ELet ws1 k b p e1 e2 ws2 -> [e1, e2]
    EIf ws1 e1 e2 e3 ws2     -> [e1, e2, e3]
    ECase ws1 e branches ws2 -> e :: (branchExps branches)
    EComment ws s e1         -> [e1]
    EOption ws1 s1 ws2 s2 e1 -> [e1]

------------------------------------------------------------------------------
-- Conversion

valToTrace : Val -> Trace
valToTrace v = case v.v_ of
  VConst (_, trace) -> trace
  _                 -> Debug.crash "valToTrace"

------------------------------------------------------------------------------
-- Location Substitutions
-- Expression Substitutions

type alias Subst = Dict.Dict LocId Num
type alias SubstPlus = Dict.Dict LocId (P.WithInfo Num)
type alias SubstMaybeNum = Dict.Dict LocId (Maybe Num)

type alias ESubst = Dict.Dict EId Exp__

type alias TwoSubsts = { lsubst : Subst, esubst : ESubst }

-- For unparsing traces, possibily inserting variables: d
type alias SubstStr = Dict.Dict LocId String

applyLocSubst : Subst -> Exp -> Exp
applyLocSubst s = applySubst { lsubst = s, esubst = Dict.empty }

applyESubst : ESubst -> Exp -> Exp
applyESubst s = applySubst { lsubst = Dict.empty, esubst = s }

applySubst : TwoSubsts -> Exp -> Exp
applySubst subst exp =
  let replacer =
    (\e ->
      let e__ = e.val.e__ in
      let e__ConstReplaced =
        case e__ of
          EConst ws n loc wd ->
            let locId = Utils.fst3 loc in
            case Dict.get locId subst.lsubst of
              Just n' -> EConst ws n' loc wd
              Nothing -> e__
              -- 10/28: substs from createMousePosCallbackSlider only bind
              -- updated values (unlike substs from Sync)
          _ -> e__
      in
      let e__' =
        case Dict.get e.val.eid subst.esubst of
          Just e__New -> e__New
          Nothing     -> e__ConstReplaced
      in
      P.WithInfo (Exp_ e__' e.val.eid) e.start e.end
    )
  in
  mapExp replacer exp


unfrozenLocIdsAndNumbers : Exp -> List (LocId, Num)
unfrozenLocIdsAndNumbers exp =
  foldExpViaE__
    (\e__ acc ->
      case e__ of
        EConst _ n (locId, "!", _) _ -> acc
        EConst _ n (locId,   _, _) _ -> (locId, n) :: acc
        _                            -> acc
    )
    []
    exp

{-
-- for now, LocId instead of EId
type alias ESubst = Dict.Dict LocId Exp__

applyESubst : ESubst -> Exp -> Exp
applyESubst esubst =
  mapExpViaExp__ <| \e__ -> case e__ of
    EConst _ i -> case Dict.get (Utils.fst3 i) esubst of
                    Nothing   -> e__
                    Just e__' -> e__'
    _          -> e__
-}


-----------------------------------------------------------------------------
-- Utility

branchExps : List Branch -> List Exp
branchExps branches =
  List.map
    (\b -> let (Branch_ _ _ exp _) = b.val in exp)
    branches

branchPats : List Branch -> List Pat
branchPats branches =
  List.map
    (\b -> let (Branch_ _ pat _ _) = b.val in pat)
    branches

-- Need parent expression since case expression branches into several scopes
isScope : Maybe Exp -> Exp -> Bool
isScope maybeParent exp =
  let isObviouslyScope =
    case exp.val.e__ of
      ELet _ _ _ _ _ _ _ -> True
      EFun _ _ _ _       -> True
      _                  -> False
  in
  case maybeParent of
    Just parent ->
      case parent.val.e__ of
        ECase _ predicate branches _ -> predicate /= exp
        _                            -> isObviouslyScope
    Nothing -> isObviouslyScope


identifiersSet : Exp -> Set.Set Ident
identifiersSet exp =
  identifiersList exp
  |> Set.fromList


identifiersList : Exp -> List Ident
identifiersList exp =
  let folder e__ acc =
    case e__ of
      EVar _ ident ->
        ident::acc

      EFun _ pats _ _ ->
        (List.concatMap identifiersListInPat pats) ++ acc

      ECase _ _ branches _ ->
        let pats = branchPats branches in
        (List.concatMap identifiersListInPat pats) ++ acc

      ELet _ _ _ pat _ _ _ ->
        (identifiersListInPat pat) ++ acc

      _ ->
        acc
  in
  foldExpViaE__
    folder
    []
    exp

identifiersListInPat : Pat -> List Ident
identifiersListInPat pat =
  case pat.val of
    PVar _ ident _              -> [ident]
    PList _ pats _ (Just pat) _ -> List.concatMap identifiersListInPat (pat::pats)
    PList _ pats _ Nothing    _ -> List.concatMap identifiersListInPat pats
    _                           -> []


identifierCounts : Exp -> Dict.Dict Ident Int
identifierCounts exp =
  List.foldl
    (\ident counts ->
      Dict.update
        ident
        (\old ->
          case old of
            Just count -> Just (count + 1)
            Nothing    -> Just 1
        )
        counts
    )
    Dict.empty
    (identifiersList exp)


-----------------------------------------------------------------------------
-- Lang Options

-- all options should appear before the first non-comment expression

getOptions : Exp -> List (String, String)
getOptions e = case e.val.e__ of
  EOption _ s1 _ s2 e1 -> (s1.val, s2.val) :: getOptions e1
  EComment _ _ e1      -> getOptions e1
  _                    -> []


------------------------------------------------------------------------------
-- Error Messages

errorPrefix = "[Little Error]" -- NOTE: same as errorPrefix in Native/codeBox.js
errorMsg s  = Debug.crash <| errorPrefix ++ "\n\n" ++ s

strPos p =
  let (i,j) = (toString p.line, toString p.col) in
  "(Line:" ++ i ++ " Col:" ++ j ++ ")"


------------------------------------------------------------------------------
-- Abstract Syntax Helpers

-- NOTE: the Exp builders use dummyPos

val : Val_ -> Val
val = flip Val [-1]

exp_ : Exp__ -> Exp_
exp_ = flip Exp_ (-1)

withDummyRange x  = P.WithInfo x P.dummyPos P.dummyPos
withDummyPos e__  = P.WithInfo (exp_ e__) P.dummyPos P.dummyPos
  -- TODO rename withDummyPos

dummyLoc_ b = (0, b, "")
dummyTrace_ b = TrLoc (dummyLoc_ b)

dummyLoc = dummyLoc_ unann
dummyTrace = dummyTrace_ unann

ePlus e1 e2 = withDummyPos <| EOp "" (withDummyRange Plus) [e1,e2] ""

eBool  = withDummyPos << EBase " " << Bool
eStr   = withDummyPos << EBase " " << String
eTrue  = eBool True
eFalse = eBool False

eApp e es = case es of
  []      -> Debug.crash "eApp"
  [e1]    -> withDummyPos <| EApp "\n" e [e1] ""
  e1::es' -> eApp (withDummyPos <| EApp " " e [e1] "") es'

eFun ps e = case ps of
  []      -> Debug.crash "eFun"
  [p]     -> withDummyPos <| EFun " " [p] e ""
  p::ps'  -> withDummyPos <| EFun " " [p] (eFun ps' e) ""

ePair e1 e2 = withDummyPos <| EList " " [e1,e2] "" Nothing ""

noWidgetDecl = withDummyRange NoWidgetDecl

eLets xes eBody = case xes of
  (x,e)::xes' -> withDummyPos <|
                   ELet "\n" Let False (withDummyRange (PVar " " x noWidgetDecl)) e (eLets xes' eBody) ""
  []          -> eBody

eVar0 a        = withDummyPos <| EVar "" a
eVar a         = withDummyPos <| EVar " " a
eConst0 a b    = withDummyPos <| EConst "" a b noWidgetDecl
eConst a b     = withDummyPos <| EConst " " a b noWidgetDecl
eList0 a b     = withDummyPos <| EList "" a "" b ""
eList a b      = withDummyPos <| EList " " a "" b ""
eComment a b   = withDummyPos <| EComment " " a b

pVar0 a        = withDummyRange <| PVar "" a noWidgetDecl
pVar a         = withDummyRange <| PVar " " a noWidgetDecl
pList ps       = withDummyRange <| PList " " ps "" Nothing ""

-- note: dummy ids...
vTrue    = vBool True
vFalse   = vBool False
vBool    = val << VBase << Bool
vStr     = val << VBase << String
vConst   = val << VConst
vBase    = val << VBase
vList    = val << VList
vHole    = val << VHole

unwrapVList : Val -> Maybe (List Val_)
unwrapVList v =
  case v.v_ of
    VList vs -> Just <| List.map .v_ vs
    _        -> Nothing

-- TODO names/types

unwrapVList_ : String -> Val -> List Val_
unwrapVList_ s v = case v.v_ of
  VList vs -> List.map .v_ vs
  _        -> Debug.crash <| "unwrapVList_: " ++ s

unwrapVBaseString_ : String -> Val_ -> String
unwrapVBaseString_ s v_ = case v_ of
  VBase (String k) -> k
  _                -> Debug.crash <| "unwrapVBaseString_: " ++ s
