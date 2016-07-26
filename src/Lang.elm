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
type alias Type   = P.WithInfo Type_
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
  | ETyp WS Pat Type Exp WS
  | EColonType WS Exp WS Type WS

    -- EFun [] e     impossible
    -- EFun [p] e    (\p. e)
    -- EFun ps e     (\(p1 ... pn) e) === (\p1 (\p2 (... (\pn e) ...)))

    -- EApp f []     impossible
    -- EApp f [x]    (f x)
    -- EApp f xs     (f x1 ... xn) === ((... ((f x1) x2) ...) xn)

type Type_
  = TNum WS
  | TBool WS
  | TString WS
  | TList WS Type WS
  | TDict WS Type Type WS
  | TTuple WS (List Type) WS (Maybe Type) WS
  | TArrow WS (List Type) WS
  | TVar WS Ident

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

type alias Env = List (Ident, Val)


------------------------------------------------------------------------------
-- Unparsing

strBaseVal v = case v of
  Bool True  -> "true"
  Bool False -> "false"
  String s   -> "\'" ++ s ++ "\'"
  Star       -> "X"

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
    EComment ws s e1              -> wrapAndMap (EComment ws s (recurse e1))
    EOption ws1 s1 ws2 s2 e1      -> wrapAndMap (EOption ws1 s1 ws2 s2 (recurse e1))
    ELet ws1 k b p e1 e2 ws2      -> wrapAndMap (ELet ws1 k b p (recurse e1) (recurse e2) ws2)
    ETyp ws1 pat tipe e ws2       -> wrapAndMap (ETyp ws1 pat tipe (recurse e) ws2)
    EColonType ws1 e ws2 tipe ws3 -> wrapAndMap (EColonType ws1 (recurse e) ws2 tipe ws3)

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
-- For each matching node, ancestors appear in order: root first, match last.
findAllWithAncestors : (Exp -> Bool) -> Exp -> List (List Exp)
findAllWithAncestors predicate exp =
  findAllWithAncestors_ predicate [] exp

findAllWithAncestors_ : (Exp -> Bool) -> List Exp -> Exp -> List (List Exp)
findAllWithAncestors_ predicate ancestors exp =
  let ancestorsAndThis = ancestors ++ [exp] in
  let thisResult       = if predicate exp then [ancestorsAndThis] else [] in
  let recurse exp      = findAllWithAncestors_ predicate ancestorsAndThis exp in
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
    EApp ws1 f es ws2             -> f :: es
    ELet ws1 k b p e1 e2 ws2      -> [e1, e2]
    EIf ws1 e1 e2 e3 ws2          -> [e1, e2, e3]
    ECase ws1 e branches ws2      -> e :: (branchExps branches)
    EComment ws s e1              -> [e1]
    EOption ws1 s1 ws2 s2 e1      -> [e1]
    ETyp ws1 pat tipe e ws2       -> [e]
    EColonType ws1 e ws2 tipe ws3 -> [e]


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
eStr0  = withDummyPos << EBase "" << String
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

intSlider a b =
  withDummyRange <|
    IntSlider (withDummyRange a) (withDummyRange "-") (withDummyRange b) Nothing

colorNumberSlider = intSlider 0 499

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
pList0 ps      = withDummyRange <| PList "" ps "" Nothing ""
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


eRaw__ = EVar
eRaw0  = eVar0
eRaw   = eVar

listOfRaw = listOfVars

listOfVars xs =
  case xs of
    []     -> []
    x::xs' -> eVar0 x :: List.map eVar xs'

listOfPVars xs =
  case xs of
    []     -> []
    x::xs' -> pVar0 x :: List.map pVar xs'

listOfNums ns =
  case ns of
    []     -> []
    n::ns' -> eConst0 n dummyLoc :: List.map (flip eConst dummyLoc) ns'

-- listOfNums1 = List.map (flip eConst dummyLoc)

type alias AnnotatedNum = (Num, Frozen, WidgetDecl)
  -- may want to move this up into EConst

listOfAnnotatedNums : List AnnotatedNum -> List Exp
listOfAnnotatedNums list =
  case list of
    [] -> []
    (n,ann,wd) :: list' ->
      withDummyPos (EConst "" n (dummyLoc_ ann) wd) :: listOfAnnotatedNums1 list'

listOfAnnotatedNums1 =
 List.map (\(n,ann,wd) -> withDummyPos (EConst " " n (dummyLoc_ ann) wd))

minMax x y             = (min x y, max x y)
minNumTr (a,t1) (b,t2) = if a <= b then (a,t1) else (b,t2)
maxNumTr (a,t1) (b,t2) = if a >= b then (a,t1) else (b,t2)
minMaxNumTr nt1 nt2    = (minNumTr nt1 nt2, maxNumTr nt1 nt2)

plusNumTr (n1,t1) (n2,t2)  = (n1 + n2, TrOp Plus [t1, t2])
minusNumTr (n1,t1) (n2,t2) = (n1 + n2, TrOp Minus [t1, t2])
