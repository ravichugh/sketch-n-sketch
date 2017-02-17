module Lang exposing (..)

import String
import Debug
import Dict
import Set
import Debug
import Regex

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

type alias Pat     = P.WithInfo Pat_
type alias Exp     = P.WithInfo Exp_
type alias Type    = P.WithInfo Type_
type alias Op      = P.WithInfo Op_
type alias Branch  = P.WithInfo Branch_
type alias TBranch = P.WithInfo TBranch_

-- TODO add constant literals to patterns, and match 'svg'
type Pat_
  = PVar WS Ident WidgetDecl
  | PConst WS Num
  | PBase WS EBaseVal
  | PList WS (List Pat) WS (Maybe Pat) WS
  | PAs WS Ident WS Pat

type Op_
  -- nullary ops
  = Pi
  | DictEmpty
  -- unary ops
  | Cos | Sin | ArcCos | ArcSin
  | Floor | Ceil | Round
  | ToStr
  | Sqrt
  | Explode
  | DebugLog
  --  | DictMem   -- TODO: add this
  -- binary ops
  | Plus | Minus | Mult | Div
  | Lt | Eq
  | Mod | Pow
  | ArcTan2
  | DictGet
  | DictRemove
  -- trinary ops
  | DictInsert

type alias EId  = Int
type alias Exp_ = { e__ : Exp__, eid : EId }

type Exp__
  = EConst WS Num Loc WidgetDecl
  | EBase WS EBaseVal
  | EVar WS Ident
  | EFun WS (List Pat) Exp WS -- WS: before (, before )
  -- TODO remember paren whitespace for multiple pats, like TForall
  -- | EFun WS (OneOrMany Pat) Exp WS
  | EApp WS Exp (List Exp) WS
  | EOp WS Op (List Exp) WS
  | EList WS (List Exp) WS (Maybe Exp) WS
  | EIf WS Exp Exp Exp WS
  | ECase WS Exp (List Branch) WS
  | ETypeCase WS Pat (List TBranch) WS
  | ELet WS LetKind Rec Pat Exp Exp WS
  | EComment WS String Exp
  | EOption WS (P.WithInfo String) WS (P.WithInfo String) Exp
  | ETyp WS Pat Type Exp WS
  | EColonType WS Exp WS Type WS
  | ETypeAlias WS Pat Type Exp WS

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
  | TNull WS
  | TList WS Type WS
  | TDict WS Type Type WS
  | TTuple WS (List Type) WS (Maybe Type) WS
  | TArrow WS (List Type) WS
  | TUnion WS (List Type) WS
  | TNamed WS Ident
  | TVar WS Ident
  | TForall WS (OneOrMany (WS, Ident)) Type WS
  | TWildcard WS

type alias WS = String

type OneOrMany a          -- track concrete syntax for:
  = One a                 --   x
  | Many WS (List a) WS   --   ws1~(x1~...~xn-ws2)

type Branch_  = Branch_ WS Pat Exp WS
type TBranch_ = TBranch_ WS Type Exp WS

type LetKind = Let | Def
type alias Rec = Bool

type alias WidgetDecl = P.WithInfo WidgetDecl_

type WidgetDecl_
  = IntSlider (P.WithInfo Int) Token (P.WithInfo Int) Caption
  | NumSlider (P.WithInfo Num) Token (P.WithInfo Num) Caption
  | NoWidgetDecl -- rather than Nothing, to work around parser types

type Widget
  = WIntSlider Int Int String Int Loc
  | WNumSlider Num Num String Num Loc
  | WPointSlider NumTr NumTr

type alias Widgets = List Widget

type alias Token = P.WithInfo String

type alias Caption = Maybe (P.WithInfo String)

type alias VTrace = List EId
type alias Val    = { v_ : Val_, vtrace : VTrace }

type Val_
  = VConst NumTr
  | VBase VBaseVal
  | VClosure (Maybe Ident) Pat Exp Env
  | VList (List Val)
  | VDict VDict_

type alias VDict_ = Dict.Dict (String, String) Val

type alias NumTr = (Num, Trace)

defaultQuoteChar = "'"
type alias QuoteChar = String

-- TODO combine all base exps/vals into PBase/EBase/VBase
type VBaseVal -- unlike Ints, these cannot be changed by Sync
  = VBool Bool
  | VString String
  | VNull

type EBaseVal
  = EBool Bool
  | EString QuoteChar String
  | ENull

type Trace = TrLoc Loc | TrOp Op_ (List Trace)

type alias Env = List (Ident, Val)
type alias Backtrace = List Exp

------------------------------------------------------------------------------
-- Unparsing

strBaseVal v = case v of
  VBool True  -> "true"
  VBool False -> "false"
  VString s   -> "'" ++ s ++ "'"
  VNull       -> "null"

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
    VDict d          -> "<dict " ++ (Dict.toList d |> List.map (\(k, v) -> (toString k) ++ ":" ++ (foo v)) |> String.join " ") ++ ">"

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
  Explode       -> "explode"
  Sqrt          -> "sqrt"
  Mod           -> "mod"
  Pow           -> "pow"
  DictEmpty     -> "empty"
  DictInsert    -> "insert"
  DictGet       -> "get"
  DictRemove    -> "remove"
  DebugLog      -> "debug"

strLoc (k, b, mx) =
  "k" ++ toString k ++ (if mx == "" then "" else "_" ++ mx) ++ b

strTrace tr = case tr of
  TrLoc l   -> strLoc l
  TrOp op l ->
    Utils.parens (String.concat
      [strOp op, " ", String.join " " (List.map strTrace l)])

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

-- Children visited/replaced first. (Post-order traversal.)
--
-- Note that visit order matters for some code (clone detection: leaf visit order matters to determine argument order).
--
-- Children are visited in right-to-left order (opposite of order returned by the childExps function).
-- This lets you use mapFoldExp as a foldr: a simple cons in the accumulator will result in nodes in left-to-right order.
mapFoldExp : (Exp -> a -> (Exp, a)) -> a -> Exp -> (Exp, a)
mapFoldExp f initAcc e =
  let recurse = mapFoldExp f in
  let wrap e__ = P.WithInfo (Exp_ e__ e.val.eid) e.start e.end in
  let wrapAndMap = f << wrap in
  -- Make sure exps are left-to-right so they are visted right-to-left.
  let recurseAll initAcc exps =
    exps
    |> List.foldr
        (\exp (newExps, acc) ->
          let (newExp, newAcc) = recurse acc exp in
          (newExp::newExps, newAcc)
        )
        ([], initAcc)
  in
  case e.val.e__ of
    EConst _ _ _ _ -> f e initAcc
    EBase _ _      -> f e initAcc
    EVar _ _       -> f e initAcc
    EFun ws1 ps e1 ws2 ->
      let (newE1, newAcc) = recurse initAcc e1 in
      wrapAndMap (EFun ws1 ps newE1 ws2) newAcc

    EApp ws1 e1 es ws2 ->
      let (newEs, newAcc)  = recurseAll initAcc es in
      let (newE1, newAcc2) = recurse newAcc e1 in
      wrapAndMap (EApp ws1 newE1 newEs ws2) newAcc2

    EOp ws1 op es ws2 ->
      let (newEs, newAcc) = recurseAll initAcc es in
      wrapAndMap (EOp ws1 op newEs ws2) newAcc

    EList ws1 es ws2 Nothing ws3 ->
      let (newEs, newAcc) = recurseAll initAcc es in
      wrapAndMap (EList ws1 newEs ws2 Nothing ws3) newAcc

    EList ws1 es ws2 (Just e1) ws3 ->
      let (newEs, newAcc)  = recurseAll initAcc es in
      let (newE1, newAcc2) = recurse newAcc e1 in
      wrapAndMap (EList ws1 newEs ws2 (Just newE1) ws3) newAcc2

    EIf ws1 e1 e2 e3 ws2 ->
      case recurseAll initAcc [e1, e2, e3] of
        ([newE1, newE2, newE3], newAcc) -> wrapAndMap (EIf ws1 newE1 newE2 newE3 ws2) newAcc
        _                               -> Debug.crash "I'll buy you a beer if this line of code executes. - Brian"

    ECase ws1 e1 branches ws2 ->
      let (newBranches, newAcc) =
        branches
        |> List.foldr
            (\branch (newBranches, acc) ->
              let (Branch_ bws1 p ei bws2) = branch.val in
              let (newEi, newAcc) = recurse acc ei in
              ({ branch | val = Branch_ bws1 p newEi bws2 }::newBranches, newAcc)
            )
            ([], initAcc)
      in
      let (newE1, newAcc2) = recurse newAcc e1 in
      wrapAndMap (ECase ws1 newE1 newBranches ws2) newAcc2

    ETypeCase ws1 pat tbranches ws2 ->
      let (newBranches, newAcc) =
        tbranches
        |> List.foldr
            (\tbranch (newBranches, acc) ->
              let (TBranch_ bws1 t ei bws2) = tbranch.val in
              let (newEi, newAcc) = recurse acc ei in
              ({ tbranch | val = TBranch_ bws1 t newEi bws2 }::newBranches, newAcc)
            )
            ([], initAcc)
      in
      wrapAndMap (ETypeCase ws1 pat newBranches ws2) newAcc

    EComment ws s e1 ->
      let (newE1, newAcc) = recurse initAcc e1 in
      wrapAndMap (EComment ws s newE1) newAcc

    EOption ws1 s1 ws2 s2 e1 ->
      let (newE1, newAcc) = recurse initAcc e1 in
      wrapAndMap (EOption ws1 s1 ws2 s2 newE1) newAcc

    ELet ws1 k b p e1 e2 ws2 ->
      let (newE2, newAcc) = recurse initAcc e2 in
      let (newE1, newAcc2) = recurse newAcc e1 in
      wrapAndMap (ELet ws1 k b p newE1 newE2 ws2) newAcc2

    ETyp ws1 pat tipe e1 ws2 ->
      let (newE1, newAcc) = recurse initAcc e1 in
      wrapAndMap (ETyp ws1 pat tipe newE1 ws2) newAcc

    EColonType ws1 e1 ws2 tipe ws3 ->
      let (newE1, newAcc) = recurse initAcc e1 in
      wrapAndMap (EColonType ws1 newE1 ws2 tipe ws3) newAcc

    ETypeAlias ws1 pat tipe e1 ws2 ->
      let (newE1, newAcc) = recurse initAcc e1 in
      wrapAndMap (ETypeAlias ws1 pat tipe newE1 ws2) newAcc


mapExp : (Exp -> Exp) -> Exp -> Exp
mapExp f e =
  -- Accumulator thrown away; just need something that type checks.
  let (newExp, _) = mapFoldExp (\exp _ -> (f exp, ())) () e in
  newExp

-- Preserves EIds
mapExpViaExp__ : (Exp__ -> Exp__) -> Exp -> Exp
mapExpViaExp__ f e =
  let f_ exp = replaceE__ exp (f exp.val.e__) in
  mapExp f_ e

mapVal : (Val -> Val) -> Val -> Val
mapVal f v = case v.v_ of
  VList vs         -> f { v | v_ = VList (List.map (mapVal f) vs) }
  VDict d          -> f { v | v_ = VDict (Dict.map (\_ v -> mapVal f v) d) } -- keys ignored
  VConst _         -> f v
  VBase _          -> f v
  VClosure _ _ _ _ -> f v

foldVal : (Val -> a -> a) -> Val -> a -> a
foldVal f v a = case v.v_ of
  VList vs         -> f v (List.foldl (foldVal f) a vs)
  VDict d          -> f v (List.foldl (foldVal f) a (Dict.values d)) -- keys ignored
  VConst _         -> f v a
  VBase _          -> f v a
  VClosure _ _ _ _ -> f v a

-- Fold through preorder traversal
foldExp : (Exp -> a -> a) -> a -> Exp -> a
foldExp f acc exp =
  List.foldl f acc (flattenExpTree exp)

foldExpViaE__ : (Exp__ -> a -> a) -> a -> Exp -> a
foldExpViaE__ f acc exp =
  let f_ exp = f exp.val.e__ in
  foldExp f_ acc exp

replaceExpNode : EId -> Exp -> Exp -> Exp
replaceExpNode eid newNode root =
  mapExp
      (\exp ->
        if exp.val.eid == eid
        then newNode
        else exp
      )
      root

replaceExpNodePreservingPreceedingWhitespace : EId -> Exp -> Exp -> Exp
replaceExpNodePreservingPreceedingWhitespace eid newNode root =
  mapExp
      (\exp ->
        if exp.val.eid == eid
        then replacePrecedingWhitespace (precedingWhitespace exp) newNode
        else exp
      )
      root

replaceExpNodeE__ : Exp -> Exp -> Exp -> Exp
replaceExpNodeE__ oldNode newNode root =
  replaceExpNodeE__ByEId oldNode.val.eid newNode root

replaceExpNodeE__ByEId : EId -> Exp -> Exp -> Exp
replaceExpNodeE__ByEId eid newNode root =
  let esubst = Dict.singleton eid newNode.val.e__ in
  applyESubst esubst root


mapType : (Type -> Type) -> Type -> Type
mapType f tipe =
  let recurse = mapType f in
  let wrap t_ = P.WithInfo t_ tipe.start tipe.end in
  case tipe.val of
    TNum _       -> f tipe
    TBool _      -> f tipe
    TString _    -> f tipe
    TNull _      -> f tipe
    TNamed _ _   -> f tipe
    TVar _ _     -> f tipe
    TWildcard _  -> f tipe

    TList ws1 t1 ws2        -> f (wrap (TList ws1 (recurse t1) ws2))
    TDict ws1 t1 t2 ws2     -> f (wrap (TDict ws1 (recurse t1) (recurse t2) ws2))
    TArrow ws1 ts ws2       -> f (wrap (TArrow ws1 (List.map recurse ts) ws2))
    TUnion ws1 ts ws2       -> f (wrap (TUnion ws1 (List.map recurse ts) ws2))
    TForall ws1 vars t1 ws2 -> f (wrap (TForall ws1 vars (recurse t1) ws2))

    TTuple ws1 ts ws2 mt ws3 ->
      f (wrap (TTuple ws1 (List.map recurse ts) ws2 (Utils.mapMaybe recurse mt) ws3))

foldType : (Type -> a -> a) -> Type -> a -> a
foldType f tipe acc =
  let foldTypes f tipes acc = List.foldl (\t acc -> foldType f t acc) acc tipes in
  case tipe.val of
    TNum _          -> acc |> f tipe
    TBool _         -> acc |> f tipe
    TString _       -> acc |> f tipe
    TNull _         -> acc |> f tipe
    TNamed _ _      -> acc |> f tipe
    TVar _ _        -> acc |> f tipe
    TWildcard _     -> acc |> f tipe
    TList _ t _     -> acc |> foldType f t |> f tipe
    TDict _ t1 t2 _ -> acc |> foldType f t1 |> foldType f t2 |> f tipe
    TForall _ _ t _ -> acc |> foldType f t |> f tipe
    TArrow _ ts _   -> acc |> foldTypes f ts |> f tipe
    TUnion _ ts _   -> acc |> foldTypes f ts |> f tipe

    TTuple _ ts _ Nothing _  -> acc |> foldTypes f ts |> f tipe
    TTuple _ ts _ (Just t) _ -> acc |> foldTypes f (ts++[t]) |> f tipe


------------------------------------------------------------------------------
-- Traversing

-- Returns pre-order list of expressions
-- O(n^2) memory
flattenExpTree : Exp -> List Exp
flattenExpTree exp =
  exp :: List.concatMap flattenExpTree (childExps exp)

findFirstNode : (Exp -> Bool) -> Exp -> Maybe Exp
findFirstNode predicate exp =
  if predicate exp then
    Just exp
  else
    childExps exp
    |> Utils.mapFirstSuccess (findFirstNode predicate)

findExpByEId : Exp -> EId -> Maybe Exp
findExpByEId program targetEId =
  findFirstNode (\exp -> exp.val.eid == targetEId) program

-- justFindExpByEId is in LangTools (it needs the unparser for error messages).
-- LangTools.justFindExpByEId : EId -> Exp -> Exp
-- LangTools.justFindExpByEId eid exp =
--   findExpByEId exp eid
--   |> Utils.fromJust__ (\() -> "Couldn't find eid " ++ toString eid ++ " in " ++ unparseWithIds exp)

findExpByLocId : Exp -> LocId -> Maybe Exp
findExpByLocId program targetLocId =
  let isTarget exp =
    case exp.val.e__ of
      EConst _ _ (locId, _, _) _ -> locId == targetLocId
      _                          -> False
  in
  findFirstNode isTarget program

locIdToEId : Exp -> LocId -> Maybe EId
locIdToEId program locId =
  findExpByLocId program locId |> Maybe.map (.val >> .eid)


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

-- Children returned ordered left-to-right.
childExps : Exp -> List Exp
childExps e =
  case e.val.e__ of
    EConst _ _ _ _          -> []
    EBase _ _               -> []
    EVar _ _                -> []
    EFun ws1 ps e_ ws2      -> [e_]
    EOp ws1 op es ws2       -> es
    EList ws1 es ws2 m ws3  ->
      case m of
        Just e  -> es ++ [e]
        Nothing -> es
    EApp ws1 f es ws2               -> f :: es
    ELet ws1 k b p e1 e2 ws2        -> [e1, e2]
    EIf ws1 e1 e2 e3 ws2            -> [e1, e2, e3]
    ECase ws1 e branches ws2        -> e :: (branchExps branches)
    ETypeCase ws1 pat tbranches ws2 -> tbranchExps tbranches
    EComment ws s e1                -> [e1]
    EOption ws1 s1 ws2 s2 e1        -> [e1]
    ETyp ws1 pat tipe e ws2         -> [e]
    EColonType ws1 e ws2 tipe ws3   -> [e]
    ETypeAlias ws1 pat tipe e ws2   -> [e]


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
      let e__ConstReplaced =
        let e__ = e.val.e__ in
        case e__ of
          EConst ws n loc wd ->
            let locId = Utils.fst3 loc in
            case Dict.get locId subst.lsubst of
              Just n_ -> EConst ws n_ loc wd
              Nothing -> e__
              -- 10/28: substs from createMousePosCallbackSlider only bind
              -- updated values (unlike substs from Sync)
          _ -> e__
      in
      let e__New =
        case Dict.get e.val.eid subst.esubst of
          Just e__New -> e__New
          Nothing     -> e__ConstReplaced
      in
      replaceE__ e e__New
    )
  in
  mapExp replacer exp


applyESubstPreservingPrecedingWhitespace : ESubst -> Exp -> Exp
applyESubstPreservingPrecedingWhitespace esubst exp =
  let replacer =
    (\e ->
      case Dict.get e.val.eid esubst of
        Just e__New -> replaceE__PreservingPrecedingWhitespace e e__New
        Nothing     -> e
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

tbranchExps : List TBranch -> List Exp
tbranchExps tbranches =
  List.map
    (\b -> let (TBranch_ _ _ exp _) = b.val in exp)
    tbranches

branchPats : List Branch -> List Pat
branchPats branches =
  List.map
    (\b -> let (Branch_ _ pat _ _) = b.val in pat)
    branches

tbranchTypes : List TBranch -> List Type
tbranchTypes tbranches =
  List.map
    (\b -> let (TBranch_ _ tipe exp _) = b.val in tipe)
    tbranches

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

isNumber : Exp -> Bool
isNumber exp =
  case exp.val.e__ of
    EConst _ _ _ _ -> True
    _              -> False

-- Disregards syncOptions
isFrozenNumber : Exp -> Bool
isFrozenNumber exp =
  case exp.val.e__ of
    EConst _ _ (_, ann, _) _ -> ann == frozen
    _                        -> False

varsOfPat : Pat -> List Ident
varsOfPat pat =
  case pat.val of
    PConst _ _              -> []
    PBase _ _               -> []
    PVar _ x _              -> [x]
    PList _ ps _ Nothing _  -> List.concatMap varsOfPat ps
    PList _ ps _ (Just p) _ -> List.concatMap varsOfPat (p::ps)
    PAs _ x _ p             -> x::(varsOfPat p)


flattenPatTree : Pat -> List Pat
flattenPatTree pat =
  pat :: List.concatMap flattenPatTree (childPats pat)


childPats : Pat -> List Pat
childPats pat =
  case pat.val of
    PConst _ _              -> []
    PBase _ _               -> []
    PVar _ _ _              -> []
    PList _ ps _ Nothing _  -> ps
    PList _ ps _ (Just p) _ -> ps ++ [p]
    PAs _ _ _ p             -> [p]


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
crashWithMsg s  = Debug.crash <| errorPrefix ++ "\n\n" ++ s
errorMsg s      = Err <| errorPrefix ++ "\n\n" ++ s

strPos = P.strPos


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

replaceE__ : Exp -> Exp__ -> Exp
replaceE__ e e__ = let e_ = e.val in { e | val = { e_ | e__ = e__ } }

replaceP_ : Pat -> Pat_ -> Pat
replaceP_ p p_ = { p | val = p_ }

replaceE__PreservingPrecedingWhitespace : Exp -> Exp__ -> Exp
replaceE__PreservingPrecedingWhitespace e e__ =
  replaceE__ e e__ |> replacePrecedingWhitespace (precedingWhitespace e)

replaceBranchExp : Branch -> Exp -> Branch
replaceBranchExp branch exp =
  let (Branch_ bws1 p _ bws2) = branch.val in
  { branch | val = Branch_ bws1 p exp bws2 }

replaceTBranchExp : TBranch -> Exp -> TBranch
replaceTBranchExp tbranch exp =
  let (TBranch_ tbws1 tipe _ tbws2) = tbranch.val in
  { tbranch | val = TBranch_ tbws1 tipe exp tbws2 }

replaceEId : Exp -> EId -> Exp
replaceEId e eid = let e_ = e.val in { e | val = { e_ | eid = eid } }

scrubEId e =  replaceEId e -1

dummyLoc_ b = (0, b, "")
dummyTrace_ b = TrLoc (dummyLoc_ b)

dummyLoc = dummyLoc_ unann
dummyTrace = dummyTrace_ unann

eOp op_ es = withDummyPos <| EOp " " (withDummyRange op_) es ""

ePlus e1 e2 = eOp Plus [e1, e2]

eBool  = withDummyPos << EBase " " << EBool
eStr   = withDummyPos << EBase " " << EString defaultQuoteChar
eStr0  = withDummyPos << EBase "" << EString defaultQuoteChar
eTrue  = eBool True
eFalse = eBool False

eApp e es = withDummyPos <| EApp " " e es ""
eFun ps e = withDummyPos <| EFun " " ps e ""

desugarEApp e es = case es of
  []      -> Debug.crash "desugarEApp"
  [e1]    -> eApp e [e1]
  e1::es_ -> desugarEApp (eApp e [e1]) es_

desugarEFun ps e = case ps of
  []      -> Debug.crash "desugarEFun"
  [p]     -> eFun [p] e
  p::ps_  -> eFun [p] (desugarEFun ps_ e)

ePair e1 e2 = withDummyPos <| EList " " [e1,e2] "" Nothing ""

noWidgetDecl = withDummyRange NoWidgetDecl

rangeSlider kind a b =
  withDummyRange <|
    kind (withDummyRange a) (withDummyRange "-") (withDummyRange b) Nothing

intSlider = rangeSlider IntSlider
numSlider = rangeSlider NumSlider

colorNumberSlider = intSlider 0 499

eLets xes eBody = case xes of
  (x,e)::xes_ -> eLet [(x,e)] (eLets xes_ eBody)
  []          -> eBody


-- Given [("a", aExp), ("b", bExp)] bodyExp
-- Produces (let [a b] [aExp bExp] bodyExp)
--
-- If given singleton list, produces a simple non-list let.
eLetOrDef : LetKind -> List (Ident, Exp) -> Exp -> Exp
eLetOrDef letKind namesAndAssigns bodyExp =
  let (pat, assign) =
    case List.unzip namesAndAssigns of
      ([name], [assign]) -> (pVar name, replacePrecedingWhitespace " " assign)
      (names, assigns)   -> (pListOfPVars names, eList (cleanupListWhitespace " " assigns) Nothing)
  in
  withDummyPos <|
  ELet "\n" letKind False pat assign bodyExp ""

eLet : List (Ident, Exp) -> Exp -> Exp
eLet = eLetOrDef Let

eDef : List (Ident, Exp) -> Exp -> Exp
eDef = eLetOrDef Def


eVar0 a        = withDummyPos <| EVar "" a
eVar a         = withDummyPos <| EVar " " a
eConst0 a b    = withDummyPos <| EConst "" a b noWidgetDecl
eConst a b     = withDummyPos <| EConst " " a b noWidgetDecl
eList0 a b     = withDummyPos <| EList "" a "" b ""
eList a b      = withDummyPos <| EList " " a "" b ""
eTuple0 a      = eList0 a Nothing
eTuple a       = eList a Nothing
eComment a b   = withDummyPos <| EComment " " a b

pVar0 a        = withDummyRange <| PVar "" a noWidgetDecl
pVar a         = withDummyRange <| PVar " " a noWidgetDecl
pList0 ps      = withDummyRange <| PList "" ps "" Nothing ""
pList ps       = withDummyRange <| PList " " ps "" Nothing ""
pAs x p        = withDummyRange <| PAs " " x " " p

pListOfPVars names = pList (listOfPVars names)

-- note: dummy ids...
vTrue    = vBool True
vFalse   = vBool False
vBool    = val << VBase << VBool
vStr     = val << VBase << VString
vConst   = val << VConst
vBase    = val << VBase
vList    = val << VList
vDict    = val << VDict

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
  VBase (VString k) -> k
  _                 -> Debug.crash <| "unwrapVBaseString_: " ++ s


eRaw__ = EVar
eRaw0  = eVar0
eRaw   = eVar

listOfRaw = listOfVars

listOfVars xs =
  case xs of
    []     -> []
    x::xs_ -> eVar0 x :: List.map eVar xs_

listOfPVars xs =
  case xs of
    []     -> []
    x::xs_ -> pVar0 x :: List.map pVar xs_

listOfNums ns =
  case ns of
    []     -> []
    n::ns_ -> eConst0 n dummyLoc :: List.map (flip eConst dummyLoc) ns_

-- listOfNums1 = List.map (flip eConst dummyLoc)

type alias AnnotatedNum = (Num, Frozen, WidgetDecl)
  -- may want to move this up into EConst

listOfAnnotatedNums : List AnnotatedNum -> List Exp
listOfAnnotatedNums list =
  case list of
    [] -> []
    (n,ann,wd) :: list_ ->
      withDummyPos (EConst "" n (dummyLoc_ ann) wd) :: listOfAnnotatedNums1 list_

listOfAnnotatedNums1 =
 List.map (\(n,ann,wd) -> withDummyPos (EConst " " n (dummyLoc_ ann) wd))

minMax x y             = (min x y, max x y)
minNumTr (a,t1) (b,t2) = if a <= b then (a,t1) else (b,t2)
maxNumTr (a,t1) (b,t2) = if a >= b then (a,t1) else (b,t2)
minMaxNumTr nt1 nt2    = (minNumTr nt1 nt2, maxNumTr nt1 nt2)

plusNumTr (n1,t1) (n2,t2)  = (n1 + n2, TrOp Plus [t1, t2])
minusNumTr (n1,t1) (n2,t2) = (n1 + n2, TrOp Minus [t1, t2])


------------------------------------------------------------------------------
-- Whitespace helpers

precedingWhitespace : Exp -> String
precedingWhitespace exp =
  precedingWhitespaceExp__ exp.val.e__


indentationOf : Exp -> String
indentationOf exp =
  String.split "\n" (precedingWhitespace exp) |> Utils.last "Lang.indentationOf"

precedingWhitespacePat : Pat -> String
precedingWhitespacePat pat =
  case pat.val of
    PVar   ws ident wd         -> ws
    PConst ws n                -> ws
    PBase  ws v                -> ws
    PList  ws1 es ws2 rest ws3 -> ws1
    PAs    ws1 ident ws2 p     -> ws1


precedingWhitespaceExp__ : Exp__ -> String
precedingWhitespaceExp__ e__ =
  case e__ of
    EBase      ws v                     -> ws
    EConst     ws n l wd                -> ws
    EVar       ws x                     -> ws
    EFun       ws1 ps e1 ws2            -> ws1
    EApp       ws1 e1 es ws2            -> ws1
    EList      ws1 es ws2 rest ws3      -> ws1
    EOp        ws1 op es ws2            -> ws1
    EIf        ws1 e1 e2 e3 ws2         -> ws1
    ELet       ws1 kind rec p e1 e2 ws2 -> ws1
    ECase      ws1 e1 bs ws2            -> ws1
    ETypeCase  ws1 pat bs ws2           -> ws1
    EComment   ws s e1                  -> ws
    EOption    ws1 s1 ws2 s2 e1         -> ws1
    ETyp       ws1 pat tipe e ws2       -> ws1
    EColonType ws1 e ws2 tipe ws3       -> ws1
    ETypeAlias ws1 pat tipe e ws2       -> ws1


addPrecedingWhitespace : String -> Exp -> Exp
addPrecedingWhitespace newWs exp =
  mapPrecedingWhitespace (\oldWs -> oldWs ++ newWs) exp


replacePrecedingWhitespace : String -> Exp -> Exp
replacePrecedingWhitespace newWs exp =
  mapPrecedingWhitespace (\oldWs -> newWs) exp


replacePrecedingWhitespacePat : String -> Pat -> Pat
replacePrecedingWhitespacePat newWs pat =
  mapPrecedingWhitespacePat (\oldWs -> newWs) pat


-- Does not recurse.
mapPrecedingWhitespace : (String -> String) -> Exp -> Exp
mapPrecedingWhitespace mapWs exp =
  let e__New =
    case exp.val.e__ of
      EBase      ws v                     -> EBase      (mapWs ws) v
      EConst     ws n l wd                -> EConst     (mapWs ws) n l wd
      EVar       ws x                     -> EVar       (mapWs ws) x
      EFun       ws1 ps e1 ws2            -> EFun       (mapWs ws1) ps e1 ws2
      EApp       ws1 e1 es ws2            -> EApp       (mapWs ws1) e1 es ws2
      EList      ws1 es ws2 rest ws3      -> EList      (mapWs ws1) es ws2 rest ws3
      EOp        ws1 op es ws2            -> EOp        (mapWs ws1) op es ws2
      EIf        ws1 e1 e2 e3 ws2         -> EIf        (mapWs ws1) e1 e2 e3 ws2
      ELet       ws1 kind rec p e1 e2 ws2 -> ELet       (mapWs ws1) kind rec p e1 e2 ws2
      ECase      ws1 e1 bs ws2            -> ECase      (mapWs ws1) e1 bs ws2
      ETypeCase  ws1 pat bs ws2           -> ETypeCase  (mapWs ws1) pat bs ws2
      EComment   ws s e1                  -> EComment   (mapWs ws) s e1
      EOption    ws1 s1 ws2 s2 e1         -> EOption    (mapWs ws1) s1 ws2 s2 e1
      ETyp       ws1 pat tipe e ws2       -> ETyp       (mapWs ws1) pat tipe e ws2
      EColonType ws1 e ws2 tipe ws3       -> EColonType (mapWs ws1) e ws2 tipe ws3
      ETypeAlias ws1 pat tipe e ws2       -> ETypeAlias (mapWs ws1) pat tipe e ws2
  in
  replaceE__ exp e__New


cleanupListWhitespace : String -> List Exp -> List Exp
cleanupListWhitespace sepWs exps =
  case exps of
    []                  -> []
    firstExp::laterExps ->
      replacePrecedingWhitespace "" firstExp :: List.map (replacePrecedingWhitespace sepWs) laterExps

cleanupPatListWhitespace : String -> List Pat -> List Pat
cleanupPatListWhitespace sepWs pats =
  case pats of
    []                  -> []
    firstPat::laterPats ->
      replacePrecedingWhitespacePat "" firstPat :: List.map (replacePrecedingWhitespacePat sepWs) laterPats


-- copyListWhitespace is in LangTools to access the unparser
-- copyListWhitespace : Exp -> Exp -> Exp
-- copyListWhitespace templateList list =
--   case (templateList.val.e__, list.val.e__) of
--     (EList ws1 _ ws2 _ ws3, EList _ heads _ maybeTail _) ->
--       replaceE__ list (EList ws1 heads ws2 maybeTail ws3)
--
--     _ ->
--       Debug.crash <| "Lang.copyListWs expected lists, but given " ++ unparseWithIds templateList ++ " and " ++ unparseWithIds list


-- Unindents until an expression is flush to the edge, then adds spaces to the indentation.
replaceIndentation : String -> Exp -> Exp
replaceIndentation spaces exp =
  indent spaces (unindent exp)


-- Finds lowest amount of indentation and then removes it from all expressions.
unindent : Exp -> Exp
unindent exp =
  let smallestIndentation =
    exp
    |> foldExpViaE__
        (\e__ smallest ->
          case Regex.find Regex.All (Regex.regex "\n( *)$") (precedingWhitespaceExp__ e__) |> List.map .submatches |> List.concat of
            [Just indentation] -> if String.length indentation < String.length smallest then indentation else smallest
            _                  -> smallest
        )
        "                                                                                                                                                                                                                                        "
  in
  let removeIndentation ws =
    ws |> Regex.replace Regex.All (Regex.regex ("\n" ++ smallestIndentation)) (\_ -> "\n")
  in
  mapExp (mapPrecedingWhitespace removeIndentation) exp


-- Increases indentation by spaces string.
indent : String -> Exp -> Exp
indent spaces e =
  -- let recurse = indent spaces in
  -- let wrap e__ = P.WithInfo (Exp_ e__ e.val.eid) e.start e.end in
  let processWS ws =
    ws |> String.reverse
       |> Regex.replace (Regex.AtMost 1) (Regex.regex "\n") (\_ -> spaces ++ "\n")
       |> String.reverse
  in
  mapExp (mapPrecedingWhitespace processWS) e


mapPrecedingWhitespacePat : (String -> String) -> Pat -> Pat
mapPrecedingWhitespacePat mapWs pat =
  let pat__ =
    case pat.val of
      PVar   ws ident wd         -> PVar   (mapWs ws) ident wd
      PConst ws n                -> PConst (mapWs ws) n
      PBase  ws v                -> PBase  (mapWs ws) v
      PList  ws1 es ws2 rest ws3 -> PList  (mapWs ws1) es ws2 rest ws3
      PAs    ws1 ident ws2 p     -> PAs    (mapWs ws1) ident ws2 p
  in
  { pat | val = pat__ }
