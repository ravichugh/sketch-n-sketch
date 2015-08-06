module Lang where

import String
import Debug
import Dict

import OurParser2 as P
import Utils

------------------------------------------------------------------------------

type alias Loc = (LocId, Frozen, Ident)  -- "" rather than Nothing b/c comparable
type alias LocId = Int
type alias Ident = String
type alias Num = Float

type alias Frozen = String -- b/c comparable
(frozen, unann, thawed) = ("!", "", "?")

type alias Pat    = P.WithInfo Pat_
type alias Exp    = P.WithInfo Exp_
type alias Op     = P.WithInfo Op_
type alias Branch = P.WithInfo Branch_
type alias Range  = P.WithInfo Range_

type Pat_
  = PVar Ident
  | PList (List Pat) (Maybe Pat)

type Op_
  -- nullary ops
  = Pi
  -- unary ops
  | Cos | Sin | ArcCos | ArcSin
  | Floor | Ceil | Round
  | ToStr
  -- binary ops
  | Plus | Minus | Mult | Div
  | Lt | Eq
  -- internal ops
  | RangeOffset Int

type Exp_
  = EConst Num Loc
  | EBase BaseVal
  | EVar Ident
  | EFun (List Pat) Exp
  | EApp Exp (List Exp)
  | EOp Op (List Exp)
  | EList (List Exp) (Maybe Exp)
  | EIndList (List Range)
  | EIf Exp Exp Exp
  | ECase Exp (List Branch)
  | ELet LetKind Rec Pat Exp Exp
  | EComment String Exp
  | EOption (P.WithInfo String) (P.WithInfo String) Exp

    -- EFun [] e     impossible
    -- EFun [p] e    (\p. e)
    -- EFun ps e     (\(p1 ... pn) e) === (\p1 (\p2 (... (\pn e) ...)))
    
    -- EApp f []     impossible
    -- EApp f [x]    (f x)
    -- EApp f xs     (f x1 ... xn) === ((... ((f x1) x2) ...) xn)

type alias Branch_ = (Pat, Exp)

type LetKind = Let | Def
type alias Rec = Bool

type Range_ -- right now, Exps are always EConsts
  = Interval Exp Exp
  | Point Exp

type Val
  = VConst NumTr
  | VBase BaseVal
  | VClosure (Maybe Ident) Pat Exp Env
  | VList (List Val)
  | VHole Int

type alias NumTr = (Num, Trace)

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

strRange : Bool -> Int -> Range -> String
strRange showLocs k r = case r.val of
  Point e        -> sExp_ showLocs k e
  Interval e1 e2 -> sExp_ showLocs k e1 ++ ".." ++ sExp_ showLocs k e2

strVal     = strVal_ False
strValLocs = strVal_ True

strNum     = toString

strVal_ : Bool -> Val -> String
strVal_ showTraces v =
  let foo = strVal_ showTraces in
  case v of
    VConst (i,tr)    -> strNum i
                          ++ if | showTraces -> Utils.braces (strTrace tr)
                                | otherwise  -> ""
    VBase b          -> strBaseVal b
    VClosure _ _ _ _ -> "<fun>"
    VList vs         -> Utils.bracks (String.join " " (List.map foo vs))
    VHole i          -> "HOLE_" ++ toString i

strOp op = case op of
  Plus  -> "+"
  Minus -> "-"
  Mult  -> "*"
  Div   -> "/"
  Lt    -> "<"
  Eq    -> "="
  Pi    -> "pi"
  Cos   -> "cos"
  Sin   -> "sin"
  ArcCos -> "arccos"
  ArcSin -> "arcsin"
  Floor -> "floor"
  Ceil  -> "ceiling"
  Round -> "round"
  ToStr -> "toString"
  RangeOffset i -> "[[rangeOffset " ++ toString i ++ "]]"

strLoc (k, b, mx) =
  "k" ++ toString k ++ (if mx == "" then "" else "_" ++ mx) ++ b

strTrace tr = case tr of
  TrLoc l   -> strLoc l
  TrOp op l ->
    Utils.parens (String.concat
      [strOp op, " ", String.join " " (List.map strTrace l)])

strPat p = case p.val of
  PVar x     -> x
  PList ps m -> let s = Utils.spaces (List.map strPat ps) in
                case m of
                  Nothing   -> Utils.bracks s
                  Just rest -> Utils.bracks (s ++ " | " ++ strPat rest)

tab k = String.repeat k "  "

sExpK k     = (++) (tab k) << sExp_ False k
sExpLocsK k = (++) (tab k) << sExp_ True k
sExp        = sExpK 0
sExpLocs    = sExpLocsK 0

sExp_ : Bool -> Int -> Exp -> String
sExp_ showLocs k e =
  let foo = sExp_ showLocs in
  let indent = maybeIndent showLocs k in
  case e.val of
    EBase v -> strBaseVal v
    EConst i l ->
      let (_,b,_) = l in
      toString i
        ++ b
        ++ if showLocs then Utils.braces (strLoc l) else ""
    EVar x -> x
    EFun [p] e ->
      Utils.parens <| "\\" ++ strPat p ++ indent e
    EFun ps e ->
      let args = Utils.spaces (List.map strPat ps) in
      Utils.parens <| "\\" ++ Utils.parens args ++ indent e
    EApp e1 [e2] ->
      Utils.parens <| foo k e1 ++ " " ++ indent e2
    EApp e1 es ->
      Utils.parens <|
        let s1 = foo k e1
            ss = List.map (foo (k+1)) es
            s2 = Utils.spaces ss in
        if fitsOnLine s2
        then s1 ++ " " ++ s2
        else String.join ("\n" ++ tab (k+1)) (s1::ss)
    EOp op es ->
      Utils.parens <| String.join " " (strOp op.val :: List.map (foo k) es)
    EIf e1 e2 e3 ->
      let s =
        Utils.parens <| Utils.spaces [ "if", foo k e1, foo k e2, foo k e3 ] in
      if fitsOnLine s then s
      else
      Utils.parens <|
        "if " ++ foo k e1 ++ "\n" ++
          tab (k+1) ++ foo (k+1) e2 ++ "\n" ++
          tab (k+1) ++ foo (k+1) e3
    EList es mrest ->
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
    EIndList rs ->
      Utils.ibracks <|
        let rstrs = List.map (strRange showLocs k) rs
            totstr = Utils.spaces rstrs
        in if fitsOnLine totstr then
          totstr
        else String.join ("\n" ++ tab k ++ " ") rstrs
    ELet Let b p e1 e2 ->
      Utils.parens <|
        let k' = if isLet e2 then k else k + 1 in
        (if b then "letrec " else "let ") ++ strPat p ++
          indent e1 ++ "\n" ++
          tab k' ++ foo k' e2
    ELet Def b p e1 e2 ->
      let s = if b then "defrec " else "def " in
      Utils.parens (s ++ strPat p ++ indent e1) ++ "\n" ++
      tab k ++ foo k e2
    ECase e1 l ->
      let bar (pi,ei) =
        tab (k+1) ++ Utils.parens (strPat pi ++ " " ++ foo (k+1) ei) in
      Utils.parens <|
        "case " ++ foo k e1 ++ "\n" ++ Utils.lines (List.map (bar << .val) l)
    EComment s e1 ->
      ";" ++ s ++ "\n" ++
      tab k ++ foo k e1
    EOption s1 s2 e1 ->
      "# " ++ s1.val ++ ": " ++ s2.val ++ "\n" ++
      tab k ++ foo k e1

maybeIndent showLocs k e =
  let s = sExp_ showLocs (k+1) e in
  if | fitsOnLine s -> " " ++ s
     | otherwise    -> "\n" ++ tab (k+1) ++ s

-- TODO take into account indent and other prefix of current line
fitsOnLine s =
  if | String.length s > 70               -> False
     | List.member '\n' (String.toList s) -> False
     | otherwise                          -> True

isLet e = case e.val of
  ELet _ _ _ _ _  -> True
  EComment _ e1 -> isLet e1
  _             -> False


------------------------------------------------------------------------------
-- Mapping WithInfo/WithPos

mapValField f r = { r | val <- f r.val }


------------------------------------------------------------------------------
-- Mapping

mapExp : (Exp_ -> Exp_) -> Exp -> Exp
mapExp f e =
  let foo = mapExp f in
  let g e_ = P.WithInfo (f e_) e.start e.end in
  case e.val of
    EConst _ _     -> g e.val
    EBase _        -> g e.val
    EVar _         -> g e.val
    EFun ps e'     -> g (EFun ps (foo e'))
    EApp e1 es     -> g (EApp (foo e1) (List.map foo es))
    EOp op es      -> g (EOp op (List.map foo es))
    EList es m     -> g (EList (List.map foo es) (Utils.mapMaybe foo m))
    EIndList rs    -> let foo r_ = case r_ of
                        Interval e1 e2 -> Interval (mapExp f e1) (mapExp f e2)
                        Point e1       -> Point (mapExp f e1)
                      in
                      g (EIndList (List.map (mapValField foo) rs))
    EIf e1 e2 e3   -> g (EIf (foo e1) (foo e2) (foo e3))
    ECase e1 l     -> g (ECase (foo e1) (List.map (mapValField (\(p,ei) -> (p, foo ei))) l))
    EComment s e1  -> g (EComment s (foo e1))
    EOption s1 s2 e1 -> g (EOption s1 s2 (foo e1))
    ELet k b p e1 e2 -> g (ELet k b p (foo e1) (foo e2))

mapVal : (Val -> Val) -> Val -> Val
mapVal f v = case v of
  VList vs         -> f (VList (List.map (mapVal f) vs))
  VConst _         -> f v
  VBase _          -> f v
  VClosure _ _ _ _ -> f v
  VHole _          -> f v

foldVal : (Val -> a -> a) -> Val -> a -> a
foldVal f v a = case v of
  VList vs         -> f v (List.foldl (foldVal f) a vs)
  VConst _         -> f v a
  VBase _          -> f v a
  VClosure _ _ _ _ -> f v a
  VHole _          -> f v a


------------------------------------------------------------------------------
-- Substitutions

type alias Subst = Dict.Dict LocId Num

applySubst : Subst -> Exp -> Exp
applySubst subst e = (\e_ -> P.WithInfo e_ e.start e.end) <| case e.val of
  EConst n l ->
    case Dict.get (Utils.fst3 l) subst of
      Just i -> EConst i l
   -- Nothing -> EConst n l
  EBase _    -> e.val
  EVar _     -> e.val
  EFun ps e' -> EFun ps (applySubst subst e')
  EOp op es  -> EOp op (List.map (applySubst subst) es)
  EList es m -> EList (List.map (applySubst subst) es)
                      (Utils.mapMaybe (applySubst subst) m)
  EIndList rs ->
    let f r_ = case r_ of
      Interval e1 e2 -> Interval (applySubst subst e1) (applySubst subst e2)
      Point e1       -> Point (applySubst subst e1) in
    EIndList (List.map (mapValField f) rs)
  EApp f es  -> EApp (applySubst subst f) (List.map (applySubst subst) es)
  ELet k b p e1 e2 ->
    ELet k b p (applySubst subst e1) (applySubst subst e2) -- TODO
  EIf e1 e2 e3 ->
    EIf (applySubst subst e1) (applySubst subst e2) (applySubst subst e3)
  ECase e l ->
    ECase (applySubst subst e) (List.map (mapValField (\(p,ei) -> (p, applySubst subst ei))) l)
  EComment s e1 ->
    EComment s (applySubst subst e1)
  EOption s1 s2 e1 ->
    EOption s1 s2 (applySubst subst e1)


------------------------------------------------------------------------------
-- Lang Options

-- all options should appear before the first non-comment expression

getOptions : Exp -> List (String, String)
getOptions e = case e.val of
  EOption s1 s2 e1 -> (s1.val, s2.val) :: getOptions e1
  EComment _ e1    -> getOptions e1
  _                -> []


------------------------------------------------------------------------------
-- Abstract Syntax Helpers

-- NOTE: the Exp builders use dummyPos

withDummyPos e_ = P.WithInfo e_ P.dummyPos P.dummyPos

dummyLoc_ b = (0, b, "")
dummyTrace_ b = TrLoc (dummyLoc_ b)

dummyLoc = dummyLoc_ unann
dummyTrace = dummyTrace_ unann

ePlus e1 e2 = withDummyPos <| EOp (withDummyPos Plus) [e1,e2]

eBool  = withDummyPos << EBase << Bool
eTrue  = eBool True
eFalse = eBool False

vBool  = VBase << Bool
vTrue  = vBool True
vFalse = vBool False
vStr   = VBase << String

eApp e es = case es of
  [e1]    -> withDummyPos <| EApp e [e1]
  e1::es' -> eApp (withDummyPos <| EApp e [e1]) es'

eFun ps e = case ps of
  [p]     -> withDummyPos <| EFun [p] e
  p::ps'  -> withDummyPos <| EFun [p] (eFun ps' e)

ePair e1 e2 = withDummyPos <| EList [e1,e2] Nothing

eLets xes eBody = case xes of
  (x,e)::xes' -> withDummyPos <|
                   ELet Let False (withDummyPos (PVar x)) e (eLets xes' eBody)
  []          -> eBody

eVar a         = withDummyPos <| EVar a
eConst a b     = withDummyPos <| EConst a b
eList a b      = withDummyPos <| EList a b
eComment a b   = withDummyPos <| EComment a b

pVar a         = withDummyPos <| PVar a
