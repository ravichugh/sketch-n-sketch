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
  = PVar Ident WidgetDecl
  | PConst Num
  | PBase BaseVal
  | PList (List Pat) (Maybe Pat)

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
  = EConst Num Loc WidgetDecl
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

strRange : Bool -> Int -> Range -> String
strRange showLocs k r = case r.val of
  Point e        -> sExp_ showLocs k e
  Interval e1 e2 -> sExp_ showLocs k e1 ++ ".." ++ sExp_ showLocs k e2

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
  Eq            -> " = "
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
  PVar x _   -> x -- TODO strWidgetDecl wd, but not called anyway
  PList ps m -> let s = Utils.spaces (List.map strPat ps) in
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
    EBase v -> strBaseVal v
    EConst i l _ -> -- TODO
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
  if fitsOnLine s
    then " " ++ s
    else "\n" ++ tab (k+1) ++ s

-- TODO take into account indent and other prefix of current line
fitsOnLine s =
  if String.length s > 70 then False
  else if List.member '\n' (String.toList s) then False
  else True

isLet e = case e.val.e__ of
  ELet _ _ _ _ _  -> True
  EComment _ e1 -> isLet e1
  _             -> False


------------------------------------------------------------------------------
-- Mapping WithInfo/WithPos

mapValField f r = { r | val = f r.val }


------------------------------------------------------------------------------
-- Mapping

mapExp : (Exp__ -> Exp__) -> Exp -> Exp
mapExp f e =
  let foo = mapExp f in
  let g e__ = P.WithInfo (Exp_ (f e__) e.val.eid) e.start e.end in
  case e.val.e__ of
    EConst _ _ _   -> g e.val.e__
    EBase _        -> g e.val.e__
    EVar _         -> g e.val.e__
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


------------------------------------------------------------------------------
-- Location Substitutions
-- Expression Substitutions

type alias Subst = Dict.Dict LocId Num
type alias SubstPlus = Dict.Dict LocId (P.WithInfo Num)
type alias SubstMaybeNum = Dict.Dict LocId (Maybe Num)

type alias ESubst = Dict.Dict EId Exp__

type alias TwoSubsts = { lsubst : Subst, esubst : ESubst }

applyLocSubst : Subst -> Exp -> Exp
applyLocSubst s = applySubst { lsubst = s, esubst = Dict.empty }

applyESubst : ESubst -> Exp -> Exp
applyESubst s = applySubst { lsubst = Dict.empty, esubst = s }

applySubst : TwoSubsts -> Exp -> Exp
applySubst subst e =
  (\e__ ->
    let e__' =
      case Dict.get e.val.eid subst.esubst of
        Just eNew -> eNew
        Nothing   -> e__
    in
    P.WithInfo (Exp_ e__' e.val.eid) e.start e.end) <|
 case e.val.e__ of
  EConst n l wd ->
    case Dict.get (Utils.fst3 l) subst.lsubst of
      Just i -> EConst i l wd
      Nothing -> EConst n l wd
      -- 10/28: substs from createMousePosCallbackSlider only bind
      -- updated values (unlike substs from Sync)
  EBase _    -> e.val.e__
  EVar _     -> e.val.e__
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


unfrozenLocIdsAndNumbers : Exp -> List (LocId, Num)
unfrozenLocIdsAndNumbers e =
  case e.val.e__ of
    EConst n l wd ->
      case l of
        (locId, "!", _) -> []
        (locId, _,   _) -> [(locId, n)]

    EBase _    -> []
    EVar _     -> []
    EFun ps e' -> unfrozenLocIdsAndNumbers e'
    EOp op es  -> List.concat (List.map unfrozenLocIdsAndNumbers es)
    EList es m ->
      case m of
        Just e  -> List.concat (List.map unfrozenLocIdsAndNumbers es) |> List.append (unfrozenLocIdsAndNumbers e)
        Nothing -> List.concat (List.map unfrozenLocIdsAndNumbers es)

    EIndList rs -> []
    EApp f es  -> List.concat (List.map unfrozenLocIdsAndNumbers es)
    ELet k b p e1 e2 -> (unfrozenLocIdsAndNumbers e1) ++ (unfrozenLocIdsAndNumbers e2)
    EIf e1 e2 e3 -> (unfrozenLocIdsAndNumbers e1) ++ (unfrozenLocIdsAndNumbers e2) ++ (unfrozenLocIdsAndNumbers e3)
    ECase e l -> (unfrozenLocIdsAndNumbers e) ++ List.concat (List.map (\branch -> unfrozenLocIdsAndNumbers (snd branch.val)) l)
    EComment s e1 -> (unfrozenLocIdsAndNumbers e1)
    EOption s1 s2 e1 -> (unfrozenLocIdsAndNumbers e1)

{-
-- for now, LocId instead of EId
type alias ESubst = Dict.Dict LocId Exp__

applyESubst : ESubst -> Exp -> Exp
applyESubst esubst =
  mapExp <| \e__ -> case e__ of
    EConst _ i -> case Dict.get (Utils.fst3 i) esubst of
                    Nothing   -> e__
                    Just e__' -> e__'
    _          -> e__
-}


------------------------------------------------------------------------------
-- Lang Options

-- all options should appear before the first non-comment expression

getOptions : Exp -> List (String, String)
getOptions e = case e.val.e__ of
  EOption s1 s2 e1 -> (s1.val, s2.val) :: getOptions e1
  EComment _ e1    -> getOptions e1
  _                -> []


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

ePlus e1 e2 = withDummyPos <| EOp (withDummyRange Plus) [e1,e2]

eBool  = withDummyPos << EBase << Bool
eTrue  = eBool True
eFalse = eBool False

eApp e es = case es of
  []      -> Debug.crash "eApp"
  [e1]    -> withDummyPos <| EApp e [e1]
  e1::es' -> eApp (withDummyPos <| EApp e [e1]) es'

eFun ps e = case ps of
  []      -> Debug.crash "eFun"
  [p]     -> withDummyPos <| EFun [p] e
  p::ps'  -> withDummyPos <| EFun [p] (eFun ps' e)

ePair e1 e2 = withDummyPos <| EList [e1,e2] Nothing

noWidgetDecl = withDummyRange NoWidgetDecl

eLets xes eBody = case xes of
  (x,e)::xes' -> withDummyPos <|
                   ELet Let False (withDummyRange (PVar x noWidgetDecl)) e (eLets xes' eBody)
  []          -> eBody

eVar a         = withDummyPos <| EVar a
eConst a b     = withDummyPos <| EConst a b noWidgetDecl
eList a b      = withDummyPos <| EList a b
eComment a b   = withDummyPos <| EComment a b

pVar a         = withDummyRange <| PVar a noWidgetDecl

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
