module Lang where

import List ((::))
import List
import String
import Debug
import Dict

import Utils

------------------------------------------------------------------------------

type alias Loc = Int
type alias Ident = String

type Pat
  = PVar Ident
  | PList (List Pat) (Maybe Pat)

type Op
  = Plus | Minus | Mult
  | Lt

type Exp
  = EConst Int Loc
  | EBase BaseVal
  | EVar Ident
  | EFun (List Pat) Exp
  | EApp Exp (List Exp)
  | EOp Op (List Exp)
  | EList (List Exp) (Maybe Exp)
  | EIf Exp Exp Exp
  | ELet Bool Ident Exp Exp -- TODO

    -- EFun [] e     impossible
    -- EFun [p] e    (\p. e)
    -- EFun ps e     (\(p1 ... pn) e) === (\p1 (\p2 (... (\pn e) ...)))
    
    -- EApp f []     impossible
    -- EApp f [x]    (f x)
    -- EApp f xs     (f x1 ... xn) === ((... ((f x1) x2) ...) xn)

type Val
  = VConst Int Trace
  | VBase BaseVal
  | VClosure (Maybe Ident) Pat Exp Env
  | VList (List Val)
  | VHole

type BaseVal -- unlike Ints, these cannot be changed by Sync
  = Bool Bool
  | String String

type Trace = TrLoc Loc | TrOp Op (List Trace)


------------------------------------------------------------------------------
-- Big-Step Operational Semantics

type alias Env = List (Ident, Val)

match : (Pat, Val) -> Maybe Env
match pv = case pv of
  (PVar x, v) -> Just [(x,v)]
  (PList ps Nothing, VList vs) ->
    Utils.bindMaybe matchList (Utils.maybeZip ps vs)
  (PList ps (Just rest), VList vs) ->
    let (n,m) = (List.length ps, List.length vs) in
    if | n > m     -> Nothing
       | otherwise -> let (vs1,vs2) = Utils.split n vs in
                      (rest, VList vs2) `cons` (matchList (Utils.zip ps vs1))
  (PList _ _, _) -> Nothing

matchList : List (Pat, Val) -> Maybe Env
matchList pvs =
  List.foldl (\pv acc ->
    case (acc, match pv) of
      (Just old, Just new) -> Just (new ++ old)
      _                    -> Nothing
  ) (Just []) pvs

cons : (Pat, Val) -> Maybe Env -> Maybe Env
cons pv menv =
  case (menv, match pv) of
    (Just env, Just env') -> Just (env' ++ env)
    _                     -> Nothing

lookupVar env x =
  case Utils.maybeFind x env of
    Just v -> v
    Nothing -> Debug.crash <| "eval: var " ++ Utils.bracks x

eval : Env -> Exp -> Val
eval env e = case e of

  EConst i l -> VConst i (TrLoc l)
  EBase v    -> VBase v
  EVar x     -> lookupVar env x
  EFun [p] e -> VClosure Nothing p e env
  EOp op es  -> evalOp env op es

  EList es m ->
    let vs = List.map (eval env) es in
    case m of
      Nothing   -> VList vs
      Just rest -> case eval env rest of
                     VList vs' -> VList (vs ++ vs')

  EIf e1 e2 e3 ->
    case eval env e1 of
      VBase (Bool True)  -> eval env e2
      VBase (Bool False) -> eval env e3

  EApp e1 [e2] ->
    let (v1,v2) = (eval env e1, eval env e2) in
    case v1 of
      VClosure Nothing p e env' ->
        case (p, v2) `cons` Just env' of
          Just env'' -> eval env'' e
      VClosure (Just f) p e env' ->
        case (PVar f, v1) `cons` ((p, v2) `cons` Just env') of
          Just env'' -> eval env'' e

  ELet True f e1 e2 ->
    case eval env e1 of
      VClosure Nothing x body env' ->
        let _   = Utils.assert "eval letrec" (env == env') in
        let v1' = VClosure (Just f) x body env in
        case (PVar f, v1') `cons` Just env of
          Just env' -> eval env' e2

  -- abstract syntactic sugar
  EFun ps e  -> eval env (eFun ps e)
  EApp e1 es -> eval env (eApp e1 es)
  ELet False x e1 e2 -> eval env (EApp (EFun [PVar x] e2) [e1])

evalOp env op es =
  case (op, List.map (eval env) es) of
    (Plus, [VConst i1 t1, VConst i2 t2]) -> VConst (i1+i2) (TrOp op [t1,t2])
    (Minus, [VConst i1 t1, VConst i2 t2]) -> VConst (i1-i2) (TrOp op [t1,t2])
    (Mult, [VConst i1 t1, VConst i2 t2]) -> VConst (i1*i2) (TrOp op [t1,t2])
    (Lt  , [VConst i1 t1, VConst i2 t2]) -> vBool (i1<i2)

run : Exp -> Val
run = eval []


------------------------------------------------------------------------------
-- Unparsing

strBaseVal v = case v of
  Bool True  -> "true"
  Bool False -> "false"

strVal     = strVal_ False
strValLocs = strVal_ True

strVal_ showTraces v =
  let foo = strVal_ showTraces in
  case v of
    VConst i tr      -> toString i
                          ++ if | showTraces -> Utils.braces (strTrace tr)
                                | otherwise  -> ""
    VBase b          -> strBaseVal b
    VClosure _ _ _ _ -> "<fun>"
    VList vs         -> Utils.bracks (String.join " " (List.map foo vs))
    VHole            -> "??"

strOp op = case op of {Plus -> "+"; Minus -> "-"; Mult -> "*"; Lt -> "<"}

strLoc l = "k" ++ toString l

strTrace tr = case tr of
  TrLoc l   -> strLoc l
  TrOp op l ->
    Utils.parens (String.concat
      [strOp op, " ", String.join " " (List.map strTrace l)])

strPat p = case p of
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

sExp_ showLocs k e =
  let foo = sExp_ showLocs in
  case e of
    EBase v        -> strBaseVal v
    EConst i l     -> toString i
                        ++ if | showLocs  -> Utils.braces (strLoc l)
                              | otherwise -> ""
    EVar x         -> x
    EFun [p] e     -> Utils.parens <|
                        "\\" ++ strPat p ++ "\n" ++ tab (k+1) ++ foo (k+1) e
    EFun ps e      -> Utils.parens <|
                        "\\" ++ Utils.parens (Utils.spaces (List.map strPat ps)) ++ "\n" ++
                          tab (k+1) ++ foo (k+1) e
    EApp e1 [e2]   -> Utils.parens <| foo k e1 ++ " " ++ foo k e2
    EApp e1 es     -> Utils.parens <|
                        foo k e1 ++ " " ++ Utils.spaces (List.map (foo k) es)
    EOp op [e1,e2] -> Utils.parens <|
                        String.join " " [strOp op, foo k e1, foo k e2]
    EList es mrest -> Utils.bracks <|
                        let s = String.join " " (List.map (foo k) es) in
                        case mrest of
                          Nothing -> s
                          Just e  -> s ++ " | " ++ foo k e
    EIf e1 e2 e3   -> Utils.parens <|
                        "if " ++ foo k e1 ++ "\n" ++
                          tab (k+1) ++ foo (k+1) e2 ++ "\n" ++
                          tab (k+1) ++ foo (k+1) e3
    ELet b x e1 e2 -> Utils.parens <|
                        (if b then "letrec " else "let ") ++ x ++ "\n" ++
                          tab (k+1) ++ foo (k+1) e1 ++ "\n" ++
                          tab (k+1) ++ foo (k+1) e2


------------------------------------------------------------------------------
-- Substitutions

type alias Subst = Dict.Dict Loc Int

applySubst : Subst -> Exp -> Exp
applySubst subst e = case e of
  EConst _ l -> case Dict.get l subst of Just i -> EConst i l
  EBase _    -> e
  EVar _     -> e
  EFun _ _   -> e   -- not recursing into lambdas
  EOp op es  -> EOp op (List.map (applySubst subst) es)
  EList es m -> EList (List.map (applySubst subst) es)
                      (Utils.mapMaybe (applySubst subst) m)
  EApp f es  -> EApp (applySubst subst f) (List.map (applySubst subst) es)
  ELet b x e1 e2 ->
    ELet b x (applySubst subst e1) (applySubst subst e2) -- TODO
  EIf e1 e2 e3 ->
    EIf (applySubst subst e1) (applySubst subst e2) (applySubst subst e3)


------------------------------------------------------------------------------
-- Value Contexts

type alias VContext = Val
  -- invariant: a VContext is a Val with exactly one VHole

fillHoleWith : VContext -> Val -> Val
fillHoleWith vc w = case vc of
  VHole            -> w
  VConst _ _       -> vc
  VClosure _ _ _ _ -> vc   -- not recursing into closures
  VList vs         -> VList (List.map (flip fillHoleWith w) vs)

type VDiff = Same Val | Diff VContext Val Val

diff : Val -> Val -> Maybe VDiff
diff v1 v2 = 
  let res = diff_ v1 v2 in
  case res of
    Just (Diff vc w1 w2) ->
      let (v1',v2') = (fillHoleWith vc w1, fillHoleWith vc w2) in
      if | eqV (v1,v1') && eqV (v2,v2') -> res
         | otherwise ->
             Debug.crash (String.join "\n"
               ["bad diff", strVal vc, strVal w1, strVal w2])
    _ -> res

eqV (v1,v2) = case (v1, v2) of            -- equality modulo traces
  (VConst i tr, VConst j _) -> i == j
  (VList vs1, VList vs2) ->
    case Utils.maybeZip vs1 vs2 of
      Nothing -> False
      Just l  -> List.all eqV l
  _ -> False
  
-- assuming that v1 is the value resulting from eval (so it has proper locs)
-- and that v2 has dummy locs

diff_ v1 v2 = case (v1, v2) of
  (VConst i tr, VConst j _) ->
    if | i == j    -> Just (Same (VConst i tr))  -- cf. comment above
       | otherwise -> Just (Diff VHole v1 (VConst j tr))
  (VList vs1, VList vs2) ->
    case Utils.maybeZip vs1 vs2 of
      Nothing -> Nothing
      Just l ->
        List.foldr (\(vi1,vi2) acc ->
          case acc of
            Nothing -> Nothing
            Just (Same (VList vs)) ->
              case diff_ vi1 vi2 of
                Nothing              -> Nothing
                Just (Same v)        -> justSameVList (v::vs)
                Just (Diff vc w1 w2) -> Just (Diff (VList (vc::vs)) w1 w2)
            Just (Diff (VList vs) w1 w2) ->
              case diff_ vi1 vi2 of
                Nothing              -> Nothing
                Just (Same v)        -> Just (Diff (VList (v::vs)) w1 w2)
                Just (Diff _ _ _)    -> Nothing
            Just (Diff _ _ _) ->
              Debug.crash "diff_: error?"
        ) (justSameVList []) l
  _ ->
    if | v1 == v2  -> Just (Same v1)
       | otherwise -> Nothing
           
justSameVList = Just << Same << VList
      

------------------------------------------------------------------------------
-- Abstract Syntax Helpers

dummyLoc = 0
dummyTrace = TrLoc dummyLoc
eConst = flip EConst dummyLoc
vConst = flip VConst dummyTrace
ePlus e1 e2 = EOp Plus [e1,e2]

eBool  = EBase << Bool
eTrue  = eBool True
eFalse = eBool False

vBool  = VBase << Bool
vTrue  = vBool True
vFalse = vBool False

eApp e es = case es of
  [e1]    -> EApp e [e1]
  e1::es' -> eApp (EApp e [e1]) es'

eFun xs e = case xs of
  [x]     -> EFun [x] e
  x::xs'  -> EFun [x] (eFun xs' e)

