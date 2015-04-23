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

type alias Pat = Ident -- TODO

type Op
  = Plus | Mult
  | Lt

type Exp
  = EConst Int Loc
  | EBase BaseVal
  | EVar Ident
  | EFun (List Pat) Exp
  | EApp Exp (List Exp)
  | EOp Op (List Exp)
  | EList (List Exp)
  | EIf Exp Exp Exp
  | ELet Ident Exp Exp -- TODO

    -- EFun [] e     impossible
    -- EFun [p] e    (\p. e)
    -- EFun ps e     (\(p1 ... pn) e) === (\p1 (\p2 (... (\pn e) ...)))
    
    -- EApp f []     impossible
    -- EApp f [x]    (f x)
    -- EApp f xs     (f x1 ... xn) === ((... ((f x1) x2) ...) xn)

type Val
  = VConst Int Trace
  | VBase BaseVal
  | VClosure Pat Exp Env
  | VList (List Val)
  | VHole

type BaseVal -- unlike Ints, these cannot be changed by Sync
  = Bool Bool
  | String String

type Trace = TrLoc Loc | TrOp Op (List Trace)


------------------------------------------------------------------------------
-- Big-Step Operational Semantics

type alias Env = List (Ident, Val)

lookupVar env x =
  case Utils.maybeFind x env of
    Just v -> v
    Nothing -> Debug.crash <| "eval: var " ++ Utils.bracks x

eval : Env -> Exp -> Val
eval env e = case e of
  EConst i l -> VConst i (TrLoc l)
  EBase v    -> VBase v
  EVar x     -> lookupVar env x
  EFun [x] e -> VClosure x e env
  EOp op es  -> evalOp env op es
  EList es   -> VList (List.map (eval env) es)
  EIf e1 e2 e3 ->
    case eval env e1 of
      VBase (Bool True)  -> eval env e2
      VBase (Bool False) -> eval env e3
  EApp e1 [e2] ->
    case eval env e1 of VClosure x e env' ->
      let v2 = eval env e2 in eval ((x,v2)::env') e

  -- abstract syntactic sugar
  EFun xs e  -> eval env (eFun xs e)
  EApp e1 es -> eval env (eApp e1 es)
  ELet x e1 e2 -> eval env (EApp (EFun [x] e2) [e1])

evalOp env op es =
  case (op, List.map (eval env) es) of
    (Plus, [VConst i1 t1, VConst i2 t2]) -> VConst (i1+i2) (TrOp op [t1,t2])
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
    VClosure x e env -> "<fun>"
    VList vs         -> Utils.bracks (String.join " " (List.map foo vs))
    VHole            -> "??"

strOp op = case op of {Plus -> "+"; Mult -> "*"; Lt -> "<"}

strLoc l = "k" ++ toString l

strTrace tr = case tr of
  TrLoc l   -> strLoc l
  TrOp op l ->
    Utils.parens (String.concat
      [strOp op, " ", String.join " " (List.map strTrace l)])

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
    EFun [x] e     -> Utils.parens <|
                        "\\" ++ x ++ "\n" ++ tab (k+1) ++ foo (k+1) e
    EFun xs e      -> Utils.parens <|
                        "\\" ++ Utils.parens (Utils.spaces xs) ++ "\n" ++
                          tab (k+1) ++ foo (k+1) e
    EApp e1 [e2]   -> Utils.parens <| foo k e1 ++ " " ++ foo k e2
    EApp e1 es     -> Utils.parens <|
                        foo k e1 ++ " " ++ Utils.spaces (List.map (foo k) es)
    EOp op [e1,e2] -> Utils.parens <|
                        String.join " " [strOp op, foo k e1, foo k e2]
    EList es       -> Utils.bracks <| String.join " " (List.map (foo k) es)
    EIf e1 e2 e3   -> Utils.parens <|
                        "if " ++ foo k e1 ++ "\n" ++
                          tab (k+1) ++ foo (k+1) e2 ++ "\n" ++
                          tab (k+1) ++ foo (k+1) e3
    ELet x e1 e2   -> Utils.parens <|
                        "let " ++ x ++ "\n" ++
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
  EList es   -> EList (List.map (applySubst subst) es)
  EApp f es  -> EApp (applySubst subst f) (List.map (applySubst subst) es)
  ELet x e1 e2 -> ELet x (applySubst subst e1) (applySubst subst e2) -- TODO
  EIf e1 e2 e3 ->
    EIf (applySubst subst e1) (applySubst subst e2) (applySubst subst e3)


------------------------------------------------------------------------------
-- Value Contexts

type alias VContext = Val
  -- invariant: a VContext is a Val with exactly one VHole

fillHoleWith : VContext -> Val -> Val
fillHoleWith vc w = case vc of
  VHole          -> w
  VConst _ _     -> vc
  VClosure _ _ _ -> vc   -- not recursing into closures
  VList vs       -> VList (List.map (flip fillHoleWith w) vs)

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

