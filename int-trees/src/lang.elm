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

type Op = Plus

type Exp
  = EConst Int Loc
  | EVar Ident
  | EFun Ident Exp
  | EApp Exp Exp
  | EOp Op (List Exp)
  | EList (List Exp)
  | ELet Ident Exp Exp

type Val
  = VConst Int Trace
  | VClosure Ident Exp Env
  | VList (List Val)
  | VHole

type Trace = TrLoc Loc | TrOp Op (List Trace)


------------------------------------------------------------------------------
-- Big-Step Operational Semantics

type alias Env = List (Ident, Val)

eval : Env -> Exp -> Val
eval env e = case e of
  EConst i l -> VConst i (TrLoc l)
  EVar x     -> case Utils.maybeFind x env of Just v -> v
  EFun x e   -> VClosure x e env
  EOp op es  -> evalOp env op es
  EList es   -> VList (List.map (eval env) es)
  EApp e1 e2 -> case eval env e1 of VClosure x e env' ->
                  let v2 = eval env e2 in eval ((x,v2)::env') e
  ELet x e1 e2 -> eval env (EApp (EFun x e2) e1)

evalOp env op es =
  case (op, List.map (eval env) es) of
    (Plus, [VConst i1 t1, VConst i2 t2]) -> VConst (i1+i2) (TrOp op [t1,t2])

run : Exp -> Val
run = eval []


------------------------------------------------------------------------------
-- Unparsing

strExp     = strExp_ False
strExpLocs = strExp_ True

strExp_ showLocs e =
  let foo = strExp_ showLocs in
  case e of
    EConst i l     -> toString i
                        ++ if | showLocs  -> Utils.braces (strLoc l)
                              | otherwise -> ""
    EVar x         -> x
    EFun x e       -> Utils.parens ("fun " ++ x ++ " -> " ++ foo e)
    EApp e1 e2     -> foo e1 ++ Utils.parens (foo e2)
    EOp op [e1,e2] -> String.join " " [foo e1, strOp op, foo e2]
    EList es       -> Utils.bracks (String.join ", " (List.map foo es))

strVal     = strVal_ False
strValLocs = strVal_ True

strVal_ showTraces v =
  let foo = strVal_ showTraces in
  case v of
    VConst i tr      -> toString i
                          ++ if | showTraces -> Utils.braces (strTrace tr)
                                | otherwise  -> ""
    VClosure x e env -> "<fun>"
    VList vs         -> Utils.bracks (String.join ", " (List.map foo vs))
    VHole            -> "??"

strOp op = case op of {Plus -> "+"}

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
    EConst i l     -> toString i
                        ++ if | showLocs  -> Utils.braces (strLoc l)
                              | otherwise -> ""
    EVar x         -> x
    EFun x e       -> Utils.parens <|
                        "fn (" ++ x ++ ")\n" ++ tab (k+1) ++ foo (k+1) e
    EApp e1 e2     -> Utils.parens <| foo k e1 ++ " " ++ foo k e2
    EOp op [e1,e2] -> Utils.parens <|
                        String.join " " [strOp op, foo k e1, foo k e2]
    EList es       -> Utils.bracks <| String.join ", " (List.map (foo k) es)
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
  EVar _     -> e
  EFun _ _   -> e   -- not recursing into lambdas
  EOp op es  -> EOp op (List.map (applySubst subst) es)
  EList es   -> EList (List.map (applySubst subst) es)
  EApp e1 e2 -> EApp (applySubst subst e1) (applySubst subst e2)
  ELet x e1 e2 -> ELet x (applySubst subst e1) (applySubst subst e2) -- TODO


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
    Nothing
           
justSameVList = Just << Same << VList
      

------------------------------------------------------------------------------
-- Abstract Syntax Helpers

dummyLoc = 0
dummyTrace = TrLoc dummyLoc
eConst = flip EConst dummyLoc
vConst = flip VConst dummyTrace
ePlus e1 e2 = EOp Plus [e1,e2]
eApp e es = case es of
  [e1]    -> EApp e e1
  e1::es' -> eApp (EApp e e1) es'
eFun xs e = case xs of
  [x]     -> EFun x e
  x::xs'  -> EFun x (eFun xs' e)

