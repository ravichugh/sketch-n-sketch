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

diff : Val -> Val -> Maybe (Maybe (VContext, Val, Val))
diff v1 v2 =
  let res = diff_ v1 v2 in
  case res of
    Just (Just (vc,w1,w2)) ->
      let v12 = (fillHoleWith vc w1, fillHoleWith vc w2) in
      if | (v1,v2) == v12 -> res
         | otherwise      -> Debug.crash "bad diff"
    _ -> res

diff_ v1 v2 =
  case (v1, v2) of
    (VConst i1 _, VConst i2 _) ->
      if | i1 == i2  -> Just Nothing
         | otherwise -> Just (Just (VHole, v1, v2))
    (VList vs1, VList vs2) ->
      if | List.length vs1 /= List.length vs2 -> Nothing
         | otherwise ->
             List.foldr (\(v1,v2) acc ->
               case acc of
                 Nothing -> Nothing
                 Just (Just _) ->
                   if | v1 == v2  -> acc
                      | otherwise -> Nothing
                 Just Nothing ->
                   case diff_ v1 v2 of
                     Nothing      -> Nothing
                     Just Nothing -> Just Nothing
                     Just (Just (VList vcs, w1, w2)) ->
                       if | v1 == v2 -> Just (Just (VList (v1::vcs), w1, w2))
                          | otherwise -> Nothing
             ) (Just Nothing) (Utils.zip vs1 vs2)
    (_, _) -> Nothing


------------------------------------------------------------------------------
-- Abstract Syntax Helpers

dummyLoc = 0
dummyTrace = TrLoc dummyLoc
eConst = flip EConst dummyLoc
ePlus e1 e2 = EOp Plus [e1,e2]
eApp e es = case es of
  [e1]    -> EApp e e1
  e1::es' -> eApp (EApp e e1) es'

