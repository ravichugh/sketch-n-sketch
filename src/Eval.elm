module Eval (run, parseAndRun, evalDelta) where

import Debug

import Lang exposing (..)
import LangParser exposing (prelude)
import Utils

------------------------------------------------------------------------------
-- Big-Step Operational Semantics

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

-- eval propagates output environment in order to extract
-- initial environment from prelude

eval_ : Env -> Exp -> Val
eval_ env e = fst <| eval env e

eval : Env -> Exp -> (Val, Env)
eval env e =

  let ret v = (v, env) in

  case e of

  EConst i l -> ret <| VConst (i, TrLoc l)
  EBase v    -> ret <| VBase v
  EVar x     -> ret <| lookupVar env x
  EFun [p] e -> ret <| VClosure Nothing p e env
  EOp op es  -> ret <| evalOp env op es

  EList es m ->
    let vs = List.map (eval_ env) es in
    case m of
      Nothing   -> ret <| VList vs
      Just rest -> case eval_ env rest of
                     VList vs' -> ret <| VList (vs ++ vs')

  EIf e1 e2 e3 ->
    case eval_ env e1 of
      VBase (Bool True)  -> eval env e2
      VBase (Bool False) -> eval env e3

  ECase e1 l ->
    let v1 = eval_ env e1 in
    case evalBranches env v1 l of
      Just v2 -> ret v2

  EApp e1 [e2] ->
    let (v1,v2) = (eval_ env e1, eval_ env e2) in
    case v1 of
      VClosure Nothing p e env' ->
        case (p, v2) `cons` Just env' of
          Just env'' -> eval env'' e
      VClosure (Just f) p e env' ->
        case (PVar f, v1) `cons` ((p, v2) `cons` Just env') of
          Just env'' -> eval env'' e

  ELet True (PVar f) e1 e2 ->
    case eval_ env e1 of
      VClosure Nothing x body env' ->
        let _   = Utils.assert "eval letrec" (env == env') in
        let v1' = VClosure (Just f) x body env in
        case (PVar f, v1') `cons` Just env of
          Just env' -> eval env' e2

  -- abstract syntactic sugar
  EFun ps e  -> eval env (eFun ps e)
  EApp e1 es -> eval env (eApp e1 es)
  ELet False p e1 e2 -> eval env (EApp (EFun [p] e2) [e1])

  -- errors
  ELet True (PList _ _) _ _ -> Debug.crash "eval: multi letrec"

evalOp env op es =
  case List.map (eval_ env) es of
    [VConst (i,it), VConst (j,jt)] ->
      case op of
        Plus  -> VConst (evalDelta op [i,j], TrOp op [it,jt])
        Minus -> VConst (evalDelta op [i,j], TrOp op [it,jt])
        Mult  -> VConst (evalDelta op [i,j], TrOp op [it,jt])
        Div   -> VConst (evalDelta op [i,j], TrOp op [it,jt])
        Lt    -> vBool  (i < j)
    [] ->
      case op of
        Pi    -> VConst (pi, TrOp op [])
    [VConst (n,t)] ->
      case op of
        Cos    -> VConst (cos n, TrOp op [t])
        Sin    -> VConst (sin n, TrOp op [t])
        ArcCos -> VConst (acos n, TrOp op [t])
        ArcSin -> VConst (asin n, TrOp op [t])

evalBranches env v =
  List.foldl (\(p,e) acc ->
    case (acc, (p,v) `cons` Just env) of
      (Just done, _)       -> Just done
      (Nothing, Just env') -> Just (eval_ env' e)
      _                    -> Nothing

  ) Nothing

evalDelta op [i,j] =
  let f =
    case op of
      Plus  -> (+)
      Minus -> (-)
      Mult  -> (*)
      Div   -> (/)
  in
  f i j

run : Exp -> Val
run e =
  let initEnv = snd (eval [] prelude) in
  eval_ initEnv e

parseAndRun : String -> String
parseAndRun = strVal << run << LangParser.parseE

