module Eval (run, run_, parseAndRun, evalDelta) where

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

matchError p v =
  Err <| "pattern-match error:\n" ++ strPat p ++ "\n" ++ strVal v

lookupVar env x =
  case Utils.maybeFind x env of
    Just v  -> Ok v
    Nothing -> Err <| "undefined variable: " ++ Utils.bracks x

-- eval propagates output environment in order to extract
-- initial environment from prelude

type alias Res a = Result String a

eval_ : Env -> Exp -> Res Val
eval_ env e = Utils.mapOk fst (eval env e)

eval : Env -> Exp -> Res (Val, Env)
eval env e =

  let ret v = Ok (v, env) in

  case e of

  EConst i l -> ret <| VConst (i, TrLoc l)
  EBase v    -> ret <| VBase v
  EFun [p] e -> ret <| VClosure Nothing p e env

  EVar x     -> Utils.bindOk ret <| lookupVar env x
  EOp op es  -> Utils.bindOk ret <| evalOp env op es

  EList es m ->
    flip Utils.bindOk (evalList_ env es) <| \vs ->
      case m of
        Nothing   -> ret <| VList vs
        Just rest -> case eval_ env rest of
                       Ok (VList vs') -> ret <| VList (vs ++ vs')
                       Ok v           -> Err <| "rest not a list: " ++ strVal v
                       Err e          -> Err e

  EIf e1 e2 e3 ->
    flip Utils.bindOk (eval_ env e1) <| \v1 ->
      case v1 of
        VBase (Bool True)  -> eval env e2
        VBase (Bool False) -> eval env e3
        _                  -> Err <| "guard isn't a boolean: " ++ strVal v1

  ECase e1 l ->
    flip Utils.bindOk (eval_ env e1) <| \v1 ->
      case pickBranch env v1 l of
        Just (env', e2) -> eval env' e2
        Nothing         -> Err <| "no patterns matched: " ++ strVal v1

  EApp e1 [e2] ->
    let (v1,v2) = (eval_ env e1, eval_ env e2) in
    flip Utils.bindOk (eval_ env e1) <| \v1 ->
    flip Utils.bindOk (eval_ env e2) <| \v2 ->
      case v1 of
        VClosure Nothing p eBody env' ->
          case (p, v2) `cons` Just env' of
            Just env'' -> eval env'' eBody
            Nothing    -> matchError p v2
        VClosure (Just f) p eBody env' ->
          case (PVar f, v1) `cons` ((p, v2) `cons` Just env') of
            Just env'' -> eval env'' eBody
            Nothing    -> Err "match error with app recursive func..."
        _ ->
          Err <| "trying to apply non-function value: " ++ strVal v1

  ELet _ True (PVar f) e1 e2 ->
    flip Utils.bindOk (eval_ env e1) <| \v1 ->
      case v1 of
        VClosure Nothing x body env' ->
          let _   = Utils.assert "eval letrec" (env == env') in
          let v1' = VClosure (Just f) x body env in
          case (PVar f, v1') `cons` Just env of
            Just env' -> eval env' e2
            Nothing   -> matchError (PVar f) v1'
        _ ->
          Err <| "letrec with non-function value: " ++ strVal v1

  EComment _ e1 -> eval env e1

  -- abstract syntactic sugar
  EFun ps e  -> eval env (eFun ps e)
  EApp e1 es -> eval env (eApp e1 es)
  ELet _ False p e1 e2 -> eval env (EApp (EFun [p] e2) [e1])

  -- errors
  ELet _ True (PList _ _) _ _ -> Debug.crash "eval: multi letrec"

evalOp env op es =
  case evalList_ env es of
    Ok [VConst (i,it), VConst (j,jt)] ->
      case op of
        Plus  -> Ok <| VConst (evalDelta op [i,j], TrOp op [it,jt])
        Minus -> Ok <| VConst (evalDelta op [i,j], TrOp op [it,jt])
        Mult  -> Ok <| VConst (evalDelta op [i,j], TrOp op [it,jt])
        Div   -> Ok <| VConst (evalDelta op [i,j], TrOp op [it,jt])
        Lt    -> Ok <| vBool  (i < j)
        Eq    -> Ok <| vBool  (i == j)
    Ok [VBase (String s1), VBase (String s2)] ->
      case op of
        Plus  -> Ok <| VBase (String (s1 ++ s2))
        Eq    -> Ok <| vBool (s1 == s2)
    Ok [] ->
      case op of
        Pi    -> Ok <| VConst (pi, TrOp op [])
    Ok [VConst (n,t)] ->
      case op of
        Cos    -> Ok <| VConst (cos n, TrOp op [t])
        Sin    -> Ok <| VConst (sin n, TrOp op [t])
        ArcCos -> Ok <| VConst (acos n, TrOp op [t])
        ArcSin -> Ok <| VConst (asin n, TrOp op [t])
        Floor  -> Ok <| VConst (toFloat <| floor n, TrOp op [t])
        Ceil   -> Ok <| VConst (toFloat <| ceiling n, TrOp op [t])
        Round  -> Ok <| VConst (toFloat <| round n, TrOp op [t])
        ToStr  -> Ok <| VBase (String (toString n))
    Ok [VBase (Bool b)] ->
      case op of
        ToStr  -> Ok <| VBase (String (toString b))

consOk : Result err a -> Result err (List a) -> Result err (List a)
consOk mx my = case (mx, my) of
  (Ok x, Ok xs) -> Ok (x::xs)
  (Err e, _)    -> Err e
  (_, Err e)    -> Err e

evalList_ env es =
  List.foldr (\e acc -> consOk (eval_ env e) acc) (Ok []) es

pickBranch env v =
  List.foldl (\(p,e) acc ->
    case (acc, (p,v) `cons` Just env) of
      (Just done, _)       -> Just done
      (Nothing, Just env') -> Just (env', e)
      _                    -> Nothing

  ) Nothing

evalDelta op is =
  case (op, is) of
    (Plus,   [i,j]) -> (+) i j
    (Minus,  [i,j]) -> (-) i j
    (Mult,   [i,j]) -> (*) i j
    (Div,    [i,j]) -> (/) i j
    (Cos,    [n])   -> cos n
    (Sin,    [n])   -> sin n
    (ArcCos, [n])   -> acos n
    (ArcSin, [n])   -> asin n
    (Pi,     [])    -> pi
    (Floor,  [n])   -> toFloat <| floor n
    (Ceil,   [n])   -> toFloat <| ceiling n
    (Round,  [n])   -> toFloat <| round n
    _               -> Debug.crash <| "Eval.evalDelta " ++ strOp op

run : Exp -> Res Val
run e =
  Utils.bindOk
    (\(_,initEnv) -> eval_ initEnv e)
    (eval [] prelude)

run_ : Exp -> Val
run_ = Utils.fromOk_ << run

parseAndRun : String -> String
parseAndRun s =
  case LangParser.parseE s of
    Err e -> Debug.crash <| "parseAndRun (parse failed): " ++ e
    Ok e ->
      case run e of
        Err e -> Debug.crash <| "parseAndRun (eval failed): " ++ e
        Ok v  -> strVal v

