module Eval (run, parseAndRun, evalDelta) where

import Debug

import Lang exposing (..)
import LangParser2 as Parser
import Utils

------------------------------------------------------------------------------
-- Big-Step Operational Semantics

match : (Pat, Val) -> Maybe Env
match (p,v) = case (p.val, v.v_) of
  (PVar x, _) -> Just [(x,v)]
  (PList ps Nothing, VList vs) ->
    Utils.bindMaybe matchList (Utils.maybeZip ps vs)
  (PList ps (Just rest), VList vs) ->
    let (n,m) = (List.length ps, List.length vs) in
    if | n > m     -> Nothing
       | otherwise -> let (vs1,vs2) = Utils.split n vs in
                      (rest, vList vs2) `cons` (matchList (Utils.zip ps vs1))
                        -- dummy VTrace, since VList itself doesn't matter
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

lookupVar env x pos =
  case Utils.maybeFind x env of
    Just v -> v
    Nothing -> errorMsg <| strPos pos ++ " variable not found: " ++ x

-- eval propagates output environment in order to extract
-- initial environment from prelude

-- eval inserts dummyPos during evaluation

eval_ : Env -> Exp -> Val
eval_ env e = fst <| eval env e

eval : Env -> Exp -> (Val, Env)
eval env e =

  -- ret retains original eid, retReplace doesn't
  let ret v_       = (Val v_ e.val.eid, env) in
  let retReplace v = (v, env) in

  case e.val.e__ of

  EConst i l -> ret <| VConst (i, TrLoc l)
  EBase v    -> ret <| VBase v
  EVar x     -> ret <| (lookupVar env x e.start).v_
  EFun [p] e -> ret <| VClosure Nothing p e env
  EOp op es  -> ret <| evalOp env op es

  EList es m ->
    let vs = List.map (eval_ env) es in
    case m of
      Nothing   -> ret <| VList vs
      Just rest -> case (eval_ env rest).v_ of
                     VList vs' -> ret <| VList (vs ++ vs')

  EIndList rs -> 
    let vs = List.concat <| List.map rangeToList rs in
    if isSorted vs
    then ret <| VList vs
    else Debug.crash <| "indices not strictly increasing: " ++ strVal (vList vs)

  EIf e1 e2 e3 ->
    case (eval_ env e1).v_ of
      VBase (Bool True)  -> eval env e2
      VBase (Bool False) -> eval env e3
      _                  -> errorMsg <| strPos e1.start ++ " if-statement expected a Bool but got something else."

  ECase e1 l ->
    let v1 = eval_ env e1 in
    case evalBranches env v1 l of
      Just v2 -> retReplace v2
      _       -> errorMsg <| strPos e1.start ++ " non-exhaustive case statement"

  EApp e1 [e2] ->
    let (v1,v2) = (eval_ env e1, eval_ env e2) in
    case v1.v_ of
      VClosure Nothing p e env' ->
        case (p, v2) `cons` Just env' of
          Just env'' -> eval env'' e
      VClosure (Just f) p e env' ->
        case (pVar f, v1) `cons` ((p, v2) `cons` Just env') of
          Just env'' -> eval env'' e
      _ ->
        errorMsg <| strPos e1.start ++ " not a function"

  ELet _ True p e1 e2 ->
    let v1 = eval_ env e1 in
    case (p.val, v1.v_) of
      (PVar f, VClosure Nothing x body env') ->
        let _   = Utils.assert "eval letrec" (env == env') in
        let v1' = Val (VClosure (Just f) x body env) v1.vtrace in
        case (pVar f, v1') `cons` Just env of
          Just env' -> eval env' e2
      (PList _ _, _) ->
        errorMsg <|
          strPos e1.start ++
          "mutually recursive functions (i.e. letrec [...] [...] e) \
           not yet implemented"

  EComment _ e1 -> eval env e1
  EOption _ _ e1 -> eval env e1

  -- abstract syntactic sugar
  EFun ps e  -> eval env (eFun ps e)
  EApp e1 es -> eval env (eApp e1 es)
  ELet _ False p e1 e2 -> eval env (eApp (eFun [p] e2) [e1])

evalOp env opWithInfo es =
  let (op,opStart) = (opWithInfo.val, opWithInfo.start) in
  case List.map (.v_ << eval_ env) es of
    [VConst (i,it), VConst (j,jt)] ->
      case op of
        Plus  -> VConst (evalDelta op [i,j], TrOp op [it,jt])
        Minus -> VConst (evalDelta op [i,j], TrOp op [it,jt])
        Mult  -> VConst (evalDelta op [i,j], TrOp op [it,jt])
        Div   -> VConst (evalDelta op [i,j], TrOp op [it,jt])
        Lt    -> VBase (Bool (i < j))
        Eq    -> VBase (Bool (i == j))
    [VBase (String s1), VBase (String s2)] ->
      case op of
        Plus  -> VBase (String (s1 ++ s2))
        Eq    -> VBase (Bool (s1 == s2))
    [] ->
      case op of
        Pi    -> VConst (pi, TrOp op [])
    [VConst (n,t)] ->
      case op of
        Cos    -> VConst (cos n, TrOp op [t])
        Sin    -> VConst (sin n, TrOp op [t])
        ArcCos -> VConst (acos n, TrOp op [t])
        ArcSin -> VConst (asin n, TrOp op [t])
        Floor  -> VConst (toFloat <| floor n, TrOp op [t])
        Ceil   -> VConst (toFloat <| ceiling n, TrOp op [t])
        Round  -> VConst (toFloat <| round n, TrOp op [t])
        ToStr  -> VBase (String (toString n))
    [VBase (Bool b)] ->
      case op of
        ToStr  -> VBase (String (toString b))
    [_, _] ->
      case op of
        -- polymorphic inequality, added for Prelude.addExtras
        Eq     -> VBase (Bool False)
    _ ->
      errorMsg
        <| "Bad arguments to " ++ strOp op ++ " operator " ++ strPos opStart
        ++ ":\n" ++ Utils.lines (List.map sExp es)

evalBranches env v l =
  List.foldl (\(p,e) acc ->
    case (acc, (p,v) `cons` Just env) of
      (Just done, _)       -> Just done
      (Nothing, Just env') -> Just (eval_ env' e)
      _                    -> Nothing

  ) Nothing (List.map .val l)

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

    (RangeOffset i, [n1,n2]) ->
      let m = n1 + toFloat i in
      if | m > n2    -> n2
         | otherwise -> m

    _               -> errorMsg <| "Eval.evalDelta " ++ strOp op

initEnv = snd (eval [] Parser.prelude)

run : Exp -> Val
run e =
  eval_ initEnv e

parseAndRun : String -> String
parseAndRun = strVal << run << Utils.fromOk_ << Parser.parseE

rangeOff l1 i l2 = TrOp (RangeOffset i) [TrLoc l1, TrLoc l2]

-- Inflates a range to a list, which is then Concat-ed in eval
rangeToList : Range -> List Val
rangeToList r =
  let err () = errorMsg "Range not specified with numeric constants" in
  case r.val of
    -- dummy VTraces...
    Point e -> case e.val.e__ of
      EConst n l -> [ vConst (n, rangeOff l 0 l) ]
      _          -> err ()
    Interval e1 e2 -> case (e1.val.e__, e2.val.e__) of
      (EConst n1 l1, EConst n2 l2) ->
        let walkVal i =
          let m = n1 + toFloat i in
          let tr = rangeOff l1 i l2 in
          if | m < n2    -> vConst (m,  tr) :: walkVal (i + 1)
             | otherwise -> vConst (n2, tr) :: []
        in
        walkVal 0
      _ -> err ()

-- Could compute this in one pass along with rangeToList
isSorted = isSorted_ Nothing
isSorted_ mlast vs = case vs of
  []     -> True
  v::vs' ->
    let (VConst (j,_)) = v.v_ in
    case mlast of
      Nothing -> isSorted_ (Just j) vs'
      Just i  -> if | i < j -> isSorted_ (Just j) vs'
                    | otherwise -> False
