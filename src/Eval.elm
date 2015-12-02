module Eval (run, parseAndRun, parseAndRun_, evalDelta, eval) where

import Debug

import Lang exposing (..)
import LangParser2 as Parser
import Utils

------------------------------------------------------------------------------
-- Big-Step Operational Semantics

match : (Pat, Val) -> Maybe Env
match (p,v) = case (p.val, v.v_) of
  (PVar x _, _) -> Just [(x,v)]
  (PList ps Nothing, VList vs) ->
    Utils.bindMaybe matchList (Utils.maybeZip ps vs)
  (PList ps (Just rest), VList vs) ->
    let (n,m) = (List.length ps, List.length vs) in
    if | n > m     -> Nothing
       | otherwise -> let (vs1,vs2) = Utils.split n vs in
                      (rest, vList vs2) `cons` (matchList (Utils.zip ps vs1))
                        -- dummy VTrace, since VList itself doesn't matter
  (PList _ _, _) -> Nothing
  (PConst n, VConst (n',_)) ->
    if | n == n'   -> Just []
       | otherwise -> Nothing
  (PBase bv, VBase bv') ->
    if | bv == bv' -> Just []
       | otherwise -> Nothing

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

mkCap mcap l =
  let s =
    case (mcap, l) of
      (Just cap, _)       -> cap.val
      (Nothing, (_,_,"")) -> strLoc l
      (Nothing, (_,_,x))  -> x
  in
  s ++ ": "

-- eval propagates output environment in order to extract
-- initial environment from prelude

-- eval inserts dummyPos during evaluation

eval_ : Env -> Exp -> (Val, Widgets)
eval_ env e = fst <| eval env e

eval : Env -> Exp -> ((Val, Widgets), Env)
eval env e =

  let ret v_                         = ((Val v_ [e.val.eid], []), env) in
  let retAdd eid (v,envOut)          = ((Val v.v_ (eid::v.vtrace), []), envOut) in
  let retAddWs eid ((v,ws),envOut)   = ((Val v.v_ (eid::v.vtrace), ws), envOut) in
  let retAddThis_ (v,envOut)         = retAdd e.val.eid (v,envOut) in
  let retAddThis v                   = retAddThis_ (v, env) in
  let retBoth (v,w)                  = (({v | vtrace <- e.val.eid :: v.vtrace},w), env) in
  let addWidgets ws1 ((v1,ws2),env1) = ((v1, ws1 ++ ws2), env1) in

  case e.val.e__ of

  EConst i l wd ->
    let v_ = VConst (i, TrLoc l) in
    case wd.val of
      NoWidgetDecl         -> ret v_
      IntSlider a _ b mcap -> retBoth (Val v_ [], [WIntSlider a.val b.val (mkCap mcap l) (floor i) l])
      NumSlider a _ b mcap -> retBoth (Val v_ [], [WNumSlider a.val b.val (mkCap mcap l) i l])

  EBase v    -> ret <| VBase v
  EVar x     -> retAddThis <| lookupVar env x e.start
  EFun [p] e -> ret <| VClosure Nothing p e env
  EOp op es  -> retAddWs e.val.eid ((evalOp env op es), env)

  EList es m ->
    let (vs,wss) = List.unzip (List.map (eval_ env) es) in
    let ws = List.concat wss in
    case m of
      Nothing   -> retBoth <| (Val (VList vs) [], ws)
      Just rest ->
        let (vRest, ws') = eval_ env rest in
        case vRest.v_ of
          VList vs' -> retBoth <| (Val (VList (vs ++ vs')) [], ws ++ ws')
          _         -> errorMsg <| strPos rest.start ++ " rest expression not a list."

  EIndList rs ->
    let vs = List.concat <| List.map rangeToList rs in
    if isSorted vs
    then ret <| VList vs
    else Debug.crash <| "indices not strictly increasing: " ++ strVal (vList vs)

  EIf e1 e2 e3 ->
    let (v1,ws1) = eval_ env e1 in
    case v1.v_ of
      VBase (Bool True)  -> addWidgets ws1 <| eval env e2
      VBase (Bool False) -> addWidgets ws1 <| eval env e3
      _                  -> errorMsg <| strPos e1.start ++ " if-exp expected a Bool but got something else."

  ECase e1 l ->
    let (v1,ws1) = eval_ env e1 in
    case evalBranches env v1 l of
      Just (v2,ws2) -> retBoth (v2, ws1 ++ ws2)
      _             -> errorMsg <| strPos e1.start ++ " non-exhaustive case statement"

  EApp e1 [e2] ->
    let ((v1,ws1),(v2,ws2)) = (eval_ env e1, eval_ env e2) in
    let ws = ws1 ++ ws2 in
    case v1.v_ of
      VClosure Nothing p eBody env' ->
        case (p, v2) `cons` Just env' of
          Just env'' -> addWidgets ws <| eval env'' eBody -- TODO add eid to vTrace
      VClosure (Just f) p eBody env' ->
        case (pVar f, v1) `cons` ((p, v2) `cons` Just env') of
          Just env'' -> addWidgets ws <| eval env'' eBody -- TODO add eid to vTrace
      _ ->
        errorMsg <| strPos e1.start ++ " not a function: " ++ (sExp e)

  ELet _ True p e1 e2 ->
    let (v1,ws1) = eval_ env e1 in
    case (p.val, v1.v_) of
      (PVar f _, VClosure Nothing x body env') ->
        let _   = Utils.assert "eval letrec" (env == env') in
        let v1' = Val (VClosure (Just f) x body env) v1.vtrace in
        case (pVar f, v1') `cons` Just env of
          Just env' -> addWidgets ws1 <| eval env' e2
      (PList _ _, _) ->
        errorMsg <|
          strPos e1.start ++
          "mutually recursive functions (i.e. letrec [...] [...] e) \
           not yet implemented"

  EComment _ e1 -> eval env e1
  EOption _ _ e1 -> eval env e1

  -- abstract syntactic sugar

  EFun ps e1           -> retAddWs e1.val.eid <| eval env (eFun ps e1)
  EApp e1 es           -> retAddWs e.val.eid  <| eval env (eApp e1 es)
  ELet _ False p e1 e2 -> retAddWs e2.val.eid <| eval env (eApp (eFun [p] e2) [e1])

evalOp env opWithInfo es =
  let (op,opStart) = (opWithInfo.val, opWithInfo.start) in
  let (vs,wss) = List.unzip (List.map (eval_ env) es) in
  (\vOut -> (Val vOut [], List.concat wss)) <|
  case List.map .v_ vs of
    [VConst (i,it), VConst (j,jt)] ->
      case op of
        Plus    -> VConst (evalDelta op [i,j], TrOp op [it,jt])
        Minus   -> VConst (evalDelta op [i,j], TrOp op [it,jt])
        Mult    -> VConst (evalDelta op [i,j], TrOp op [it,jt])
        Div     -> VConst (evalDelta op [i,j], TrOp op [it,jt])
        Mod     -> VConst (evalDelta op [i,j], TrOp op [it,jt])
        Pow     -> VConst (evalDelta op [i,j], TrOp op [it,jt])
        ArcTan2 -> VConst (evalDelta op [i,j], TrOp op [it,jt])
        Lt      -> VBase (Bool (i < j))
        Eq      -> VBase (Bool (i == j))
    [VBase (String s1), VBase (String s2)] ->
      case op of
        Plus  -> VBase (String (s1 ++ s2))
        Eq    -> VBase (Bool (s1 == s2))
    [] ->
      case op of
        Pi    -> VConst (pi, TrOp op [])
    [VConst (n,t)] ->
      case op of
        Cos    -> VConst (evalDelta op [n], TrOp op [t])
        Sin    -> VConst (evalDelta op [n], TrOp op [t])
        ArcCos -> VConst (evalDelta op [n], TrOp op [t])
        ArcSin -> VConst (evalDelta op [n], TrOp op [t])
        Floor  -> VConst (evalDelta op [n], TrOp op [t])
        Ceil   -> VConst (evalDelta op [n], TrOp op [t])
        Round  -> VConst (evalDelta op [n], TrOp op [t])
        Sqrt   -> VConst (evalDelta op [n], TrOp op [t])
        ToStr  -> VBase (String (toString n))
    [VBase (Bool b)] ->
      case op of
        ToStr  -> VBase (String (toString b))
    [VBase (String s)] ->
      case op of
        ToStr  -> VBase (String (strBaseVal (String s)))
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

    (Plus,    [i,j]) -> (+) i j
    (Minus,   [i,j]) -> (-) i j
    (Mult,    [i,j]) -> (*) i j
    (Div,     [i,j]) -> (/) i j
    (Pow,     [i,j]) -> (^) i j
    (Mod,     [i,j]) -> toFloat <| (%) (floor i) (floor j)
                         -- might want an error/warning for non-int
    (ArcTan2, [i,j]) -> atan2 i j

    (Cos,     [n])   -> cos n
    (Sin,     [n])   -> sin n
    (ArcCos,  [n])   -> acos n
    (ArcSin,  [n])   -> asin n
    (Floor,   [n])   -> toFloat <| floor n
    (Ceil,    [n])   -> toFloat <| ceiling n
    (Round,   [n])   -> toFloat <| round n
    (Sqrt,    [n])   -> sqrt n

    (Pi,      [])    -> pi

    (RangeOffset i, [n1,n2]) ->
      let m = n1 + toFloat i in
      if | m > n2    -> n2
         | otherwise -> m

    _                -> errorMsg <| "Eval.evalDelta " ++ strOp op

initEnv = snd (eval [] Parser.prelude)

run : Exp -> (Val, Widgets)
run e =
  eval_ initEnv e

parseAndRun : String -> String
parseAndRun = strVal << fst << run << Utils.fromOk_ << Parser.parseE

parseAndRun_ = strVal_ True << fst << run << Utils.fromOk_ << Parser.parseE

rangeOff l1 i l2 = TrOp (RangeOffset i) [TrLoc l1, TrLoc l2]

-- Inflates a range to a list, which is then Concat-ed in eval
rangeToList : Range -> List Val
rangeToList r =
  let err () = errorMsg "Range not specified with numeric constants" in
  case r.val of
    -- dummy VTraces...
    -- TODO: maybe add widgets
    Point e -> case e.val.e__ of
      EConst n l _ -> [ vConst (n, rangeOff l 0 l) ]
      _            -> err ()
    Interval e1 e2 -> case (e1.val.e__, e2.val.e__) of
      (EConst n1 l1 _, EConst n2 l2 _) ->
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
