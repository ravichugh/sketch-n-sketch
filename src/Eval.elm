module Eval (run, parseAndRun, evalDelta) where

import Debug

import Lang exposing (..)
import LangParser2 as Parser
import Utils

------------------------------------------------------------------------------
-- Big-Step Operational Semantics

match : (Pat, Val) -> Maybe Env
match (p,v) = case (p.val, v) of
  (PVar x _, _) -> Just [(x,v)]
  (PList ps Nothing, VList vs) ->
    Utils.bindMaybe matchList (Utils.maybeZip ps vs)
  (PList ps (Just rest), VList vs) ->
    let (n,m) = (List.length ps, List.length vs) in
    if n > m then Nothing
    else
      let (vs1,vs2) = Utils.split n vs in
      (rest, VList vs2) `cons` (matchList (Utils.zip ps vs1))
  (PList _ _, _) -> Nothing
  (PConst n, VConst (n',_)) -> if n == n' then Just [] else Nothing
  (PBase bv, VBase bv') -> if bv == bv' then Just [] else Nothing
  _ -> Debug.crash "Eval.match"

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

  let ret v                          = ((v, []), env) in
  let retBoth vw                     = (vw, env) in
  let addWidgets ws1 ((v1,ws2),env1) = ((v1, ws1 ++ ws2), env1) in

  case e.val of

  EConst i l wd ->
    let v = VConst (i, TrLoc l) in
    case wd.val of
      NoWidgetDecl         -> ret v
      IntSlider a _ b mcap -> retBoth (v, [WIntSlider a.val b.val (mkCap mcap l) (floor i) l])
      NumSlider a _ b mcap -> retBoth (v, [WNumSlider a.val b.val (mkCap mcap l) i l])

  EBase v    -> ret <| VBase v
  EVar x     -> ret <| lookupVar env x e.start
  EFun [p] e -> ret <| VClosure Nothing p e env
  EOp op es  -> retBoth <| evalOp env op es

  EList es m ->
    let (vs,wss) = List.unzip (List.map (eval_ env) es) in
    let ws = List.concat wss in
    case m of
      Nothing   -> retBoth <| (VList vs, ws)
      Just rest -> case eval_ env rest of
                     (VList vs', ws') -> retBoth <| (VList (vs ++ vs'), ws ++ ws')
                     _                -> errorMsg <| strPos rest.start ++ " rest expression not a list."

  EIndList rs -> Debug.crash "eval EIndList"

{-
  EIndList rs ->
      let vrs = List.concat <| List.map rangeToList rs
      in ret <| VList vrs
-}

  EIf e1 e2 e3 ->
    let (v1,ws1) = eval_ env e1 in
    case v1 of
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
    case v1 of
      VClosure Nothing p e env' ->
        case (p, v2) `cons` Just env' of
          Just env'' -> addWidgets ws <| eval env'' e
          _          -> errorMsg <| strPos e1.start ++ "bad environment"
      VClosure (Just f) p e env' ->
        case (pVar f, v1) `cons` ((p, v2) `cons` Just env') of
          Just env'' -> addWidgets ws <| eval env'' e
          _          -> errorMsg <| strPos e1.start ++ "bad environment"
      _ ->
        errorMsg <| strPos e1.start ++ " not a function"

  ELet _ True p e1 e2 ->
    let (v1,ws1) = eval_ env e1 in
    case (p.val, v1) of
      (PVar f _, VClosure Nothing x body env') ->
        let _   = Utils.assert "eval letrec" (env == env') in
        let v1' = VClosure (Just f) x body env in
        case (pVar f, v1') `cons` Just env of
          Just env' -> addWidgets ws1 <| eval env' e2
          _         -> errorMsg <| strPos e.start ++ "bad ELet"
      (PList _ _, _) ->
        errorMsg <|
          strPos e1.start ++
          "mutually recursive functions (i.e. letrec [...] [...] e) \
           not yet implemented"
      _ ->
        errorMsg <| strPos e.start ++ "bad ELet"

  EComment _ e1 -> eval env e1
  EOption _ _ e1 -> eval env e1

  -- abstract syntactic sugar
  EFun ps e  -> eval env (eFun ps e)
  EApp e1 es -> eval env (eApp e1 es)
  ELet _ False p e1 e2 -> eval env (eApp (eFun [p] e2) [e1])

evalOp env opWithInfo es =
  let (op,opStart) = (opWithInfo.val, opWithInfo.start) in
  let (vs,wss) = List.unzip (List.map (eval_ env) es) in
  let error () =
    errorMsg
      <| "Bad arguments to " ++ strOp op ++ " operator " ++ strPos opStart
      ++ ":\n" ++ Utils.lines (List.map sExp es)
  in
  (\vOut -> (vOut, List.concat wss)) <|
  case vs of
    [VConst (i,it), VConst (j,jt)] ->
      case op of
        Plus    -> VConst (evalDelta op [i,j], TrOp op [it,jt])
        Minus   -> VConst (evalDelta op [i,j], TrOp op [it,jt])
        Mult    -> VConst (evalDelta op [i,j], TrOp op [it,jt])
        Div     -> VConst (evalDelta op [i,j], TrOp op [it,jt])
        Mod     -> VConst (evalDelta op [i,j], TrOp op [it,jt])
        Pow     -> VConst (evalDelta op [i,j], TrOp op [it,jt])
        ArcTan2 -> VConst (evalDelta op [i,j], TrOp op [it,jt])
        Lt      -> vBool  (i < j)
        Eq      -> vBool  (i == j)
        _       -> error ()
    [VBase (String s1), VBase (String s2)] ->
      case op of
        Plus  -> VBase (String (s1 ++ s2))
        Eq    -> vBool (s1 == s2)
        _     -> error ()
    [] ->
      case op of
        Pi    -> VConst (pi, TrOp op [])
        _     -> error ()
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
        _     -> error ()
    [VBase (Bool b)] ->
      case op of
        ToStr  -> VBase (String (toString b))
        _     -> error ()
    [VBase (String s)] ->
      case op of
        ToStr  -> VBase (String (strBaseVal (String s)))
        _     -> error ()
    _ ->
      error ()

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

    _               -> errorMsg <| "Eval.evalDelta " ++ strOp op

initEnv = snd (eval [] Parser.prelude)

run : Exp -> (Val, Widgets)
run e =
  eval_ initEnv e

parseAndRun : String -> String
parseAndRun = strVal << fst << run << Utils.fromOk_ << Parser.parseE

-- Inflates a range to a list, which is then Concat-ed in eval
rangeToList : ERange -> List Val
rangeToList r =
    let (l,u) = r.val
    in
      case (l.val, u.val) of
        (EConst nl tl _, EConst nu tu _) ->
           let walkVal i =
             let j = toFloat i in
             if (nl + j) < nu
               then VConst (nl + j, TrOp (RangeOffset i) [TrLoc tl]) :: walkVal (i + 1)
               else [ VConst (nu, TrLoc tu) ]
           in
           if nl == nu
             then [ VConst (nl, TrLoc tl) ]
             else walkVal 0
        _ -> errorMsg "Range not specified with numeric constants"
