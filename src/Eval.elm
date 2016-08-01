module Eval (run, parseAndRun, parseAndRun_, evalDelta, eval) where

import Debug
import Dict
import String

import Lang exposing (..)
import LangUnparser exposing (unparse)
import LangParser2 as Parser
import Types
import Utils

------------------------------------------------------------------------------
-- Big-Step Operational Semantics

match : (Pat, Val) -> Maybe Env
match (p,v) = case (p.val, v.v_) of
  (PVar _ x _, _) -> Just [(x,v)]
  (PList _ ps _ Nothing _, VList vs) ->
    Utils.bindMaybe matchList (Utils.maybeZip ps vs)
  (PList _ ps _ (Just rest) _, VList vs) ->
    let (n,m) = (List.length ps, List.length vs) in
    if n > m then Nothing
    else
      let (vs1,vs2) = Utils.split n vs in
      (rest, vList vs2) `cons` (matchList (Utils.zip ps vs1))
        -- dummy VTrace, since VList itself doesn't matter
  (PList _ _ _ _ _, _) -> Nothing
  (PConst _ n, VConst (n',_)) -> if n == n' then Just [] else Nothing
  (PBase _ bv, VBase bv') -> if bv == bv' then Just [] else Nothing
  _ -> Debug.crash <| "Eval.match " ++ (toString p.val) ++ " vs " ++ (toString v.v_)

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

lookupVar env bt x pos =
  case Utils.maybeFind x env of
    Just v -> v
    Nothing -> errorWithBacktrace bt <| strPos pos ++ " variable not found: " ++ x

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

eval_ : Env -> Backtrace -> Exp -> (Val, Widgets)
eval_ env bt e = fst <| eval env bt e

eval : Env -> Backtrace -> Exp -> ((Val, Widgets), Env)
eval env bt e =

  let ret v_                         = ((Val v_ [e.val.eid], []), env) in
  let retAdd eid (v,envOut)          = ((Val v.v_ (eid::v.vtrace), []), envOut) in
  let retAddWs eid ((v,ws),envOut)   = ((Val v.v_ (eid::v.vtrace), ws), envOut) in
  let retAddThis_ (v,envOut)         = retAdd e.val.eid (v,envOut) in
  let retAddThis v                   = retAddThis_ (v, env) in
  let retBoth (v,w)                  = (({v | vtrace = e.val.eid :: v.vtrace},w), env) in
  let addWidgets ws1 ((v1,ws2),env1) = ((v1, ws1 ++ ws2), env1) in

  let bt' =
    if e.start.line >= 1
    then e::bt
    else bt
  in

  case e.val.e__ of

  EConst _ i l wd ->
    let v_ = VConst (i, TrLoc l) in
    case wd.val of
      NoWidgetDecl         -> ret v_
      IntSlider a _ b mcap -> retBoth (Val v_ [], [WIntSlider a.val b.val (mkCap mcap l) (floor i) l])
      NumSlider a _ b mcap -> retBoth (Val v_ [], [WNumSlider a.val b.val (mkCap mcap l) i l])

  EBase _ v      -> ret <| VBase v
  EVar _ x       -> retAddThis <| lookupVar env (e::bt) x e.start
  EFun _ [p] e _ -> ret <| VClosure Nothing p e env
  EOp _ op es _  -> retAddWs e.val.eid ((evalOp env (e::bt) op es), env)

  EList _ es _ m _ ->
    let (vs,wss) = List.unzip (List.map (eval_ env bt') es) in
    let ws = List.concat wss in
    case m of
      Nothing   -> retBoth <| (Val (VList vs) [], ws)
      Just rest ->
        let (vRest, ws') = eval_ env bt' rest in
        case vRest.v_ of
          VList vs' -> retBoth <| (Val (VList (vs ++ vs')) [], ws ++ ws')
          _         -> errorWithBacktrace (e::bt) <| strPos rest.start ++ " rest expression not a list."

  EIndList _ rs _ ->
    let vs = List.concat <| List.map rangeToList rs in
    if isSorted vs
    then ret <| VList vs
    else Debug.crash <| "indices not strictly increasing: " ++ strVal (vList vs)

  EIf _ e1 e2 e3 _ ->
    let (v1,ws1) = eval_ env bt e1 in
    case v1.v_ of
      VBase (Bool True)  -> addWidgets ws1 <| eval env bt e2
      VBase (Bool False) -> addWidgets ws1 <| eval env bt e3
      _                  -> errorWithBacktrace (e::bt) <| strPos e1.start ++ " if-exp expected a Bool but got something else."

  ECase _ e1 bs _ ->
    let (v1,ws1) = eval_ env (e::bt) e1 in
    case evalBranches env (e::bt) v1 bs of
      Just (v2,ws2) -> retBoth (v2, ws1 ++ ws2)
      _             -> errorWithBacktrace (e::bt) <| strPos e1.start ++ " non-exhaustive case statement"

  ETypeCase _ pat tbranches _ ->
    case evalTBranches env (e::bt) pat tbranches of
      Just (v,ws) -> retBoth (v, ws)
      _           -> errorWithBacktrace (e::bt) <| strPos pat.start ++ " non-exhaustive typecase statement"

  EApp _ e1 [e2] _ ->
    let ((v1,ws1),(v2,ws2)) = (eval_ env bt' e1, eval_ env bt' e2) in
    let ws = ws1 ++ ws2 in
    case v1.v_ of
      VClosure Nothing p eBody env' ->
        case (p, v2) `cons` Just env' of
          Just env'' -> addWidgets ws <| eval env'' bt' eBody -- TODO add eid to vTrace
          _          -> errorWithBacktrace (e::bt) <| strPos e1.start ++ "bad environment"
      VClosure (Just f) p eBody env' ->
        case (pVar f, v1) `cons` ((p, v2) `cons` Just env') of
          Just env'' -> addWidgets ws <| eval env'' bt' eBody -- TODO add eid to vTrace
          _          -> errorWithBacktrace (e::bt) <| strPos e1.start ++ "bad environment"
      _ ->
        errorWithBacktrace (e::bt) <| strPos e1.start ++ " not a function"

  ELet _ _ True p e1 e2 _ ->
    let (v1,ws1) = eval_ env bt' e1 in
    case (p.val, v1.v_) of
      (PVar _ f _, VClosure Nothing x body env') ->
        let _   = Utils.assert "eval letrec" (env == env') in
        let v1' = Val (VClosure (Just f) x body env) v1.vtrace in
        case (pVar f, v1') `cons` Just env of
          Just env' -> addWidgets ws1 <| eval env' bt' e2
          _         -> errorWithBacktrace (e::bt) <| strPos e.start ++ "bad ELet"
      (PList _ _ _ _ _, _) ->
        errorWithBacktrace (e::bt) <|
          strPos e1.start ++
          "mutually recursive functions (i.e. letrec [...] [...] e) \
           not yet implemented"
           -- Implementation also requires modifications to LangTransform.simply
           -- so that clean up doesn't prune the funtions.
      _ ->
        errorWithBacktrace (e::bt) <| strPos e.start ++ "bad ELet"

  EComment _ _ e1       -> eval env bt e1
  EOption _ _ _ _ e1    -> eval env bt e1
  ETyp _ _ _ e1 _       -> eval env bt e1
  EColonType _ e1 _ _ _ -> eval env bt e1
  ETypeAlias _ _ _ e1 _ -> eval env bt e1

  -- abstract syntactic sugar

  EFun _ ps e1 _           -> retAddWs e1.val.eid <| eval env bt' (eFun ps e1)
  EApp _ e1 es _           -> retAddWs e.val.eid  <| eval env bt' (eApp e1 es)
  ELet _ _ False p e1 e2 _ -> retAddWs e2.val.eid <| eval env bt' (eApp (eFun [p] e2) [e1])


evalOp env bt opWithInfo es =
  let (op,opStart) = (opWithInfo.val, opWithInfo.start) in
  let (vs,wss) = List.unzip (List.map (eval_ env bt) es) in
  let error () =
    errorWithBacktrace bt
      <| "Bad arguments to " ++ strOp op ++ " operator " ++ strPos opStart
      ++ ":\n" ++ Utils.lines (Utils.zip vs es |> List.map (\(v,e) -> (strVal v) ++ " from " ++ (unparse e)))
  in
  let emptyVTrace val_ = Val val_ [] in
  let nullaryOp args retVal =
    case args of
      [] -> retVal
      _  -> error ()
  in
  let unaryMathOp op args =
    case args of
      [VConst (n,t)] -> VConst (evalDelta bt op [n], TrOp op [t]) |> emptyVTrace
      _              -> error ()
  in
  let binMathOp op args =
    case args of
      [VConst (i,it), VConst (j,jt)] -> VConst (evalDelta bt op [i,j], TrOp op [it,jt]) |> emptyVTrace
      _                              -> error ()
  in
  let args = List.map .v_ vs in
  let newVal =
    case op of
      Plus    -> case args of
        [VBase (String s1), VBase (String s2)] -> VBase (String (s1 ++ s2)) |> emptyVTrace
        _                                      -> binMathOp op args
      Minus     -> binMathOp op args
      Mult      -> binMathOp op args
      Div       -> binMathOp op args
      Mod       -> binMathOp op args
      Pow       -> binMathOp op args
      ArcTan2   -> binMathOp op args
      Lt        -> case args of
        [VConst (i,it), VConst (j,jt)] -> VBase (Bool (i < j)) |> emptyVTrace
        _                              -> error ()
      Eq        -> case args of
        [VConst (i,it), VConst (j,jt)] -> VBase (Bool (i == j)) |> emptyVTrace
        [_, _]                         -> VBase (Bool False) |> emptyVTrace -- polymorphic inequality, added for Prelude.addExtras
        _                              -> error ()
      Pi         -> nullaryOp args (VConst (pi, TrOp op [])) |> emptyVTrace
      DictEmpty  -> nullaryOp args (VDict Dict.empty) |> emptyVTrace
      DictInsert -> case vs of
        [vkey, val, {v_}] -> case v_ of
          VDict d -> VDict (Dict.insert (valToDictKey bt vkey.v_) val d) |> emptyVTrace
          _       -> error()
        _                 -> error ()
      DictGet    -> case args of
        [key, VDict d] -> Utils.getWithDefault (valToDictKey bt key) (VBase Null |> emptyVTrace) d
        _              -> error ()
      DictRemove -> case args of
        [key, VDict d] -> VDict (Dict.remove (valToDictKey bt key) d) |> emptyVTrace
        _              -> error ()
      Cos        -> unaryMathOp op args
      Sin        -> unaryMathOp op args
      ArcCos     -> unaryMathOp op args
      ArcSin     -> unaryMathOp op args
      Floor      -> unaryMathOp op args
      Ceil       -> unaryMathOp op args
      Round      -> unaryMathOp op args
      Sqrt       -> unaryMathOp op args
      DebugLog   -> case vs of
        [v] -> let _ = Debug.log (strVal v) "" in v
        _   -> error ()
      ToStr      -> case vs of
        [val] -> VBase (String (strVal val)) |> emptyVTrace
        _     -> error ()
      RangeOffset _ -> error ()
  in
  (newVal, List.concat wss)

evalBranches env bt v bs =
  List.foldl (\(Branch_ _ pat exp _) acc ->
    case (acc, (pat,v) `cons` Just env) of
      (Just done, _)       -> Just done
      (Nothing, Just env') -> Just (eval_ env' bt exp)
      _                    -> Nothing

  ) Nothing (List.map .val bs)


evalTBranches env bt pat tbranches =
  List.foldl (\(TBranch_ _ tipe exp _) result ->
    if result == Nothing && Types.typeCaseMatch env pat tipe
    then Just (eval_ env bt exp)
    else result
  ) Nothing (List.map .val tbranches)


evalDelta bt op is =
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
      if m > n2 then n2 else m

    _                -> errorWithBacktrace bt <| "Eval.evalDelta " ++ strOp op

valToDictKey : Backtrace -> Val_ -> (String, String)
valToDictKey bt val_ =
  case val_ of
    VConst (n, tr)   -> (toString n, "num")
    VBase (Bool b)   -> (toString b, "bool")
    VBase (String s) -> (toString s, "string")
    VBase Null       -> ("", "null")
    VList vals       -> (toString <| List.map ((valToDictKey bt) << .v_) vals, "list")
    _                -> errorWithBacktrace bt <| "Cannot use " ++ (strVal (val val_)) ++ " in a key to a dictionary."

initEnv = snd (eval [] [] Parser.prelude)

run : Exp -> (Val, Widgets)
run e =
  eval_ initEnv [] e

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
      EConst _ n l _ -> [ vConst (n, rangeOff l 0 l) ]
      _              -> err ()
    Interval e1 _ e2 -> case (e1.val.e__, e2.val.e__) of
      (EConst _ n1 l1 _, EConst _ n2 l2 _) ->
        let walkVal i =
          let m = n1 + toFloat i in
          let tr = rangeOff l1 i l2 in
          if m < n2
            then vConst (m,  tr) :: walkVal (i + 1)
            else vConst (n2, tr) :: []
        in
        walkVal 0
      _ -> err ()

-- Could compute this in one pass along with rangeToList
isSorted = isSorted_ Nothing
isSorted_ mlast vs = case vs of
  []     -> True
  v::vs' ->
    case v.v_ of
      VConst (j,_) ->
        case mlast of
          Nothing -> isSorted_ (Just j) vs'
          Just i  -> if i < j
                       then isSorted_ (Just j) vs'
                       else False
      _ ->
        Debug.crash "isSorted"

btString : Backtrace -> String
btString bt =
  case bt of
    [] -> ""
    mostRecentExp::others ->
      let singleLineExpStrs =
        others
        |> List.map (Utils.head_ << String.lines << String.trimLeft << unparse)
        |> List.reverse
        |> String.join "\n"
      in
      singleLineExpStrs ++ "\n" ++ (unparse mostRecentExp)


errorWithBacktrace bt message =
  errorMsg <| (btString bt) ++ "\n" ++ message
