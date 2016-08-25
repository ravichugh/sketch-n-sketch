module Eval (run, parseAndRun, parseAndRun_, evalDelta, eval) where

import Debug
import Dict
import String

import Lang exposing (..)
import LangUnparser exposing (unparse, unparsePat)
import LangParser2 as Parser
import Types
import Utils

------------------------------------------------------------------------------
-- Big-Step Operational Semantics

match : (Pat, Val) -> Maybe Env
match (p,v) = case (p.val, v.v_) of
  (PVar _ x _, _) -> Just [(x,v)]
  (PAs _ x _ innerPat, _) ->
    case match (innerPat, v) of
      Just env -> Just ((x,v)::env)
      Nothing -> Nothing
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
  (PBase _ bv, VBase bv') -> if (eBaseToVBase bv) == bv' then Just [] else Nothing
  _ -> Debug.crash <| "Little evaluator bug: Eval.match " ++ (toString p.val) ++ " vs " ++ (toString v.v_)


matchList : List (Pat, Val) -> Maybe Env
matchList pvs =
  List.foldl (\pv acc ->
    case (acc, match pv) of
      (Just old, Just new) -> Just (new ++ old)
      _                    -> Nothing
  ) (Just []) pvs


typeCaseMatch : Env -> Backtrace -> Pat -> Type -> Result String Bool
typeCaseMatch env bt pat tipe =
  case (pat.val, tipe.val) of
    (PList _ pats _ Nothing _, TTuple _ typeList _ maybeRestType _) ->
      let typeListsMatch =
        Utils.zip pats typeList
        |> List.map (\(p, t) -> typeCaseMatch env bt p t)
        |> Utils.projOk
        |> Result.map (\bools -> List.all ((==) True) bools)
      in
      case typeListsMatch of
        Err s -> Err s
        Ok False -> Ok False
        Ok True ->
          case maybeRestType of
            Nothing ->
              Ok (List.length pats == List.length typeList)
            Just restType ->
              if List.length pats >= List.length typeList then
                -- Check the rest part of the tuple type against the rest of the vars in the pattern
                List.drop (List.length typeList) pats
                |> List.map (\p -> typeCaseMatch env bt p restType)
                |> Utils.projOk
                |> Result.map (\bools -> List.all ((==) True) bools)
              else
                -- Maybe we should make an error here
                Ok False

    (PVar _ ident _, _) ->
      lookupVar env bt ident pat.start
      |> Result.map (\val -> Types.valIsType val tipe)

    _ -> errorWithBacktrace bt <| "unexpected pattern in typecase: " ++ (unparsePat pat) ++ "\n\nAllowed patterns are bare identifiers and [ident1 ident2 ...]"


cons : (Pat, Val) -> Maybe Env -> Maybe Env
cons pv menv =
  case (menv, match pv) of
    (Just env, Just env') -> Just (env' ++ env)
    _                     -> Nothing


lookupVar env bt x pos =
  case Utils.maybeFind x env of
    Just v  -> Ok v
    Nothing -> errorWithBacktrace bt <| strPos pos ++ " variable not found: " ++ x ++ "\nVariables in scope: " ++ (String.join " " <| List.map fst env)


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

eval_ : Env -> Backtrace -> Exp -> Result String (Val, Widgets)
eval_ env bt e = Result.map fst <| eval env bt e


eval : Env -> Backtrace -> Exp -> Result String ((Val, Widgets), Env)
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
      NoWidgetDecl         -> Ok <| ret v_
      IntSlider a _ b mcap -> Ok <| retBoth (Val v_ [], [WIntSlider a.val b.val (mkCap mcap l) (floor i) l])
      NumSlider a _ b mcap -> Ok <| retBoth (Val v_ [], [WNumSlider a.val b.val (mkCap mcap l) i l])

  EBase _ v      -> Ok <| ret <| VBase (eBaseToVBase v)
  EVar _ x       -> Result.map retAddThis <| lookupVar env (e::bt) x e.start
  EFun _ [p] e _ -> Ok <| ret <| VClosure Nothing p e env
  EOp _ op es _  -> Result.map (\res -> retAddWs e.val.eid (res, env)) <| evalOp env (e::bt) op es

  EList _ es _ m _ ->
    case Utils.projOk <| List.map (eval_ env bt') es of
      Err s -> Err s
      Ok results ->
        let (vs,wss) = List.unzip results in
        let ws = List.concat wss in
        case m of
          Nothing   -> Ok <| retBoth <| (Val (VList vs) [], ws)
          Just rest ->
            case eval_ env bt' rest of
              Err s -> Err s
              Ok (vRest, ws') ->
                case vRest.v_ of
                  VList vs' -> Ok <| retBoth <| (Val (VList (vs ++ vs')) [], ws ++ ws')
                  _         -> errorWithBacktrace (e::bt) <| strPos rest.start ++ " rest expression not a list."

  EIf _ e1 e2 e3 _ ->
    case eval_ env bt e1 of
      Err s -> Err s
      Ok (v1,ws1) ->
        case v1.v_ of
          VBase (VBool True)  -> Result.map (addWidgets ws1) <| eval env bt e2
          VBase (VBool False) -> Result.map (addWidgets ws1) <| eval env bt e3
          _                   -> errorWithBacktrace (e::bt) <| strPos e1.start ++ " if-exp expected a Bool but got something else."

  ECase _ e1 bs _ ->
    case eval_ env (e::bt) e1 of
      Err s -> Err s
      Ok (v1,ws1) ->
        case evalBranches env (e::bt) v1 bs of
          Ok (Just (v2,ws2)) -> Ok <| retBoth (v2, ws1 ++ ws2)
          Err s              -> Err s
          _                  -> errorWithBacktrace (e::bt) <| strPos e1.start ++ " non-exhaustive case statement"

  ETypeCase _ pat tbranches _ ->
    case evalTBranches env (e::bt) pat tbranches of
      Ok (Just (v,ws)) -> Ok <| retBoth (v, ws)
      Err s            -> Err s
      _                -> errorWithBacktrace (e::bt) <| strPos pat.start ++ " non-exhaustive typecase statement"

  EApp _ e1 [e2] _ ->
    case eval_ env bt' e1 of
      Err s       -> Err s
      Ok (v1,ws1) ->
        case eval_ env bt' e2 of
          Err s       -> Err s
          Ok (v2,ws2) ->
            let ws = ws1 ++ ws2 in
            case v1.v_ of
              VClosure Nothing p eBody env' ->
                case (p, v2) `cons` Just env' of
                  Just env'' -> Result.map (addWidgets ws) <| eval env'' bt' eBody -- TODO add eid to vTrace
                  _          -> errorWithBacktrace (e::bt) <| strPos e1.start ++ "bad environment"
              VClosure (Just f) p eBody env' ->
                case (pVar f, v1) `cons` ((p, v2) `cons` Just env') of
                  Just env'' -> Result.map (addWidgets ws) <| eval env'' bt' eBody -- TODO add eid to vTrace
                  _          -> errorWithBacktrace (e::bt) <| strPos e1.start ++ "bad environment"
              _ ->
                errorWithBacktrace (e::bt) <| strPos e1.start ++ " not a function"

  ELet _ _ True p e1 e2 _ ->
    case eval_ env bt' e1 of
      Err s       -> Err s
      Ok (v1,ws1) ->
        case (p.val, v1.v_) of
          (PVar _ f _, VClosure Nothing x body env') ->
            let _   = Utils.assert "eval letrec" (env == env') in
            let v1' = Val (VClosure (Just f) x body env) v1.vtrace in
            case (pVar f, v1') `cons` Just env of
              Just env' -> Result.map (addWidgets ws1) <| eval env' bt' e2
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

  EColonType _ e1 _ t1 _ ->
    case t1.val of
      -- using (e : Point) as a "point widget annotation"
      TNamed _ a ->
        if String.trim a /= "Point" then eval env bt e1
        else
          eval env bt e1 |> Result.map (\result ->
            let ((v,ws),env') = result in
            case v.v_ of
              VList [v1, v2] ->
                case (v1.v_, v2.v_) of
                  (VConst nt1, VConst nt2) ->
                    ((v, ws ++ [WPointSlider nt1 nt2]), env')
                  _ ->
                    result
              _ ->
                result
            )
      _ ->
        eval env bt e1

  EComment _ _ e1       -> eval env bt e1
  EOption _ _ _ _ e1    -> eval env bt e1
  ETyp _ _ _ e1 _       -> eval env bt e1
  -- EColonType _ e1 _ _ _ -> eval env bt e1
  ETypeAlias _ _ _ e1 _ -> eval env bt e1

  -- abstract syntactic sugar

  EFun _ ps e1 _           -> Result.map (retAddWs e1.val.eid) <| eval env bt' (eFun ps e1)
  EApp _ e1 es _           -> Result.map (retAddWs e.val.eid)  <| eval env bt' (eApp e1 es)
  ELet _ _ False p e1 e2 _ -> Result.map (retAddWs e2.val.eid) <| eval env bt' (eApp (eFun [p] e2) [e1])


evalOp env bt opWithInfo es =
  let (op,opStart) = (opWithInfo.val, opWithInfo.start) in
  let argsEvaledRes = List.map (eval_ env bt) es |> Utils.projOk in
  case argsEvaledRes of
    Err s -> Err s
    Ok argsEvaled ->
      let (vs,wss) = List.unzip argsEvaled in
      let error () =
        errorWithBacktrace bt
          <| "Bad arguments to " ++ strOp op ++ " operator " ++ strPos opStart
          ++ ":\n" ++ Utils.lines (Utils.zip vs es |> List.map (\(v,e) -> (strVal v) ++ " from " ++ (unparse e)))
      in
      let emptyVTrace val_   = Val val_ [] in
      let emptyVTraceOk val_ = Ok (emptyVTrace val_) in
      let nullaryOp args retVal =
        case args of
          [] -> emptyVTraceOk retVal
          _  -> error ()
      in
      let unaryMathOp op args =
        case args of
          [VConst (n,t)] -> VConst (evalDelta bt op [n], TrOp op [t]) |> emptyVTraceOk
          _              -> error ()
      in
      let binMathOp op args =
        case args of
          [VConst (i,it), VConst (j,jt)] -> VConst (evalDelta bt op [i,j], TrOp op [it,jt]) |> emptyVTraceOk
          _                              -> error ()
      in
      let args = List.map .v_ vs in
      let newValRes =
        case op of
          Plus    -> case args of
            [VBase (VString s1), VBase (VString s2)] -> VBase (VString (s1 ++ s2)) |> emptyVTraceOk
            _                                        -> binMathOp op args
          Minus     -> binMathOp op args
          Mult      -> binMathOp op args
          Div       -> binMathOp op args
          Mod       -> binMathOp op args
          Pow       -> binMathOp op args
          ArcTan2   -> binMathOp op args
          Lt        -> case args of
            [VConst (i,it), VConst (j,jt)] -> VBase (VBool (i < j)) |> emptyVTraceOk
            _                              -> error ()
          Eq        -> case args of
            [VConst (i,it), VConst (j,jt)]           -> VBase (VBool (i == j)) |> emptyVTraceOk
            [VBase (VString s1), VBase (VString s2)] -> VBase (VBool (s1 == s2)) |> emptyVTraceOk
            [_, _]                                   -> VBase (VBool False) |> emptyVTraceOk -- polymorphic inequality, added for Prelude.addExtras
            _                                        -> error ()
          Pi         -> nullaryOp args (VConst (pi, TrOp op []))
          DictEmpty  -> nullaryOp args (VDict Dict.empty)
          DictInsert -> case vs of
            [vkey, val, {v_}] -> case v_ of
              VDict d -> valToDictKey bt vkey.v_ |> Result.map (\dkey -> VDict (Dict.insert dkey val d) |> emptyVTrace)
              _       -> error()
            _                 -> error ()
          DictGet    -> case args of
            [key, VDict d] -> valToDictKey bt key |> Result.map (\dkey -> Utils.getWithDefault dkey (VBase VNull |> emptyVTrace) d)
            _              -> error ()
          DictRemove -> case args of
            [key, VDict d] -> valToDictKey bt key |> Result.map (\dkey -> VDict (Dict.remove dkey d) |> emptyVTrace)
            _              -> error ()
          Cos        -> unaryMathOp op args
          Sin        -> unaryMathOp op args
          ArcCos     -> unaryMathOp op args
          ArcSin     -> unaryMathOp op args
          Floor      -> unaryMathOp op args
          Ceil       -> unaryMathOp op args
          Round      -> unaryMathOp op args
          Sqrt       -> unaryMathOp op args
          Explode    -> case args of
            [VBase (VString s)] -> VList (List.map (vStr << String.fromChar) (String.toList s)) |> emptyVTraceOk
            _                   -> error ()
          DebugLog   -> case vs of
            [v] -> let _ = Debug.log (strVal v) "" in Ok v
            _   -> error ()
          ToStr      -> case vs of
            [val] -> VBase (VString (strVal val)) |> emptyVTraceOk
            _     -> error ()
      in
      newValRes
      |> Result.map (\newVal -> (newVal, List.concat wss))


-- Returns Ok Nothing if no branch matches
-- Returns Ok (Just results) if branch matches and no execution errors
-- Returns Err s if execution error
evalBranches env bt v bs =
  List.foldl (\(Branch_ _ pat exp _) acc ->
    case (acc, (pat,v) `cons` Just env) of
      (Ok (Just done), _)     -> acc
      (Ok Nothing, Just env') -> eval_ env' bt exp |> Result.map Just
      (Err s, _)              -> acc
      _                       -> Ok Nothing

  ) (Ok Nothing) (List.map .val bs)


-- Returns Ok Nothing if no branch matches
-- Returns Ok (Just results) if branch matches and no execution errors
-- Returns Err s if execution error
evalTBranches env bt pat tbranches =
  List.foldl (\(TBranch_ _ tipe exp _) acc ->
    case (acc, typeCaseMatch env bt pat tipe) of
      (Ok (Just done), _)       -> acc
      (Ok Nothing, Ok didMatch) -> if didMatch then eval_ env bt exp |> Result.map Just else acc
      (Ok Nothing, Err s)       -> Err s
      (Err s, _)                -> acc
  ) (Ok Nothing) (List.map .val tbranches)


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

    _                -> crashWithBacktrace bt <| "Little evaluator bug: Eval.evalDelta " ++ strOp op


eBaseToVBase eBaseVal =
  case eBaseVal of
    EBool b     -> VBool b
    EString _ b -> VString b
    ENull       -> VNull


valToDictKey : Backtrace -> Val_ -> Result String (String, String)
valToDictKey bt val_ =
  case val_ of
    VConst (n, tr)    -> Ok <| (toString n, "num")
    VBase (VBool b)   -> Ok <| (toString b, "bool")
    VBase (VString s) -> Ok <| (toString s, "string")
    VBase VNull       -> Ok <| ("", "null")
    VList vals        ->
      vals
      |> List.map ((valToDictKey bt) << .v_)
      |> Utils.projOk
      |> Result.map (\keyStrings -> (toString keyStrings, "list"))
    _                 -> errorWithBacktrace bt <| "Cannot use " ++ (strVal (val val_)) ++ " in a key to a dictionary."

initEnvRes = Result.map snd <| (eval [] [] Parser.prelude)
initEnv = Utils.fromOk "Eval.initEnv" <| initEnvRes

run : Exp -> Result String (Val, Widgets)
run e = eval_ initEnv [] e

parseAndRun : String -> String
parseAndRun = strVal << fst << Utils.fromOk_ << run << Utils.fromOkay "parseAndRun" << Parser.parseE

parseAndRun_ = strVal_ True << fst << Utils.fromOk_ << run << Utils.fromOkay "parseAndRun_" << Parser.parseE

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

crashWithBacktrace bt message =
  crashWithMsg <| (btString bt) ++ "\n" ++ message
