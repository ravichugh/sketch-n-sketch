module Eval exposing (doEval, eval, evalDelta)

import Debug
import Dict
import String

import ImpureGoodies
import Lang exposing (..)
import ValUnparser exposing (strVal_, strOp, strLoc)
import ElmUnparser
import ElmParser as Parser
import Syntax exposing (Syntax)
import Types
import Utils
import Record
import Info
import ParserUtils
import LangUtils exposing (..)
import HTMLValParser

import UpdateRegex exposing (evalRegexExtractFirstIn)
import UpdateStack
import UpdateUtils
import Results exposing (Results, ok1)
import LazyList
import ValBuilder as Vb
import ValUnbuilder as Vu

valToDictKey : Syntax -> Backtrace -> Val -> Result String (String, String)
valToDictKey syntax bt v =
  case LangUtils.valToDictKey syntax v of
    Err msg -> errorWithBacktrace syntax bt msg
    Ok i -> Ok i

------------------------------------------------------------------------------
-- Big-Step Operational Semantics

match : (Pat, Val) -> Maybe Env
match (p,v) = case (p.val.p__, v.v_) of
  (PWildcard _, _) -> Just []
  (PVar _ x _, _) -> Just [(x,v)]
  (PAs _ _ x _ innerPat, _) ->
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
      let vRest =
        { v_ = VList vs2
        , provenance = Provenance (provenanceEnv v.provenance) (eApp (eVar0 "drop") [provenanceExp v.provenance, eConstDummyLoc (toFloat n)]) [v] -- TODO: should be based on "drop" prelude function and a dummy int. Doesn't matter for selected val -> program EId determination.
        , parents = Parents []
        }
      in
      cons (rest, vRest) (matchList (Utils.zip ps vs1))
        -- dummy Provenance, since VList itself doesn't matter
  (PList _ _ _ _ _, _) -> Nothing
  (PConst _ n, VConst _ (n_,_)) -> if n == n_ then Just [] else Nothing
  (PConst _ n, _) -> Nothing
  (PBase _ bv, VBase bv_) -> if (eBaseToVBase bv) == bv_ then Just [] else Nothing
  (PBase _ _, _) -> Nothing
  (PParens _ innerPat _, _) -> match (innerPat, v)
  (PRecord _ ps _, VRecord values) ->
    let vkeys = Dict.keys values in
    case Record.getPatternMatch Utils.recordKey identity ps vkeys of
      Nothing -> Nothing
      Just patsValues ->
        matchList (List.map (\(p, vkey) -> (Utils.recordValue p, case Dict.get vkey values of
          Just v -> v
          Nothing -> Debug.crash <| "Internal error: key " ++ toString vkey ++ " not found in record value."
        )) patsValues)
  (PRecord _ _ _ , _) -> Nothing

  --_ -> Debug.crash <| "Little evaluator bug: Eval.match " ++ (toString p.val.p__) ++ " vs " ++ (toString v.v_)


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
    (Just env, Just env_) -> Just (env_ ++ env)
    _                     -> Nothing


lookupVar syntax env bt x pos =
  case Utils.maybeFind x env of
    Just v  -> Ok v
    Nothing -> errorWithBacktrace syntax bt <| strPos pos ++ " variable not found: " ++ x ++ "\nVariables in scope: " ++ (String.join " " <| List.map Tuple.first env)


mkCap mcap l =
  let s =
    case (mcap, l) of
      (Just cap, _)       -> cap.val
      (Nothing, (_,_,"")) -> strLoc l
      (Nothing, (_,_,x))  -> x
  in
  s ++ ": "

doEval : Syntax -> Env -> Exp -> Result String ((Val, Widgets), Env)
doEval syntax env e =
  eval syntax env [] e
  |> Result.map (\((val, widgets), env) -> ((val, postProcessWidgets widgets), env))


-- Do not use: you lose parent tagging.
-- provenanceToMaybeVal : Provenance -> Maybe Val
-- provenanceToMaybeVal (Provenance env e vs) =
--   eval env [] e
--   |> Result.map (\((val, widgets), env) -> val)
--   |> Result.toMaybe


-- eval propagates output environment in order to extract
-- initial environment from prelude

-- eval inserts dummyPos during evaluation

-- Just like eval, but ignore envOut
eval_ : Syntax -> Env -> Backtrace -> Exp -> Result String (Val, Widgets)
eval_ syntax env bt e = Result.map Tuple.first <| eval syntax env bt e


eval : Syntax -> Env -> Backtrace -> Exp -> Result String ((Val, Widgets), Env)
eval syntax env bt e =
  let makeProvenance basedOn = Provenance env e basedOn in

  --let _ = Debug.log ("Evaluating " ++ (Syntax.unparser Syntax.Elm e)) () in

  -- Deeply tag value's children to say the child flowed through here.
  --
  -- Need mutation in order to also affect values already bound to variables, etc.
  let addParent_ vParent v =
    let _ =
      case v.v_ of
        VConst _ _       -> ()
        VBase _          -> ()
        VClosure _ _ _ _ -> ()
        VList vals       -> let _ = List.map (addParent_ vParent) vals                 in ()
        VDict dict       -> let _ = Dict.map (\_ val -> (addParent_ vParent) val) dict in ()
        VRecord recs     -> let _ = Dict.map (\_ val -> (addParent_ vParent) val) recs in ()
        VFun _ _ _ _     -> ()
    in
    let priorParents = valParents v in
    let _ = ImpureGoodies.mutateRecordField v.parents "_0" (vParent::priorParents) in
    ()
  in
  let addParent v =
    if Parser.isProgramEId e.val.eid then
      case v.v_ of
        VConst _ _       -> v
        VBase _          -> v
        VClosure _ _ _ _ -> v
        VList vals       -> let _ = List.map (addParent_ v) vals               in v -- non-mutating: { v | v_ = VList (List.map (addParent_ v) vals) }
        VDict dict       -> let _ = Dict.map (\_ val -> addParent_ v val) dict in v -- non-mutating: { v | v_ = VDict (Dict.map (\_ val -> addParent_ v val) dict) }
        VRecord dict     -> let _ = Dict.map (\_ val -> addParent_ v val) dict in v
        VFun _ _ _ _     -> v
    else
      v
  in

  -- Only use introduceVal, ret, or retBoth for new values (i.e. not var lookups): they do not preserve parents
  let introduceVal basedOn v_  = addParent <| Val v_ (makeProvenance basedOn) (Parents []) in
  let retBoth basedOn (v_, ws) = ((introduceVal basedOn v_, ws), env) in
  let ret basedOn v_           = retBoth basedOn (v_, []) in

  let retV basedOn v                             = ((addParent { v | provenance = makeProvenance basedOn}, []), env) in
  let retVBoth basedOn (v, ws)                   = ((addParent { v | provenance = makeProvenance basedOn}, ws), env) in
  let retAddWs ws1 (v1, ws2)                     = (v1, ws1 ++ ws2) in
  let addParentToRet ((v,ws),envOut)             = ((addParent v, ws), envOut) in
  let addProvenanceToValWidgets basedOn (v,ws)   = (addParent { v | provenance = makeProvenance basedOn}, ws) in
  let addProvenanceToRet basedOn ((vws), envOut) = (addProvenanceToValWidgets basedOn vws, envOut) in
  let addWidgets ws1 ((v1,ws2),env1)             = ((v1, ws1 ++ ws2), env1) in


  let bt_ =
    if e.start.line >= 1
    then e::bt
    else bt
  in

  case e.val.e__ of

  EConst _ n loc wd ->
    let v_ = VConst Nothing (n, TrLoc loc) in
    let retVal = introduceVal [] v_ in
    case wd.val of
      NoWidgetDecl         -> Ok ((retVal, []), env)
      IntSlider a _ b mcap hidden ->
        let widget = WIntSlider a.val b.val (mkCap mcap loc) (floor n) retVal loc hidden in
        Ok ((retVal, [widget]), env)
      NumSlider a _ b mcap hidden ->
        let widget = WNumSlider a.val b.val (mkCap mcap loc) n retVal loc hidden in
        Ok ((retVal, [widget]), env)

  EBase _ v     -> Ok <| ret [] <| VBase (eBaseToVBase v)
  EVar _ x      -> Result.map (\v -> retV [v] v) <| lookupVar syntax env (e::bt) x e.start
  EFun _ ps e _ -> Ok <| ret [] <| VClosure Nothing ps e env
  EOp _ _ op es _ -> Result.map (\res -> addParentToRet (res, env)) <| evalOp syntax env e (e::bt) op es

  EList _ es _ m _ ->
    case Utils.projOk <| List.map (eval_ syntax env bt_) (Utils.listValues es) of
      Err s -> Err s
      Ok results ->
        let (vs,wss) = List.unzip results in
        let ws = List.concat wss in
        case m of
          Nothing   -> Ok <| retBoth vs (VList vs, ws)
          Just rest ->
            case eval_ syntax env bt_ rest of
              Err s -> Err s
              Ok (vRest, ws_) ->
                case vRest.v_ of
                  VList vs_ -> Ok <| retBoth (vs ++ [vRest]) (VList (vs ++ vs_), ws ++ ws_)
                  _         -> errorWithBacktrace syntax (e::bt) <| strPos rest.start ++ " rest expression not a list, but " ++ valToString vRest

  ERecord _ mi es _ ->
    case Utils.projOk <| List.map (eval_ syntax env bt_) (Utils.recordValues es) of
      Err s -> Err s
      Ok results ->
       let (vs,wss) = List.unzip results in
       let ws = List.concat wss in
       let keys = Utils.recordKeys es in
       let dict = Dict.fromList <| Utils.zip keys vs in
       case mi of
         Nothing -> Ok <| retBoth vs (VRecord dict, ws)
         Just (e, init) ->
           case eval_ syntax env bt_ e of
             Err s -> Err s
             Ok (vInit, ws_) ->
               case vInit.v_ of
                 VRecord dInit -> -- Completely wrong !
                   let newDict = Dict.union dict dInit in --The first overrides the second.
                   Ok <| retBoth vs (VRecord newDict, ws)
                 _ -> errorWithBacktrace syntax (e::bt) <| strPos init.start ++ " init expression not a dict."

  ESelect ws0 e _ wsId id ->
    case eval_ syntax env bt_ e of
      Err s -> Err s
      Ok (d, ws) ->
        case d.v_ of
          VRecord dict ->
            case Dict.get id dict of
              Just v -> Ok <| retBoth [d] (v.v_, ws)
              _ ->
                  let suggestions = Utils.stringSuggestions (Dict.keys dict) id in
                  errorWithBacktrace syntax (e::bt) <| strPos wsId.end ++ " Key " ++ id ++ " not found." ++ (case suggestions of
                    [] -> ""
                    l -> " Did you mean '" ++ String.join "', or '" l ++ "'?"
                  )
          _ -> errorWithBacktrace syntax (e::bt) <| strPos e.start ++ " select expression applied to non-dict "

  -- Alternatively, could choose not to add a basedOn record for if/case/typecase (simply pass value through, maybe add parent)
  -- But that would suggest that we *might* avoid doing so for EApp as well, which is more dubious. We'll see.
  EIf _ e1 _ e2 _ e3 _ ->
    case eval_ syntax env bt e1 of
      Err s -> Err s
      Ok (v1,ws1) ->
        case v1.v_ of
          VBase (VBool True)  -> Result.map (\(((v,_),_) as result) -> addProvenanceToRet [v] <| addWidgets ws1 result) <| eval syntax env bt e2 -- Provenence basedOn vals control-flow agnostic: do not include scrutinee
          VBase (VBool False) -> Result.map (\(((v,_),_) as result) -> addProvenanceToRet [v] <| addWidgets ws1 result) <| eval syntax env bt e3 -- Provenence basedOn vals control-flow agnostic: do not include scrutinee
          _                   -> errorWithBacktrace syntax (e::bt) <| strPos e1.start ++ " if-exp expected a Bool but got something else."

  ECase _ e1 bs _ ->
    case eval_ syntax env (e::bt) e1 of
      Err s -> Err s
      Ok (v1,ws1) ->
        case evalBranches syntax env (e::bt) v1 bs of
          -- retVBoth and not addProvenanceToRet b/c only lets should return inner env
          Ok (Just (v2,ws2)) -> Ok <| retVBoth [v2] (v2, ws1 ++ ws2) -- Provenence basedOn vals control-flow agnostic: do not include scrutinee
          Err s              -> Err s
          _                  -> errorWithBacktrace syntax (e::bt) <| strPos e1.start ++ " non-exhaustive case statement, cannot match " ++ valToString v1

  ETypeCase _ e1 tbranches _ ->
    case eval_ syntax env (e::bt) e1 of
      Err s -> Err s
      Ok (v1,ws1) ->
        case evalTBranches syntax env (e::bt) v1 tbranches of
          -- retVBoth and not addProvenanceToRet b/c only lets should return inner env
          Ok (Just (v2,ws2)) -> Ok <| retVBoth [v2] (v2, ws1 ++ ws2) -- Provenence basedOn vals control-flow agnostic: do not include scrutinee
          Err s              -> Err s
          _                  -> errorWithBacktrace syntax (e::bt) <| strPos e1.start ++ " non-exhaustive typecase statement"

  EApp _ e1 [] _ _ ->
    errorWithBacktrace syntax (e::bt) <| strPos e1.start ++ " application with no arguments"

  EApp sp0 e1 es appStyle sp1 ->
    if appStyle == InfixApp && eVarUnapply e1 == Just "++" then
        case es of
          [eLeft, eRight] -> -- We rewrite ++ to a call to "append" or "plus" depending on the arguments
            case (eval_ syntax env bt_ eLeft, eval_ syntax env bt_ eRight) of
              (Ok (v1, ws1), Ok (v2, ws2)) ->
                 case (v1.v_, v2.v_) of
                   (VBase (VString x), VBase (VString y)) ->
                     eval syntax ([("x", v1), ("y", v2)] ++ env) bt_  (replaceE__ e <|
                       EOp space1 space1 (withDummyRange Plus) [replaceE__ eLeft <| EVar space0 "x", replaceE__ eRight <| EVar space0 "y"] space0)
                   _ ->
                     eval syntax ([("x", v1), ("y", v2)] ++ env) bt_ (replaceE__ e <|
                       EApp sp0 (replaceE__ e1 <| EVar sp0 "append") [replaceE__ eLeft <| EVar space1 "x", replaceE__ eRight <| EVar space1 "y"] SpaceApp sp1)
              (Err s, _) -> Err s
              (_, Err s) -> Err s
          _ -> Err ("++ should be called with two arguments, was called on "++toString (List.length es)++". ")
    else
    case eval_ syntax env bt_ e1 of
      Err s -> Err s
      Ok (v1,ws1) ->
        let evalVApp: Val -> List Exp -> Result String ((Val, Widgets), Env)
            evalVApp v1 es =
          case v1.v_ of
            VClosure maybeRecName ps funcBody closureEnv ->
              let argValsAndFuncRes =
                case maybeRecName of
                  Nothing    -> apply syntax env bt bt_ e ps es funcBody closureEnv
                  Just fName -> apply syntax env bt bt_ e ps es funcBody ((fName, v1)::closureEnv)
              in
              -- Do not record dependence on closure (which function to execute is essentially control flow).
              -- Dependence on function is implicit by being dependent on some value computed by an expression in the function.
              -- Instead, point to return value (and, hence, the final expression of) the function. This will also
              -- transitively point to any arguments used.
              argValsAndFuncRes
              |> Result.map (\(argVals, (fRetVal, fRetWs)) ->
                let perhapsCallWidget =
                  if Parser.isProgramEId e.val.eid && Parser.isProgramEId funcBody.val.eid
                  then [WCall v1 argVals fRetVal fRetWs]
                  else []
                in
                retVBoth [fRetVal] (fRetVal, ws1 ++ fRetWs ++ perhapsCallWidget)
              )
            VFun name argList evalDef _ ->
              let arity = List.length argList in
              let availableArgs = List.length es in
              if availableArgs < arity then -- Partial application, hence eta expansion
                let funconverted = replaceV_ v1 <| VClosure
                     Nothing
                     (List.map (\a -> withDummyPatInfo <| PVar space1 a noWidgetDecl) <| argList)
                     (replaceE__ e <| EApp sp0 (replaceE__ e <| EVar space1 name)
                       (List.map (withDummyExpInfo << EVar space1) <| argList) SpaceApp sp0) ((name, v1)::env) in
                evalVApp funconverted es
              else
                let (arguments, remaining) = Utils.split arity es in
                case Utils.projOk <| List.map (eval_ syntax env bt_) arguments of
                  Err s -> Err s
                  Ok argumentsVal ->
                    let basicResult = evalDef (List.map Tuple.first argumentsVal) |> Result.map (\((v,_) as result) -> addProvenanceToValWidgets [v] result) in
                    if List.length remaining > 0 then
                      case basicResult of
                        Err s -> errorWithBacktrace syntax bt_ s
                        Ok (v, _) -> evalVApp v remaining
                    else
                      case basicResult of
                        Err s -> errorWithBacktrace syntax bt_ s
                        Ok vw -> Ok (vw, env)
            _ ->
              errorWithBacktrace syntax (e::bt) <| strPos e1.start ++ " not a function"
        in evalVApp v1 es

  ELet _ _ False p _ e1 _ e2 _ ->
    case eval_ syntax env bt_ e1 of
      Err s       -> Err s
      Ok (v1,ws1) ->
        case cons (p, v1) (Just env) of
          Just env_ ->
            -- Don't add provenance: fine to say value is just from the let body.
            -- (We consider equations to be mobile).
            Result.map (addWidgets ws1) <| eval syntax env_ bt_ e2

          Nothing   ->
            errorWithBacktrace syntax (e::bt) <| strPos e.start ++ " could not match pattern " ++ (Syntax.patternUnparser syntax >> Utils.squish) p ++ " with " ++ valToString v1


  ELet _ _ True p _ e1 _ e2 _ ->
    case eval_ syntax env bt_ e1 of
      Err s       -> Err s
      Ok (v1,ws1) ->
        case ((patEffectivePat p).val.p__, v1.v_) of
          (PVar _ fname _, VClosure Nothing x body env_) ->
            let v1Named = { v1 | v_ = VClosure (Just fname) x body env_ } in
            case cons (pVar fname, v1Named) (Just env) of
              -- Don't add provenance: fine to say value is just from the let body.
              -- (We consider equations to be mobile).
              Just env_ -> Result.map (addWidgets ws1) <| eval syntax env_ bt_ e2
              _         -> errorWithBacktrace syntax (e::bt) <| strPos e.start ++ "bad ELet"
          (PList _ _ _ _ _, _) ->
            errorWithBacktrace syntax (e::bt) <|
              strPos e1.start ++
              """mutually recursive functions (i.e. letrec [...] [...] e) \
                 not yet implemented""" --"
               -- Implementation also requires modifications to LangSimplify.simply
               -- so that clean up doesn't prune the funtions.
          _ ->
            errorWithBacktrace syntax (e::bt) <| strPos e.start ++ " bad letrec"

  EColonType _ e1 _ t1 _ ->
    -- Pass-through, so don't add provenance.
    case t1.val of
      -- using (e : Point) as a "point widget annotation"
      TApp _ a _ ->
        if String.trim a /= "Point" then eval syntax env bt e1
        else
          eval syntax env bt e1 |> Result.map (\(((v,ws),env_) as result) ->
            case v.v_ of
              VList [v1, v2] ->
                case (v1.v_, v2.v_) of
                  (VConst _ nt1, VConst _ nt2) ->
                    let vNew = {v | v_ = VList [{v1 | v_ = VConst (Just (X, nt2, v2)) nt1}, {v2 | v_ = VConst (Just (Y, nt1, v1)) nt2}]} in
                    ((vNew, ws ++ [WPoint nt1 v1 nt2 v2]), env_)
                  _ ->
                    result
              _ ->
                result
            )
      _ ->
        eval syntax env bt e1

  EOption _ _ _ _ e1    -> eval syntax env bt e1
  ETyp _ _ _ e1 _       -> eval syntax env bt e1
  -- EColonType _ e1 _ _ _ -> eval syntax env bt e1
  ETypeAlias _ _ _ e1 _ -> eval syntax env bt e1
  ETypeDef _ _ _ _ _ e1 _ -> eval syntax env bt e1
  EParens _ e1 _ _      -> eval syntax env bt e1
  EHole _ (Just val)    -> Ok <| retV [val] val
  EHole _ Nothing       -> errorWithBacktrace syntax (e::bt) <| strPos e.start ++ " empty hole!"


evalOp : Syntax -> Env -> Exp -> Backtrace -> Op -> List Exp -> Result String (Val, Widgets)
evalOp syntax env e bt opWithInfo es =
  let (op,opStart) = (opWithInfo.val, opWithInfo.start) in
  let argsEvaledRes = List.map (eval_ syntax env bt) es |> Utils.projOk in
  case argsEvaledRes of
    Err s -> Err s
    Ok argsEvaled ->
      let (vs,wss) = List.unzip argsEvaled in
      let error () =
        errorWithBacktrace syntax bt
          <| "Bad arguments to " ++ strOp op ++ " operator " ++ strPos opStart
          ++ ":\n" ++ Utils.lines (Utils.zip vs es |> List.map (\(v,e) -> (valToString v) ++ " from " ++ (Syntax.unparser syntax e)))
      in
      let addProvenance val_   = Val val_ (Provenance env e vs) (Parents []) in
      let addProvenanceOk val_ = Ok (addProvenance val_) in
      let nullaryOp args retVal_ =
        case args of
          [] -> addProvenanceOk retVal_
          _  -> error ()
      in
      let unaryMathOp op args =
        case args of
          [VConst _ (n,t)] -> VConst Nothing (evalDelta syntax bt op [n], TrOp op [t]) |> addProvenanceOk
          _                -> error ()
      in
      let binMathOp op args =
        case args of
          [VConst maybeAxisAndOtherDim1 (i,it), VConst maybeAxisAndOtherDim2 (j,jt)] ->
            let maybeAxisAndOtherDim =
              case (op, maybeAxisAndOtherDim1, maybeAxisAndOtherDim2) of
                (Plus, Just axisAndOtherDim, Nothing)  -> Just axisAndOtherDim
                (Plus, Nothing, Just axisAndOtherDim)  -> Just axisAndOtherDim
                (Minus, Just axisAndOtherDim, Nothing) -> Just axisAndOtherDim
                _                                      -> Nothing
            in
            VConst maybeAxisAndOtherDim (evalDelta syntax bt op [i,j], TrOp op [it,jt]) |> addProvenanceOk
          _  ->
            error ()
      in
      let args = List.map .v_ vs in
      let newValRes =
        case op of
          Plus    -> case args of
            [VBase (VString s1), VBase (VString s2)] -> VBase (VString (s1 ++ s2)) |> addProvenanceOk
            _                                        -> binMathOp op args
          Minus     -> binMathOp op args
          Mult      -> binMathOp op args
          Div       -> binMathOp op args
          Mod       -> binMathOp op args
          Pow       -> binMathOp op args
          ArcTan2   -> binMathOp op args
          Lt        -> case args of
            [VConst _ (i,it), VConst _ (j,jt)] -> VBase (VBool (i < j)) |> addProvenanceOk
            _                                  -> error ()
          Eq        ->
            let valEquals: List Val_ -> Result String Bool
                valEquals args =
                 case args of
                   [VConst _ (i,it), VConst _ (j,jt)]       -> Ok <| i == j
                   [VBase baseVal1, VBase baseVal2]         -> Ok <| baseVal1 == baseVal2
                   [VDict d1, VDict d2]                     ->
                     Dict.merge
                       (\k1 v1 b -> Ok False)
                       (\k v1 v2 b -> b |> Result.andThen (\bo -> if bo then valEquals [v1.v_, v2.v_] else Ok bo))
                       (\k2 v2 b -> Ok False)
                       d1 d2  (Ok True)
                   [VRecord d1, VRecord d2]                 ->
                     Dict.merge
                       (\k1 v1 b -> Ok False)
                       (\k v1 v2 b -> b |> Result.andThen (\bo -> if bo then valEquals [v1.v_, v2.v_] else Ok bo))
                       (\k2 v2 b -> Ok False)
                       d1 d2  (Ok True)
                   [VList l1, VList l2]                     ->
                     if List.length l1 /= List.length l2 then Ok False
                     else List.foldl (\(v1, v2) b -> b |> Result.andThen(\bo -> if bo then valEquals [v1.v_, v2.v_] else Ok bo)) (Ok True) (Utils.zip l1 l2)
                   [_, _]                                   -> Err <| "Values not comparable:" ++ String.join "==" (List.map valToString vs) ++ "Values should have the same types and not be closures"-- polymorphic inequality, added for Prelude.addExtras
                   _                                        -> Err "Equality has exactly two arguments"
            in valEquals args |> Result.map (\x -> VBase (VBool x) |> addProvenance)
          Pi         -> nullaryOp args (VConst Nothing (pi, TrOp op []))
          DictEmpty  -> nullaryOp args (VDict Dict.empty)
          DictFromList -> case vs of
            [list] -> case Vu.list (Vu.tuple2 Ok Ok) list of
              Ok listPairs ->
                List.map (\(vkey, vval) ->
                  valToDictKey syntax bt vkey |> Result.map (\dkey -> (dkey, vval))
                ) listPairs
                |> Utils.projOk
                |> Result.map (Dict.fromList >> VDict >> addProvenance)
              _                 -> error ()
            _                 -> error ()
          DictInsert -> case vs of
            [vkey, val, {v_}] -> case v_ of
              VDict d -> valToDictKey syntax bt vkey |> Result.map (\dkey -> VDict (Dict.insert dkey val d) |> addProvenance)
              _       -> error()
            _                 -> error ()
          DictGet    -> case vs of
            [key, {v_}] -> case v_ of
              VDict d     ->
                valToDictKey syntax bt key |> Result.map (\dkey ->
                  case Dict.get dkey d of
                    Nothing -> Vb.constructor addProvenance "Nothing" []
                    Just x -> Vb.constructor addProvenance "Just" [x]
                  )
              _           -> error()
            _           -> error ()
          DictRemove -> case vs of
            [key, {v_}] -> case v_ of
              VDict d      -> valToDictKey syntax bt key |> Result.map (\dkey -> VDict (Dict.remove dkey d) |> addProvenance)
              _            -> error ()
            _           -> error ()
          Cos        -> unaryMathOp op args
          Sin        -> unaryMathOp op args
          ArcCos     -> unaryMathOp op args
          ArcSin     -> unaryMathOp op args
          Floor      -> unaryMathOp op args
          Ceil       -> unaryMathOp op args
          Round      -> unaryMathOp op args
          Sqrt       -> unaryMathOp op args
          Explode    -> case args of
            [VBase (VString s)] ->
              String.toList s
              |> List.map String.fromChar
              |> Utils.mapi0
                  (\(i, charStr) ->
                    { v_ = VBase (VString charStr)
                    , provenance = Provenance env (eCall "nth" [e, eConstDummyLoc (toFloat i)]) vs
                    , parents = Parents []
                    }
                  )
              |> VList
              |> addProvenanceOk
            _                   -> error ()
          DebugLog   -> case vs of
            [val] -> case val.v_ of
               VBase (VString v) as r -> let _ = ImpureGoodies.log v in Ok val
               _  -> let _ = ImpureGoodies.log (valToString val) in Ok val
            _   -> error ()
          NoWidgets  -> case vs of
            [v] -> Ok v -- Widgets removed  below.
            _   -> error ()
          ToStr      -> case vs of
            [val] -> VBase (VString (valToString val)) |> addProvenanceOk
            _     -> error ()
          ToStrExceptStr -> case vs of
            [val] -> case val.v_ of
              VBase (VString v) as r -> r |> addProvenanceOk
              v -> VBase (VString (valToString val)) |> addProvenanceOk
            _     -> error ()
          RegexExtractFirstIn -> case vs of
            [regexp, string] ->
              evalRegexExtractFirstIn regexp string
            _     -> error ()
      in
      case newValRes of
        Err s     -> Err s
        Ok newVal ->
          let newWidgets =
            case (op, args, vs) of
              (Plus, [VConst (Just (axis, otherDimNumTr, otherDirVal)) numTr, VConst Nothing amountNumTr], [_, amountVal]) ->
                let (baseXNumTr, baseYNumTr, endXVal, endYVal) =
                  if axis == X
                  then (numTr, otherDimNumTr, newVal, otherDirVal)
                  else (otherDimNumTr, numTr, otherDirVal, newVal)
                in
                [WOffset1D baseXNumTr baseYNumTr axis Positive amountNumTr amountVal endXVal endYVal]
              (Plus, [VConst Nothing amountNumTr, VConst (Just (axis, otherDimNumTr, otherDirVal)) numTr], [amountVal, _]) ->
                let (baseXNumTr, baseYNumTr, endXVal, endYVal) =
                  if axis == X
                  then (numTr, otherDimNumTr, newVal, otherDirVal)
                  else (otherDimNumTr, numTr, otherDirVal, newVal)
                in
                [WOffset1D baseXNumTr baseYNumTr axis Positive amountNumTr amountVal endXVal endYVal]
              (Minus, [VConst (Just (axis, otherDimNumTr, otherDirVal)) numTr, VConst Nothing amountNumTr], [_, amountVal]) ->
                let (baseXNumTr, baseYNumTr, endXVal, endYVal) =
                  if axis == X
                  then (numTr, otherDimNumTr, newVal, otherDirVal)
                  else (otherDimNumTr, numTr, otherDirVal, newVal)
                in
                [WOffset1D baseXNumTr baseYNumTr axis Negative amountNumTr amountVal endXVal endYVal]
              _ -> []
          in
          let widgets =
            case op of
              NoWidgets -> []
              _         -> List.concat wss ++ newWidgets
          in
          Ok (newVal, widgets)


-- Returns Ok Nothing if no branch matches
-- Returns Ok (Just results) if branch matches and no execution errors
-- Returns Err s if execution error
evalBranches syntax env bt v bs =
  List.foldl (\(Branch_ _ pat exp _) acc ->
    case (acc, cons (pat,v) (Just env)) of
      (Ok (Just done), _)     -> acc
      (Ok Nothing, Just env_) -> eval_ syntax env_ bt exp |> Result.map Just
      (Err s, _)              -> acc
      _                       -> Ok Nothing

  ) (Ok Nothing) (List.map .val bs)


-- Returns Ok Nothing if no branch matches
-- Returns Ok (Just results) if branch matches and no execution errors
-- Returns Err s if execution error
evalTBranches syntax env bt val tbranches =
  List.foldl (\(TBranch_ _ tipe exp _) acc ->
    case acc of
      Ok (Just done) ->
        acc

      Ok Nothing ->
        if Types.valIsType val tipe then
          eval_ syntax env bt exp |> Result.map Just
        else
          acc

      Err s ->
        acc
  ) (Ok Nothing) (List.map .val tbranches)


evalDelta syntax bt op is =
  case Lang.maybeEvalMathOp op is of
    Just result -> result
    Nothing     -> crashWithBacktrace syntax bt <| "Little evaluator bug: Eval.evalDelta " ++ strOp op


-- Using this recursive function rather than desugaring to single
-- applications: cleaner provenance (one record for entire app).
--
-- Returns: Result String (argVals, (functionResult, widgets))
apply syntax env bt bt_ e psLeft esLeft funcBody closureEnv =
  let recurse = apply syntax env bt bt_ e in
  case (psLeft, esLeft) of
    ([], []) ->
      eval_ syntax closureEnv bt_ funcBody |> Result.map (\valAndWs -> ([], valAndWs))

    ([], esLeft) ->
      eval_ syntax closureEnv bt_ funcBody
      |> Result.andThen
          (\(fRetVal1, fRetWs1) ->
            case fRetVal1.v_ of
              VClosure maybeRecName ps funcBody closureEnv ->
                case maybeRecName of
                  Nothing    -> recurse ps esLeft funcBody closureEnv                      |> Result.map (\(argVals, (v2, ws2)) -> (argVals, (v2, fRetWs1 ++ ws2)))
                  Just fName -> recurse ps esLeft funcBody ((fName, fRetVal1)::closureEnv) |> Result.map (\(argVals, (v2, ws2)) -> (argVals, (v2, fRetWs1 ++ ws2)))
              _ ->
                errorWithBacktrace syntax (e::bt) <| strPos e.start ++ " too many arguments given to function"
          )

    (psLeft, []) ->
      let
        -- Based-on provenance is only concerned with concrete values, so the provenance here
        -- is moot (i.e. for an application (e1 e2) the provenance of e1 is ignored).
        -- The provenance that matters is already attached to the values in the closureEnv.
        finalVal = { v_ = VClosure Nothing psLeft funcBody closureEnv, provenance = dummyProvenance, parents = Parents [] }
      in
      Ok ([], (finalVal, []))

    (p::psLeft, e::esLeft) ->
      case eval_ syntax env bt_ e of
        Err s -> Err s
        Ok (argVal, argWs) ->
          case cons (p, argVal) (Just closureEnv) of
            Just closureEnv -> recurse psLeft esLeft funcBody closureEnv |> Result.map (\(laterArgs, (v2, ws2)) -> (argVal::laterArgs, (v2, argWs ++ ws2)))
            Nothing         -> errorWithBacktrace syntax (e::bt) <| strPos e.start ++ " bad arguments to function, cannot match " ++ Syntax.patternUnparser Syntax.Elm p ++ " with " ++ Syntax.unparser Syntax.Elm e ++ "(evaluates to " ++ valToString argVal ++ ")"


eBaseToVBase eBaseVal =
  case eBaseVal of
    EBool b     -> VBool b
    EString _ b -> VString b
    ENull       -> VNull

postProcessWidgets widgets =
  let dedupedWidgets = {-Utils.dedup-} widgets in
  -- partition so that hidden and point sliders don't affect indexing
  -- (and, thus, positioning) of range sliders
  --
  let (rangeWidgets, pointWidgets) =
    dedupedWidgets |>
      List.partition (\widget ->
        case widget of
          WIntSlider _ _ _ _ _ _ False -> True
          WNumSlider _ _ _ _ _ _ False -> True
          WIntSlider _ _ _ _ _ _ True  -> False
          WNumSlider _ _ _ _ _ _ True  -> False
          WPoint _ _ _ _               -> False
          WOffset1D _ _ _ _ _ _ _ _    -> False
          WCall _ _ _ _                -> False
      )
  in
  rangeWidgets ++ pointWidgets

btString : Syntax -> Backtrace -> String
btString syntax bt =
  case bt of
    [] -> ""
    mostRecentExp::others ->
      let singleLineExpStrs =
        others
        |> List.map (Utils.head_ << String.lines << String.trimLeft << Syntax.unparser syntax)
        |> List.reverse
        |> String.join "\n"
      in
      singleLineExpStrs ++ "\n" ++ (Syntax.unparser syntax mostRecentExp)


errorWithBacktrace syntax bt message =
  errorMsg <| (btString syntax bt) ++ "\n" ++ message

crashWithBacktrace syntax bt message =
  crashWithMsg <| (btString syntax bt) ++ "\n" ++ message
