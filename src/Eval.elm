module Eval exposing (
  doEval,
  eval,
  EvalOptions,
  evalContext,
  withParentsProvenanceWidgets,
  withoutParentsProvenanceWidgets,
  evalDelta,
  evalDeclarations)

import Debug
import Dict
import String

import ImpureGoodies
import Lang exposing (..)
import ValUnparser exposing (strVal_, strOp, strLoc)
import LeoParser as Parser
import LeoLang
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
import Set


valToDictKey : Syntax -> Backtrace -> Val -> Result String (String, String)
valToDictKey syntax bt v =
  case LangUtils.valToDictKey syntax v of
    Err msg -> Err <| backtraceMessage syntax bt msg
    Ok i -> Ok i

------------------------------------------------------------------------------
-- Big-Step Operational Semantics

match : (Pat, Val) -> Maybe Env
match (p,v) = case (p.val.p__, v.v_) of
  (PWildcard _, _) -> Just []
  (PVar _ x _, _) -> Just [(x,v)]
  (PAs _ innerPat1 _ innerPat2, _) ->
    case match (innerPat1, v) of
      Just env -> case match (innerPat2, v) of
        Just env2 -> Just (env2 ++ env)
        Nothing -> Nothing
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
  (PColonType _ p _ tipe, _) ->
     if Types.valIsType v tipe then match (p, v) else Nothing
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
    Nothing -> Err <| backtraceMessage syntax bt <| strPos pos ++ " variable not found: " ++ x ++ "\nVariables in scope: " ++ (String.join " " <| List.map Tuple.first env)


mkCap mcap l =
  let s =
    case (mcap, l) of
       (Just cap, _)       -> cap.val
       (Nothing, (_,_,"")) -> strLoc l
       (Nothing, (_,_,x))  -> x
  in
  s ++ ": "

doEval : EvalOptions -> Syntax -> Env -> Exp -> Result String ((Val, Widgets), Env)
doEval options syntax env e =
  eval [] (evalContext options syntax env [] e)
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

type alias EvalResult = ((Val, Widgets), Env)
type alias EvalOptions = {recordParents: Bool, recordProvenance: Bool, recordWidgets: Bool}
withParentsProvenanceWidgets = EvalOptions True True True
withoutParentsProvenanceWidgets = EvalOptions False False False

type EvalStack =
    EvalReturn EvalResult (Maybe (EvalResult -> EvalStack))
  | EvalError String
  | EvalContinue EvalOptions Syntax Env Backtrace Exp (Maybe (EvalResult -> EvalStack))

-- Just like eval, but ignore envOut
eval_ : EvalOptions -> Syntax -> Env -> Backtrace -> Exp -> Result String (Val, Widgets)
eval_ options syntax env bt e = Result.map Tuple.first <| eval [] (evalContext options syntax env bt e)

evalReturn: EvalResult -> EvalStack
evalReturn r = EvalReturn r Nothing

evalContext: EvalOptions -> Syntax -> Env -> Backtrace -> Exp -> EvalStack
evalContext options syntax env bt e = EvalContinue options syntax env bt e Nothing

evalContinue: EvalOptions -> Syntax -> Env -> Backtrace -> Exp -> (EvalResult -> EvalStack) -> EvalStack
evalContinue options syntax env bt e continuation = EvalContinue options syntax env bt e <| Just continuation

eval: List (EvalResult -> EvalStack) -> EvalStack -> Result String EvalResult
eval callbacks currentStack =
  case currentStack of
    EvalError msg -> Err msg
    EvalReturn result mb ->
      case Utils.maybeCons mb callbacks of
        [] -> Ok result
        f :: tail -> eval tail <| f result
    EvalContinue options syntax env bt e mb ->
      getEvalStack options syntax env bt e |>
      eval (Utils.maybeCons mb callbacks)

getEvalStack: EvalOptions -> Syntax -> Env -> Backtrace -> Exp -> EvalStack
getEvalStack options syntax env bt (Expr exp_ as e) =
  let e_start = exp_.start in

  --let _ = Debug.log ("Evaluating " ++ (Syntax.unparser Syntax.Leo e)) () in

  -- Deeply tag value's children to say the child flowed through here.
  --
  let addParent = if options.recordParents then
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
       \v ->
        if Parser.isProgramEId (expEId e) then
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
    else
    identity
  in

  let makeProvenance basedOn = Provenance env e basedOn in
  -- Only use introduceVal, ret, or retBoth for new values (i.e. not var lookups): they do not preserve parents
  let introduceVal =
    if options.recordParents then
       \basedOn v_  -> addParent <| Val v_ (makeProvenance basedOn) (Parents [])
    else
       \basedOn v_  -> Val v_ (makeProvenance basedOn) (Parents [])
  in
  let retBoth basedOn (v_, ws) = ((introduceVal basedOn v_, ws), env) in
  let ret basedOn v_           = retBoth basedOn (v_, []) in

  let retV = if options.recordProvenance then
     \basedOn v -> ((addParent { v | provenance = makeProvenance basedOn}, []), env)
     else
       if options.recordParents then
        \basedOn v -> ((addParent v, []), env)
       else
        \basedOn v -> ((v, []), env)
  in
  let retVBoth =
    if options.recordParents then
       \basedOn (v, ws)  -> ((addParent { v | provenance = makeProvenance basedOn}, ws), env)
    else
       \basedOn (v, ws)  -> (({ v | provenance = makeProvenance basedOn}, ws), env)
    in
  let retAddWs ws1 (v1, ws2)                     = (v1, ws1 ++ ws2) in
  let addParentToRet ((v,ws),envOut)             = ((addParent v, ws), envOut) in
  let addProvenanceToValWidgets basedOn (v,ws)   = (addParent { v | provenance = makeProvenance basedOn}, ws) in
  let addProvenanceToRet basedOn ((vws), envOut) = (addProvenanceToValWidgets basedOn vws, envOut) in
  let addWidgets ws1 ((v1,ws2),env1)             = ((v1, ws1 ++ ws2), env1) in
  let bt_ =
    if e_start.line >= 1
    then e::bt
    else bt
  in

  case unwrapExp e of

  EConst _ n loc wd ->
    let v_ = VConst Nothing (n, TrLoc loc) in
    let retVal = introduceVal [] v_ in
    if options.recordWidgets then
      evalReturn ((retVal, []), env)
    else
    case wd.val of
      NoWidgetDecl         ->
        evalReturn ((retVal, []), env)
      IntSlider a _ b mcap hidden ->
        let widget = WIntSlider a.val b.val (mkCap mcap loc) (floor n) retVal loc hidden in
        evalReturn ((retVal, [widget]), env)
      NumSlider a _ b mcap hidden ->
        let widget = WNumSlider a.val b.val (mkCap mcap loc) n retVal loc hidden in
        evalReturn ((retVal, [widget]), env)

  EBase _ v     -> evalReturn <| ret [] <| VBase (eBaseToVBase v)
  EVar _ x      -> case lookupVar syntax env (e::bt) x e_start of
    Err msg -> EvalError msg
    Ok v -> evalReturn <| retV [v] v
  EFun _ ps e _ -> evalReturn <| ret [] <| VClosure [] ps e env
  EOp _ _ op es _ ->
     let aux revVsComputed revWsComputed esRemaining = case esRemaining of
       [] ->
          case evalOp syntax env e (e::bt) op es (List.reverse revVsComputed) (List.reverse revWsComputed) of
            Err msg -> EvalError msg
            Ok res -> evalReturn <| if options.recordParents then addParentToRet (res, env) else (res, env)
       e1 :: esTail ->
          evalContinue options syntax env bt_ e1 <| \((v, ws), _) ->
            aux (v :: revVsComputed) (Utils.reverseInsert ws revWsComputed) esTail
     in aux [] [] es

  EList _ es _ m _ ->
    let aux revVsComputed revWsComputed esRemaining = case esRemaining of
      [] ->
         let vs = List.reverse revVsComputed in
         let ws = List.reverse revWsComputed in
         case m of
           Nothing   -> evalReturn <| retBoth vs (VList vs, ws)
           Just (Expr rest as eRest) ->
             evalContinue options syntax env bt_ eRest <| \((vRest, ws_), _) ->
               case vRest.v_ of
                 VList vs_ -> evalReturn <| retBoth (vs ++ [vRest]) (VList (vs ++ vs_), ws ++ ws_)
                 _ -> errorWithBacktrace syntax (e::bt) <| strPos rest.start ++ " rest expression not a list, but " ++ valToString vRest

      (spc, e1) :: esTail ->
         evalContinue options syntax env bt_ e1 <| \((v, ws), _) ->
           aux (v :: revVsComputed) (Utils.reverseInsert ws revWsComputed) esTail
    in aux [] [] es

  ERecord _ mi (Declarations _ _ _ letexpsGroups as declarations) _ -> let
      computeResInitDictWidgets continuation = case mi of
        Nothing -> continuation Nothing Dict.empty []
        Just (Expr init as exprInit, ws) ->
          evalContinue options syntax env bt_ exprInit <| \((v, ws), _) ->
            case v.v_ of
              VRecord d ->
                continuation (Just v) d ws
              _ -> errorWithBacktrace syntax (e::bt) <| strPos init.start ++ " init expression not a record, but " ++ valToString v
    in
    computeResInitDictWidgets <| \v d ws ->
    evalDeclarations options syntax env bt_ declarations <| \newEnv widgets ->
       -- Find the value of each newly added ident and adds it to records.
       let ids = letexpsGroups |> elemsOf |> List.concatMap (\(LetExp _ _ p _ _ _) -> publicIdentifiersListIntPat p) in
       let kvs = VRecord (Dict.union (ids |> Set.fromList |> Set.map (
         \i -> (i, lookupVar syntax newEnv (e::bt) i e_start |> Utils.fromOk "Record variable"))
             |> Set.toList |> Dict.fromList) d) in
       evalReturn <| retBoth (Maybe.withDefault [] <| Maybe.map (\x -> [x]) v) (kvs, ws)

  ESelect ws0 (Expr exp1_ as e1) _ wsId id ->
    evalContinue options syntax env bt_ e1 <| \((vRecord, ws), _) ->
      case vRecord.v_ of
        VRecord dict ->
          case Dict.get id dict of
            Just vValue ->
              let default () = evalReturn <| retBoth [vRecord] (vValue.v_, ws) in
              case vValue.v_ of
                VClosure recEnv (p1::pTail) body closureEnv ->
                  case pVarUnapply p1 of
                    Just "this" -> -- recursive records.
                      evalContext options syntax
                        [("this", vRecord), ("recRecordMethod", vValue)] bt
                        (eApp (eVar "recRecordMethod") [eVar "this"])
                    _ -> default()
                _ -> default ()
            _ ->
                let suggestions = Utils.stringSuggestions (Dict.keys dict) id in
                errorWithBacktrace syntax (e1::bt) <| strPos wsId.end ++ " Key " ++ id ++ " not found." ++ (case suggestions of
                  [] -> ""
                  l -> " Did you mean '" ++ String.join "', or '" l ++ "'?"
                )
        _ -> errorWithBacktrace syntax (e1::bt) <| strPos exp1_.start ++ " select expression applied to non-dict "

  -- Alternatively, could choose not to add a basedOn record for if/case/typecase (simply pass value through, maybe add parent)
  -- But that would suggest that we *might* avoid doing so for EApp as well, which is more dubious. We'll see.
  EIf _ (Expr e1 as eCond) _ eThen _ eElse _ ->
    evalContinue options syntax env bt eCond <| \((v1, ws1), _) -> let
        afterEval (((v, _), _) as result) =
          evalReturn <| addProvenanceToRet [v] <| addWidgets ws1 result -- Provenence basedOn vals control-flow agnostic: do not include scrutinee
      in case v1.v_ of
          VBase (VBool True)  ->
            evalContinue options syntax env bt eThen afterEval
          VBase (VBool False) ->
            evalContinue options syntax env bt eElse afterEval
          _                   -> errorWithBacktrace syntax (e::bt) <| strPos e1.start ++ " if-exp expected a Bool but got something else."

  ECase _ (Expr e1 as eScrutinee) bs _ ->
    evalContinue options syntax env (e::bt) eScrutinee <| \((v1, ws1), _) ->
      evalBranches options syntax env (e::bt) v1 bs (\() ->
        errorWithBacktrace syntax (e::bt) <| strPos e1.start ++ " non-exhaustive case statement, cannot match " ++ valToString v1
      ) <| \((v2,ws2), _) ->
        evalReturn <| retVBoth [v2] (v2, ws1 ++ ws2) -- Provenence basedOn vals control-flow agnostic: do not include scrutinee

  EApp sp0 (Expr exp1_ as e1) es appStyle sp1 ->
    let e1 = Expr exp1_ in
    if appStyle == InfixApp && eVarUnapply e1 == Just "++" then
        case es of
          [eLeft, eRight] -> -- We rewrite ++ to a call to "append" or "plus" depending on the arguments
            evalContinue options syntax env bt_ eLeft <| \((v1, ws1), _) ->
            evalContinue options syntax env bt_ eRight <| \((v2, ws2), _) ->
            case (v1.v_, v2.v_) of
              (VBase (VString x), VBase (VString y)) ->
                evalContext options syntax ([("x", v1), ("y", v2)] ++ env) bt_  (replaceE__ e <|
                  EOp space1 space1 (withDummyRange Plus) [replaceE__ eLeft <| EVar space0 "x", replaceE__ eRight <| EVar space0 "y"] space0)
              _ ->
                evalContext options  syntax ([("x", v1), ("y", v2)] ++ env) bt_ (replaceE__ e <|
                  EApp sp0 (replaceE__ e1 <| EVar sp0 "append") [replaceE__ eLeft <| EVar space1 "x", replaceE__ eRight <| EVar space1 "y"] SpaceApp sp1)
          _ -> EvalError <| "++ should be called with two arguments, was called on "++toString (List.length es)++". "
    else
    evalContinue options syntax env bt_ e1 <| \((v1, ws1), _) -> let
    evalVApp: Val -> Widgets -> List Exp -> EvalStack
    evalVApp v1 ws es =
       case v1.v_ of
         VClosure recNames ps funcBody env_ ->
           apply options syntax env bt bt_ e ps es funcBody (expandRecEnv recNames env_) <| \argVals ((fRetVal, fRetWs), _) ->
             -- Do not record dependence on closure (which function to execute is essentially control flow).
             -- Dependence on function is implicit by being dependent on some value computed by an expression in the function.
             -- Instead, point to return value (and, hence, the final expression of) the function. This will also
             -- transitively point to any arguments used.
             let perhapsCallWidget =
               if options.recordWidgets && Parser.isProgramEId (expEId e) && Parser.isProgramEId (expEId funcBody)
               then [WCall v1 argVals fRetVal fRetWs]
               else []
             in
             evalReturn <| retVBoth [fRetVal] (fRetVal, ws1 ++ fRetWs ++ perhapsCallWidget)
         VFun name argList evalDef _ -> let
             arity = List.length argList
             availableArgs = List.length es
           in if availableArgs < arity then let -- Partial application, hence eta expansion
               funconverted = replaceV_ v1 <| VClosure
                  []
                  (List.map (\a -> withDummyPatInfo <| PVar space1 a noWidgetDecl) <| argList)
                  (replaceE__ e <| EApp sp0 (replaceE__ e <| EVar space1 name)
                    (List.map (withDummyExpInfo << EVar space1) <| argList) SpaceApp sp0) ((name, v1)::env)
             in evalVApp funconverted ws es
           else let
             (arguments, remaining) =
               Utils.split arity es

             aux arguments revArgVals revArgWidgets =
               case arguments of
               arg :: argTail ->
                 evalContinue options syntax env bt_ arg <| \((argVal, argWidgets), _) ->
                   aux argTail (argVal :: revArgVals) (Utils.reverseInsert argWidgets revArgWidgets)
               [] -> let
                 argumentsVal = List.reverse revArgVals
                 basicResult = evalDef argumentsVal in
                 case basicResult of
                   Err s -> errorWithBacktrace syntax bt_ s
                   Ok ((v, newWs) as vw) ->
                     if List.length remaining > 0 then
                       evalVApp v (List.reverse <| Utils.reverseInsert newWs revArgWidgets) remaining
                     else
                       evalReturn (vw, env) in

             aux arguments [] (List.reverse ws)
         VRecord keyValues ->
           case (vRecordUnapplyField ctorDataType v1, vRecordUnapplyField ctorArgs v1 |> Maybe.andThen vRecordUnapply) of
             (Just _, Just args) -> -- let's add n more argument to the datatype, as if it was partially applied
               let positionArgToAdd = Dict.size args + 1 in
               let aux: Int -> List Exp -> Dict.Dict String Val -> Widgets -> EvalStack
                   aux n arguments newArgs revArgWidgets = case arguments of
                     arg :: argTail ->
                       evalContinue options syntax env bt_ arg <| \((argVal, argWidgets), _) ->
                         aux (n + 1) argTail (Dict.insert (argName n) argVal newArgs) (Utils.reverseInsert argWidgets revArgWidgets)
                     [] -> let
                       basicResult = replaceV_ v1 <|  VRecord (Dict.insert ctorArgs (replaceV_ v1 <| VRecord newArgs) keyValues)
                       vw = (basicResult, List.reverse revArgWidgets) in

                       evalReturn (vw, env)
               in aux positionArgToAdd es args (List.reverse ws)
             _ ->
               errorWithBacktrace syntax (e::bt) <| strPos exp1_.start ++ " not a function or a partially applied datatype"
         _ ->
           errorWithBacktrace syntax (e::bt) <| strPos exp1_.start ++ " not a function or a partially applied datatype" in
    evalVApp v1 [] es

  ELet wsLet lk declarations _ e2 ->
    evalDeclarations options syntax env bt_ declarations <| \newEnv widgets ->
      evalContinue options syntax newEnv bt_ e2 <| \result ->
        evalReturn <| addWidgets widgets <| result

  EColonType _ e1 _ t1 _ ->
    -- Pass-through, so don't add provenance.
    case t1.val.t__ of
      -- using (e : Point) as a "point widget annotation"
      TVar _ "Point" ->
        evalContinue options syntax env bt e1 <| \(((v,ws),env_) as result) ->
          if options.recordWidgets then
          case v.v_ of
            VList [v1, v2] ->
              case (v1.v_, v2.v_) of
                (VConst _ nt1, VConst _ nt2) ->
                  let vNew = {v | v_ = VList [{v1 | v_ = VConst (Just (X, nt2, v2)) nt1}, {v2 | v_ = VConst (Just (Y, nt1, v1)) nt2}]} in
                  evalReturn ((vNew, ws ++ [WPoint nt1 v1 nt2 v2]), env_)
                _ ->
                  evalReturn result
            _ ->
              evalReturn result
          else evalReturn result
      _ ->
        evalContext options syntax env bt e1

  EParens _ e1 _ _      -> evalContext options syntax env bt e1
  EHole _ (ESnapHole val) -> evalReturn <| retV [val] val
  EHole _ EEmptyHole    -> errorWithBacktrace syntax (e::bt) <| strPos e_start ++ " empty hole!"


evalOp : Syntax -> Env -> Exp -> Backtrace -> Op -> List Exp -> List Val -> Widgets -> Result String (Val, Widgets)
evalOp syntax env e bt opWithInfo es vs wss = let
  (op,opStart) = (opWithInfo.val, opWithInfo.start)
  error () =
    Err <| backtraceMessage syntax bt
       <| "Bad arguments to " ++ strOp op ++ " operator " ++ strPos opStart
       ++ ":\n" ++ Utils.lines (Utils.zip vs es |> List.map (\(v,e) -> (valToString v) ++ " from " ++ (Syntax.unparser syntax e)))
  addProvenance val_   = Val val_ (Provenance env e vs) (Parents [])
  addProvenanceOk val_ = Ok (addProvenance val_)
  mkClosureOrError () =
    let vLength = List.length vs in
    let nbArgs = LeoLang.arity opWithInfo in
    if vLength >= nbArgs then error() else
    let vars = List.range 1 nbArgs |> List.map (\i -> Parser.implicitVarName ++ if (i == 1) then "" else toString i) in
    let (varsComputed, varsRemaining) = Utils.split vLength vars in
    VClosure [] (varsRemaining |> List.map pVar) (
       eOp opWithInfo.val (List.map eVar vars)) (Utils.reverseInsert (List.map2 (,) varsComputed vs) env) |> addProvenanceOk
  nullaryOp args retVal_ =
    case args of
       [] -> addProvenanceOk retVal_
       _  -> error ()
  unaryMathOp op args =
    case args of
       [VConst _ (n,t)] -> VConst Nothing (evalDelta syntax bt op [n], TrOp op [t]) |> addProvenanceOk
       _ -> mkClosureOrError ()
  binMathOp op args =
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
       _ -> mkClosureOrError ()
  args = List.map .v_ vs
  newValRes =
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
        [VBase (VString i), VBase (VString j)] -> VBase (VBool (i < j)) |> addProvenanceOk
        _ -> mkClosureOrError ()
      Eq        ->
        if List.length vs < 2 then mkClosureOrError () else
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
      CurrentEnv -> nullaryOp args (Vb.list (Vb.tuple2 Vb.string Vb.identity) (builtinVal "env") env).v_
      DictFromList -> case vs of
        [list] -> case Vu.list (Vu.tuple2 Ok Ok) list of
          Ok listPairs ->
            List.map (\(vkey, vval) ->
              valToDictKey syntax bt vkey |> Result.map (\dkey -> (dkey, vval))
            ) listPairs
            |> Utils.projOk
            |> Result.map (Dict.fromList >> VDict >> addProvenance)
          _                 -> error ()
        _ -> mkClosureOrError ()
      DictInsert -> case vs of
        [vkey, val, {v_}] -> case v_ of
          VDict d -> valToDictKey syntax bt vkey |> Result.map (\dkey -> VDict (Dict.insert dkey val d) |> addProvenance)
          _       -> error()
        _  -> mkClosureOrError ()
      DictGet    -> case vs of
        [key, {v_}] -> case v_ of
          VDict d     ->
            valToDictKey syntax bt key |> Result.map (\dkey ->
              case Dict.get dkey d of
                Nothing -> Vb.constructor addProvenance "Nothing" []
                Just x -> Vb.constructor addProvenance "Just" [x]
              )
          _           -> error()
        _           -> mkClosureOrError ()
      DictRemove -> case vs of
        [key, {v_}] -> case v_ of
          VDict d      -> valToDictKey syntax bt key |> Result.map (\dkey -> VDict (Dict.remove dkey d) |> addProvenance)
          _            -> error ()
        _           -> mkClosureOrError ()
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
        _                   -> mkClosureOrError ()
      DebugLog   -> case vs of
        [val] -> case val.v_ of
           VBase (VString v) as r -> let _ = ImpureGoodies.log v in Ok val
           _  -> let _ = ImpureGoodies.log (valToString val) in Ok val
        _   -> mkClosureOrError ()
      NoWidgets  -> case vs of
        [v] -> Ok v -- Widgets removed  below.
        _   -> mkClosureOrError ()
      ToStr      -> case vs of
        [val] -> VBase (VString (valToString val)) |> addProvenanceOk
        _     -> mkClosureOrError ()
      ToStrExceptStr -> case vs of
        [val] -> case val.v_ of
          VBase (VString v) as r -> r |> addProvenanceOk
          v -> VBase (VString (valToString val)) |> addProvenanceOk
        _     -> mkClosureOrError ()
      RegexExtractFirstIn -> case vs of
        [regexp, string] ->
          evalRegexExtractFirstIn regexp string
        _     -> mkClosureOrError () in
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
           _         -> wss ++ newWidgets
      in
      Ok (newVal, widgets)

evalDeclarations: EvalOptions -> Syntax -> Env -> Backtrace -> Declarations -> (Env -> List Widget -> EvalStack) -> EvalStack
evalDeclarations options syntax env bt (Declarations  _ _ _ letexpsGroups) continuation =
  let aux: GroupsOf LetExp -> List Widget -> Env -> EvalStack
      aux letexpsGroups widgets env = case letexpsGroups of
     [] -> continuation env widgets
     (rec, expHead) :: tail -> let
       i = List.length expHead
       evalGroup revPs revVs revWs group = case group of
         (LetExp _ _ p _ _ e1) :: groupTail ->
           evalContinue options syntax env bt e1 <| \((v, w), _) ->
             evalGroup (p::revPs) (v::revVs) (Utils.reverseInsert w revWs) groupTail
         [] -> let
           patterns = List.reverse revPs
           values = List.reverse revVs
           newWidgets = List.reverse revWs in
          if rec then
            let allNames = patterns |> List.map (\p -> case patEffectivePat p |> .val |> .p__ of
              PVar _ fname _ -> Ok fname
              _ -> Err "Recursive function with non-variable pattern!") |> Utils.projOk
            in
            case allNames of
              Err msg -> EvalError msg
              Ok recNames ->
                let nonRecEnv = List.map2 (,) recNames values in
                let newValues = values |> List.map2 (\name value ->
                     case value.v_ of
                       VClosure [] x body env_ -> Ok <| replaceV_ value <| VClosure recNames x body (nonRecEnv ++ env)
                       _ -> Err <| "Expected VClosures for recursivity, got " ++ valToString value
                         ) recNames |> Utils.projOk
                in
                case newValues of
                  Err msg -> EvalError msg
                  Ok newVals ->
                    let newEnv = List.map2 (,) recNames newVals ++ env in
                    aux tail (widgets ++ newWidgets) newEnv
          else
            case Utils.foldLeft (Just env) (Utils.zip patterns values) <|
                                 \mbEnv (name, value) -> cons (name, value) mbEnv
            of
             Just newEnv -> aux tail (widgets ++ newWidgets) newEnv
             _         -> errorWithBacktrace syntax bt <| (bt |> Utils.head "bt.first" |> (\(Expr exp) -> exp.start) |> strPos) ++
               "match error in deconstructing the value of " ++ (Utils.zip patterns values |> List.map (\(name, value) ->
                 (Syntax.patternUnparser Syntax.Leo name) ++ " with " ++ valToString value
               ) |> String.join "\n") in
       evalGroup [] [] [] expHead
  in aux letexpsGroups [] env

-- Returns Ok Nothing if no branch matches
-- Returns Ok (Just results) if branch matches and no execution errors
-- Returns Err s if execution error
evalBranches options syntax env bt v bs ifError continuation = let
  aux bs = case bs of
    [] -> ifError ()
    b :: bTail ->
      let (Branch_ _ pat exp _) = b.val in
      case cons (pat, v) (Just env) of
        Nothing -> aux bTail -- Tail-recursive !
        Just env_ ->
          evalContinue options syntax env_ bt exp continuation in
  aux bs

evalDelta syntax bt op is =
  case Lang.maybeEvalMathOp op is of
    Just result -> result
    Nothing     -> crashWithBacktrace syntax bt <| "Little evaluator bug: Eval.evalDelta " ++ strOp op


-- Using this recursive function rather than desugaring to single
-- applications: cleaner provenance (one record for entire app).
--
-- Returns: Result String (argVals, (functionResult, widgets))
apply: EvalOptions -> Syntax -> Env -> Backtrace -> Backtrace -> Exp -> List Pat -> List Exp -> Exp -> Env -> (List Val -> EvalResult -> EvalStack) -> EvalStack
apply options syntax env bt bt_ (Expr exp_ as e) psLeft esLeft funcBody closureEnv continuation = let
  aux: List Pat -> List Exp -> Exp -> Env -> List Val -> Widgets -> EvalStack
  aux psLeft esLeft funcBody closureEnv revArgVals revArgWidgets = case (psLeft, esLeft) of
    ([], []) ->
      evalContinue options syntax closureEnv bt_ funcBody <| continuation (List.reverse revArgVals)

    ([], esLeft) ->
      evalContinue options syntax closureEnv bt_ funcBody <| \((fRetVal1, fRetWs1), _) ->
        case fRetVal1.v_ of
          VClosure recNames ps newFuncBody closureEnv -> let
              recEnv = recNames |> List.map (\fName ->
                (fName, Utils.maybeFind fName closureEnv |> Utils.fromJust_ "[Internal error] Did not find recursive closure in its environment"))
            in aux ps esLeft newFuncBody (recEnv ++ closureEnv) revArgVals (Utils.reverseInsert fRetWs1 revArgWidgets)
          _ ->
            errorWithBacktrace syntax (e::bt) <| strPos exp_.start ++ " too many arguments given to function"

    (psLeft, []) -> let
        -- Based-on provenance is only concerned with concrete values, so the provenance here
        -- is moot (i.e. for an application (e1 e2) the provenance of e1 is ignored).
        -- The provenance that matters is already attached to the values in the closureEnv.
        finalVal = { v_ = VClosure [] psLeft funcBody closureEnv, provenance = dummyProvenance, parents = Parents [] }
      in continuation (List.reverse revArgVals) ((finalVal, []), env)

    (p::psLeft, e::esLeft) ->
      evalContinue options syntax env bt_ e <| \((argVal, argWs), _) ->
        case cons (p, argVal) (Just closureEnv) of
          Just newClosureEnv ->
            aux psLeft esLeft funcBody newClosureEnv (argVal::revArgVals) (Utils.reverseInsert argWs revArgWidgets)
          Nothing         ->
            errorWithBacktrace syntax (e::bt) <| strPos exp_.start ++ " bad arguments to function, cannot match " ++ Syntax.patternUnparser Syntax.Leo p ++ " with " ++ Syntax.unparser Syntax.Leo e ++ "(evaluates to " ++ valToString argVal ++ ")" in
  aux psLeft esLeft funcBody closureEnv [] []



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


backtraceMessage syntax bt message =
   (btString syntax bt) ++ "\n" ++ message

errorWithBacktrace syntax bt message =
  errorMsg <| backtraceMessage syntax bt message

crashWithBacktrace syntax bt message =
  crashWithMsg <| (btString syntax bt) ++ "\n" ++ message

errorPrefix = "[Evaluation Error]" -- NOTE: same as errorPrefix in Native/codeBox.js
crashWithMsg s  = Debug.crash <| errorPrefix ++ "\n\n" ++ s
errorMsg s      = EvalError <| errorPrefix ++ "\n\n" ++ s
