module Eval exposing (PBEHoleSeen, run, doEval, doEvalEarlyAbort, runUntilTheEnd, simpleEvalToMaybeVal, parseAndRun, parseAndRun_, evalDelta, initEnv, addSubsumingPriorWidgets)

import Debug
import Dict
import String

import ImpureGoodies
import Lang exposing (..)
import LangTools -- To name widgets properly
import Provenance
import ValUnparser exposing (..)
import ValWidgets
import FastParser exposing (parseE, prelude)
import Syntax exposing (Syntax)
import Types
import Utils

import ImpureGoodies

------------------------------------------------------------------------------
-- Big-Step Operational Semantics

match : (Pat, Val) -> Maybe Env
match (p,v) = case (p.val.p__, v.v_) of
  (PWildcard _, _) -> Just []
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
      let vRest =
        { v_ = VList vs2
        , provenance = Provenance (eApp (eVar0 "drop") [provenanceExp v.provenance, eConstDummyLoc (toFloat n)]) [v] -- TODO: should be based on "drop" prelude function and a dummy int. Doesn't matter for selected val -> program EId determination.
        , parents = Parents []
        }
      in
      cons (rest, vRest) (matchList (Utils.zip ps vs1))
        -- dummy Provenance, since VList itself doesn't matter
  (PList _ _ _ _ _, _) -> Nothing
  (PConst _ n, VConst _ (n_,_)) -> if n == n_ then Just [] else Nothing
  (PBase _ bv, VBase bv_) -> if (eBaseToVBase bv) == bv_ then Just [] else Nothing
  (PParens _ innerPat _, _) -> match (innerPat, v)
  _ -> Debug.crash <| "Little evaluator bug: Eval.match " ++ (toString p.val.p__) ++ " vs " ++ (toString v.v_)


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
  s


runUntilTheEnd : Exp -> Bool
runUntilTheEnd = always False

initEnvRes =
  let endingEId = (expEffectiveExp prelude).val.eid in -- Same as in LangTools.preludeIdentifiers
  eval (Just endingEId) runUntilTheEnd Syntax.Little [] [] { pbeHolesSeen = [] } prelude
  |> Result.map (Tuple.second >> Utils.fromJust_  "Eval.initEnvRes")

initEnv : Env
initEnv = Utils.fromOk "Eval.initEnv" <| initEnvRes

run : Syntax -> Exp -> Result String (Val, Widgets)
run syntax e =
  -- doEval syntax initEnv e |> Result.map Tuple.first
  ImpureGoodies.logTimedRun "Eval.run" (\() ->
    doEval syntax initEnv e |> Result.map Utils.fst3
  )

doEval : Syntax -> Env -> Exp -> Result String ((Val, Widgets), Maybe Env, List PBEHoleSeen)
doEval = doEvalEarlyAbort Nothing runUntilTheEnd


type EarlyAbort a = EarlyAbort a

type alias PBEHoleSeen         = (Exp, Env, Result String Val)
type alias PBEHolesSeenRefCell = { pbeHolesSeen : List PBEHoleSeen }

-- Return the value of some particular expression in the program the first time it is encountered.
doEvalEarlyAbort : Maybe EId -> (Exp -> Bool) -> Syntax -> Env -> Exp -> Result String ((Val, Widgets), Maybe Env, List PBEHoleSeen)
doEvalEarlyAbort maybeRetEnvEId abortPred syntax initEnv e =
  let pbeHolesSeenRefCell = { pbeHolesSeen = [] } in
  ImpureGoodies.tryCatch "EarlyAbort"
    (\()               -> eval maybeRetEnvEId abortPred syntax initEnv [] pbeHolesSeenRefCell e)
    (\(EarlyAbort ret) -> ret)
  |> Result.map (\((val, widgets), maybeEnv) -> ((val, postProcessWidgets e widgets), maybeEnv, pbeHolesSeenRefCell.pbeHolesSeen))


-- Do not use: you lose parent tagging.
-- provenanceToMaybeVal : Provenance -> Maybe Val
-- provenanceToMaybeVal (Provenance env e vs) =
--   eval env [] { pbeHolesSeen = [] } e
--   |> Result.map (\((val, widgets), env) -> val)
--   |> Result.toMaybe


-- eval propagates output environment in order to extract
-- initial environment from prelude

-- eval inserts dummyPos during evaluation

-- -- Like eval, but ignore envOut
-- eval_ : Syntax -> Env -> Backtrace -> Exp -> Result String (Val, Widgets)
-- eval_ syntax env bt e = Result.map Tuple.first <| eval Nothing runUntilTheEnd syntax env bt { pbeHolesSeen = [] } e


simpleEvalToMaybeVal : Exp -> Maybe Val
simpleEvalToMaybeVal e = eval Nothing runUntilTheEnd Syntax.Elm initEnv [] { pbeHolesSeen = [] } e |> Result.toMaybe |> Maybe.map (\((val, _), _) -> val)


eval : Maybe EId -> (Exp -> Bool) -> Syntax -> Env -> Backtrace -> PBEHolesSeenRefCell -> Exp -> Result String ((Val, Widgets), Maybe Env)
eval maybeRetEnvEId abortPred syntax env bt pbeHolesSeenRefCell e =

  let makeProvenance basedOn = Provenance e basedOn in

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
    in
    let priorParents = valParents v in
    let _ = ImpureGoodies.mutateRecordField v.parents "_0" (vParent::priorParents) in
    ()
  in
  let addParent v =
    if FastParser.isProgramEId e.val.eid then
      case v.v_ of
        VConst _ _       -> v
        VBase _          -> v
        VClosure _ _ _ _ -> v
        VList vals       -> let _ = List.map (addParent_ v) vals               in v -- non-mutating: { v | v_ = VList (List.map (addParent_ v) vals) }
        VDict dict       -> let _ = Dict.map (\_ val -> addParent_ v val) dict in v -- non-mutating: { v | v_ = VDict (Dict.map (\_ val -> addParent_ v val) dict) }
    else
      v
  in

  let retEnv deeperRetEnv =
    if Just e.val.eid == maybeRetEnvEId then
      Just env
    else
      deeperRetEnv
  in
  let retEnvHere = retEnv Nothing in

  -- Only use introduceVal, ret, or retBoth for new values (i.e. not var lookups): they do not preserve parents
  let introduceVal basedOn v_               = addParent <| Val v_ (makeProvenance basedOn) (Parents []) in
  let retBoth basedOn (v_, ws) deeperRetEnv = ((introduceVal basedOn v_, ws), retEnv deeperRetEnv) in
  let ret basedOn v_                        = retBoth basedOn (v_, []) Nothing in

  let retV basedOn v                             = ((addParent { v | provenance = makeProvenance basedOn}, []), retEnvHere) in
  let retVBoth basedOn (v, ws) deeperRetEnv      = ((addParent { v | provenance = makeProvenance basedOn}, ws), retEnv deeperRetEnv) in
  -- let retAddWs ws1 (v1, ws2)                     = (v1, ws1 ++ ws2) in
  let addParentToRet ((v,ws),envOut)             = ((addParent v, ws), envOut) in
  let addProvenanceToRet basedOn ((v,ws),envOut) = ((addParent { v | provenance = makeProvenance basedOn}, ws), envOut) in
  let addWidgets ws1 ((v1,ws2),env1)             = ((v1, ws1 ++ ws2), env1) in
  let attachEarlierRetEnv earlierRetEnv ((v1,ws1), env1) = ((v1, ws1), Utils.orMaybe env1 earlierRetEnv) in
  let attachLaterRetEnv laterRetEnv ((v1,ws1), env1) = ((v1, ws1), Utils.orMaybe laterRetEnv env1) in



  let bt_ =
    if e.start.line >= 1
    then e::bt
    else bt
  in

  (\ret ->
    if abortPred e
    then ImpureGoodies.throw (EarlyAbort ret)
    else ret
  ) <|
  (Result.map (\(((v, ws), env) as origRet) ->
    let maybeOp =
      case e.val.e__ of
        EOp _ opWithInfo _ _ -> Just opWithInfo.val
        _                    -> Nothing
    in
    case (ValWidgets.valToMaybeWidget v, FastParser.isProgramEId e.val.eid, maybeOp) of
      (_, _, Just NoWidgets) ->
        origRet -- Unfortunately, wrapping your noWidgets call in parens will cause the widget to be regenerated by the EParens. :(
      -- (Just (WPoint _ _ _ _ _ as newWidget), _, _) -> -- Experiment: showing points from Prelude
      --   ((v, ws |> addSubsumingPriorWidgets newWidget)
      --   , env
      --   )
      (Just newWidget, True, _) ->
        -- let _ =
        --   case newWidget of
        --     WPoint _ _ _ _ _ -> Utils.log <| "New point widget from " ++ toString e.val.e__
        --     _                -> ()
        -- in
        ((v, ws |> addSubsumingPriorWidgets newWidget)
        , env
        )
      _ -> origRet
  )) <|
  case e.val.e__ of

  EConst _ n loc wd ->
    let v_ = VConst Nothing (n, TrLoc loc) in
    let retVal = introduceVal [] v_ in
    case wd.val of
      NoWidgetDecl         -> Ok ((retVal, []), retEnvHere)
      IntSlider a _ b mcap hidden ->
        let widget = WIntSlider a.val b.val (mkCap mcap loc) (floor n) retVal loc hidden in
        Ok ((retVal, [widget]), retEnvHere)
      NumSlider a _ b mcap hidden ->
        let widget = WNumSlider a.val b.val (mkCap mcap loc) n retVal loc hidden in
        Ok ((retVal, [widget]), retEnvHere)

  EBase _ v     -> Ok <| ret [] <| VBase (eBaseToVBase v)
  EVar _ x      -> Result.map (\v -> retV [v] v) <| lookupVar syntax env (e::bt) x e.start
  EFun _ ps e _ -> Ok <| ret [] <| VClosure Nothing ps e env
  EOp _ op es _ -> Result.map (\(res, deeperRetEnv) -> addParentToRet (res, retEnv deeperRetEnv)) <| evalOp maybeRetEnvEId abortPred syntax env e (e::bt) pbeHolesSeenRefCell op es

  EList _ es _ m _ ->
    case Utils.projOk <| List.map (eval maybeRetEnvEId abortPred syntax env bt_ pbeHolesSeenRefCell) (List.map Tuple.second es) of
      Err s -> Err s
      Ok results ->
        let (vws, deeperRetEnvs) = List.unzip results in
        let deeperRetEnv = deeperRetEnvs |> Utils.findLast ((/=) Nothing) |> Maybe.withDefault Nothing in
        let (vs,wss) = List.unzip vws in
        let ws = List.concat wss in
        case (m, vs, True) of -- FastParser.isProgramEId e.val.eid) of -- need to be able to draw offsets off points calculated in prelude but returned to the program
          (Nothing, [v1, v2], True) ->
            case (v1.v_, v2.v_) of
              (VConst _ nt1, VConst _ nt2) ->
                -- Add tracing for drawing offsets.
                -- Cycles, baby. So otherCoordinateOf(X) actual does equal Y (needed at least for displaying offsets at the appropriate times)
                let coordinateInfo1 = (X, nt2, v2) in
                let taggedV1 = {v1 | v_ = VConst (Just coordinateInfo1) nt1} in
                let taggedV2 = {v2 | v_ = VConst (Just (Y, nt1, taggedV1)) nt2} in
                let _ = ImpureGoodies.mutateRecordField coordinateInfo1 "_2" taggedV2 in
                Ok <| retBoth vs (VList [taggedV1, taggedV2], ws) deeperRetEnv
              _ -> Ok <| retBoth vs (VList vs, ws) deeperRetEnv
          (Nothing, _, _)   -> Ok <| retBoth vs (VList vs, ws) deeperRetEnv
          (Just rest, _, _) ->
            case eval maybeRetEnvEId abortPred syntax env bt_ pbeHolesSeenRefCell rest of
              Err s -> Err s
              Ok ((vRest, ws_), deeperRetEnv_) ->
                case vRest.v_ of
                  VList vs_ -> Ok <| retBoth (vs ++ [vRest]) (VList (vs ++ vs_), ws ++ ws_) (Utils.orMaybe deeperRetEnv_ deeperRetEnv)
                  _         -> errorWithBacktrace syntax (e::bt) <| strPos rest.start ++ " rest expression not a list."

  -- Alternatively, could choose not to add a basedOn record for if/case/typecase (simply pass value through, maybe add parent)
  -- But that would suggest that we *might* avoid doing so for EApp as well, which is more dubious. We'll see.
  EIf _ e1 _ e2 _ e3 _ ->
    case eval maybeRetEnvEId abortPred syntax env bt_ pbeHolesSeenRefCell e1 of
      Err s -> Err s
      Ok ((v1,ws1), deeperRetEnv1) ->
        case v1.v_ of
          VBase (VBool True)  -> Result.map (\(((v,_),_) as result) -> attachLaterRetEnv retEnvHere <| attachEarlierRetEnv deeperRetEnv1 <| addProvenanceToRet [v] <| addWidgets ws1 result) <| eval maybeRetEnvEId abortPred syntax env bt_ pbeHolesSeenRefCell e2 -- Provenence basedOn vals control-flow agnostic: do not include scrutinee
          VBase (VBool False) -> Result.map (\(((v,_),_) as result) -> attachLaterRetEnv retEnvHere <| attachEarlierRetEnv deeperRetEnv1 <| addProvenanceToRet [v] <| addWidgets ws1 result) <| eval maybeRetEnvEId abortPred syntax env bt_ pbeHolesSeenRefCell e3 -- Provenence basedOn vals control-flow agnostic: do not include scrutinee
          _                   -> errorWithBacktrace syntax (e::bt) <| strPos e1.start ++ " if-exp expected a Bool but got something else."

  ECase _ e1 bs _ ->
    case eval maybeRetEnvEId abortPred syntax env bt_ pbeHolesSeenRefCell e1 of
      Err s -> Err s
      Ok ((v1,ws1), deeperRetEnv1) ->
        case evalBranches maybeRetEnvEId abortPred syntax env bt_ pbeHolesSeenRefCell v1 bs of
          -- retVBoth and not addProvenanceToRet b/c only lets should return inner env
          Ok (Just ((v2,ws2), deeperRetEnv2)) -> Ok <| retVBoth [v2] (v2, ws1 ++ ws2) (Utils.orMaybe deeperRetEnv2 deeperRetEnv1) -- Provenence basedOn vals control-flow agnostic: do not include scrutinee
          Err s                               -> Err s
          _                                   -> errorWithBacktrace syntax (e::bt) <| strPos e1.start ++ " non-exhaustive case statement"

  ETypeCase _ e1 tbranches _ ->
    case eval maybeRetEnvEId abortPred syntax env bt_ pbeHolesSeenRefCell e1 of
      Err s -> Err s
      Ok ((v1,ws1), deeperRetEnv1) ->
        case evalTBranches maybeRetEnvEId abortPred syntax env bt_ pbeHolesSeenRefCell v1 tbranches of
          -- retVBoth and not addProvenanceToRet b/c only lets should return inner env
          Ok (Just ((v2,ws2), deeperRetEnv2)) -> Ok <| retVBoth [v2] (v2, ws1 ++ ws2) (Utils.orMaybe deeperRetEnv2 deeperRetEnv1) -- Provenence basedOn vals control-flow agnostic: do not include scrutinee
          Err s                               -> Err s
          _                                   -> errorWithBacktrace syntax (e::bt) <| strPos e1.start ++ " non-exhaustive typecase statement"

  EApp _ e1 [] _ _ ->
    errorWithBacktrace syntax (e::bt) <| strPos e1.start ++ " application with no arguments"

  EApp _ e1 es _ _ ->
    case eval maybeRetEnvEId abortPred syntax env bt_ pbeHolesSeenRefCell e1 of
      Err s       -> Err s
      Ok ((v1,ws1), deeperRetEnv1) ->
        case v1.v_ of
          VClosure maybeRecName ps funcBody closureEnv ->
            let argValsAndFuncRes =
              case maybeRecName of
                Nothing    -> apply maybeRetEnvEId abortPred syntax env bt bt_ pbeHolesSeenRefCell e ps es funcBody closureEnv
                Just fName -> apply maybeRetEnvEId abortPred syntax env bt bt_ pbeHolesSeenRefCell e ps es funcBody ((fName, v1)::closureEnv)
            in
            -- Do not record dependence on closure (which function to execute is essentially control flow).
            -- Dependence on function is implicit by being dependent on some value computed by an expression in the function.
            -- Instead, point to return value (and, hence, the final expression of) the function. This will also
            -- transitively point to any arguments used.
            argValsAndFuncRes
            |> Result.map (\(argVals, ((fRetVal, fRetWs), deeperRetEnv2)) -> -- deeperRetEnv2 is from args and body
              let perhapsCallWidget =
                if FastParser.isProgramEId e.val.eid && FastParser.isProgramEId funcBody.val.eid
                then [WCall e.val.eid v1 argVals fRetVal fRetWs]
                else []
              in
              let perhapsPointWidgetsFromPrelude =
                -- If a Prelude function returns a list of points, we do want to display them, but behind any widgets from the args.
                if FastParser.isProgramEId e.val.eid && FastParser.isPreludeEId funcBody.val.eid then
                  fRetVal
                  |> vListToMaybeVals
                  |> Maybe.withDefault []
                  |> List.filterMap ValWidgets.valToMaybeWidget
                  |> List.filter isPointWidget
                else
                  []
              in
              retVBoth [fRetVal] (fRetVal, perhapsPointWidgetsFromPrelude ++ ws1 ++ fRetWs ++ perhapsCallWidget) (Utils.orMaybe deeperRetEnv2 deeperRetEnv1)
            )

          _ ->
            errorWithBacktrace syntax (e::bt) <| strPos e1.start ++ " not a function"

  ELet _ _ False p _ e1 _ e2 _ ->
    case eval maybeRetEnvEId abortPred syntax env bt_ pbeHolesSeenRefCell e1 of
      Err s       -> Err s
      Ok ((v1,ws1), deeperRetEnv1) ->
        case cons (p, v1) (Just env) of
          Just env_ ->
            -- Don't add provenance: fine to say value is just from the let body.
            -- (We consider equations to be mobile).
            eval maybeRetEnvEId abortPred syntax env_ bt_ pbeHolesSeenRefCell e2
            |> Result.map (addWidgets ws1 >> attachEarlierRetEnv deeperRetEnv1 >> attachLaterRetEnv retEnvHere)

          Nothing   ->
            errorWithBacktrace syntax (e::bt) <| strPos e.start ++ " could not match pattern " ++ (Syntax.patternUnparser syntax >> Utils.squish) p ++ " with " ++ strVal v1


  ELet _ _ True p _ e1 _ e2 _ ->
    case eval maybeRetEnvEId abortPred syntax env bt_ pbeHolesSeenRefCell e1 of
      Err s       -> Err s
      Ok ((v1,ws1), deeperRetEnv1) ->
        case ((patEffectivePat p).val.p__, v1.v_) of
          (PVar _ fname _, VClosure Nothing x body env_) ->
            let _   = Utils.assert "eval letrec" (env == env_) in
            let v1Named = { v1 | v_ = VClosure (Just fname) x body env } in
            case cons (pVar fname, v1Named) (Just env) of
              -- Don't add provenance: fine to say value is just from the let body.
              -- (We consider equations to be mobile).
              Just env_ ->
                eval maybeRetEnvEId abortPred syntax env_ bt_ pbeHolesSeenRefCell e2
                |> Result.map (addWidgets ws1 >> attachEarlierRetEnv deeperRetEnv1 >> attachLaterRetEnv retEnvHere)

              _ ->
                errorWithBacktrace syntax (e::bt) <| strPos e.start ++ "bad ELet"
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
    -- case t1.val of
    --   -- using (e : Point) as a "point widget annotation"
    --   TNamed _ a ->
    --     if String.trim a /= "Point" then eval abortPred syntax env bt_ pbeHolesSeenRefCell e1
    --     else
    --       eval abortPred syntax env bt_ pbeHolesSeenRefCell e1 |> Result.map (\(((v,ws),env_) as result) ->
    --         case v.v_ of
    --           VList [v1, v2] ->
    --             case (v1.v_, v2.v_) of
    --               (VConst _ nt1, VConst _ nt2) ->
    --                 let vNew = {v | v_ = VList [{v1 | v_ = VConst (Just (X, nt2, v2)) nt1}, {v2 | v_ = VConst (Just (Y, nt1, v1)) nt2}]} in
    --                 ((vNew, ws), env_)
    --                 -- ((vNew, ws ++ [WPoint nt1 v1 nt2 v2 v]), env_)
    --               _ ->
    --                 result
    --           _ ->
    --             result
    --         )
    --   _ ->
    eval maybeRetEnvEId abortPred syntax env bt_ pbeHolesSeenRefCell e1 |> Result.map (attachEarlierRetEnv retEnvHere)

  EComment _ _ e1       -> eval maybeRetEnvEId abortPred syntax env bt_ pbeHolesSeenRefCell e1 |> Result.map (attachEarlierRetEnv retEnvHere)
  EOption _ _ _ _ e1    -> eval maybeRetEnvEId abortPred syntax env bt_ pbeHolesSeenRefCell e1 |> Result.map (attachEarlierRetEnv retEnvHere)
  ETyp _ _ _ e1 _       -> eval maybeRetEnvEId abortPred syntax env bt_ pbeHolesSeenRefCell e1 |> Result.map (attachEarlierRetEnv retEnvHere)
  ETypeAlias _ _ _ e1 _ -> eval maybeRetEnvEId abortPred syntax env bt_ pbeHolesSeenRefCell e1 |> Result.map (attachEarlierRetEnv retEnvHere)
  EParens _ e1 _ _      -> eval maybeRetEnvEId abortPred syntax env bt_ pbeHolesSeenRefCell e1 |> Result.map (attachEarlierRetEnv retEnvHere)

  EHole _ (HoleNamed "terminationCondition") ->
    let parentIf = List.head bt |> Maybe.withDefault (eHoleNamed " * Nothing * ") in
    if Utils.count ((==) parentIf) bt >= 2 -- Recurse once.
    then Ok <| ret [] <| VBase (VBool True)
    else Ok <| ret [] <| VBase (VBool False)

  EHole _ (HoleVal val)        -> Ok <| retV [val] val -- I would think we should just return return the held val as is (i.e. retV [val] val) but that approach seems to sometimes cause infinite loop problems during widget deduping in postProcessWidgets below. Currently we are only evaluating expressions with holes during mouse drags while drawing new shapes AND there are snaps for that new shape. UPDATE: the infinite loop problem should be fixed, should be okay to use `retV [val] val`, changed when needed.
  EHole _ (HoleLoc locId)      -> errorWithBacktrace syntax (e::bt) <| strPos e.start ++ " loc hole " ++ toString locId ++ "!"
  EHole _ HoleEmpty            -> errorWithBacktrace syntax (e::bt) <| strPos e.start ++ " empty hole!"
  EHole _ (HolePredicate _)    -> errorWithBacktrace syntax (e::bt) <| strPos e.start ++ " predicate hole!"
  EHole _ (HoleNamed name)     -> errorWithBacktrace syntax (e::bt) <| strPos e.start ++ " empty hole " ++ name ++ "!" -- PBE sketch filling relys on this error format
  EHole _ (HolePBE examples _) ->
    let seenCount = Utils.count (\(seenHoleExp,_,_) -> seenHoleExp == e) pbeHolesSeenRefCell.pbeHolesSeen in
    case Utils.maybeGeti1 (seenCount + 1) examples of
      Just (_,_,_,e1) ->
        let
          -- Wacky mess in case hole recurses. Unlikely.
          pbeHoleSeenBeforeEval = (e, env, Err "example not evaluated yet")
          _ = ImpureGoodies.mutateRecordField pbeHolesSeenRefCell "pbeHolesSeen" (pbeHolesSeenRefCell.pbeHolesSeen ++ [pbeHoleSeenBeforeEval])
        in
        let holesSeenIToReplace = List.length pbeHolesSeenRefCell.pbeHolesSeen in
        let evaledResult = eval maybeRetEnvEId abortPred syntax env bt_ pbeHolesSeenRefCell e1 in
        -- Now we have the result of example, so replace the record.
        let
          pbeHoleSeenAfterEval = (e, env, evaledResult |> Result.map (\((v,_),_)-> v))
          _ = ImpureGoodies.mutateRecordField pbeHolesSeenRefCell "pbeHolesSeen" (Utils.replacei holesSeenIToReplace pbeHoleSeenAfterEval pbeHolesSeenRefCell.pbeHolesSeen)
        in
        Result.map (\(((v,_),_) as result) -> attachLaterRetEnv retEnvHere <| addProvenanceToRet [v] result) <| evaledResult

      Nothing ->
        errorWithBacktrace syntax (e::bt) <| strPos e.start ++ " encountered PBE hole too many times—no more examples to use for evaluation!"


evalOp : Maybe EId -> (Exp -> Bool) -> Syntax -> Env -> Exp -> Backtrace -> PBEHolesSeenRefCell -> Op -> List Exp -> Result String ((Val, Widgets), Maybe Env)
evalOp maybeRetEnvEId abortPred syntax env e bt pbeHolesSeenRefCell opWithInfo es =
  let (op,opStart) = (opWithInfo.val, opWithInfo.start) in
  let argsEvaledRes = List.map (eval maybeRetEnvEId abortPred syntax env bt pbeHolesSeenRefCell) es |> Utils.projOk in
  case argsEvaledRes of
    Err s -> Err s
    Ok argsEvaled ->
      let (vws, deeperRetEnvs) = List.unzip argsEvaled in
      let retEnv = deeperRetEnvs |> Utils.findLast ((/=) Nothing) |> Maybe.withDefault Nothing in
      let (vs,wss) = List.unzip vws in
      let error () =
        errorWithBacktrace syntax bt
          <| "Bad arguments to " ++ strOp op ++ " operator " ++ strPos opStart
          ++ ":\n" ++ Utils.lines (Utils.zip vs es |> List.map (\(v,e) -> (strVal v) ++ " from " ++ (Syntax.unparser syntax e)))
      in
      let addProvenance val_   = Val val_ (Provenance e vs) (Parents []) in
      let addProvenanceOk val_ = Ok (addProvenance val_) in
      let nullaryOp args retVal_ =
        case args of
          [] -> addProvenanceOk retVal_
          _  -> error ()
      in
      let unaryMathOp op args =
        case args of
          [VConst _ (n,t)] -> VConst Nothing (evalDelta syntax bt pbeHolesSeenRefCell op [n], TrOp op [t]) |> addProvenanceOk
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
            VConst maybeAxisAndOtherDim (evalDelta syntax bt pbeHolesSeenRefCell op [i,j], TrOp op [it,jt]) |> addProvenanceOk
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
          Eq        -> case args of
            [VConst _ (i,it), VConst _ (j,jt)]       -> VBase (VBool (i == j)) |> addProvenanceOk
            [VBase (VString s1), VBase (VString s2)] -> VBase (VBool (s1 == s2)) |> addProvenanceOk
            [_, _]                                   -> VBase (VBool False) |> addProvenanceOk -- polymorphic inequality, added for Prelude.addExtras
            _                                        -> error ()
          Pi         -> nullaryOp args (VConst Nothing (pi, TrOp op []))
          DictEmpty  -> nullaryOp args (VDict Dict.empty)
          DictInsert -> case vs of
            [vkey, val, {v_}] -> case v_ of
              VDict d -> valToDictKey syntax bt vkey.v_ |> Result.map (\dkey -> VDict (Dict.insert dkey val d) |> addProvenance)
              _       -> error()
            _                 -> error ()
          DictGet    -> case args of
            [key, VDict d] -> valToDictKey syntax bt key |> Result.map (\dkey -> Utils.getWithDefault dkey (VBase VNull |> addProvenance) d)
            _              -> error ()
          DictRemove -> case args of
            [key, VDict d] -> valToDictKey syntax bt key |> Result.map (\dkey -> VDict (Dict.remove dkey d) |> addProvenance)
            _              -> error ()
          Cos        -> unaryMathOp op args
          Sin        -> unaryMathOp op args
          ArcCos     -> unaryMathOp op args
          ArcSin     -> unaryMathOp op args
          Abs        -> unaryMathOp op args
          Floor      -> unaryMathOp op args
          Ceil       -> unaryMathOp op args
          Round      -> unaryMathOp op args
          Sqrt       -> unaryMathOp op args
          Ln         -> unaryMathOp op args
          Explode    -> case args of
            [VBase (VString s)] ->
              String.toList s
              |> List.map String.fromChar
              |> Utils.mapi0
                  (\(i, charStr) ->
                    { v_ = VBase (VString charStr)
                    , provenance = Provenance (eCall "nth" [e, eConstDummyLoc (toFloat i)]) vs
                    , parents = Parents []
                    }
                  )
              |> VList
              |> addProvenanceOk
            _                   -> error ()
          DebugLog   -> case vs of
            [v] -> let _ = Debug.log (strVal v) "" in Ok v
            _   -> error ()
          NoWidgets  -> case vs of
            [v] -> Ok v -- Widgets removed  below.
            _   -> error ()
          ToStr      -> case vs of
            [val] -> VBase (VString (strVal val)) |> addProvenanceOk
            _     -> error ()
          OptNumToString -> case vs of
            [val] -> case val.v_ of
              VConst _ (num, _) -> (VBase <| VString <| toString num) |> addProvenanceOk
              VBase (VString v) as r -> r |> addProvenanceOk
              _     -> error ()
            _     -> error ()
      in
      case newValRes of
        Err s     -> Err s
        Ok newVal ->
          let newWidgets =
            case (FastParser.isProgramEId e.val.eid, op, args, vs) of
              (True, Plus, [VConst (Just (axis, otherDimNumTr, otherDirVal)) numTr, VConst Nothing amountNumTr], [_, amountVal]) ->
                let (baseXNumTr, baseYNumTr, endXVal, endYVal) =
                  if axis == X
                  then (numTr, otherDimNumTr, newVal, otherDirVal)
                  else (otherDimNumTr, numTr, otherDirVal, newVal)
                in
                [WOffset1D baseXNumTr baseYNumTr axis Positive amountNumTr amountVal endXVal endYVal]
              (True, Plus, [VConst Nothing amountNumTr, VConst (Just (axis, otherDimNumTr, otherDirVal)) numTr], [amountVal, _]) ->
                let (baseXNumTr, baseYNumTr, endXVal, endYVal) =
                  if axis == X
                  then (numTr, otherDimNumTr, newVal, otherDirVal)
                  else (otherDimNumTr, numTr, otherDirVal, newVal)
                in
                [WOffset1D baseXNumTr baseYNumTr axis Positive amountNumTr amountVal endXVal endYVal]
              (True, Minus, [VConst (Just (axis, otherDimNumTr, otherDirVal)) numTr, VConst Nothing amountNumTr], [_, amountVal]) ->
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
          Ok ((newVal, widgets), retEnv)


-- Returns Ok Nothing if no branch matches
-- Returns Ok (Just results) if branch matches and no execution errors
-- Returns Err s if execution error
evalBranches maybeRetEnvEId abortPred syntax env bt pbeHolesSeenRefCell v bs =
  List.foldl (\(Branch_ _ pat exp _) acc ->
    case (acc, cons (pat,v) (Just env)) of
      (Ok (Just done), _)     -> acc
      (Ok Nothing, Just env_) -> eval maybeRetEnvEId abortPred syntax env_ bt pbeHolesSeenRefCell exp |> Result.map Just
      (Err s, _)              -> acc
      _                       -> Ok Nothing

  ) (Ok Nothing) (List.map .val bs)


-- Returns Ok Nothing if no branch matches
-- Returns Ok (Just results) if branch matches and no execution errors
-- Returns Err s if execution error
evalTBranches maybeRetEnvEId abortPred syntax env bt pbeHolesSeenRefCell val tbranches =
  List.foldl (\(TBranch_ _ tipe exp _) acc ->
    case acc of
      Ok (Just done) ->
        acc

      Ok Nothing ->
        if Types.valIsType val tipe then
          eval maybeRetEnvEId abortPred syntax env bt pbeHolesSeenRefCell exp |> Result.map Just
        else
          acc

      Err s ->
        acc
  ) (Ok Nothing) (List.map .val tbranches)


evalDelta syntax bt pbeHolesSeenRefCell op is =
  case Lang.maybeEvalMathOp op is of
    Just result -> result
    Nothing     -> crashWithBacktrace syntax bt <| "Little evaluator bug: Eval.evalDelta " ++ strOp op


recursionLimit : Int
recursionLimit = 100


-- Using this recursive function rather than desugaring to single
-- applications: cleaner provenance (one record for entire app).
--
-- Returns: Result String (argVals, (functionResult, widgets))
apply maybeRetEnvEId abortPred syntax env bt bt_ pbeHolesSeenRefCell e psLeft esLeft funcBody closureEnv =
  if Utils.count (.val >> .eid >> (==) e.val.eid) bt > recursionLimit then
    -- Testing for too much recursion here instead of in eval for two reasons:
    -- (1) Naive backtrace length is not linear with amount of memory needed: valToSameVals crashes somewhere, probably because deeply recursive functions that return the same value as a pass-through value cause the widget subsumer to follow the provenance back up the return stack
    -- (2) More efficient to test here than in eval
    errorWithBacktrace syntax (e::bt) <| strPos e.start ++ " Too much recursion!!" -- Better than crashing the browser.
  else

  let recurse = apply maybeRetEnvEId abortPred syntax env bt bt_ pbeHolesSeenRefCell e in
  case (psLeft, esLeft) of
    ([], []) ->
      eval maybeRetEnvEId abortPred syntax closureEnv bt_ pbeHolesSeenRefCell funcBody |> Result.map (\valAndWsAndRetEnv -> ([], valAndWsAndRetEnv))

    ([], esLeft) ->
      eval maybeRetEnvEId abortPred syntax closureEnv bt_ pbeHolesSeenRefCell funcBody
      |> Result.andThen
          (\((fRetVal1, fRetWs1), deeperRetEnv1) ->
            case fRetVal1.v_ of
              VClosure maybeRecName ps funcBody closureEnv ->
                case maybeRecName of
                  Nothing    -> recurse ps esLeft funcBody closureEnv                      |> Result.map (\(argVals, ((v2, ws2), deeperRetEnv2)) -> (argVals, ((v2, fRetWs1 ++ ws2), Utils.orMaybe deeperRetEnv2 deeperRetEnv1)))
                  Just fName -> recurse ps esLeft funcBody ((fName, fRetVal1)::closureEnv) |> Result.map (\(argVals, ((v2, ws2), deeperRetEnv2)) -> (argVals, ((v2, fRetWs1 ++ ws2), Utils.orMaybe deeperRetEnv2 deeperRetEnv1)))
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
      Ok ([], ((finalVal, []), Nothing))

    (p::psLeft, e::esLeft) ->
      case eval maybeRetEnvEId abortPred syntax env bt_ pbeHolesSeenRefCell e of
        Err s -> Err s
        Ok ((argVal, argWs), deeperRetEnv1) ->
          case cons (p, argVal) (Just closureEnv) of
            Just closureEnv -> recurse psLeft esLeft funcBody closureEnv |> Result.map (\(laterArgs, ((v2, ws2), deeperRetEnv2)) -> (argVal::laterArgs, ((v2, argWs ++ ws2), Utils.orMaybe deeperRetEnv2 deeperRetEnv1)))
            Nothing         -> errorWithBacktrace syntax (e::bt) <| strPos e.start ++ " bad arguments to function"


eBaseToVBase eBaseVal =
  case eBaseVal of
    EBool b     -> VBool b
    EString _ b -> VString b
    ENull       -> VNull


valToDictKey : Syntax -> Backtrace -> Val_ -> Result String (String, String)
valToDictKey syntax bt val_ =
  case val_ of
    VConst _ (n, tr)  -> Ok <| (toString n, "num")
    VBase (VBool b)   -> Ok <| (toString b, "bool")
    VBase (VString s) -> Ok <| (toString s, "string")
    VBase VNull       -> Ok <| ("", "null")
    VList vals        ->
      vals
      |> List.map ((valToDictKey syntax bt) << .v_)
      |> Utils.projOk
      |> Result.map (\keyStrings -> (toString keyStrings, "list"))
    _                 -> errorWithBacktrace syntax bt <| "Cannot use " ++ (strVal { v_ = val_, provenance = dummyProvenance, parents = Parents [] }) ++ " in a key to a dictionary."


-- Multiple expresssions in the program can produce essentially identical overlaping widgets, but each
-- referring to a different expression.
--
-- We only want to keep the last such widget produced.
--
-- This function will add the given widget to the end of this list,
-- removing prior identical widgets produced by previous expressions.
addSubsumingPriorWidgets : Widget -> List Widget -> List Widget
addSubsumingPriorWidgets widget widgets =
  case ValWidgets.widgetToMaybeVal widget of
    Just v ->
      let
        priorValsToRemove = Provenance.valToSameVals v
        wsSamePointSubsumed =
          widgets
          |> List.filter
              (\priorWidget ->
                case ValWidgets.widgetToMaybeVal priorWidget of
                  Just widgetVal -> not <| ValWidgets.isSameWidgetType widget priorWidget && List.member widgetVal priorValsToRemove
                  _              -> True
              )
      in
      wsSamePointSubsumed ++ [widget]

    _ ->
      widgets ++ [widget]


-- Need to revisit this to do deeper dedup.
postProcessWidgets : Exp -> List Widget -> List Widget
postProcessWidgets program widgets =
  let
    dedupedWidgets = Utils.dedup widgets

    -- partition so that hidden and point sliders don't affect indexing
    -- (and, thus, positioning) of range sliders
    (rangeWidgets, pointWidgets) =
      dedupedWidgets |>
        List.partition (\widget ->
          case widget of
            WIntSlider _ _ _ _ _ _ False -> True
            WNumSlider _ _ _ _ _ _ False -> True
            WIntSlider _ _ _ _ _ _ True  -> False
            WNumSlider _ _ _ _ _ _ True  -> False
            WPoint _ _ _ _ _             -> False
            WOffset1D _ _ _ _ _ _ _ _    -> False
            WCall _ _ _ _ _              -> False
            WList _                      -> False
        )

    rangeWidgetsBetterDescriptions =
      rangeWidgets
      |> List.map
          (\widget ->
            case widget of
              WIntSlider low high caption curVal valVal loc isHidden -> WIntSlider low high (LangTools.expNameForEIdWithDefault caption program (valExp valVal).val.eid) curVal valVal loc isHidden
              WNumSlider low high caption curVal valVal loc isHidden -> WNumSlider low high (LangTools.expNameForEIdWithDefault caption program (valExp valVal).val.eid) curVal valVal loc isHidden
              _                                                      -> widget
          )
  in
  rangeWidgetsBetterDescriptions ++ pointWidgets

parseAndRun : String -> String
parseAndRun = strVal << Tuple.first << Utils.fromOk_ << run Syntax.Little << Utils.fromOkay "parseAndRun" << parseE

parseAndRun_ = strVal_ True << Tuple.first << Utils.fromOk_ << run Syntax.Little << Utils.fromOkay "parseAndRun_" << parseE

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

