module Update exposing  (..)

import Tuple exposing (first)
import Lang exposing (..)
import Eval exposing (doEval)
import Utils

-- Make sure that Env |- Exp evaluates to oldVal
update : Env -> Exp__ -> Val -> Val -> Result String (Env, Exp__)
update env e oldVal newVal =
  case e of
    EConst ws num loc widget -> Ok <| (env, val_to_exp ws newVal)
    EBase ws m -> Ok <| (env, val_to_exp ws newVal)
    EVar ws is ->
      (case env of
        []            -> Err <| "No " ++ is ++ " found. \nVariables in scope: " ++ (String.join " " <| List.map Tuple.first env)
        (k0,v0) :: l_ -> if is == k0
                           then Ok ((is, newVal) :: l_, e)
                           else
                             Result.map (Tuple.mapFirst (\newEnv -> (k0, v0) :: newEnv)) <| update l_ e oldVal newVal)
    EFun ws0 [p] e ws1 ->
      -- oldVal ==  VClosure Nothing p e env
      (case newVal.v_ of
        VClosure Nothing newP newE newEnv -> Ok (newEnv, EFun ws0 [newP] newE ws1)
        _ -> Err <| "Expected non-recursive closure, got " ++ toString newVal
      )
    EFun ws0 ps e ws1 ->
      update env (desugarEFun ps e).val.e__ oldVal newVal
      |> Result.map (\(newEnv, newExp) ->
        case newExp of
          EFun _ [p] newBody _ -> (newEnv, EFun ws0 ps newBody ws1)
          _ -> Debug.crash <| "Failed to recover a Fun, got " ++ toString newExp
        )

    EApp ws0 e1 [e2] ws1 ->
      case doEval env e1 of
      Err s       -> Err s
      Ok ((v1, _),_) ->
        case doEval env e2 of
          Err s       -> Err s
          Ok ((v2, _),_) ->
            case v1.v_ of
              VClosure Nothing p eBody env_ ->
                case consWithInversion (p, v2) (Just (env_, \newEnv_ -> \newP -> \newBody -> val <| VClosure Nothing p eBody newEnv_)) of
                  Just (env__, consBuilder) ->
                     -- consBuilder: Env -> ((Pat, Val), (newPat: Pat) -> (newBody: Exp) -> VClosure)
                      update env__ eBody.val.e__ oldVal newVal
                      |> Result.map (\(newEnv, newBody) ->
                           (consBuilder newEnv, newBody)
                         )
                      |> Result.map (\(((newPat, newArg), patBodyToClosure), newBody) ->
                        let newClosure = patBodyToClosure newPat newBody in
                        let e1_updated = update env e1.val.e__ v1 newClosure in
                        let e2_updated = update env e2.val.e__ v2 newArg in
                        Result.map2 (\(envE1, newE1) (envE2, newE2) ->
                          (triCombine env envE1 envE2, EApp ws0 newE1 [newE2] ws1)
                        ) (e1_updated |> Result.map (Tuple.mapSecond (replaceE__ e1))) (e2_updated |> Result.map (Tuple.mapSecond (replaceE__ e1)))
                      )
                      |> Utils.unwrapNestedResult
                  _          -> Err <| strPos e1.start ++ "bad environment"
              VClosure (Just f) p eBody env_ ->
                case consWithInversion (pVar f, v1) (consWithInversion (p, v2) (Just (env_, \newEnv_ -> \newP -> \newBody -> val <| VClosure (Just f) p eBody newEnv_))) of
                  Just (env__, consBuilder) ->
                     -- consBuilder: Env -> ((Pat, Val), ((Pat, Val), (newPat: Pat) -> (newBody: Exp) -> VClosure))
                      update env__ eBody.val.e__ oldVal newVal
                      |> Result.map (\(newEnv, newBody) ->
                           (consBuilder newEnv, newBody)
                         )
                      |> Result.map (\(((newPatFun, newArgFun), ((newPat, newArg), patBodyToClosure)), newBody) ->
                        let newClosure = if newArgFun /= v1 then -- Just propagate the change to the closure itself
                            newArgFun
                          else -- Regular replacement
                            patBodyToClosure newPat newBody in

                        let e1_updated = update env e1.val.e__ v1 newClosure in
                        let e2_updated = update env e2.val.e__ v2 newArg in
                        Result.map2 (\(envE1, newE1) (envE2, newE2) ->
                          (triCombine env envE1 envE2, EApp ws0 newE1 [newE2] ws1)
                        ) (e1_updated |> Result.map (Tuple.mapSecond (replaceE__ e1))) (e2_updated |> Result.map (Tuple.mapSecond (replaceE__ e1)))
                      )
                      |> Utils.unwrapNestedResult
                  _          -> Err <| strPos e1.start ++ "bad environment"
              _ ->
                Err <| strPos e1.start ++ " not a function"

    _ -> Debug.crash <| "Non-supported update " ++ toString (env, e, oldVal, newVal)



triCombine: Env -> Env -> Env -> Env
triCombine originalEnv newEnv1 newEnv2 =
  let aux acc originalEnv newEnv1 newEnv2 =
    case (originalEnv, newEnv1, newEnv2) of
      ([], [], []) -> acc
      ((x, v1)::oe, (y, v2)::ne1, (z, v3)::ne2) ->
        if x /= y || y /= z || x /= z then
          Debug.crash <| "Expected environments to have the same variables, got\n" ++
           toString x ++ " = " ++ toString v1 ++ "\n" ++
           toString y ++ " = " ++ toString v2 ++ "\n" ++
           toString z ++ " = " ++ toString v3
        else
          if v2 == v1 then aux (acc ++ [(x, v3)]) oe ne1 ne2
          else aux (acc ++ [(x, v2)]) oe ne1 ne2
      _ -> Debug.crash <| "Expected environments to have the same size, got\n" ++
           toString originalEnv ++ ", " ++ toString newEnv1 ++ ", " ++ toString newEnv2
    in
  aux [] originalEnv newEnv1 newEnv2

consWithInversion : (Pat, Val) -> Maybe (Env, Env -> a) -> Maybe (Env, Env -> ((Pat, Val), a))
consWithInversion pv menv =
  case (menv, matchWithInversion pv) of
    (Just (env, envToA), Just (env_, envToPatVal)) -> Just (env_ ++ env,
      \newEnv ->
        let (newEnv_, newEnvTail) = Utils.split (List.length env_) newEnv in
        (envToPatVal newEnv_, envToA newEnvTail)
      )
    _                     -> Nothing

matchWithInversion : (Pat, Val) -> Maybe (Env, Env -> (Pat, Val))
matchWithInversion (p,v) = case (p.val.p__, v.v_) of
  (PVar ws x wd, _) -> Just ([(x,v)], \newEnv ->
     case newEnv of
       [(x, newV)] -> (p, newV)
       _ -> Debug.crash <| "Not the same shape before/after pattern update: " ++ toString newEnv ++ " should have length 1"
     )
  (PAs ws0 x ws1 innerPat, _) ->
    matchWithInversion (innerPat, v) |> Maybe.map
      (\(env, envReverse) -> ((x,v)::env, \newEnv ->
        case newEnv of
          (_, newV)::newEnv2 ->
            if newV == v then
              case envReverse newEnv2 of
              (newInnerPat, newVal) -> (replaceP__ p <| PAs ws0 x ws1 newInnerPat, newVal)
            else
              case envReverse newEnv2 of
              (newInnerPat, _)      -> (replaceP__ p <| PAs ws0 x ws1 newInnerPat, newV)

          _ -> Debug.crash <| "Not the same shape before/after pattern update: " ++ toString newEnv ++ " should have length >= 1"
      ))

  (PList ws0 ps ws1 Nothing ws2, VList vs) ->
    (Utils.maybeZip ps vs)
    |> Maybe.andThen matchListWithInversion
    |> Maybe.map (\(env, envRenewer) ->
      (env, envRenewer >> \(newPats, newVals) ->
        (replaceP__ p <| PList ws0 newPats ws1 Nothing ws2, val <| VList newVals)
      )
    )
  (PList ws0 ps ws1 (Just rest) ws2, VList vs) ->
    let (n,m) = (List.length ps, List.length vs) in
    if n > m then Nothing
    else
      let (vs1,vs2) = Utils.split n vs in
      Utils.zip ps vs1
      |> matchListWithInversion
      |> consWithInversion (rest, vList vs2) -- Maybe (Env, Env -> ((Pat, Val), (List Pat, List Val)))
      |> Maybe.map (\(env, envRenewer) ->
        (env, envRenewer >> (\((newPat, newVal), (newPats, newVals)) ->
          case newVal.v_ of
            VList otherVals ->
              (replaceP__ p <| PList ws0 newPats ws1 (Just newPat) ws2, val <| (VList <| newVals ++ otherVals))
            _ -> Debug.crash <| "RHS of list pattern is not a list: " ++ toString newVal
        ))
      )
        -- dummy VTrace, since VList itself doesn't matter
  (PList _ _ _ _ _, _) -> Nothing
  (PConst _ n, VConst _ (n_,_)) -> if n == n_ then Just ([], \newEnv -> (p, v)) else Nothing
  (PBase _ bv, VBase bv_) -> if (eBaseToVBase bv) == bv_ then Just ([], \newEnv -> (p, v)) else Nothing
  _ -> Debug.crash <| "Little evaluator bug: Eval.match " ++ (toString p.val.p__) ++ " vs " ++ (toString v.v_)

matchListWithInversion : List (Pat, Val) -> Maybe (Env, Env -> (List Pat, List Val))
matchListWithInversion pvs =
  List.foldl (\pv acc -> --: Maybe (Env, List (Env -> (Pat, Val, Env)))
    case (acc, matchWithInversion pv) of
      (Just (old, oldEnvBuilders), Just (new, newEnvBuilder)) -> Just (new ++ old,
          oldEnvBuilders ++ [\newEnv ->
            let (headNewEnv, tailNewEnv) = Utils.split (List.length new) newEnv in
            let (newPat, newVal) = newEnvBuilder headNewEnv in
            (newPat, newVal, tailNewEnv)
          ]
        )
      _                    -> Nothing
  ) (Just ([], [])) pvs
  |> Maybe.map (\(finalEnv, res) -> -- res: List (Env -> (Pat, Val, Env)), but we want Env -> (Pat, Val), combining pattern/values into lists
    (finalEnv, \newEnv ->
      let (newPats, newVals, _) =
        List.foldl (\eToPVE (pats, vals, env)->
          let (p, v, e) = eToPVE env in
          (pats ++ [p], vals ++ [v], e)
          )  ([], [], newEnv) res in
      (newPats, newVals)
    ))

val_to_exp: WS -> Val -> Exp__
val_to_exp ws v =
  case v.v_ of
    VConst mb num     -> EConst ws (first num) (0, unann, "") (withDummyInfo NoWidgetDecl)
    VBase (VBool b)   -> EBase  ws <| EBool b
    VBase (VString s) -> EBase  ws <| EString defaultQuoteChar s
    VBase (VNull)     -> EBase  ws <| ENull
    --VClosure Nothing pattern body env -> EFun ws (List pattern) body -- Not sure about this one.
    VList vals -> EList ws (List.map (val_to_exp (withDummyInfo "") >> withDummyExpInfo) vals) (withDummyInfo "") Nothing <| withDummyInfo ""
    _ -> Debug.crash <| "Trying to get an exp of the value " ++ toString v
    --VDict vs ->

eBaseToVBase eBaseVal =
  case eBaseVal of
    EBool b     -> VBool b
    EString _ b -> VString b
    ENull       -> VNull