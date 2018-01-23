module Update exposing  (..)

import Tuple exposing (first)
import Lang exposing (..)
import LangUnparser exposing (unparse)
import Eval exposing (doEval)
import Utils
import Syntax
import Results exposing (Results, ok1, oks, errs)
import MissingNumberMethods exposing (..)

-- Make sure that Env |- Exp evaluates to oldVal
update : Env -> Exp -> Val -> Val -> Results String (Env, Exp)
update env e oldVal newVal =
  --let _ = Debug.log (String.concat [envToString env, unparse e] ++ " <-- " ++ (valToString newVal)) 1 in
  case e.val.e__ of
    EConst ws num loc widget -> ok1 <| (env, replaceE__ e <| EConst ws (getNum newVal) loc widget)
    EBase ws m -> ok1 <| (env, val_to_exp ws newVal)
    EVar ws is ->
      (case env of
        []            -> errs <| "No " ++ is ++ " found. \nVariables in scope: " ++ (String.join " " <| List.map Tuple.first env)
        (k0,v0) :: l_ -> if is == k0
                           then ok1 ((is, newVal) :: l_, e)
                           else
                             Results.map (Tuple.mapFirst (\newEnv -> (k0, v0) :: newEnv)) <| update l_ e oldVal newVal)
    EList ws elems ws2 Nothing ws3 ->
      case (oldVal.v_, newVal.v_) of
        (VList origVals, VList newOutVals) ->
          if List.length origVals == List.length newOutVals then
            updateList env elems origVals newOutVals
            |> Results.map (\(env, l) ->
              (env, replaceE__ e <| EList ws l ws2 Nothing ws3)
            )
          else errs <| "Cannot (yet) update a list " ++ unparse e ++ " with list of different length: " ++ valToString newVal
        _ -> errs <| "Cannot update a list " ++ unparse e ++ " with non-list " ++ valToString newVal

    --EList ws elems ws2 Nothing ws3 ->
    --  errs ""
    EFun ws0 ps e ws1 ->
      -- oldVal ==  VClosure Nothing p e env
      (case newVal.v_ of
        VClosure Nothing newPs newE newEnv -> ok1 (newEnv, replaceE__ e <| EFun ws0 newPs newE ws1)
        _ -> errs <| "Expected non-recursive closure, got " ++ toString newVal
      )

    EApp ws0 e1 e2s ws1 ->
      case doEval Syntax.Elm env e1 of
      Err s       -> errs s
      Ok ((v1, _),_) ->
        case List.map (doEval Syntax.Elm env) e2s |> Utils.projOk of
          Err s       -> errs s
          Ok v2ls ->
            let v2s = List.map (\((v2, _), _) -> v2) v2ls in
            case v1.v_ of
              VClosure Nothing ps eBody env_ as vClosure ->
                case conssWithInversion (ps, v2s) (Just (env_, \newEnv_ newPs newBody -> replaceV_ v1 <| VClosure Nothing newPs newBody newEnv_)) of
                  Just (env__, consBuilder) ->
                     -- consBuilder: Env -> ((Pat, Val), (newPat: Pat) -> (newBody: Exp) -> VClosure)
                      update env__ eBody oldVal newVal
                      |> Results.map (\(newEnv, newBody) ->
                           (consBuilder newEnv, newBody)
                         )
                      |> Results.map (\(((newPats, newArgs), patsBodyToClosure), newBody) ->
                        let newClosure = patsBodyToClosure newPats newBody in
                        let e1_updated = update env e1 v1 newClosure in
                        let e2s_updated = updateList env e2s v2s newArgs in
                        Results.map2 (\((envE1, newE1), (envE2, newE2s)) ->
                          (triCombine env envE1 envE2, replaceE__ e <| EApp ws0 newE1 newE2s ws1)
                        ) e1_updated e2s_updated
                      )
                      |> Results.flatten
                  _          -> errs <| strPos e1.start ++ "bad environment"
              VClosure (Just f) ps eBody env_ ->
                case consWithInversion (pVar f, v1) (conssWithInversion (ps, v2s) (Just (env_, \newEnv_ newPs newBody -> replaceV_ v1 <| VClosure (Just f) newPs newBody newEnv_))) of
                  Just (env__, consBuilder) ->
                     -- consBuilder: Env -> ((Pat, Val), ((Pat, Val), (newPat: Pat) -> (newBody: Exp) -> VClosure))
                      update env__ eBody oldVal newVal
                      |> Results.map (\(newEnv, newBody) ->
                           (consBuilder newEnv, newBody)
                         )
                      |> Results.map (\(((newPatFun, newArgFun), ((newPats, newArgs), patsBodyToClosure)), newBody) ->
                        let newClosure = if newArgFun /= v1 then -- Just propagate the change to the closure itself
                            newArgFun
                          else -- Regular replacement
                            patsBodyToClosure newPats newBody in

                        let e1_updated = update env e1 v1 newClosure in
                        let e2s_updated = updateList env e2s v2s newArgs in
                        Results.map2 (\((envE1, newE1), (envE2, newE2s)) ->
                          (triCombine env envE1 envE2, replaceE__ e <| EApp ws0 newE1 newE2s ws1)
                        ) e1_updated e2s_updated
                      )
                      |> Results.flatten
                  _          -> errs <| strPos e1.start ++ "bad environment"
              _ ->
                errs <| strPos e1.start ++ " not a function"

    EIf ws0 cond thn els ws1 ->
      case doEval Syntax.Elm env cond of
        Ok (({ v_ }, _), _) ->
          case v_ of
            VBase (VBool b) ->
              if b then
                update env thn oldVal newVal
                |> Results.map (\(env, newThn) ->
                  (env, replaceE__ e <| EIf ws0 cond newThn els ws1)
                )
              else
                update env els oldVal newVal
                |> Results.map (\(env, newEls) ->
                  (env, replaceE__ e <| EIf ws0 cond thn newEls ws1)
                )
            _ -> errs <| "Expected boolean condition, got " ++ toString v_
        Err s -> errs s

    EParens ws0 eInside ws1 ->
      update env eInside oldVal newVal
      |> Results.map (\(env, eReturn) -> (env, replaceE__ e <| EParens ws0 eReturn ws1))

    EOp sp1 op opArgs sp2 ->
      case (op.val, opArgs) of
        (NoWidgets, [arg]) -> update env arg oldVal newVal |> Results.map (\(newEnv, newArg) -> (newEnv, replaceE__ e <| EOp sp1 op [newArg] sp2))
        _ ->
          case Utils.projOk <| List.map (doEval Syntax.Elm env) opArgs of
            Err msg -> errs msg
            Ok argsEvaled ->
              let ((vs, wss), envs) = Tuple.mapFirst List.unzip <| List.unzip argsEvaled in
              let args = List.map .v_ vs in
              let rebuildOp newOpArgs =
                newOpArgs
                |> Results.andThen (\newArgs ->
                     updateList env opArgs vs newArgs
                   )
                |> Results.map (\(newEnv, newOpArgs) ->
                  (newEnv, replaceE__ e <| EOp sp1 op newOpArgs sp2)) in
              case op.val of
                Plus    -> case args of
                  [VBase (VString s1), VBase (VString s2)] -> errs "String concat not implemented" --VBase (VString (s1 ++ s2)) |> addProvenanceOk
                  _                                        -> maybeUpdateMathOp op vs oldVal newVal |> rebuildOp
                Minus     -> maybeUpdateMathOp op vs oldVal newVal |> rebuildOp
                Mult      -> maybeUpdateMathOp op vs oldVal newVal |> rebuildOp
                Div       -> maybeUpdateMathOp op vs oldVal newVal |> rebuildOp
                Mod       -> maybeUpdateMathOp op vs oldVal newVal |> rebuildOp
                Pow       -> maybeUpdateMathOp op vs oldVal newVal |> rebuildOp
                ArcTan2   -> maybeUpdateMathOp op vs oldVal newVal |> rebuildOp
                Pi         -> maybeUpdateMathOp op vs oldVal newVal |> rebuildOp
                {--Lt        -> case args of
                  [VConst _ (i,it), VConst _ (j,jt)] -> VBase (VBool (i < j)) |> addProvenanceOk
                  _                                  -> error ()
                Eq        -> case args of
                  [VConst _ (i,it), VConst _ (j,jt)]       -> VBase (VBool (i == j)) |> addProvenanceOk
                  [VBase (VString s1), VBase (VString s2)] -> VBase (VBool (s1 == s2)) |> addProvenanceOk
                  [_, _]                                   -> VBase (VBool False) |> addProvenanceOk -- polymorphic inequality, added for Prelude.addExtras
                  _                                        -> error ()
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
                --}
                Cos        -> maybeUpdateMathOp op vs oldVal newVal |> rebuildOp
                Sin        -> maybeUpdateMathOp op vs oldVal newVal |> rebuildOp
                ArcCos     -> maybeUpdateMathOp op vs oldVal newVal |> rebuildOp
                ArcSin     -> maybeUpdateMathOp op vs oldVal newVal |> rebuildOp
                Floor      -> maybeUpdateMathOp op vs oldVal newVal |> rebuildOp
                Ceil       -> maybeUpdateMathOp op vs oldVal newVal |> rebuildOp
                Round      -> maybeUpdateMathOp op vs oldVal newVal |> rebuildOp
                Sqrt       -> maybeUpdateMathOp op vs oldVal newVal |> rebuildOp
                Explode    -> Debug.crash "Not implemented: update Explode "
                DebugLog   -> Debug.crash "Not implemented: update DebugLog "
                ToStr      ->
                  case newVal.v_ of
                    VBase (VString s) ->
                      case Syntax.parser Syntax.Elm s of
                        Err msg -> errs <| "Could not parse new output value '"++s++"' for ToStr expression " ++ toString msg
                        Ok parsed ->
                          case doEval Syntax.Elm [] parsed of
                            Err msg -> errs msg
                            Ok ((v, _), _) ->
                              case (opArgs, vs) of
                                ([opArg], [arg]) -> update env opArg arg v
                                  |> Results.map (\(env, newOpArg) -> (env, replaceE__ e <| EOp sp1 op [newOpArg] sp2))
                                e -> errs <| "[internal error] Wrong number of arguments in update: " ++ toString e
                    e -> errs <| "Expected string, got " ++ toString e
                _ -> errs <| "Not yet supported operation update " ++ envToString env ++ "|-" ++ unparse e ++ " <-- " ++ valToString newVal ++ " (was " ++ valToString oldVal ++ ")"
    _ -> errs <| "Non-supported update " ++ envToString env ++ "|-" ++ unparse e ++ " <-- " ++ valToString newVal ++ " (was " ++ valToString oldVal ++ ")"


-- Compares the new reference with and original (which are at distance at most 2PI) and reports the relative smallest change to n
angleUpdate: Float -> Float -> Float -> Results String (List Num)
angleUpdate new old n =
   let increment = new - old in
  --Two directions are possible, we take the closest first modulo 2 PI
   if increment <= pi && increment >= -pi then
     ok1 [n + increment]
   else if increment > pi then
     ok1 [n + increment - 2*pi]
   else
     ok1 [n + increment + 2*pi]

maybeUpdateMathOp : Op -> List Val -> Val -> Val -> Results String (List Val)
maybeUpdateMathOp op operandVals oldOutVal newOutVal =
  case (oldOutVal.v_, newOutVal.v_) of
    (VConst _ (oldOut, _), VConst _ (newOut, _)) ->
      if oldOut == newOut then ok1 operandVals else
      let operands = operandVals
        |> List.map (\operand ->
            case operand.v_ of
              VConst _ (v, _) -> Just v
              _ -> Nothing
            )
        |> Utils.projJusts in
      case operands of
        Nothing -> errs <| "Operands do not form a list of numbers: " ++ toString operandVals
        Just operands ->
          let result = case (op.val, operands) of
                (Plus,    [l,r]) -> oks [[newOut - r, r], [l, newOut - l]]
                (Minus,   [l,r]) -> oks [[r + newOut, r], [l, l - newOut]]
                (Mult,    [l,r]) -> oks [[l, newOut / l], [newOut / r, r]]
                (Div,     [l,r]) -> if newOut /= 0 then oks [[r * newOut, r], [l, l / newOut]]
                                    else ok1 [r * newOut, r]
                (Pow,     [l,r]) -> if l < 0 && r >= 0 && floor r == ceiling r then --Powers of negative must be with integer numbers
                                      if (floor r) % 2 == 0 then -- The result should be positive
                                        if newOut >= 0 then ok1 [0 - newOut ** (1 / r), r]
                                        else if newOut == -1 && l == -1 then
                                          if r > 0 then oks [[l, r - 1], [l, r + 1]]
                                          else ok1 [l, r + 1]
                                        else  errs "No way to invert l^r <-- out where l < 0, r is even, out < 0 and out /= -1"
                                      else -- r is odd, so we preserve the negative sign.
                                        if newOut >= 0 then ok1 [newOut ** (1/r), r] else ok1 [0 - ( 0 - newOut) ** (1/r), r]
                                    else if l >= 0 then
                                      if newOut > 0 then oks [[newOut ** (1 / r), r], [l, logBase l newOut]]
                                      else if floor (1 / r) == ceiling (1/r) && 1/r < 0 then oks [[0 - (-newOut) ** (1 / r), r]]
                                      else errs "No way to invert l^r <-- out where l >= 0, out < 0 and 1/r not an integer or not < 0"
                                    else errs "No way to invert l^r <-- out where l < 0 and r < 0 or r is not an integer"
                (Mod,     [l,r]) -> ok1 [l + newOut - oldOut, r]
                (ArcTan2, [l,r]) -> -- We keep the same radius but change the angle
                                    let (radius, theta) = toPolar (r, l) in
                                    let (newR, newL) = fromPolar (radius, theta + newOut - oldOut) in
                                    ok1 [newL, newR]
                (Cos,     [n])   -> let newOutClamped = clamp -1 1 newOut in
                                    let moved = acos newOutClamped in -- value between 0 and PI
                                    -- We stay on the same quadrant
                                    let movedAbsolute  = -- value between 0 and 2*PI
                                      if ((n %% (2 * pi) + 3 * pi) %% (2 * pi) - pi >= 0) then moved
                                      else 2*pi - moved in
                                    let original = (n %% (2 * pi) + 2 * pi) %% (2 * pi) in
                                    angleUpdate movedAbsolute original n

                (Sin,     [n])   -> let newOutClamped = clamp -1 1 newOut in
                                    let moved = asin newOutClamped in -- value between -pi / 2 and pi / 2
                                    -- But n might be beyond [-pi/2, pi/2] !
                                    let movedAbsolute = -- value between -PI and PI
                                      if ((n %% (2 * pi) + 2 * pi + pi / 2) %% (2 * pi) - pi <= 0) then moved
                                      else if moved > 0 then pi - moved
                                      else -pi - moved in
                                    let original = (n %% (2 * pi) + 3 * pi) %% (2 * pi) - pi in
                                    angleUpdate movedAbsolute original n
                (ArcCos,  [n])   -> ok1 [cos newOut]
                (ArcSin,  [n])   -> ok1 [sin newOut]
                (Floor,   [n])   -> ok1 [n + newOut - oldOut]
                (Ceil,    [n])   -> ok1 [n + newOut - oldOut]
                (Round,   [n])   -> ok1 [n + newOut - oldOut]
                (Sqrt,    [n])   -> ok1 [newOut * newOut]
                (Pi,      [])    -> if newOut == pi then ok1 [] else errs <| "Pi's value is 3.14159... and cannot be changed"
                _                -> errs <| "Not the correct number of arguments for " ++ toString op ++ "(" ++ toString operandVals ++ ")"
          in
          Results.map (
            List.map2 (\original newNumber ->
              case original.v_ of
                VConst m0 (oldNumber, m1) -> replaceV_ original <| VConst m0 (newNumber, m1)
                x -> Debug.crash <| "[internal error] Did not get a VConst: " ++ toString x
            ) operandVals
          ) result
    _ -> errs <| "Do not know how to revert computation " ++ toString op ++ "(" ++ toString operandVals ++ ") <-- " ++ toString newOutVal

updateList: Env -> List Exp -> List Val -> List Val -> Results String (Env, List Exp)
updateList env elems origVals newOutVals =
  let results =
    List.map3 (\inputExpr oldOut newOut ->
              update env inputExpr oldOut newOut
              ) elems origVals newOutVals in
  List.foldl (\elem acc ->
    Results.map2withError (\(x, y) -> x ++ "\n" ++ y) (\((newEnvElem, newExpElem), (envAcc, lAcc)) ->
      (triCombine env envAcc newEnvElem, lAcc ++ [newExpElem])) elem acc
    ) (ok1 (env, [])) results

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


conssWithInversion : (List Pat, List Val) -> Maybe (Env, Env -> a) -> Maybe (Env, Env -> ((List Pat, List Val), a))
conssWithInversion pvs menv =
  case (menv, matchListWithInversion pvs) of
    (Just (env, envToA), Just (env_, envToPatsVals)) -> Just (env_ ++ env,
      \newEnv ->
        let (newEnv_, newEnvTail) = Utils.split (List.length env_) newEnv in
        (envToPatsVals newEnv_, envToA newEnvTail)
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
    (if List.length ps == List.length vs then Just (ps,vs) else Nothing)
    |> Maybe.andThen matchListWithInversion
    |> Maybe.map (\(env, envRenewer) ->
      (env, envRenewer >> \(newPats, newVals) ->
        (replaceP__ p <| PList ws0 newPats ws1 Nothing ws2, replaceV_ v <| VList newVals)
      )
    )
  (PList ws0 ps ws1 (Just rest) ws2, VList vs) ->
    let (n,m) = (List.length ps, List.length vs) in
    if n > m then Nothing
    else
      let (vs1,vs2) = Utils.split n vs in
      (ps, vs1)
      |> matchListWithInversion
      |> consWithInversion (rest, replaceV_ v <| VList vs2) -- Maybe (Env, Env -> ((Pat, Val), (List Pat, List Val)))
      |> Maybe.map (\(env, envRenewer) ->
        (env, envRenewer >> (\((newPat, newVal), (newPats, newVals)) ->
          case newVal.v_ of
            VList otherVals ->
              (replaceP__ p <| PList ws0 newPats ws1 (Just newPat) ws2, replaceV_ v <| (VList <| newVals ++ otherVals))
            _ -> Debug.crash <| "RHS of list pattern is not a list: " ++ toString newVal
        ))
      )
        -- dummy VTrace, since VList itself doesn't matter
  (PList _ _ _ _ _, _) -> Nothing
  (PConst _ n, VConst _ (n_,_)) -> if n == n_ then Just ([], \newEnv -> (p, v)) else Nothing
  (PBase _ bv, VBase bv_) -> if eBaseToVBase bv == bv_ then Just ([], \newEnv -> (p, v)) else Nothing
  _ -> Debug.crash <| "Little evaluator bug: Eval.match " ++ (toString p.val.p__) ++ " vs " ++ (toString v.v_)

matchListWithInversion : (List Pat, List Val) -> Maybe (Env, Env -> (List Pat, List Val))
matchListWithInversion (ps, vs) =
  List.foldl (\pv acc -> --: Maybe (Env, List (Env -> (Pat, Val, Env)))
    case (acc, matchWithInversion pv) of
      (Just (old, oldEnvBuilders), Just (new, newEnvBuilder)) -> Just (new ++ old,
           [\newEnv ->
            let (headNewEnv, tailNewEnv) = Utils.split (List.length new) newEnv in
            let (newPat, newVal) = newEnvBuilder headNewEnv in
            (newPat, newVal, tailNewEnv)
          ] ++ oldEnvBuilders
        )
      _                    -> Nothing
  ) (Just ([], [])) (Utils.zip ps vs)
  |> Maybe.map (\(finalEnv, envBuilders) -> -- envBuilders: List (Env -> (Pat, Val, Env)), but we want Env -> (Pat, Val), combining pattern/values into lists
    (finalEnv, \newEnv ->
      let (newPats, newVals, _) =
        List.foldl (\eToPVE (pats, vals, env)->
          let (p, v, e) = eToPVE env in
          ([p] ++ pats, [v] ++ vals, e)
          )  ([], [], newEnv) envBuilders in
      (newPats, newVals)
    ))
  {--|> (\x -> case x of
     Just (env, envBuilder) ->
       let _ = Debug.log ("matchListWithinversion" ++ toString pvs) (envBuilder env) in
       x
     Nothing -> Nothing
  )--}

val_to_exp: WS -> Val -> Exp
val_to_exp ws v =
  withDummyExpInfo <| case v.v_ of
    VConst mb num     -> EConst ws (first num) dummyLoc noWidgetDecl
    VBase (VBool b)   -> EBase  ws <| EBool b
    VBase (VString s) -> EBase  ws <| EString defaultQuoteChar s
    VBase (VNull)     -> EBase  ws <| ENull
    VList vals -> EList ws (List.map (val_to_exp ws) vals) ws Nothing <| ws
    VClosure Nothing patterns body env -> EFun ws patterns body ws -- Not sure about this one.
    _ -> Debug.crash <| "Trying to get an exp of the value " ++ toString v
    --VDict vs ->

getNum: Val -> Num
getNum v =
  case v.v_ of
    VConst _ (num, _) -> num
    _ -> Debug.crash <| "Espected VConst, got " ++ toString v


eBaseToVBase eBaseVal =
  case eBaseVal of
    EBool b     -> VBool b
    EString _ b -> VString b
    ENull       -> VNull

envToString: Env -> String
envToString env =
  case env of
    [] -> ""
    (v, value)::tail -> v ++ "->" ++ unparse (val_to_exp (ws "") value) ++ " " ++ (envToString tail)

valToString: Val -> String
valToString v = case v.v_ of
   VClosure Nothing patterns body env -> envToString env ++ "|-" ++ ((unparse << val_to_exp (ws " ")) v)
   _ -> (unparse << val_to_exp (ws " ")) v
