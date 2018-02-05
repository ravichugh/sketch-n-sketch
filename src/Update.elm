module Update exposing  (..)

import Tuple exposing (first)
import Lang exposing (..)
import Eval exposing (doEval)
import Utils
import Syntax
import Results exposing
  ( Results(..)
  , ok1, oks, okLazy
  , LazyList(..)
  , appendLazy, appendLazyLazy
  , lazyFromList
  , lazyCons2)
import MissingNumberMethods exposing (..)
import ValUnparser exposing (strVal)
import Lazy

unparse = Syntax.unparser Syntax.Elm

type NextAction = HandlePreviousResult ((Env, Exp) -> UpdateStack)
                | Fork Env Exp Val Output (LazyList NextAction)

type UpdateStack = UpdateResult      Env Exp
                 | UpdateIdem        Env Exp Val Output
                 | UpdateContinue    Env Exp Val Output NextAction
                 | UpdateRestart     Env Exp Val Output (LazyList NextAction)
                 | UpdateAlternative Env Exp Val Env Exp Val Output (LazyList NextAction)
                 | UpdateError String
                 -- The new expression, the new output, what to add to the stack with the result of the update above.



type Output = Raw Val | Program Exp
type alias Input = Exp


fact: Int -> (Int -> String) -> String
fact n callback =
  if n == 1 then
    callback 1
  else
    fact (n - 1) (\newN -> Debug.log "" <| "The previous step was "  ++ toString n ++ ", the last step is " ++ toString newN)

updateValue: Env -> Exp -> Output -> (Env, Exp)
updateValue env e out =
  let _ = Debug.log "UpdateValue" () in
  case out of
    Program prog ->
      let _ = Debug.log "Prog" (unparse prog) in
      (env, prog)
    Raw newVal ->
      let _ = Debug.log "Raw" (valToString newVal) in
      case e.val.e__ of
        EHole ws _ -> (env, val_to_exp ws newVal)
        EConst ws num loc widget
          -> (env, replaceE__ e <| EConst ws (getNum newVal) loc widget)
        EBase ws m
          -> (env, val_to_exp ws newVal)
        EFun sp0 ps e sp1 ->
          case newVal.v_ of
            VClosure Nothing newPs newE newEnv -> (newEnv, replaceE__ e <| EFun sp0 newPs newE sp1)
            _ -> Debug.crash "Trying to update a function with non-closure " <| valToString newVal
        _ -> (env, val_to_exp (ws "") newVal) -- TODO: Is this correct?

updateEnv: Env -> Ident -> Val -> Env
updateEnv env k value =
  case env of
    [] -> Debug.crash <| k ++ " not found in environment "
    ((kk, vv) as kv)::tail ->
      if kk == k then (kk, value)::tail else kv::updateEnv tail k value


-- Make sure that Env |- Exp evaluates to oldVal
update : Env -> Exp -> Val -> Output -> LazyList NextAction -> Results String (Env, Exp)
update env e oldVal out nextToUpdate =
  let _ = Debug.log (String.concat ["update: ", envToString env, "|-", unparse e] ++ " <-- " ++ (case out of
    Program p -> unparse p
    Raw v -> valToString v)) () in
  let isBaseVal = case e.val.e__ of
    EConst _ _ _ _ -> True
    EBase _ _ -> True
    EHole _ _ -> True
    _ -> False
  in
  if isBaseVal then
      case nextToUpdate of
        LazyNil -> ok1 <| updateValue env e out
        LazyCons head lazyTail ->
          case head of
            Fork newEnv newExp newOldVal newOut nextToUpdate2 ->
              let _ = Debug.log "ok lazy" "" in
              okLazy (updateValue env e out) (\() ->
                updateRec newEnv newExp newOldVal newOut (appendLazyLazy nextToUpdate2 lazyTail)
              )
            HandlePreviousResult f ->
              let _ = Debug.log "Continue previous" "" in
              case f (updateValue env e out) of
                UpdateError msg ->
                  Errs msg
                UpdateResult fEnv fOut ->
                  update fEnv e oldVal (Program fOut) (Lazy.force lazyTail)
                UpdateIdem fEnv newE newOldVal newOut ->
                  update fEnv newE newOldVal newOut (Lazy.force lazyTail)
                UpdateContinue fEnv newE newOldVal newOut g ->
                  update fEnv newE newOldVal newOut (LazyCons g lazyTail)
                UpdateRestart fEnv newE newOldVal newOut newNextToUpdate ->
                  update fEnv newE newOldVal newOut newNextToUpdate
                UpdateAlternative fEnv fOut fOldVal altEnv altE altOldVal altOut nextToUpdate2 ->
                  update fEnv e fOldVal (Program fOut) (appendLazy (Lazy.force lazyTail) (lazyFromList [Fork altEnv altE altOldVal altOut nextToUpdate2]))
--    EBase ws m -> --ok1 <| (env, val_to_exp ws newVal)
  else
    let updateStack = getUpdateStack env e oldVal out nextToUpdate in
    case updateStack of
      UpdateError msg ->
        Errs msg
      UpdateResult fEnv fOut ->
        update fEnv e oldVal (Program fOut) nextToUpdate
      UpdateIdem fEnv newE newOldVal newOutput ->
        update fEnv newE newOldVal newOutput nextToUpdate
      UpdateContinue fEnv newE newOldVal newOut g ->
        update fEnv newE newOldVal newOut (lazyCons2 g nextToUpdate)
      UpdateRestart fEnv newE newOldVal newOut newNextToUpdate ->
        update fEnv newE newOldVal newOut newNextToUpdate
      UpdateAlternative fEnv fOut fOldVal altEnv altE altOldVal altOut nextToUpdate2 ->
        update fEnv e fOldVal (Program fOut) (appendLazy nextToUpdate (lazyFromList [Fork altEnv altE altOldVal altOut nextToUpdate2]))

getUpdateStack : Env -> Exp -> Val -> Output -> LazyList NextAction -> UpdateStack
getUpdateStack env e oldVal out nextToUpdate =
  case e.val.e__ of
      EVar sp is ->
        case out of
          Program e -> Debug.crash <| "Don't know how to update an environment with a program: " ++ unparse e
          Raw newVal ->
            let newEnv = updateEnv env is newVal in
            --update newEnv (replaceE__ e <| EHole (ws "") Nothing) oldVal (Program e) nextToUpdate
            UpdateIdem newEnv (replaceE__ e <| EHole (ws "") Nothing) oldVal <| Program e

      EList sp1 elems sp2 Nothing sp3 ->
        case out of
          Program prog -> Debug.crash <| "??? Don't know how to update a list with a program " ++ unparse prog
          Raw newVal ->
            case (oldVal.v_, newVal.v_) of
              (VList origVals, VList newOutVals) ->
                if List.length origVals == List.length newOutVals then
                  case (elems, origVals, newOutVals) of
                    ([], [], []) ->
    --                  update env (replaceE__ e <| EHole (ws "") Nothing) oldVal (Program e) nextToUpdate
                      UpdateIdem env (replaceE__ e <| EHole (ws "") Nothing) oldVal <| Program e
                    (expHead::expTail, origHead::origTail, newHead::newTail) ->
                      UpdateContinue env expHead origHead (Raw newHead)
                        <| HandlePreviousResult <| \(newHeadEnv, newHeadExp) ->
                          UpdateContinue env (replaceE__ e <| EList sp1 expTail sp2 Nothing sp3) (replaceV_ oldVal (VList origTail))
                                (Raw (replaceV_ newVal (VList newTail)))
                            <| HandlePreviousResult <| \(newTailEnv, newTailExp) ->
                              case newTailExp.val.e__ of
                                EList _ newTail _ _ _ ->
                                  let newEnv = triCombine env newHeadEnv newTailEnv in
                                  UpdateResult newEnv <| replaceE__ e <| EList sp1 (newHeadExp :: newTail) sp2 Nothing sp3
                                _ -> Debug.crash "Internal error: Should get a list back"
                    _ -> Debug.crash "The list's length were checked, what happened??"
                else UpdateError <| "Cannot (yet) update a list " ++ unparse e ++ " with list of different length: " ++ valToString newVal
              _ -> UpdateError <| "Cannot update a list " ++ unparse e ++ " with non-list " ++ valToString newVal

      --EList ws elems sp2 (Just tail) ws3 ->
      --  UpdateError ""

      EApp sp0 e1 e2s sp1 ->
        case doEval Syntax.Elm env e1 of
        Err s       -> UpdateError s
        Ok ((v1, _),_) ->
          case List.map (doEval Syntax.Elm env) e2s |> Utils.projOk of
            Err s       -> UpdateError s
            Ok v2ls ->
              let v2s = List.map (\((v2, _), _) -> v2) v2ls in
              case v1.v_ of
                VClosure Nothing ps eBody env_ as vClosure ->
                  case conssWithInversion (ps, v2s) (Just (env_, \newEnv_ newPs newBody -> replaceV_ v1 <| VClosure Nothing newPs newBody newEnv_)) of
                    Just (env__, consBuilder) ->
                       -- consBuilder: Env -> ((Pat, Val), (newPat: Pat) -> (newBody: Exp) -> VClosure)
                        UpdateContinue env__ eBody oldVal out <| HandlePreviousResult <| \(newEnv, newBody) ->
                          let ((newPats, newArgs), patsBodyToClosure) = consBuilder newEnv in
                          let newClosure = patsBodyToClosure newPats newBody in
                          UpdateContinue env e1 v1 (Raw newClosure) <| HandlePreviousResult <| \(newE1Env, newE1) ->
                            UpdateContinue env (replaceE__ e <| EList sp0 e2s sp1 Nothing sp1) (replaceV_ oldVal <| VList v2s) (Raw (replaceV_ oldVal <| VList newArgs)) <|
                              HandlePreviousResult <| \(newE2Env, newE2List) ->
                                case newE2List.val.e__ of
                                  EList _ newE2s _ _ _ ->
                                    let finalEnv = triCombine env newE1Env newE2Env in
                                    UpdateResult finalEnv <| replaceE__ e <| EApp sp0 newE1 newE2s sp1
                                  x -> Debug.crash <| "Internal error, should have get a list, got " ++ toString x
                    _          -> UpdateError <| strPos e1.start ++ "bad environment"
                VClosure (Just f) ps eBody env_ ->
                  case consWithInversion (pVar f, v1) (conssWithInversion (ps, v2s) (Just (env_, \newEnv_ newPs newBody -> replaceV_ v1 <| VClosure (Just f) newPs newBody newEnv_))) of
                    Just (env__, consBuilder) ->
                       -- consBuilder: Env -> ((Pat, Val), ((Pat, Val), (newPat: Pat) -> (newBody: Exp) -> VClosure))
                        UpdateContinue env__ eBody oldVal out <| HandlePreviousResult <| \(newEnv, newBody) ->
                          let ((newPatFun, newArgFun), ((newPats, newArgs), patsBodytoClosure)) = consBuilder newEnv in
                          let newClosure =
                            if newArgFun /= v1 then newArgFun -- Just propagate the change to the closure itself
                            else patsBodytoClosure newPats newBody -- Regular replacement
                          in
                          UpdateContinue env e1 v1 (Raw newClosure) <| HandlePreviousResult <| \(newE1Env, newE1) ->
                            UpdateContinue env (replaceE__ e <| EList sp0 e2s sp1 Nothing sp1) (replaceV_ oldVal <| VList v2s) (Raw <| replaceV_ oldVal <| VList newArgs) <|
                              HandlePreviousResult <| \(newE2Env, newE2List) ->
                                case newE2List.val.e__ of
                                  EList _ newE2s _ _ _ ->
                                    let finalEnv = triCombine env newE1Env newE2Env in
                                    UpdateResult finalEnv <| replaceE__ e <| EApp sp0 newE1 newE2s sp1
                                  x -> Debug.crash <| "Unexpected result of updating a list " ++ toString x
                    _          -> UpdateError <| strPos e1.start ++ "bad environment"
                _ ->
                  UpdateError <| strPos e1.start ++ " not a function"

      EIf sp0 cond thn els sp1 ->
        case doEval Syntax.Elm env cond of
          Ok ((v, _), _) ->
            case v.v_ of
              VBase (VBool b) ->
                if b then
                  UpdateContinue env thn oldVal out <| HandlePreviousResult <| \(newEnv, newThn) ->
                    UpdateResult newEnv <| replaceE__ e <| EIf sp0 cond newThn els sp1
                else
                  UpdateContinue env els oldVal out <| HandlePreviousResult <| \(newEnv, newEls) ->
                    UpdateResult newEnv <| replaceE__ e <| EIf sp0 cond thn newEls sp1
              _ -> UpdateError <| "Expected boolean condition, got " ++ valToString v
          Err s -> UpdateError s

      EOp sp1 op opArgs sp2 ->
        case (op.val, opArgs) of
          (NoWidgets, [arg]) ->
            UpdateContinue env arg oldVal out <| HandlePreviousResult <| \(newEnv, newArg) ->
              UpdateResult newEnv <| replaceE__ e <| EOp sp1 op [newArg] sp2
          _ ->
            case Utils.projOk <| List.map (doEval Syntax.Elm env) opArgs of
              Err msg -> UpdateError msg
              Ok argsEvaled ->
                let ((vs, wss), envs) = Tuple.mapFirst List.unzip <| List.unzip argsEvaled in
                let args = List.map .v_ vs in
                let argList = replaceE__ e <| EList (ws "") opArgs (ws "") Nothing (ws "") in
                let handleRemainingResults lazyTail nextToUpdate =
                  lazyCons2 (HandlePreviousResult <| \(newOpEnv, newOpArgList) ->
                    case newOpArgList.val.e__ of
                      EList _ newOpArgs _ _ _ ->
                        let finalExp = replaceE__ e <| EOp sp1 op newOpArgs sp2 in
                        case Lazy.force lazyTail of
                          LazyNil -> UpdateResult newOpEnv finalExp
                          LazyCons newHead newTail -> UpdateAlternative newOpEnv finalExp oldVal
                            env argList (replaceV_ oldVal <| VList vs) (Raw <| replaceV_ oldVal <| VList newHead) (handleRemainingResults newTail nextToUpdate)
                      x -> Debug.crash <| "Unexpected return of updating a list: " ++ toString x
                    ) nextToUpdate
                in
                case op.val of
                  Explode    -> Debug.crash "Not implemented: update Explode "
                  DebugLog   -> Debug.crash "Not implemented: update DebugLog "
                  ToStr      ->
                    case out of
                      Raw v ->
                        case v.v_ of
                          VBase (VString s) ->
                            case Syntax.parser Syntax.Elm s of
                              Err msg -> UpdateError <| "Could not parse new output value '"++s++"' for ToStr expression " ++ toString msg
                              Ok parsed ->
                                case doEval Syntax.Elm [] parsed of
                                  Err msg -> UpdateError msg
                                  Ok ((v, _), _) ->
                                    case (opArgs, vs) of
                                      ([opArg], [arg]) -> UpdateContinue env opArg arg (Raw v) <|
                                          HandlePreviousResult <| \(env, newOpArg) ->
                                            UpdateResult env <| replaceE__ e <| EOp sp1 op [newOpArg] sp2
                                      e -> UpdateError <| "[internal error] Wrong number of arguments in update: " ++ toString e
                          e -> UpdateError <| "Expected string, got " ++ toString e
                      Program exp -> UpdateError <| "Don't know how to update a ToStr with a program"
                  _ ->
                    case out of
                      Raw newVal ->
                        case maybeUpdateMathOp op vs oldVal newVal of
                          Errs msg -> UpdateError msg
                          Oks LazyNil -> UpdateError "[Internal error] No result for updating."
                          Oks (LazyCons head lazyTail) ->
                            UpdateRestart env argList (replaceV_ oldVal <| VList vs)
                              (Raw <| replaceV_ oldVal <| VList head) <|
                              handleRemainingResults lazyTail nextToUpdate
                      Program exp ->
                        UpdateError <| "Don't know how to update an operation with a program"

      ECase sp1 input branches sp2 ->
        case doEval Syntax.Elm env input of
          Err msg -> UpdateError msg
          Ok ((inputVal, _), _) ->
            case branchWithInversion env inputVal branches of
              Nothing -> UpdateError <| "Match error: " ++ valToString inputVal ++ " on branches " ++ Syntax.unparser Syntax.Elm e
              Just ((branchEnv, branchExp), envValBranchBuilder) ->
                UpdateContinue branchEnv branchExp oldVal out <| HandlePreviousResult <| \(upEnv, upExp) ->
                  let (newBranchEnv, newInputVal, nBranches) = envValBranchBuilder (upEnv, upExp) in
                  UpdateContinue env input inputVal (Raw newInputVal) <| HandlePreviousResult <| \(newInputEnv, newInputExp) ->
                    let _ = Debug.log "triCombine" <| toString (envToString env, envToString newInputEnv, envToString newBranchEnv) in
                    let finalEnv = triCombine env newInputEnv newBranchEnv in
                    let finalExp = replaceE__ e <| ECase sp1 newInputExp nBranches sp2 in
                    let _ = Debug.log "Returning UpdateResult" () in
                    UpdateResult finalEnv finalExp
      --  ETypeCase WS Exp (List TBranch) WS
      ELet sp1 letKind False p e1 body sp2 ->
          case doEval Syntax.Elm env e1 of
            Err s       -> UpdateError s
            Ok ((oldE1Val,_), _) ->
              case consWithInversion (p, oldE1Val) (Just (env, (\newEnv -> newEnv))) of
                 Just (envWithE1, consBuilder) ->
                   UpdateContinue envWithE1 body oldVal out <| HandlePreviousResult <| \(newEnvWithE1, newBody) ->
                     case consBuilder newEnvWithE1 of
                      ((newPat, newE1Val), newEnvFromBody) ->
                        UpdateContinue env e1 oldE1Val (Raw newE1Val) <| HandlePreviousResult <| \(newEnvFromE1, newE1) ->
                          let finalEnv = triCombine env newEnvFromE1 newEnvFromBody in
                          let finalExp = replaceE__ e <| ELet sp1 letKind False newPat newE1 newBody sp2 in
                          UpdateResult finalEnv finalExp
                 Nothing ->
                   UpdateError <| strPos e.start ++ " could not match pattern " ++ (Syntax.patternUnparser Syntax.Elm >> Utils.squish) p ++ " with " ++ strVal oldE1Val
      ELet sp1 letKind True p e1 body sp2 ->
          case doEval Syntax.Elm env e1 of
            Err s       -> UpdateError s
            Ok ((oldE1Val,_), _) ->
              case (p.val.p__, oldE1Val.v_) of
                (PVar _ fname _, VClosure Nothing x closureBody env_) ->
                  --let _   = Utils.assert "eval letrec" (env == env_) in
                  let oldE1ValNamed = { oldE1Val | v_ = VClosure (Just fname) x closureBody env } in
                  case consWithInversion (p, oldE1ValNamed) (Just (env, (\newEnv -> newEnv))) of
                     Just (envWithE1, consBuilder) ->
                       UpdateContinue envWithE1 body oldVal out <| HandlePreviousResult <| \(newEnvWithE1, newBody) ->
                         case consBuilder newEnvWithE1 of
                           ((newPat, newE1ValNamed), newEnvFromBody) ->
                             let newE1Val = case newE1ValNamed.v_ of
                               VClosure (Just _) x vBody newEnv -> { newE1ValNamed | v_ = VClosure Nothing x vBody newEnv }
                               _ -> Debug.crash "[internal error] This should have been a recursive method"
                             in
                             UpdateContinue env e1 oldE1Val (Raw newE1Val) <| HandlePreviousResult <| \(newEnvFromE1, newE1) ->
                               let finalEnv = triCombine env newEnvFromE1 newEnvFromBody in
                               let finalExp = replaceE__ e <| ELet sp1 letKind True newPat newE1 newBody sp2 in
                               UpdateResult finalEnv finalExp
                     Nothing ->
                       UpdateError <| strPos e.start ++ " could not match pattern " ++ (Syntax.patternUnparser Syntax.Elm >> Utils.squish) p ++ " with " ++ strVal oldE1Val
                (PList _ _ _ _ _, _) ->
                    UpdateError <| strPos e1.start ++
                      """mutually recursive functions (i.e. letrec [...] [...] e) \
                         not yet implemented""" --"
                       -- Implementation also requires modifications to LangSimplify.simply
                       -- so that clean up doesn't prune the funtions.
                _ ->
                  UpdateError <| strPos e.start ++ " bad letrec"

      EComment sp msg exp ->
        UpdateContinue env exp oldVal out <| HandlePreviousResult <| \(nv, ne) -> UpdateResult nv <| replaceE__ e <| EComment sp msg ne
      EOption a b c d exp ->
        UpdateContinue env exp oldVal out <| HandlePreviousResult <| \(nv, ne) -> UpdateResult nv <| replaceE__ e <| EOption a b c d ne
      ETyp a b c exp d    ->
        UpdateContinue env exp oldVal out <| HandlePreviousResult <| \(nv, ne) -> UpdateResult nv <| replaceE__ e <| ETyp a b c ne d
      EColonType a exp b c d ->
        UpdateContinue env exp oldVal out <| HandlePreviousResult <| \(nv, ne) -> UpdateResult nv <| replaceE__ e <| EColonType a ne b c d
      ETypeAlias a b c exp d ->
        UpdateContinue env exp oldVal out <| HandlePreviousResult <| \(nv, ne) -> UpdateResult nv <| replaceE__ e <| ETypeAlias a b c ne d
      EParens sp1 exp sp2->
        UpdateContinue  env exp oldVal out <| HandlePreviousResult <| \(nv, ne) -> UpdateResult nv <| replaceE__ e <| EParens sp1 ne sp2
      {--ETypeCase sp1 e1 tbranches sp2 ->
        case eval_ syntax env (e::bt) e1 of
          Err s -> UpdateError s
          Ok (v1,sp1) ->
            case evalTBranches syntax env (e::bt) v1 tbranches of
              -- retVBoth and not addProvenanceToRet b/c only lets should return inner env
              Ok (Just (v2,sp2)) -> UpdateError "Typecase not updatable at this point"--Oks <| retVBoth [v2] (v2, sp1 ++ sp2) -- Provenence basedOn vals control-flow agnostic: do not include scrutinee
              UpdateError s              -> UpdateError s
              _                  -> UpdateError "Typecase not updatable at this point" --errorWithBacktrace syntax (e::bt) <| strPos e1.start ++ " non-exhaustive typecase statement"
      --}
      _ ->
        let outStr = case out of
          Program exp -> unparse exp
          Raw val -> valToString val
        in
        UpdateError <| "Non-supported update " ++ envToString env ++ "|-" ++ unparse e ++ " <-- " ++ outStr ++ " (was " ++ valToString oldVal ++ ")"


updateRec: Env -> Exp -> Val -> Output -> LazyList NextAction -> LazyList (Env, Exp)
updateRec env e oldVal newVal nextToUpdate =
  case update env e oldVal newVal nextToUpdate of
    Oks l -> l
    Errs msg -> LazyNil

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
        Nothing -> -- It could be that we are not dealing with numbers but with strings
          let operandsStr = operandVals
            |> List.map (\operand ->
                case operand.v_ of
                  VBase v as vb-> Just vb
                  _ -> Nothing
                )
            |> Utils.projJusts in
          case operandsStr of
            Just [VBase (VString sa) as va, VBase (VString sb) as vb] ->
              case op.val  of
                Plus ->
                  let result = case newOutVal.v_ of
                    VBase (VString s) ->
                      let saIsPrefix = String.startsWith sa s in
                      let sbIsSuffix = String.endsWith sb s in
                      if saIsPrefix && sbIsSuffix then -- Normally, we should check whitespaee to order. For now, append to left first.
                        let newva = VBase <| VString <| String.slice 0 (String.length s - String.length sb) s in
                        let newvb = VBase <| VString <| String.dropLeft (String.length sa) s in
                        oks [[newva, vb], [va, newvb]]
                      else if saIsPrefix then
                       let newvb = VBase <| VString <| String.dropLeft (String.length sa) s in
                       ok1 [va, newvb]
                      else if sbIsSuffix then
                       let newva = VBase <| VString <| String.slice 0 (String.length s - String.length sb) s in
                       ok1 [newva, vb]
                      else Errs <| "Cannot update with a string that is modifying two concatenated strings at the same time: '" ++
                        sa ++ "' + '" ++ sb ++ "' <-- '" ++ s ++ "'"
                    _ -> Errs <| "Cannot update with non-string" ++ valToString newOutVal
                  in
                  Results.map (
                      List.map2 (\original newString ->
                        replaceV_ original <| newString
                      ) operandVals
                    ) result
                o -> Errs <| "This operation is not supported for strings : " ++ toString o
            _ -> Errs <| "Operands do not form a list of numbers: " ++ toString operandVals
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
                                        else  Errs "No way to invert l^r <-- out where l < 0, r is even, out < 0 and out /= -1"
                                      else -- r is odd, so we preserve the negative sign.
                                        if newOut >= 0 then ok1 [newOut ** (1/r), r] else ok1 [0 - ( 0 - newOut) ** (1/r), r]
                                    else if l >= 0 then
                                      if newOut > 0 then oks [[newOut ** (1 / r), r], [l, logBase l newOut]]
                                      else if floor (1 / r) == ceiling (1/r) && 1/r < 0 then oks [[0 - (-newOut) ** (1 / r), r]]
                                      else Errs "No way to invert l^r <-- out where l >= 0, out < 0 and 1/r not an integer or not < 0"
                                    else Errs "No way to invert l^r <-- out where l < 0 and r < 0 or r is not an integer"
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
                (Pi,      [])    -> if newOut == pi then ok1 [] else Errs <| "Pi's value is 3.14159... and cannot be changed"
                _                -> Errs <| "Not the correct number of arguments for " ++ toString op ++ "(" ++ toString operandVals ++ ")"
          in
          Results.map (
            List.map2 (\original newNumber ->
              case original.v_ of
                VConst m0 (oldNumber, m1) -> replaceV_ original <| VConst m0 (newNumber, m1)
                x -> Debug.crash <| "[internal error] Did not get a VConst: " ++ toString x
            ) operandVals
          ) result
    _ -> Errs <| "Do not know how to revert computation " ++ toString op ++ "(" ++ toString operandVals ++ ") <-- " ++ toString newOutVal

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

branchWithInversion: Env -> Val -> List Branch -> Maybe ((Env, Exp), (Env, Exp) -> (Env, Val, List Branch))
branchWithInversion env input branches =
  case branches of
    [] -> Nothing
    head::tail ->
      case head.val of
        Branch_ sp1 pat exp sp2 ->
          case consWithInversion (pat, input) (Just (env, \newEnv -> newEnv)) of
            Nothing ->
              Maybe.map (\((augEnv, exp), patValEnvRebuilder) ->
                ((augEnv, exp),
                (\(newEnv, newExp) ->
                  let (updatedEnv, updatedVal, newTailBranches) = patValEnvRebuilder (newEnv, newExp) in
                  (updatedEnv, updatedVal, head::newTailBranches)
                  ))
              ) <| branchWithInversion env input tail
            Just (augEnv, patValEnvRebuilder) ->
              Just ((augEnv, exp), \(newAugEnv, newExp) ->
                let ((newPat, newVal), newEnv) = patValEnvRebuilder newAugEnv in
                (newEnv, newVal, (replaceB__ head <| Branch_ sp1 newPat newExp sp2) :: tail)
              )

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
  (PAs sp0 x sp1 innerPat, _) ->
    matchWithInversion (innerPat, v) |> Maybe.map
      (\(env, envReverse) -> ((x,v)::env, \newEnv ->
        case newEnv of
          (_, newV)::newEnv2 ->
            if newV == v then
              case envReverse newEnv2 of
              (newInnerPat, newVal) -> (replaceP__ p <| PAs sp0 x sp1 newInnerPat, newVal)
            else
              case envReverse newEnv2 of
              (newInnerPat, _)      -> (replaceP__ p <| PAs sp0 x sp1 newInnerPat, newV)

          _ -> Debug.crash <| "Not the same shape before/after pattern update: " ++ toString newEnv ++ " should have length >= 1"
      ))

  (PList sp0 ps sp1 Nothing sp2, VList vs) ->
    (if List.length ps == List.length vs then Just (ps,vs) else Nothing)
    |> Maybe.andThen matchListWithInversion
    |> Maybe.map (\(env, envRenewer) ->
      (env, envRenewer >> \(newPats, newVals) ->
        (replaceP__ p <| PList sp0 newPats sp1 Nothing sp2, replaceV_ v <| VList newVals)
      )
    )
  (PList sp0 ps sp1 (Just rest) sp2, VList vs) ->
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
              (replaceP__ p <| PList sp0 newPats sp1 (Just newPat) sp2, replaceV_ v <| (VList <| newVals ++ otherVals))
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
    VClosure _ patterns body env -> EFun ws patterns body ws -- Not sure about this one.
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
    (v, value)::tail -> v ++ "->" ++ (valToString value) ++ " " ++ (envToString tail)

valToString: Val -> String
valToString v = case v.v_ of
   VClosure Nothing patterns body [] -> (unparse << val_to_exp (ws " ")) v
   VClosure (Just name) patterns body [] -> "(" ++ name ++ "~" ++ ((unparse << val_to_exp (ws " ")) v) ++ ")"
   VClosure Nothing patterns body env -> "(" ++ envToString env ++ "|-" ++ ((unparse << val_to_exp (ws " ")) v) ++ ")"
   VClosure (Just name) patterns body env -> "(" ++ envToString env ++ "|" ++ name ++ "~" ++ ((unparse << val_to_exp (ws " ")) v) ++ ")"
   _ -> (unparse << val_to_exp (ws "")) v
