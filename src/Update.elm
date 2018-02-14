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
  , appendLazy, appendLazyLazy, mapLazy, andThenLazy, isLazyNil
  , lazyFromList
  , lazyCons2)
import MissingNumberMethods exposing (..)
import ValUnparser exposing (strVal)
import Lazy
import LangTools
import Set
import LangParserUtils

unparse = Syntax.unparser Syntax.Elm
unparsePattern = Syntax.patternUnparser Syntax.Elm

type NextAction = HandlePreviousResult ((Env, Exp) -> UpdateStack)
                | Fork Env Exp Val Output (LazyList NextAction)

type UpdateStack = UpdateResult      Env Exp
                 | UpdateResults     Env Exp (Lazy.Lazy (LazyList (Env, Exp)))
                 | UpdateIdem        Env Exp Val Output
                 | UpdateContinue    Env Exp Val Output NextAction
                 | UpdateRestart     Env Exp Val Output (LazyList NextAction)
                 | UpdateAlternative Env Exp Val Env Exp Val Output (LazyList NextAction)
                 | UpdateError String
                 -- The new expression, the new output, what to add to the stack with the result of the update above.



type Output = Raw Val | Program Exp
type alias Input = Exp

updateEnv: Env -> Ident -> Val -> Env
updateEnv env k value =
  case env of
    [] -> Debug.crash <| k ++ " not found in environment "
    ((kk, vv) as kv)::tail ->
      if kk == k then (kk, value)::tail else kv::updateEnv tail k value


-- Make sure that Env |- Exp evaluates to oldVal
update : Env -> Exp -> Val -> Output -> LazyList NextAction -> Results String (Env, Exp)
update env e oldVal out nextToUpdate =
{--
  let _ = Debug.log (String.concat ["update: ", unparse e, " <-- ", (case out of
    Program p -> unparse p
    Raw v -> valToString v), " -- env = " , envToString (pruneEnv e env), "|-"]) () in
--}
  let updateStack = getUpdateStackOp env e oldVal out nextToUpdate in
  case updateStack of -- callbacks to (maybe) push to the stack.
    UpdateError msg ->
      Errs msg
    UpdateIdem fEnv newE newOldVal newOutput ->
      update fEnv newE newOldVal newOutput nextToUpdate
    UpdateContinue fEnv newE newOldVal newOut g ->
      update fEnv newE newOldVal newOut (lazyCons2 g nextToUpdate)
    UpdateRestart fEnv newE newOldVal newOut newNextToUpdate ->
      update fEnv newE newOldVal newOut newNextToUpdate
    UpdateAlternative fEnv fOut fOldVal altEnv altE altOldVal altOut nextToUpdate2 ->
      update fEnv e fOldVal (Program fOut) <| appendLazy nextToUpdate <| lazyFromList [Fork altEnv altE altOldVal altOut nextToUpdate2]
    UpdateResults fEnv fOut alternatives ->
      update fEnv (replaceE__ e <| EHole (ws "") Nothing) oldVal (Program fOut) <|
        appendLazy nextToUpdate <| Results.mapLazy (\(altEnv, altExp) -> Fork altEnv (replaceE__ e <| EHole (ws "") Nothing) oldVal (Program altExp) nextToUpdate) (Lazy.force alternatives)
    UpdateResult fEnv fOut -> -- Let's consume the stack !
      case nextToUpdate of
        LazyNil -> ok1 <| (fEnv, fOut)
        LazyCons head lazyTail ->
          case head of
            Fork newEnv newExp newOldVal newOut nextToUpdate2 ->
              okLazy (fEnv, fOut) <| \() ->
                updateRec newEnv newExp newOldVal newOut <| appendLazyLazy nextToUpdate2 lazyTail
            HandlePreviousResult f ->
              case f (fEnv, fOut) of
                UpdateError msg ->
                  Errs msg
                UpdateResult fEnv fOut ->
                  update fEnv e oldVal (Program fOut) (Lazy.force lazyTail)
                UpdateResults fEnv fOut alternatives ->
                  update fEnv (replaceE__ e <| EHole (ws "") Nothing) oldVal (Program fOut) <|
                    appendLazy (Lazy.force lazyTail) <| Results.mapLazy (\(altEnv, altExp) -> Fork altEnv (replaceE__ e <| EHole (ws "") Nothing) oldVal (Program altExp) (Lazy.force lazyTail)) (Lazy.force alternatives)
                UpdateIdem fEnv newE newOldVal newOut ->
                  update fEnv newE newOldVal newOut (Lazy.force lazyTail)
                UpdateContinue fEnv newE newOldVal newOut g ->
                  update fEnv newE newOldVal newOut (LazyCons g lazyTail)
                UpdateRestart fEnv newE newOldVal newOut newNextToUpdate ->
                  update fEnv newE newOldVal newOut newNextToUpdate
                UpdateAlternative fEnv fOut fOldVal altEnv altE altOldVal altOut nextToUpdate2 ->
                  update fEnv e fOldVal (Program fOut) (appendLazy (Lazy.force lazyTail) (lazyFromList [Fork altEnv altE altOldVal altOut nextToUpdate2]))

getUpdateStackOp : Env -> Exp -> Val -> Output -> LazyList NextAction -> UpdateStack
getUpdateStackOp env e oldVal out nextToUpdate =
  case e.val.e__ of
    EHole ws _ ->
      case out of
        Program prog -> UpdateResult env prog
        Raw newVal ->   UpdateResult env <| val_to_exp ws newVal
    EConst ws num loc widget ->
      case out of
        Program prog -> UpdateResult env prog
        Raw newVal ->   UpdateResult env <| replaceE__ e <| EConst ws (getNum newVal) loc widget
    EBase ws m ->
      case out of
        Program prog -> UpdateResult env prog
        Raw newVal ->
          case m of
            EString quoteChar chars ->
              case newVal.v_ of
                VBase (VString newChars) ->   UpdateResult env <| replaceE__ e <| EBase ws (EString quoteChar newChars)
                _ -> UpdateResult env <| val_to_exp ws newVal
            _ -> UpdateResult env <| val_to_exp ws newVal
    EFun sp0 ps e sp1 ->
      case out of
        Program prog -> UpdateResult env prog
        Raw newVal ->   case newVal.v_ of
          VClosure Nothing newPs newE newEnv -> UpdateResult newEnv <| replaceE__ e <| EFun sp0 newPs newE sp1
          _ -> Debug.crash "Trying to update a function with non-closure " <| valToString newVal
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
                    UpdateIdem env (replaceE__ e <| EHole (ws "") Nothing) oldVal <| Program e
                  (expHead::expTail, origHead::origTail, newHead::newTail) ->
                    UpdateContinue env (Tuple.second expHead) origHead (Raw newHead)
                      <| HandlePreviousResult <| \(newHeadEnv, newHeadExp) ->
                        UpdateContinue env (replaceE__ e <| EList sp1 expTail sp2 Nothing sp3) (replaceV_ oldVal (VList origTail))
                              (Raw (replaceV_ newVal (VList newTail)))
                          <| HandlePreviousResult <| \(newTailEnv, newTailExp) ->
                            case newTailExp.val.e__ of
                              EList _ newTail _ _ _ ->
                                let newEnv = triCombine e env newHeadEnv newTailEnv in
                                UpdateResult newEnv <| replaceE__ e <| EList sp1 ((Tuple.first expHead, newHeadExp) :: newTail) sp2 Nothing sp3
                              _ -> Debug.crash "Internal error: Should get a list back"
                  _ -> Debug.crash "The list's length were checked, what happened??"
              else
                let results = indicesOfModification valEqual origVals newOutVals -- LazyList Exp
                  |> Results.mapLazy (\(insertionIndex, deletedCount, inserted) ->
                    -- Copy the whitespace of the previous list elements, if possible, and do this in a nested way
                    let valtoexpWhitespace = if insertionIndex < List.length elems then
                      case List.drop insertionIndex elems |> List.take 1 of
                        [(_,previousElem)] -> (\v -> Lang.copyPrecedingWhitespace previousElem (val_to_exp (ws " " ) v))
                        _ -> (\v -> val_to_exp (ws " " ) v)
                      else if List.length elems > 0 then
                        case List.drop (List.length elems - 1) elems of
                        [(_,previousElem)] -> (\v -> Lang.copyPrecedingWhitespace previousElem (val_to_exp (ws " " ) v))
                        _ -> (\v -> val_to_exp (ws " " ) v)
                      else (\v -> val_to_exp (ws " " ) v)
                    in
                    -- TODO copy space from existing element?
                    let insertedExp = List.map ((,) space0 << valtoexpWhitespace) inserted in
                    (env, replaceE__ e <|
                      EList sp1 (List.take insertionIndex elems ++ insertedExp ++ List.drop (insertionIndex + deletedCount) elems) sp2 Nothing sp3)
                  )
                in -- We need to convert this lazyList to a set of results
                case results of
                  LazyNil -> UpdateError <| "Internal error: there should have been at least one solution"
                  LazyCons (newEnv, newExp) lazyTail ->
                    UpdateResults newEnv newExp lazyTail

            _ -> UpdateError <| "Cannot update a list " ++ unparse e ++ " with non-list " ++ valToString newVal

    EList sp1 elems sp2 (Just tail) sp3 ->
      case out of
        Program prog -> Debug.crash <| "??? Don't know how to update a list with a program " ++ unparse prog
        Raw newVal ->
          case (oldVal.v_, newVal.v_) of
            (VList origVals, VList newOutVals) ->
              let elemsLength = List.length elems in
              if elemsLength <= List.length newOutVals && elemsLength >= 1 then
                let elemOrigVals = List.take elemsLength origVals in
                let elemNewVals = List.take elemsLength newOutVals in
                let tailOrigVals = List.drop elemsLength origVals in
                let tailNewVals = List.drop elemsLength newOutVals in
                let toEList eles = replaceE__ e <| EList sp1 eles sp2 Nothing sp3 in
                let toVList elvs = replaceV_ newVal <| VList elvs in
                UpdateContinue env (toEList elems) (toVList elemOrigVals) (Raw <| toVList elemNewVals) <|
                  HandlePreviousResult <| \(newElemListEnv, newElemListExp) ->
                    let newElems = case newElemListExp.val.e__ of
                      EList _ ne _ _ _ -> ne
                      o -> Debug.crash <| "Expected a list, got " ++ toString o
                    in
                    UpdateContinue env tail (toVList tailOrigVals) (Raw <| toVList tailNewVals) <|
                      HandlePreviousResult <| \(newTailEnv, newTailExp) ->
                        let finalEnv = triCombine e env newElemListEnv newTailEnv in
                        UpdateResult finalEnv <| replaceE__ e <| EList sp1 newElems sp2 (Just newTailExp) sp3
              else UpdateError <| "Cannot (yet) update a list concatenation " ++ unparse e ++ " with " ++ toString elemsLength ++ " heads by list of smaller length: " ++ valToString newVal
            _ -> UpdateError <| "Cannot update a list " ++ unparse e ++ " with non-list " ++ valToString newVal

    EApp sp0 e1 e2s appType sp1 ->
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
                          UpdateContinue env (replaceE__ e <| EList sp0 (List.map ((,) space0) e2s) sp1 Nothing sp1) (replaceV_ oldVal <| VList v2s) (Raw (replaceV_ oldVal <| VList newArgs)) <|
                            HandlePreviousResult <| \(newE2Env, newE2List) ->
                              case newE2List.val.e__ of
                                EList _ newE2s _ _ _ ->
                                  let finalEnv = triCombine e env newE1Env newE2Env in
                                  UpdateResult finalEnv <| replaceE__ e <| EApp sp0 newE1 (List.map Tuple.second newE2s) appType sp1
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
                          UpdateContinue env (replaceE__ e <| EList sp0 (List.map ((,) space0) e2s) sp1 Nothing sp1) (replaceV_ oldVal <| VList v2s) (Raw <| replaceV_ oldVal <| VList newArgs) <|
                            HandlePreviousResult <| \(newE2Env, newE2List) ->
                              case newE2List.val.e__ of
                                EList _ newE2s _ _ _ ->
                                  let finalEnv = triCombine e env newE1Env newE2Env in
                                  UpdateResult finalEnv <| replaceE__ e <| EApp sp0 newE1 (List.map Tuple.second newE2s) appType sp1
                                x -> Debug.crash <| "Unexpected result of updating a list " ++ toString x
                  _          -> UpdateError <| strPos e1.start ++ "bad environment"
              _ ->
                UpdateError <| strPos e1.start ++ " not a function"

    EIf sp0 cond sp1 thn sp2 els sp3 ->
      case doEval Syntax.Elm env cond of
        Ok ((v, _), _) ->
          case v.v_ of
            VBase (VBool b) ->
              if b then
                UpdateContinue env thn oldVal out <| HandlePreviousResult <| \(newEnv, newThn) ->
                  UpdateResult newEnv <| replaceE__ e <| EIf sp0 cond sp1 newThn sp2 els sp3
              else
                UpdateContinue env els oldVal out <| HandlePreviousResult <| \(newEnv, newEls) ->
                  UpdateResult newEnv <| replaceE__ e <| EIf sp0 cond sp1 thn sp2 newEls sp3
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
              let argList = replaceE__ e <| EList (ws "") (List.map ((,) space0) opArgs) (ws "") Nothing (ws "") in
              let handleRemainingResults lazyTail nextToUpdate =
                lazyCons2 (HandlePreviousResult <| \(newOpEnv, newOpArgList) ->
                  case newOpArgList.val.e__ of
                    EList _ newOpArgs _ _ _ ->
                      let finalExp = replaceE__ e <| EOp sp1 op (List.map Tuple.second newOpArgs) sp2 in
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
                OptNumToString ->
                  case out of
                    Program exp -> UpdateError <| "Don't know how to update a OptNumToString with a program"
                    Raw vOut ->
                      let default () =
                        case vOut.v_ of
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
                                      e -> UpdateError <| "[internal error] Wrong number of arguments in update OptNumToString: " ++ toString e
                          e -> UpdateError <| "Expected string, got " ++ toString e
                      in
                      case vs of
                        [original] ->
                          case original.v_ of
                            VBase (VString origS) ->
                              case opArgs of
                                [opArg] -> UpdateContinue env opArg original out <|
                                  HandlePreviousResult <| \(env, newOpArg) ->
                                    UpdateResult env <| replaceE__ e <| EOp sp1 op [newOpArg] sp2
                                e -> UpdateError <| "[internal error] Wrong number of argument values in update OptNumToString: " ++ toString e
                            _ -> -- Everything else is unparsed to a string, we just parse it.
                              default ()
                        _ -> UpdateError <| "[internale error] Wrong number or arguments in updateOptNumtoString: " ++ toString e
                ToStr      ->
                  case out of
                    Program exp -> UpdateError <| "Don't know how to update a ToStr with a program"
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
                _ ->
                  case out of
                    Program exp -> UpdateError <| "Don't know how to update an operation with a program"
                    Raw newVal ->
                      case maybeUpdateMathOp op vs oldVal newVal of
                        Errs msg -> UpdateError msg
                        Oks LazyNil -> UpdateError "[Internal error] No result for updating."
                        Oks (LazyCons head lazyTail) ->
                          UpdateRestart env argList (replaceV_ oldVal <| VList vs)
                            (Raw <| replaceV_ oldVal <| VList head) <|
                            handleRemainingResults lazyTail nextToUpdate

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
                  let finalEnv = triCombine e env newInputEnv newBranchEnv in
                  let finalExp = replaceE__ e <| ECase sp1 newInputExp nBranches sp2 in
                  UpdateResult finalEnv finalExp
    --  ETypeCase WS Exp (List TBranch) WS
    ELet sp1 letKind False p sp2 e1 sp3 body sp4 ->
        case doEval Syntax.Elm env e1 of
          Err s       -> UpdateError s
          Ok ((oldE1Val,_), _) ->
            case consWithInversion (p, oldE1Val) (Just (env, (\newEnv -> newEnv))) of
               Just (envWithE1, consBuilder) ->
                 UpdateContinue envWithE1 body oldVal out <| HandlePreviousResult <| \(newEnvWithE1, newBody) ->
                   case consBuilder newEnvWithE1 of
                    ((newPat, newE1Val), newEnvFromBody) ->
                      UpdateContinue env e1 oldE1Val (Raw newE1Val) <| HandlePreviousResult <| \(newEnvFromE1, newE1) ->
                        let finalEnv = triCombine e env newEnvFromE1 newEnvFromBody in
                        let finalExp = replaceE__ e <| ELet sp1 letKind False newPat sp2 newE1 sp3 newBody sp4 in
                        UpdateResult finalEnv finalExp
               Nothing ->
                 UpdateError <| strPos e.start ++ " could not match pattern " ++ (Syntax.patternUnparser Syntax.Elm >> Utils.squish) p ++ " with " ++ strVal oldE1Val
    ELet sp1 letKind True p sp2 e1 sp3 body sp4 ->
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
                             let finalEnv = triCombine e env newEnvFromE1 newEnvFromBody in
                             let finalExp = replaceE__ e <| ELet sp1 letKind True newPat sp2 newE1 sp3 newBody sp4 in
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
    EParens sp1 exp pStyle sp2->
      UpdateContinue  env exp oldVal out <| HandlePreviousResult <| \(nv, ne) -> UpdateResult nv <| replaceE__ e <| EParens sp1 ne pStyle sp2
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
      UpdateError <| "Non-supported update " ++ envToString (pruneEnv e env) ++ "|-" ++ unparse e ++ " <-- " ++ outStr ++ " (was " ++ valToString oldVal ++ ")"


updateRec: Env -> Exp -> Val -> Output -> LazyList NextAction -> LazyList (Env, Exp)
updateRec env e oldVal newVal nextToUpdate =
  case update env e oldVal newVal nextToUpdate of
    Oks l -> l
    Errs msg -> LazyNil

listPrefixEqual: Int -> (a -> a -> Bool) -> List a -> List a -> Bool
listPrefixEqual n pred l1 l2 =
  if n == 0 then True else
  case l1 of
    [] -> False
    hd::tl -> case l2 of
      [] -> False
      hd2::tl2 -> if pred hd hd2 then listPrefixEqual (n-1) pred tl tl2 else False

listEqual: (a -> a -> Bool) -> List a -> List a -> Bool
listEqual pred l1 l2 =
    case l1 of
      [] -> case l2 of
        [] -> True
        _ -> False
      hd::tl -> case l2 of
        [] -> False
        hd2::tl2 -> if pred hd hd2 then listEqual pred tl tl2 else False

indicesOfModification: (a -> a -> Bool) -> List a -> List a -> LazyList (Int, Int, List a)
indicesOfModification equalTest input output =
  let test sup = --: LazyList (Int, Int, List a)
    let potentialWithSuppression =  --: LazyList (Int, Int, List a)
      let insLength = List.length output - List.length input + sup in
      lazyFromList (List.range 0 (List.length input - sup))
      |> andThenLazy (\index ->
        if (sup > 0 || insLength > 0 || index == 0) &&
            listPrefixEqual index equalTest output input &&
            listEqual equalTest (List.drop (List.length output - (List.length input - (index + sup))) output) (List.drop (index + sup) input) then
          lazyFromList [(index, sup, List.drop index output |> List.take insLength)]
        else LazyNil
        )
    in
    if isLazyNil potentialWithSuppression then test (sup + 1) else potentialWithSuppression
  in
  test 0

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
    (VBase (VString oldOut), VBase (VString newOut)) ->
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
                    let newsa = String.slice 0 (String.length s - String.length sb) s in
                    let newsb = String.dropLeft (String.length sa) s in
                    let newva = VBase <| VString <| newsa in
                    let newvb = VBase <| VString <| newsb in
                    if String.length newsa > String.length sa then -- Addition of elements, perhaps we need to stick them to a particular side.
                      let insertion = String.slice (String.length sa) (String.length newsa) newsa in
                      if String.length sa == 0 then --Fill empty string only with spaces by default
                        if LangParserUtils.isOnlySpaces insertion then
                          oks [[newva, vb], [va, newvb]]
                        else
                          oks [[va, newvb], [newva, vb]]
                      else if String.length sb == 0 then
                        if LangParserUtils.isOnlySpaces insertion then
                          oks [[va, newvb], [newva, vb]]
                        else
                          oks [[newva, vb], [va, newvb]]
                      else -- Both original strings had chars
                        let aEndIsSspace = LangParserUtils.isOnlySpaces <| String.slice (-1) (String.length sa) sa in
                        let bStartIsSspace = LangParserUtils.isOnlySpaces <| String.slice 0 1 sb in
                        let insertedStartIsSspace = LangParserUtils.isOnlySpaces <| String.slice 0 1 insertion in
                        let insertedEndIsSspace = LangParserUtils.isOnlySpaces <| String.slice (-1) (String.length insertion) insertion in
                        -- Stick inserted expression if some end match and not the other, else keep left.
                        if (bStartIsSspace == insertedEndIsSspace) && xor aEndIsSspace insertedStartIsSspace then
                          oks [[va, newvb], [newva, vb]]
                        else
                          oks [[newva, vb], [va, newvb]]
                         --LangParserUtils.isOnlySpaces (String.slice 0 1 insertion) &&
                    else
                      oks [[newva, vb], [va, newvb]]
                  else if saIsPrefix then
                    let newvb = VBase <| VString <| String.dropLeft (String.length sa) s in
                    ok1 [va, newvb]
                  else if sbIsSuffix then
                    let newva = VBase <| VString <| String.slice 0 (String.length s - String.length sb) s in
                    ok1 [newva, vb]
                  else -- Some parts on both strings were deleted, everything in between was added.
                    let newsa = commonPrefix sa s in
                    let newsb = commonSuffix sb s in
                    let newva s = VBase <| VString <| newsa ++ s in
                    let newvb s = VBase <| VString <| s ++ newsb in
                    let insertion = String.slice (String.length newsa) (String.length s - String.length newsb) s in
                    if String.length newsa == 0 then -- Everything inserted belongs to newsa by default
                      oks [[newva insertion, newvb ""], [newva "", newvb insertion]]
                    else if String.length newsb == 0 then --Everything inserted belongs to newvb by default
                      oks [[newva "", newvb insertion], [newva insertion, newvb ""]]
                    else
                        let aEndIsSspace = LangParserUtils.isOnlySpaces <| String.slice (-1) (String.length newsa) newsa in
                        let bStartIsSspace = LangParserUtils.isOnlySpaces <| String.slice 0 1 newsb in
                        let insertedStartIsSspace = LangParserUtils.isOnlySpaces <| String.slice 0 1 insertion in
                        let insertedEndIsSspace = LangParserUtils.isOnlySpaces <| String.slice (-1) (String.length insertion) insertion in
                        -- Stick inserted expression if some end match and not the other, else keep left.
                        if (bStartIsSspace == insertedEndIsSspace) && xor aEndIsSspace insertedStartIsSspace then
                          oks [[newva "", newvb insertion], [newva insertion, newvb ""]]
                        else
                          oks [[newva insertion, newvb ""], [newva "", newvb insertion]]
                _ -> Errs <| "Cannot yet update with non-string" ++ valToString newOutVal
              in
              Results.map (
                  List.map2 (\original newString ->
                    replaceV_ original <| newString
                  ) operandVals
                ) result
            o -> Errs <| "This operation is not supported for strings : " ++ toString o
        o -> Errs <| "Expected strings, got " ++ toString o

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
        Nothing -> Errs <| "Operands do not form a list of numbers: " ++ toString operandVals
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

commonPrefix: String -> String -> String
commonPrefix =
  let aux prefix s1 s2 =
    case (String.uncons s1, String.uncons s2) of
      (Nothing, _) -> prefix
      (_, Nothing) -> prefix
      (Just (s1head, s1tail), Just (s2head, s2tail)) ->
        if s1head == s2head then aux (prefix ++ String.fromChar s1head) s1tail s2tail else prefix
  in aux ""

commonSuffix: String -> String -> String
commonSuffix s1 s2 = commonPrefix (String.reverse s1) (String.reverse s2) |> String.reverse

-- Tri combine only checks dependencies that may have been changed.
triCombine: Exp -> Env -> Env -> Env -> Env
triCombine origExp originalEnv newEnv1 newEnv2 =
  let fv = LangTools.freeIdentifiers origExp in
  --let _ = Debug.log "TriCombine starts !" () in
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
          if not (Set.member x fv) || not (valEqual v2 v1) then aux (acc ++ [(x, v2)]) oe ne1 ne2
          else aux (acc ++ [(x, v3)]) oe ne1 ne2
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
            if valEqual newV v then
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
  (PParens sp0 innerPat sp1, _) ->
    matchWithInversion (innerPat, v)
    |> Maybe.map
      (\(env, envReverse) -> (env, \newEnv ->
        case envReverse newEnv of
          (newInnerPat, newVal) -> (replaceP__ p <| PParens sp0 newInnerPat sp1, newVal)
      ))
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
    VList vals -> EList ws (List.map ((,) space0 << val_to_exp ws) vals) ws Nothing <| ws
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

removeCommonPrefix l1 l2 =
  case (l1, l2) of
    ([], _) -> (l1, l2)
    (_, []) -> (l1, l2)
    (head::tail, head2::tail2) ->
      if head == head2 then
        removeCommonPrefix tail tail2
      else (l1, l2)

pruneEnv: Exp -> Env -> Env
pruneEnv exp env = -- Remove all the initial environment that is on the right.
  --let (p1, p2) = removeCommonPrefix (List.reverse Eval.initEnv) (List.reverse env) in
  --List.reverse p1
  --List.take 5 env
  --List.take (List.length env - List.length Eval.initEnv) env
  --env
  let freeVars = LangTools.freeIdentifiers exp in
  List.filter (\(x, _) -> Set.member x freeVars) env

envToString: Env -> String
envToString env =
  case env of
    [] -> ""
    (v, value)::tail -> v ++ "->" ++ (valToString value) ++ " " ++ (envToString tail)

valToString: Val -> String
valToString v = case v.v_ of
   VClosure Nothing patterns body [] -> (unparse << val_to_exp (ws " ")) v
   VClosure (Just name) patterns body [] -> "(" ++ name ++ "~" ++ ((unparse << val_to_exp (ws " ")) v) ++ ")"
   VClosure Nothing patterns body env ->
     let e = val_to_exp (ws " ") v in
     "(" ++ envToString (pruneEnv e env) ++ "|-" ++ unparse e ++ ")"
   VClosure (Just name) patterns body env ->
     let e = val_to_exp (ws " ") v in
     "(" ++ envToString (pruneEnv e env) ++ "|" ++ name ++ "~" ++ unparse e ++ ")"
   _ -> (unparse << val_to_exp (ws "")) v



-- Equality checking
valEqual: Val -> Val -> Bool
valEqual v1 v2 = --let _ = Debug.log "valEqual of " (valToString v1, valToString v2) in
  valToString v1 == valToString v2
  {--case (v1.v_ , v2.v_) of
  (VConst _ (n1, _), VConst _ (n2, _)) -> n1 == n2
  (VBase vb1, VBase vb2) -> vb1 == vb2
  (VClosure nm1 p1 body1 env1, VClosure nm2 p2 body2 env2 ) ->
    nm1 == nm2 && listForAll2 patEqual p1 p2 && expEqual body1 body2 && envEqual (pruneEnv body1 env1) (pruneEnv body2 env2)
  (VList v1s, VList v2s) -> listForAll2 valEqual v1s v2s
  _ -> False--}

envEqual: Env -> Env -> Bool
envEqual env1 env2 = --let _ = Debug.log "envEqual " () in
  listForAll2 (\(x1, v1) (x2, v2) -> x1 == x2 && valEqual v1 v2) env1 env2

wsEqual: WS -> WS -> Bool
wsEqual ws1 ws2 = ws1.val == ws2.val

patEqual: Pat -> Pat -> Bool
patEqual p1_ p2_ = --let _ = Debug.log "patEqual " (unparsePattern p1_, unparsePattern p2_) in
  unparsePattern p1_ == unparsePattern p2_
{--  case (p1_.val.p__, p2_.val.p__) of
  (PVar sp1 ident1 _,PVar sp2 ident2 _) -> wsEqual sp1 sp2 && ident1 == ident2
  (PConst sp1 num1, PConst sp2 num2)  -> wsEqual sp1 sp2 && num1 == num2
  (PBase sp1 bv1, PBase sp2 bv2) -> wsEqual sp1 sp2 && bv1 == bv2
  (PList sp1 pats sp2 mpat sp3,PList sp4 pats2 sp5 mpat2 sp6) ->
    wsEqual sp1 sp4 && wsEqual sp2 sp5 && wsEqual sp3 sp6 && listForAll2 patEqual pats pats2
    && (case (mpat, mpat2) of
      (Nothing, Nothing) -> True
      (Just p1, Just p2) -> patEqual p1 p2
      _ -> False
    )
  (PAs sp1 name sp2 p1,PAs sp3 name2 sp4 p2) -> wsEqual sp1 sp3 && name == name2 && wsEqual sp2 sp4 && patEqual p1 p2
  (PParens sp1 p1 sp2,PParens sp3 p2 sp4) -> wsEqual sp1 sp3 && patEqual p1 p2 && wsEqual sp2 sp4
  _ -> False
  --}

branchEqual: Branch -> Branch -> Bool
branchEqual b1 b2 = case (b1.val, b2.val) of
  (Branch_ sp1 p1 e1 sp2, Branch_ sp3 p2 e2 sp4) ->
    wsEqual sp1 sp3 && wsEqual sp2 sp4 && expEqual e1 e2 && patEqual p1 p2

tbranchEqual: TBranch -> TBranch -> Bool
tbranchEqual t1 t2 = case (t1.val, t2.val) of
  (TBranch_ sp1 ty1 e1 sp2, TBranch_ sp3 ty2 e2 sp4) ->
    wsEqual sp1 sp3 && wsEqual sp2 sp4 && expEqual e1 e2 && typeEqual ty1 ty2

listForAll2: (a -> a -> Bool) -> List a -> List a -> Bool
listForAll2 f l1 l2 = case l1 of
  [] -> case l2 of
    [] -> True
    _ -> False
  h1::t1 -> case l2 of
    [] -> False
    h2::t2 -> if f h1 h2 then listForAll2 f t1 t2 else False

typeEqual: Type -> Type -> Bool
typeEqual ty1 ty2 = --let _ = Debug.log "typeEqual " (ty1, ty2) in
  case (ty1.val, ty2.val) of
  (TNum sp1, TNum sp2) -> wsEqual sp1 sp2
  (TBool sp1, TBool sp2) -> wsEqual sp1 sp2
  (TString sp1, TString sp2) -> wsEqual sp1 sp2
  (TNull sp1, TNull sp2) -> wsEqual sp1 sp2
  (TList sp1 t1 sp2, TList sp3 t2 sp4) ->  wsEqual sp1 sp3 && wsEqual sp2 sp4 && typeEqual t1 t2
  (TDict sp1 tk tv sp2, TDict sp3 tk2 tv2 sp4) -> wsEqual sp1 sp3 && wsEqual sp2 sp4 && typeEqual tk tk2 && typeEqual tv tv2
  (TTuple sp1 args sp2 mTail sp2e, TTuple sp3 args2 sp4 mTail2 sp4e) ->
    wsEqual sp1 sp3 && wsEqual sp2 sp4 && wsEqual sp2e sp4e &&
        listForAll2 typeEqual args args2 &&
        ( case (mTail, mTail2) of
          (Nothing, Nothing) -> True
          (Just t1, Just t2) -> typeEqual t1 t2
          _ -> False
        )
  (TArrow sp1 types1 sp2, TArrow sp3 types2 sp4) ->
    wsEqual sp1 sp3 && wsEqual sp2 sp4  &&
      listForAll2 typeEqual types1 types2
  (TUnion sp1 types1 sp2, TUnion sp3 types2 sp4) ->
    wsEqual sp1 sp3 && wsEqual sp2 sp4  &&
          listForAll2 typeEqual types1 types2
  (TNamed sp1 ident1, TNamed sp2 ident2) ->
    wsEqual sp1 sp2 && ident1 == ident2
  (TVar sp1 ident1, TVar sp2 ident2) ->
    wsEqual sp1 sp2 && ident1 == ident2
  (TForall sp1 ts1 t1 sp2, TForall sp3 ts2 t2 sp4) ->
    wsEqual sp1 sp3 && wsEqual sp2 sp4 &&
    ( case (ts1, ts2) of
        (One (sp1, a1), One (sp2, a2)) -> wsEqual sp1 sp2 && a1 == a2
        (Many sp1 elems sp2, Many sp3 elems2 sp4) -> wsEqual sp1 sp3 && wsEqual sp2 sp4 &&
          listForAll2 (\(sp1, a1) (sp2, a2) -> wsEqual sp1 sp2 && a1 == a2) elems elems2
        _ -> False
    ) && typeEqual t1 t2
  (TWildcard sp1, TWildcard sp2) -> wsEqual sp1 sp2
  _ -> False

expEqual: Exp -> Exp -> Bool
expEqual e1_ e2_ =
  --let _ = Debug.log "expEqual " (unparse e1_, unparse e2_) in
  unparse e1_ == unparse e2_
{--
  case (e1_.val.e__, e2_.val.e__) of
  (EConst sp1 num1 _ _, EConst sp2 num2 _ _) -> wsEqual sp1 sp2 && num1 == num2
  (EBase sp1 bv1, EBase sp2 bv2) -> wsEqual sp1 sp2 && bv1 == bv2
  (EVar sp1 id1, EVar sp2 id2) -> wsEqual sp1 sp2 && id1 == id2
  (EFun sp1 pats body sp2, EFun sp3 pats2 body2 sp4) -> wsEqual sp1 sp3 &&
    listForAll2 patEqual pats pats2 &&
    expEqual body body2 &&
    wsEqual sp2 sp4
  (EApp sp1 fun args sp2, EApp sp3 fun2 args2 sp4) ->
    wsEqual sp1 sp3 && wsEqual sp2 sp4 && expEqual fun fun2 &&
    listForAll2 expEqual args args2
  (EOp sp1 op1 args sp2, EOp sp3 op2 args2 sp4) ->
    wsEqual sp1 sp3 && wsEqual sp2 sp4 && op1.val == op2.val &&
    listForAll2 expEqual args args2
  (EList sp1 args sp2 mTail sp2e, EList sp3 args2 sp4 mTail2 sp4e) ->
    wsEqual sp1 sp3 && wsEqual sp2 sp4 && wsEqual sp2e sp4e &&
    listForAll2 expEqual args args2 &&
    ( case (mTail, mTail2) of
      (Nothing, Nothing) -> True
      (Just t1, Just t2) -> expEqual t1 t2
      _ -> False
    )
  (EIf sp11 cond1 sp12 then1 sp13 else1 sp14, EIf sp21 cond2 sp22 then2 sp23 else2 sp4) ->
    wsEqual sp11 sp21 &&
    wsEqual sp12 sp22 &&
    wsEqual sp13 sp23 &&
    wsEqual sp14 sp24 &&
    expEqual cond1 cond2 && expEqual then1 then2 && expEqual else1 else2
  (ECase sp1 input1 branches1 sp2, ECase sp3 input2 branches2 sp4) ->
    wsEqual sp1 sp3 && wsEqual sp2 sp4 &&
    expEqual input1 input2 &&
    listForAll2 branchEqual branches1 branches2
  (ETypeCase sp1 input1 tbranches1 sp2, ETypeCase sp3 input2 tbranches2 sp4) ->
    wsEqual sp1 sp3 && wsEqual sp2 sp4 &&
    expEqual input1 input2 &&
    listForAll2 tbranchEqual tbranches1 tbranches2

  (ELet sp11 lk1 rec1 pat1 sp12 exp1 sp13 body1 sp14, ELet sp21 lk2 rec2 pat2 sp22 exp2 sp23 body2 sp24) ->
    wsEqual sp11 sp21 &&
    wsEqual sp12 sp22 &&
    wsEqual sp13 sp23 &&
    wsEqual sp14 sp24 &&
    lk1 == lk2 && rec1 == rec2 &&
    patEqual pat1 pat2 && expEqual body1 body2 && expEqual exp1 exp2
  (EComment sp1 s1 e1, EComment sp2 s2 e2) ->
    wsEqual sp1 sp2 && s1 == s2 && expEqual e1 e2
  (EOption sp1 wStr1 sp2 wStr2 exp1, EOption sp3 wStr3 sp4 wStr4 exp2) ->
    wsEqual sp1 sp3 && wsEqual sp2 sp4 &&
    wStr1.val == wStr3.val && wStr2.val == wStr4.val &&
    expEqual exp1 exp2
  (ETyp sp1 pat1 t1 e1 sp2, ETyp sp3 pat2 t2 e2 sp4) ->
    wsEqual sp1 sp3 && wsEqual sp2 sp4 &&
    patEqual pat1 pat2 &&
    typeEqual t1 t2 &&
    expEqual e1 e2
  (EColonType sp1 e1 sp2 t1 sp2e, EColonType sp3 e2 sp4 t2 sp4e) ->
    wsEqual sp1 sp3 && wsEqual sp2 sp4 && wsEqual sp2e sp4e &&
    expEqual e1 e2 && typeEqual t1 t2
  (ETypeAlias sp1 pat1 t1 e1 sp2, ETypeAlias sp3 pat2 t2 e2 sp4) ->
    wsEqual sp1 sp3 && wsEqual sp2 sp4 &&
    patEqual pat1 pat2 && expEqual e1 e2 && typeEqual t1 t2
  (EParens sp1 e1 pStyle1 sp2, EParens sp3 e2 pStyle2 sp4) ->
    wsEqual sp1 sp3 && wsEqual sp2 sp4 && expEqual e1 e2 && pStyle
  (EHole sp1 (Just v1), EHole sp2 (Just v2)) ->
    wsEqual sp1 sp2 && valEqual v1 v2
  (EHole sp1 Nothing, EHole sp2 Nothing) ->
    wsEqual sp1 sp2
  _ -> False
--}
