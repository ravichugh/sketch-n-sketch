module UpdateStack exposing  (..)
import Lang exposing (..)
import Results exposing
  ( Results(..)
  , ok1, oks, okLazy
  )
import LazyList exposing (..)
import Lazy
import Syntax
import ValUnparser exposing (strVal)
import UpdateUtils exposing (..)
import Utils
import LangUtils exposing (envToString, valToString)
import Set exposing (Set)
import UpdatedEnv exposing (UpdatedEnv, original)
import Pos exposing (Pos)


type alias UpdatedExp = { val: Exp, changes: Maybe EDiffs }
type alias UpdatedExpTuple = { val: List Exp, changes: Maybe (TupleDiffs EDiffs) }

type HandlePreviousResult = HandlePreviousResult String (UpdatedEnv -> UpdatedExp -> UpdateStack)
type Fork = Fork String UpdateStack (LazyList HandlePreviousResult) (LazyList Fork)

updatedExpToString: Exp -> UpdatedExp -> String
updatedExpToString e ue =
  ue.changes |> Maybe.map (UpdateUtils.eDiffsToString "" e ue.val) |> Maybe.withDefault "<no change>"

updatedExpToStringWithPositions: Exp -> UpdatedExp -> (String, List Exp)
updatedExpToStringWithPositions e ue =
  ue.changes |> Maybe.map (UpdateUtils.eDiffsToStringPositions ElmSyntax "" (Pos 0 0, (0, 0)) e ue.val >> (\(msg, (_, l)) -> (msg, l))) |> Maybe.withDefault ("<no change>", [])

handleResultToString_: String -> HandlePreviousResult -> String
handleResultToString_ indent handleResult = case handleResult of
  HandlePreviousResult msg _ -> "\n" ++ indent ++ "Prev " ++ msg

--  Fork msg u actions -> "\n" ++ indent ++ "Fork["++ updateStackName_ (indent ++ " ") u ++" => "  ++
--      String.join ", " (List.map (handleResultToString_ (indent ++ " ")) (LazyList.toList actions)) ++ "] " ++ msg

updateStackName: UpdateStack ->  String
updateStackName = updateStackName_ ""

updateStackName_: String -> UpdateStack ->  String
updateStackName_ indent u = case u of
  UpdateResultS _ exp mb -> "\n" ++ indent ++ "Res(" ++Syntax.unparser Syntax.Elm exp.val ++ (Maybe.map (handleResultToString_ indent) mb |> Maybe.withDefault "") ++ ")"
  UpdateContextS _ exp _ o _ (Just n) -> "\n" ++ indent ++ "Contn(" ++Syntax.unparser Syntax.Elm exp ++ " <-- " ++ outputToString o ++ ")[" ++ handleResultToString_ (indent ++ " ") n ++ "]"
  UpdateContextS _ e _ o _ Nothing->   "\n" ++ indent ++ "Ctx " ++ Syntax.unparser Syntax.Elm e ++ "<--" ++ outputToString o
  UpdateResultAlternative msg u ll -> "\n" ++ indent ++ "Alt("++updateStackName u ++") then [" ++ (case Lazy.force ll of
      Just u -> updateStackName_ (indent ++ " ") u ++ "]"
      Nothing -> "]"
    )
  UpdateFails msg  -> "\n" ++ indent ++ "UpdateFails " ++ msg
  UpdateCriticalError msg -> "\n" ++ indent ++ "UpdateCriticalError " ++ msg

type UpdateStack = UpdateResultS     UpdatedEnv UpdatedExp (Maybe HandlePreviousResult)
                 | UpdateContextS    Env Exp PrevOutput Output VDiffs (Maybe HandlePreviousResult)
                 | UpdateResultAlternative String UpdateStack (Lazy.Lazy (Maybe UpdateStack))
                 | UpdateFails String -- Soft fails, might try other branches. If no branch is available, will report this error.
                 | UpdateCriticalError String
                 -- The new expression, the new output, what to add to the stack with the result of the update above.

updateResultSameEnvExp: Env -> Exp -> UpdateStack
updateResultSameEnvExp env exp = updateResult (UpdatedEnv.original env) (UpdatedExp exp Nothing)

updateResultSameEnv: Env -> Exp -> UpdateStack
updateResultSameEnv env exp = updateResult (UpdatedEnv.original env) (UpdatedExp exp (Just <| EConstDiffs EAnyDiffs))

updateResultSameEnvDiffs: Env -> Exp -> EDiffs-> UpdateStack
updateResultSameEnvDiffs env exp diffs = updateResult (UpdatedEnv.original env) (UpdatedExp exp (Just <| diffs))


updateResult: UpdatedEnv-> UpdatedExp -> UpdateStack
updateResult updatedEnv exp = UpdateResultS updatedEnv exp Nothing

updateContinue: String -> Env -> Exp -> Val -> Val -> VDiffs -> (UpdatedEnv -> UpdatedExp -> UpdateStack) -> UpdateStack
updateContinue msg env exp oldVal newVal newValDiffs continuation = UpdateContextS env exp oldVal newVal newValDiffs (Just (HandlePreviousResult msg <| continuation))

updateContext: String -> Env -> Exp -> Val -> Val -> VDiffs -> UpdateStack
updateContext msg env exp oldVal newVal newValDiffs = UpdateContextS env exp oldVal newVal newValDiffs Nothing

type alias Output = Val
type alias PrevOutput = Val

outputToString: Output -> String
outputToString = strVal

updateResults :  UpdateStack -> (Lazy.Lazy (LazyList UpdateStack)) -> UpdateStack
updateResults updateStack lazyAlternatives =
  UpdateResultAlternative "fromUpdateREsult" updateStack (lazyAlternatives |> Lazy.map (\alternatives ->
    case alternatives of
      LazyList.Nil -> Nothing
      LazyList.Cons uStack lazyTail ->
        Just (updateResults uStack lazyTail)
  ))

updateResultList: String -> LazyList UpdateStack -> UpdateStack
updateResultList msg alternatives =
  case alternatives of
    LazyList.Nil -> UpdateFails msg
    LazyList.Cons uStack lazyTail -> updateResults uStack lazyTail

updateContinueRepeat: String -> Env -> Exp -> PrevOutput -> Output -> Results String (Maybe VDiffs)->
                      (Lazy.Lazy (LazyList (Output, Results String (Maybe VDiffs)))) -> (UpdatedEnv -> UpdatedExp -> UpdateStack) -> UpdateStack
updateContinueRepeat msg env e oldVal newVal diffsResults otherNewValModifs continuation =
  updateMany diffsResults
    (\() -> continuation (UpdatedEnv.original env) (UpdatedExp e Nothing))
    <| \diffs ->
  updateContinue msg env e oldVal newVal diffs <| \newUpdatedEnv newUpdatedE ->
    UpdateResultAlternative
      ("Alternative to " ++ msg)
      (UpdateResultS newUpdatedEnv newUpdatedE <| Just (HandlePreviousResult ("alternative continuation to " ++ msg) continuation))
      (otherNewValModifs |> Lazy.map
        (\ll ->
          case ll of
            LazyList.Nil -> Nothing
            LazyList.Cons (head, headModifs) lazyTail ->
              Just <| updateContinueRepeat msg env e oldVal head headModifs lazyTail continuation
        )
    )

updateAlternatives: String -> Env -> Exp -> PrevOutput -> LazyList (Output, Results String (Maybe VDiffs)) -> (UpdatedEnv -> UpdatedExp -> UpdateStack) -> UpdateStack
updateAlternatives msg env e oldVal newValsDiffs continuation =
  case newValsDiffs of
    LazyList.Nil -> UpdateFails <| "No solution for " ++ msg
    LazyList.Cons (head, headModifs) lazyTail -> updateContinueRepeat msg env e oldVal head headModifs lazyTail continuation

updateMaybeFirst: String -> (Maybe (UpdateStack, Bool)) -> (() -> UpdateStack) -> UpdateStack
updateMaybeFirst msg mb ll =
   case mb of
     Nothing -> ll ()
     Just (u, b) ->
       if b then
         UpdateResultAlternative msg u (Lazy.lazy <| (\_ -> Just <| ll ()))
       else
         u

updateMaybeFirst2: String -> Bool -> Maybe UpdateStack -> (() -> Maybe UpdateStack) -> Maybe (UpdateStack, Bool)
updateMaybeFirst2 msg canContinueAfter mb ll =
   case mb of
     Nothing -> ll () |> Maybe.map (\u -> (u, canContinueAfter))
     Just u ->
       Just <| (UpdateResultAlternative msg u (Lazy.lazy <| (\_ -> ll ())), canContinueAfter)



-- Constructor for updating multiple expressions evaluated in the same environment.
updateContinueMultiple: String -> Env -> Exp -> List (Exp, PrevOutput, Output) -> TupleDiffs VDiffs -> (UpdatedEnv -> UpdatedExpTuple -> UpdateStack) -> UpdateStack
updateContinueMultiple  msg       env    eee    totalExpValOut                    diffs                continuation  =
  let totalExp = withDummyExpInfo <| EList space0 (totalExpValOut |> List.map (\(e, _, _) -> (space1, e))) space0 Nothing space0 in
  let aux: Int -> List Exp   -> TupleDiffs EDiffs -> UpdatedEnv -> List (Exp, PrevOutput, Output) ->TupleDiffs VDiffs -> UpdateStack
      aux  i      revAccExps    revAccEDiffs         updatedEnvAcc expValOut                        diffs =
        case diffs of
         [] ->
           let finalExpTupleDiffs = case List.reverse revAccEDiffs of
             [] -> Nothing
             l -> Just l
           in
           continuation updatedEnvAcc (UpdatedExpTuple (List.reverse (UpdateUtils.reverseInsert (List.map (\(e, _, _) -> e) expValOut) revAccExps)) finalExpTupleDiffs)
         (j, m) :: td ->
            if j > i then
              let (unchanged, remaining) = Utils.split (j - i) expValOut in
              let unchangedExps = unchanged |> List.map (\(e, _, _) -> e) in
               aux j (UpdateUtils.reverseInsert unchangedExps revAccExps) revAccEDiffs updatedEnvAcc remaining diffs
            else if j < i then Debug.crash <| "Unexpected modification index : " ++ toString j ++ ", expected " ++ toString i ++ " or above."
            else
              case expValOut of
                (e, v, out)::tail ->
                  updateContinue (toString (i + 1) ++ "/" ++ toString (List.length totalExpValOut) ++ " " ++ msg) env e v out m <|  \newUpdatedEnv newUpdatedExp ->
                    --let _ = Debug.log "started tricombine" () in
                    let newUpdatedEnvAcc = UpdatedEnv.merge eee m env updatedEnvAcc newUpdatedEnv in
                    let newRevAccEDiffs = case newUpdatedExp.changes of
                      Nothing -> revAccEDiffs
                      Just d -> (i, d)::revAccEDiffs
                    in
                    --let _ = Debug.log "Finished tricombine" () in
                    aux (i + 1) (newUpdatedExp.val::revAccExps) newRevAccEDiffs newUpdatedEnvAcc tail td
                [] -> Debug.crash <| msg ++
                   "Expected at least one element because it was modified at index " ++ toString j ++
                   ", got nothing. We are at index " ++ toString i ++ " / length = " ++ toString (List.length totalExpValOut)
  in aux 0 [] [] (UpdatedEnv.original env) totalExpValOut diffs

updateManyMbHeadTail: a -> Lazy.Lazy (LazyList (Maybe a)) -> (a -> UpdateStack) -> UpdateStack
updateManyMbHeadTail  firstdiff otherdiffs builder =
  updateResults (builder firstdiff) (otherdiffs |> Lazy.map (\otherdiffs -> otherdiffs |> LazyList.filterMap identity |> LazyList.map builder))

-- Constructor for combining multiple expressions evaluated in the same environment, when there are multiple values available.
updateOpMultiple: String-> Env -> List Exp -> (List Exp -> Exp) -> List PrevOutput -> LazyList (List Output, Results String (Maybe (TupleDiffs VDiffs))) -> UpdateStack
updateOpMultiple  hint     env    es          eBuilder             prevOutputs        outputs =
  {-let _ = Debug.log ("updateOpMultiple called with " ++ String.join "," (List.map (Syntax.unparser Syntax.Elm) es) ++
          "\nprevOutputs = " ++ (List.map valToString prevOutputs |> String.join ",") ++
          "\nupdates = \n<--" ++ (outputs |> LazyList.toList |> List.map (\(o, d) -> (List.map valToString o |> String.join ",") ++ " (diffs " ++ toString d++ ")" ) |> String.join "\n<-- ")
      ) () in-}
  let aux: Int -> List Output -> Results String (Maybe (TupleDiffs VDiffs))-> Lazy.Lazy (LazyList (List Output, Results String (Maybe (TupleDiffs VDiffs)))) -> UpdateStack
      aux  nth    outputsHead    diffResult                                   lazyTail =
    let continue = case diffResult of
       Errs msg ->
         \continuation -> UpdateCriticalError msg
       Oks LazyList.Nil ->
         \continuation -> UpdateCriticalError "[internal error] Empty diffs in updateOpMultiple"
       Oks (LazyList.Cons Nothing _) ->
         \continuation -> continuation (UpdatedEnv.original env) (UpdatedExpTuple es Nothing)
       Oks (LazyList.Cons (Just d) tail) ->
         \continuation -> updateManyMbHeadTail d tail <| \diff ->
           updateContinueMultiple (hint ++ " #" ++ toString nth) env (eList es Nothing) (Utils.zip3 es prevOutputs outputsHead) diff continuation
    in
       UpdateResultAlternative "UpdateResultAlternative maybeOp"
         (continue <| \newUpdatedEnv newUpdatedOpArgs ->
           let newUpdatedExp = UpdatedExp (eBuilder newUpdatedOpArgs.val) (Maybe.map EChildDiffs newUpdatedOpArgs.changes) in
           updateResult newUpdatedEnv newUpdatedExp)
         (lazyTail |> Lazy.map (\ll ->
           --let _ = Debug.log ("Starting to evaluate another alternative if it exists ") () in
           case ll of
             LazyList.Nil -> Nothing
             LazyList.Cons (newHead, newHeadDiffs) newLazyTail -> Just <| aux (nth + 1) newHead newHeadDiffs newLazyTail
         ))
  in
  case outputs of
    LazyList.Nil -> UpdateFails <| "[Internal error] No result for updating " ++ hint
    LazyList.Cons (outputsHead, headDiffs) lazyTail ->
      aux 1 outputsHead headDiffs lazyTail

updateMany: Results String (Maybe vdiffs) -> (() -> UpdateStack) -> (vdiffs -> UpdateStack) -> UpdateStack
updateMany diffResult onSame builder =
  case diffResult of
    Errs msg -> UpdateCriticalError msg
    Oks ll ->
      case ll of
        LazyList.Nil -> UpdateCriticalError "[internal error] Expected at least one diff, got nothing"
        LazyList.Cons Nothing _ -> onSame ()
        LazyList.Cons (Just d) tail ->
          updateManyMbHeadTail d tail builder

updateManyHeadTail: a -> Lazy.Lazy (LazyList a) -> (a -> UpdateStack) -> UpdateStack
updateManyHeadTail  firstdiff otherdiffs builder =
  updateResults (builder firstdiff) (otherdiffs |> Lazy.map (\otherdiffs -> otherdiffs |> LazyList.map builder))

updateManys: Results String (List (Maybe vdiffs)) -> (List (Maybe vdiffs) -> UpdateStack) -> UpdateStack
updateManys diffResult builder =
  case diffResult of
    Errs msg -> UpdateCriticalError msg
    Oks ll ->
      case ll of
        LazyList.Nil -> UpdateCriticalError "[internal error] Expected at least one diff, got nothing"
        LazyList.Cons l tail ->
          updateManyHeadTail l tail builder
