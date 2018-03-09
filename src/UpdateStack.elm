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
import LangUtils exposing (envToString)
import Set exposing (Set)

-- Useful to merge environments faster.
-- Maybe will containn things like "insert a variable with these dependences"
type alias Modifications = { indices {-of modification in env-}: List Int }

-- If more modifications are needed, we can do it here.
mergeModifications newIndices modif1 modif2 = Modifications newIndices

type alias Modified a = { val: a, modifs: Modifications }

type alias ModifiedEnv = Modified Env

-- Declares an environment as unmodified
originalEnv env = Modified env <| Modifications []

-- Merges two modified environments
mergeModifiedEnv: Env -> ModifiedEnv -> ModifiedEnv -> ModifiedEnv
mergeModifiedEnv env env1 env2 =
  let (finalEnv, finalIndices) = mergeEnv env env1.modifs.indices env1.val env2.modifs.indices env2.val in
  Modified finalEnv (mergeModifications finalIndices env1.modifs env2.modifs)

-- Concatenates two modified environments, keeping track of where the modifications happened.
appendModifiedEnv: ModifiedEnv -> ModifiedEnv -> ModifiedEnv
appendModifiedEnv env1 env2 =
  let n = List.length env1.val in
  let indices2 = env2.modifs.indices |> List.map (\i -> i + n) in
  let env = env1.val ++ env2.val in
  let modifs = Modifications <| env1.modifs.indices ++ indices2 in
  Modified env modifs

-- Returns the first n elements, and the remaining elements
splitModifiedEnv: Int -> ModifiedEnv -> (ModifiedEnv, ModifiedEnv)
splitModifiedEnv n env =
  let (indices1, indices2Offset) = Utils.spanWhile (\i -> i < n) env.modifs.indices in
  let (env1, env2) = Utils.split n env.val in
  let indices2 = List.map (\i -> i - n) indices2Offset in
  (Modified env1 <| Modifications indices1, Modified env2 <| Modifications indices2)

isUnmodifiedEnv: ModifiedEnv -> Bool
isUnmodifiedEnv menv = menv.modifs.indices |> List.isEmpty

modifiedEnvToString: ModifiedEnv -> String
modifiedEnvToString modifiedEnv =
  let prunedEnv acc i m e = case (m, e) of
    ([], e) -> List.reverse acc
    (j::is, head::tail) -> if j == i then prunedEnv (head::acc) (i + 1) is tail else prunedEnv acc (i + 1) m tail
  in
  "modified:" ++ envToString (prunedEnv [] 0 modifiedEnv.modifs.indices modifiedEnv.val )

-- When comparing VClosures, how to get the modifications
createModifiedEnv: Set Ident -> Env -> Env -> ModifiedEnv
createModifiedEnv ks oldEnv newEnv = --Very slow process, we need to optimize that
  let aux i freeVariables accModifs accEnv oldEnv newEnv =
    if Set.isEmpty freeVariables then Modified (List.reverse accEnv ++ newEnv) accModifs
    else case (oldEnv, newEnv) of
       ([], []) -> Modified (List.reverse accEnv) (List.reverse accModifs)
       ((oldk, oldv)::oldtail, (newk, newv)::newtail) ->
         if oldk == newk then Debug.crash "Comparing tow environments which do not have the same order of keys:" ++ oldk ++ " /= " ++ newk
         else if Set.member oldk freeVariables then
           let newModifs = if valEqual oldv newv then accModifs else i::accModifs in
           aux (i + 1) (Set.remove oldk freeVariables) newModifs (newv::accEnv) oldtail newtail
         else aux (i + 1) freeVariables accModifs (newv::accEnv) oldtail newtail

-- TODO: Split the list of NextAction to HandlePreviousResult (for continuation wrapper) and a list of Forks
type NextAction = HandlePreviousResult String (ModifiedEnv -> Exp -> UpdateStack)
                | Fork String UpdateStack (LazyList NextAction)

nextActionsToString = nextActionsToString_ ""
nextActionsToString_ indent nextAction = case nextAction of
  HandlePreviousResult msg _ -> "\n" ++ indent ++ "Prev " ++ msg
  Fork msg u actions -> "\n" ++ indent ++ "Fork["++ updateStackName_ (indent ++ " ") u ++" => "  ++
      String.join ", " (List.map (nextActionsToString_ (indent ++ " ")) (LazyList.toList actions)) ++ "] " ++ msg

updateStackName = updateStackName_ ""
updateStackName_ indent u = case u of
  UpdateResultS _ _ exp mb -> "\n" ++ indent ++ "Res(" ++Syntax.unparser Syntax.Elm exp ++ (Maybe.map (nextActionsToString_ indent) mb |> Maybe.withDefault "") ++ ")"
  UpdateContextS _ exp _ o (Just n) -> "\n" ++ indent ++ "Contn(" ++Syntax.unparser Syntax.Elm exp ++ " <-- " ++ outputToString o ++ ")[" ++ nextActionsToString_ (indent ++ " ") n ++ "]"
  UpdateContextS _ e _ o Nothing->   "\n" ++ indent ++ "Ctx " ++ Syntax.unparser Syntax.Elm e ++ "<--" ++ outputToString o
  UpdateResultAlternative msg u ll -> "\n" ++ indent ++ "Alt("++updateStackName u ++") then [" ++ (case Lazy.force ll of
      Just u -> updateStackName_ (indent ++ " ") u ++ "]"
      Nothing -> "]"
    )
  UpdateError msg  -> "\n" ++ indent ++ "UpdateError " ++ msg

type UpdateStack = UpdateResultS     ModifiedEnv Exp (Maybe NextAction)
                 | UpdateContextS    Env Exp PrevOutput Output (Maybe NextAction)
                 | UpdateResultAlternative String UpdateStack (Lazy.Lazy (Maybe UpdateStack))
                 | UpdateError String
                 -- The new expression, the new output, what to add to the stack with the result of the update above.

updateResultSameEnv env exp = UpdateResultS (originalEnv env) exp Nothing
updateResult modifiedEnv exp = UpdateResultS modifiedEnv exp Nothing
updateContinue env exp oldVal newVal n = UpdateContextS env exp oldVal newVal (Just n)
updateContext env exp oldVal newVal = UpdateContextS env exp oldVal newVal Nothing

type alias Output = Val
type alias PrevOutput = Val

outputToString = strVal

updateResults :  UpdateStack -> (Lazy.Lazy (LazyList UpdateStack)) -> UpdateStack
updateResults updateStack lazyAlternatives =
  UpdateResultAlternative "fromUpdateREsult" updateStack (lazyAlternatives |> Lazy.map (\alternatives ->
    case alternatives of
      LazyList.Nil -> Nothing
      LazyList.Cons uStack lazyTail ->
        Just (updateResults uStack lazyTail)
  ))

updateContinueRepeat: Env -> Exp -> PrevOutput -> Output -> (Lazy.Lazy (LazyList Output)) -> NextAction -> UpdateStack
updateContinueRepeat env e oldVal newVal otherNewVals nextAction =
  updateContinue env e oldVal newVal <| HandlePreviousResult "updateContinueRepeat" <| \newEnv newModifs newE ->
    UpdateResultAlternative "Alternative updateContinueRepeat" (UpdateResultS newEnv newModifs newE <| Just nextAction) (otherNewVals |> Lazy.map
      (\ll ->
        case ll of
          LazyList.Nil -> Nothing
          LazyList.Cons head lazyTail ->
            Just <| updateContinueRepeat env e oldVal head lazyTail nextAction
      )
    )

updateAlternatives: String -> Env -> Exp -> PrevOutput -> LazyList Output -> NextAction -> UpdateStack
updateAlternatives msg env e oldVal newVals nextAction =
  case newVals of
    LazyList.Nil -> UpdateError msg
    LazyList.Cons head lazyTail -> updateContinueRepeat env e oldVal head lazyTail nextAction

updateMaybeFirst: (Maybe UpdateStack) -> (Bool -> UpdateStack) -> UpdateStack
updateMaybeFirst mb ll =
   case mb of
     Nothing -> ll False
     Just u -> UpdateResultAlternative "fromMaybeFirst" u (Lazy.lazy <| (ll |> \ll _ -> Just <| ll True))

updateMaybeFirst2: (Maybe UpdateStack) -> (Bool -> Maybe UpdateStack) -> Maybe UpdateStack
updateMaybeFirst2 mb ll =
   case mb of
     Nothing -> ll False
     Just u -> Just <| UpdateResultAlternative "fromMaybeFirst" u (Lazy.lazy <| (ll |> \ll _ -> ll True))


-- Constructor for updating multiple expressions evaluated in the same environment.
updateContinueMultiple: String -> Env -> List (Exp, PrevOutput, Output) -> (ModifiedEnv -> List Exp -> UpdateStack) -> UpdateStack
updateContinueMultiple msg env totalExpValOut continuation  =
  let totalExp = withDummyExpInfo <| EList space0 (totalExpValOut |> List.map (\(e, _, _) -> (space1, e))) space0 Nothing space0 in
  let aux i revAccExps modifiedEnvAcc expValOut =
        --let _ = Debug.log "continuing aux" () in
        case expValOut of
          [] ->
            --let _ = Debug.log "continuation" () in
            continuation modifiedEnvAcc (List.reverse revAccExps)
          (e, v, out)::tail ->
            --let _ = Debug.log "updateContinue" () in
            updateContinue env e v out <|
              HandlePreviousResult (toString i ++ "/" ++ toString (List.length totalExpValOut) ++ " " ++ msg)  <| \newModifiedEnv newExp ->
                --let _ = Debug.log "started tricombine" () in
                let newModifiedEnvAcc = mergeModifiedEnv env modifiedEnvAcc newModifiedEnv in
                --let _ = Debug.log "Finished tricombine" () in
                aux (i + 1) (newExp::revAccExps) newModifiedEnvAcc tail
  in aux 1 [] (originalEnv env) totalExpValOut

-- Constructor for combining multiple expressions evaluated in the same environment, when there are multiple values available.
updateOpMultiple: String-> Env -> List Exp -> (List Exp -> Exp) -> List PrevOutput -> LazyList (List Output) -> UpdateStack
updateOpMultiple hint env es eBuilder prevOutputs outputs=
  let aux nth outputsHead lazyTail =
  updateContinueMultiple (hint ++ " #" ++ toString nth) env (Utils.zip3 es prevOutputs outputsHead) (\newModifiedEnv newOpArgs ->
    --let _ = Debug.log ("before an alternative " ++ (String.join "," <| List.map valToString head)) () in
    UpdateResultAlternative "UpdateResultAlternative maybeOp" (updateResult newModifiedEnv (eBuilder newOpArgs))
       (lazyTail |> Lazy.map (\ll ->
         --let _ = Debug.log ("Starting to evaluate another alternative if it exists ") () in
         case ll of
           LazyList.Nil -> Nothing
           LazyList.Cons newHead newLazyTail -> Just <| aux (nth + 1) newHead newLazyTail
       )))
  in
  case outputs of
    LazyList.Nil -> UpdateError <| "[Internal error] No result for updating " ++ hint
    LazyList.Cons outputsHead lazyTail ->
      aux 1 outputsHead lazyTail

