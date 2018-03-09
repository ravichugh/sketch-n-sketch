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
import UpdatedEnv exposing (UpdatedEnv, original)

-- TODO: Split the list of NextAction to HandlePreviousResult (for continuation wrapper) and a list of Forks
type NextAction = HandlePreviousResult String (UpdatedEnv -> Exp -> UpdateStack)
                | Fork String UpdateStack (LazyList NextAction)
nextActionsToString: NextAction -> String
nextActionsToString = nextActionsToString_ ""

nextActionsToString_: String -> NextAction -> String
nextActionsToString_ indent nextAction = case nextAction of
  HandlePreviousResult msg _ -> "\n" ++ indent ++ "Prev " ++ msg
  Fork msg u actions -> "\n" ++ indent ++ "Fork["++ updateStackName_ (indent ++ " ") u ++" => "  ++
      String.join ", " (List.map (nextActionsToString_ (indent ++ " ")) (LazyList.toList actions)) ++ "] " ++ msg

updateStackName: UpdateStack ->  String
updateStackName = updateStackName_ ""

updateStackName_: String -> UpdateStack ->  String
updateStackName_ indent u = case u of
  UpdateResultS _ exp mb -> "\n" ++ indent ++ "Res(" ++Syntax.unparser Syntax.Elm exp ++ (Maybe.map (nextActionsToString_ indent) mb |> Maybe.withDefault "") ++ ")"
  UpdateContextS _ exp _ o (Just n) -> "\n" ++ indent ++ "Contn(" ++Syntax.unparser Syntax.Elm exp ++ " <-- " ++ outputToString o ++ ")[" ++ nextActionsToString_ (indent ++ " ") n ++ "]"
  UpdateContextS _ e _ o Nothing->   "\n" ++ indent ++ "Ctx " ++ Syntax.unparser Syntax.Elm e ++ "<--" ++ outputToString o
  UpdateResultAlternative msg u ll -> "\n" ++ indent ++ "Alt("++updateStackName u ++") then [" ++ (case Lazy.force ll of
      Just u -> updateStackName_ (indent ++ " ") u ++ "]"
      Nothing -> "]"
    )
  UpdateError msg  -> "\n" ++ indent ++ "UpdateError " ++ msg

type UpdateStack = UpdateResultS     UpdatedEnv Exp (Maybe NextAction)
                 | UpdateContextS    Env Exp PrevOutput Output (Maybe NextAction)
                 | UpdateResultAlternative String UpdateStack (Lazy.Lazy (Maybe UpdateStack))
                 | UpdateError String
                 -- The new expression, the new output, what to add to the stack with the result of the update above.

updateResultSameEnv: Env -> Exp -> UpdateStack
updateResultSameEnv env exp = UpdateResultS (UpdatedEnv.original env) exp Nothing

updateResult: UpdatedEnv-> Exp -> UpdateStack
updateResult updatedEnv exp = UpdateResultS updatedEnv exp Nothing

updateContinue: Env -> Exp -> Val -> Val -> NextAction -> UpdateStack
updateContinue env exp oldVal newVal n = UpdateContextS env exp oldVal newVal (Just n)

updateContext: Env -> Exp -> Val -> Val -> UpdateStack
updateContext env exp oldVal newVal = UpdateContextS env exp oldVal newVal Nothing

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

updateContinueRepeat: Env -> Exp -> PrevOutput -> Output -> (Lazy.Lazy (LazyList Output)) -> NextAction -> UpdateStack
updateContinueRepeat env e oldVal newVal otherNewVals nextAction =
  updateContinue env e oldVal newVal <| HandlePreviousResult "updateContinueRepeat" <| \newUpdatedEnv newE ->
    UpdateResultAlternative "Alternative updateContinueRepeat" (UpdateResultS newUpdatedEnv newE <| Just nextAction) (otherNewVals |> Lazy.map
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
updateContinueMultiple: String -> Env -> List (Exp, PrevOutput, Output) -> (UpdatedEnv -> List Exp -> UpdateStack) -> UpdateStack
updateContinueMultiple msg env totalExpValOut continuation  =
  let totalExp = withDummyExpInfo <| EList space0 (totalExpValOut |> List.map (\(e, _, _) -> (space1, e))) space0 Nothing space0 in
  let aux: Int -> List Exp -> UpdatedEnv -> List (Exp, PrevOutput, Output) -> UpdateStack
      aux i revAccExps updatedEnvAcc expValOut =
        --let _ = Debug.log "continuing aux" () in
        case expValOut of
          [] ->
            --let _ = Debug.log "continuation" () in
            continuation updatedEnvAcc (List.reverse revAccExps)
          (e, v, out)::tail ->
            --let _ = Debug.log "updateContinue" () in
            updateContinue env e v out <|
              HandlePreviousResult (toString i ++ "/" ++ toString (List.length totalExpValOut) ++ " " ++ msg)  <| \newUpdatedEnv newExp ->
                --let _ = Debug.log "started tricombine" () in
                let newUpdatedEnvAcc = UpdatedEnv.merge env updatedEnvAcc newUpdatedEnv in
                --let _ = Debug.log "Finished tricombine" () in
                aux (i + 1) (newExp::revAccExps) newUpdatedEnvAcc tail
  in aux 1 [] (UpdatedEnv.original env) totalExpValOut

-- Constructor for combining multiple expressions evaluated in the same environment, when there are multiple values available.
updateOpMultiple: String-> Env -> List Exp -> (List Exp -> Exp) -> List PrevOutput -> LazyList (List Output) -> UpdateStack
updateOpMultiple hint env es eBuilder prevOutputs outputs=
  let aux: Int -> List Output -> (Lazy.Lazy (LazyList (List Output))) -> UpdateStack
      aux nth outputsHead lazyTail =
    updateContinueMultiple (hint ++ " #" ++ toString nth) env (Utils.zip3 es prevOutputs outputsHead) (\newUpdatedEnv newOpArgs ->
     --let _ = Debug.log ("before an alternative " ++ (String.join "," <| List.map valToString head)) () in
       UpdateResultAlternative "UpdateResultAlternative maybeOp" (updateResult newUpdatedEnv (eBuilder newOpArgs))
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

