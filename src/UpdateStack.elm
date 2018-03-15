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
  UpdateContextS _ exp _ o _ (Just n) -> "\n" ++ indent ++ "Contn(" ++Syntax.unparser Syntax.Elm exp ++ " <-- " ++ outputToString o ++ ")[" ++ nextActionsToString_ (indent ++ " ") n ++ "]"
  UpdateContextS _ e _ o _ Nothing->   "\n" ++ indent ++ "Ctx " ++ Syntax.unparser Syntax.Elm e ++ "<--" ++ outputToString o
  UpdateResultAlternative msg u ll -> "\n" ++ indent ++ "Alt("++updateStackName u ++") then [" ++ (case Lazy.force ll of
      Just u -> updateStackName_ (indent ++ " ") u ++ "]"
      Nothing -> "]"
    )
  UpdateError msg  -> "\n" ++ indent ++ "UpdateError " ++ msg

type UpdateStack = UpdateResultS     UpdatedEnv Exp (Maybe NextAction)
                 | UpdateContextS    Env Exp PrevOutput Output VDiffs (Maybe NextAction)
                 | UpdateResultAlternative String UpdateStack (Lazy.Lazy (Maybe UpdateStack))
                 | UpdateError String
                 -- The new expression, the new output, what to add to the stack with the result of the update above.

updateResultSameEnv: Env -> Exp -> UpdateStack
updateResultSameEnv env exp = UpdateResultS (UpdatedEnv.original env) exp Nothing

updateResult: UpdatedEnv-> Exp -> UpdateStack
updateResult updatedEnv exp = UpdateResultS updatedEnv exp Nothing

updateContinue: String -> Env -> Exp -> Val -> Val -> VDiffs -> (UpdatedEnv -> Exp -> UpdateStack) -> UpdateStack
updateContinue msg env exp oldVal newVal defaultVDiffs continuation = UpdateContextS env exp oldVal newVal defaultVDiffs (Just (HandlePreviousResult msg <| continuation))

updateContext: String -> Env -> Exp -> Val -> Val -> VDiffs -> UpdateStack
updateContext msg env exp oldVal newVal defaultVDiffs = UpdateContextS env exp oldVal newVal defaultVDiffs Nothing

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

updateContinueRepeat: String -> Env -> Exp -> PrevOutput -> Output -> Result String VDiffs-> (Lazy.Lazy (LazyList (Output, Result String VDiffs))) -> (UpdatedEnv -> Exp -> UpdateStack) -> UpdateStack
updateContinueRepeat msg env e oldVal newVal diffsResult otherNewValModifs continuation =
  case diffsResult of
    Err msg -> UpdateError msg
    Ok diffs ->
      updateContinue msg env e oldVal newVal diffs <| \newUpdatedEnv newE ->
        UpdateResultAlternative ("Alternative to " ++ msg) (UpdateResultS newUpdatedEnv newE <| Just (HandlePreviousResult ("alternative continuation to " ++ msg) continuation)) (otherNewValModifs |> Lazy.map
          (\ll ->
            case ll of
              LazyList.Nil -> Nothing
              LazyList.Cons (head, headModifs) lazyTail ->
                Just <| updateContinueRepeat msg env e oldVal head headModifs lazyTail continuation
          )
        )

updateAlternatives: String -> Env -> Exp -> PrevOutput -> LazyList (Output, Result String VDiffs) -> (UpdatedEnv -> Exp -> UpdateStack) -> UpdateStack
updateAlternatives msg env e oldVal newValsDiffs continuation =
  case newValsDiffs of
    LazyList.Nil -> UpdateError <| "No solution for " ++ msg
    LazyList.Cons (head, headModifs) lazyTail -> updateContinueRepeat msg env e oldVal head headModifs lazyTail continuation

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
updateContinueMultiple: String -> Env -> List (Exp, PrevOutput, Output) -> List (Int, VDiffs) -> (UpdatedEnv -> List Exp -> UpdateStack) -> UpdateStack
updateContinueMultiple  msg       env    totalExpValOut                    diffs                 continuation  =
  let totalExp = withDummyExpInfo <| EList space0 (totalExpValOut |> List.map (\(e, _, _) -> (space1, e))) space0 Nothing space0 in
  let aux: Int -> List Exp -> UpdatedEnv -> List (Exp, PrevOutput, Output) -> List (Int, VDiffs) -> UpdateStack
      aux  i      revAccExps  updatedEnvAcc expValOut                         diffs =
        case diffs of
         [] ->
           continuation updatedEnvAcc (List.reverse (UpdateUtils.reverseInsert (List.map (\(e, _, _) -> e) expValOut) revAccExps))
         (j, m) :: td ->
            if j > i then
              let (unchanged, remaining) = Utils.split (j - i) expValOut in
              let unchangedExps = unchanged |> List.map (\(e, _, _) -> e) in
               aux j (UpdateUtils.reverseInsert unchangedExps revAccExps) updatedEnvAcc remaining diffs
            else if j < i then Debug.crash <| "Unexpected modification index : " ++ toString j ++ ", expected " ++ toString i ++ " or above."
            else
              case expValOut of
                (e, v, out)::tail ->
                  updateContinue (toString (i + 1) ++ "/" ++ toString (List.length totalExpValOut) ++ " " ++ msg) env e v out m <|  \newUpdatedEnv newExp ->
                    --let _ = Debug.log "started tricombine" () in
                    let newUpdatedEnvAcc = UpdatedEnv.merge env updatedEnvAcc newUpdatedEnv in
                    --let _ = Debug.log "Finished tricombine" () in
                    aux (i + 1) (newExp::revAccExps) newUpdatedEnvAcc tail td
                [] -> Debug.crash <| msg ++
                   "Expected at least one element because it was modified at index " ++ toString j ++
                   ", got nothing. We are at index " ++ toString i ++ " / length = " ++ toString (List.length totalExpValOut)
  in aux 0 [] (UpdatedEnv.original env) totalExpValOut diffs

-- Constructor for combining multiple expressions evaluated in the same environment, when there are multiple values available.
updateOpMultiple: String-> Env -> List Exp -> (List Exp -> Exp) -> List PrevOutput -> LazyList (List Output, Result String (List (Int, VDiffs))) -> UpdateStack
updateOpMultiple  hint     env    es          eBuilder             prevOutputs        outputs =
  let aux: Int -> List Output -> Result String (List (Int, VDiffs))      -> Lazy.Lazy (LazyList (List Output, Result String (List (Int, VDiffs)))) -> UpdateStack
      aux  nth    outputsHead    diffResult                                 lazyTail =
    case diffResult of
       Err msg -> UpdateError msg
       Ok diff ->
         updateContinueMultiple (hint ++ " #" ++ toString nth) env (Utils.zip3 es prevOutputs outputsHead) diff <| \newUpdatedEnv newOpArgs ->
          --let _ = Debug.log ("before an alternative " ++ (String.join "," <| List.map valToString head)) () in
            UpdateResultAlternative "UpdateResultAlternative maybeOp" (updateResult newUpdatedEnv (eBuilder newOpArgs))
              (lazyTail |> Lazy.map (\ll ->
                --let _ = Debug.log ("Starting to evaluate another alternative if it exists ") () in
                case ll of
                  LazyList.Nil -> Nothing
                  LazyList.Cons (newHead, newHeadDiffs) newLazyTail -> Just <| aux (nth + 1) newHead newHeadDiffs newLazyTail
              ))
  in
  case outputs of
    LazyList.Nil -> UpdateError <| "[Internal error] No result for updating " ++ hint
    LazyList.Cons (outputsHead, headDiffs) lazyTail ->
      aux 1 outputsHead headDiffs lazyTail

