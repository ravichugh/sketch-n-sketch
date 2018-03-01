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


-- TODO: Split the list of NextAction to HandlePreviousResult (for continuation wrapper) and a list of Forks
type NextAction = HandlePreviousResult String (Env -> Exp -> UpdateStack)
                | Fork String UpdateStack (LazyList NextAction)

nextActionsToString = nextActionsToString_ ""
nextActionsToString_ indent nextAction = case nextAction of
  HandlePreviousResult msg _ -> "\n" ++ indent ++ "Prev " ++ msg
  Fork msg u actions -> "\n" ++ indent ++ "Fork["++ updateStackName_ (indent ++ " ") u ++" => "  ++
      String.join ", " (List.map (nextActionsToString_ (indent ++ " ")) (LazyList.toList actions)) ++ "] " ++ msg

updateStackName = updateStackName_ ""
updateStackName_ indent u = case u of
  UpdateResultS _ exp mb -> "\n" ++ indent ++ "Res(" ++Syntax.unparser Syntax.Elm exp ++ (Maybe.map (nextActionsToString_ indent) mb |> Maybe.withDefault "") ++ ")"
  UpdateContextS _ exp _ o (Just n) -> "\n" ++ indent ++ "Contn(" ++Syntax.unparser Syntax.Elm exp ++ " <-- " ++ outputToString o ++ ")[" ++ nextActionsToString_ (indent ++ " ") n ++ "]"
  UpdateContextS _ e _ o Nothing->   "\n" ++ indent ++ "Ctx " ++ Syntax.unparser Syntax.Elm e ++ "<--" ++ outputToString o
  UpdateResultAlternative msg u ll -> "\n" ++ indent ++ "Alt("++updateStackName u ++") then [" ++ (case Lazy.force ll of
      Just u -> updateStackName_ (indent ++ " ") u ++ "]"
      Nothing -> "]"
    )
  UpdateError msg  -> "\n" ++ indent ++ "UpdateError " ++ msg

type UpdateStack = UpdateResultS     Env Exp (Maybe NextAction)
                 | UpdateContextS    Env Exp PrevOutput Output (Maybe NextAction)
                 | UpdateResultAlternative String UpdateStack (Lazy.Lazy (Maybe UpdateStack))
                 | UpdateError String
                 -- The new expression, the new output, what to add to the stack with the result of the update above.

updateResult env exp = UpdateResultS env exp Nothing
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
  updateContinue env e oldVal newVal <| HandlePreviousResult "updateContinueRepeat" <| \newEnv newE ->
    UpdateResultAlternative "Alternative updateContinueRepeat" (UpdateResultS newEnv newE <| Just nextAction) (otherNewVals |> Lazy.map
      (\ll ->
        case ll of
          LazyList.Nil -> Nothing
          LazyList.Cons head lazyTail ->
            Just <| updateContinueRepeat env e oldVal head lazyTail nextAction
      )
    )

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