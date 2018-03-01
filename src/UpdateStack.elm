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

type NextAction = HandlePreviousResult String (Env -> Exp -> UpdateStack)
                | Fork String UpdateStack (LazyList NextAction)

nextActionsToString = nextActionsToString_ ""
nextActionsToString_ indent nextAction = case nextAction of
  HandlePreviousResult msg _ -> "\n" ++ indent ++ "Prev " ++ msg
  Fork msg u actions -> "\n" ++ indent ++ "Fork["++ updateStackName_ (indent ++ " ") u ++" => "  ++
      String.join ", " (List.map (nextActionsToString_ (indent ++ " ")) (LazyList.toList actions)) ++ "] " ++ msg

updateStackName = updateStackName_ ""
updateStackName_ indent u = case u of
  UpdateResult _ exp -> "\n" ++ indent ++ "Res(" ++Syntax.unparser Syntax.Elm exp ++ ")"
  UpdateContinue _ exp _ o n -> "\n" ++ indent ++ "Contn(" ++Syntax.unparser Syntax.Elm exp ++ " <-- " ++ outputToString o ++ ")[" ++ nextActionsToString_ (indent ++ " ") n ++ "]"
  UpdateContext _ e _ o -> "\n" ++ indent ++ "Ctx " ++ Syntax.unparser Syntax.Elm e ++ "<--" ++ outputToString o
  UpdateResultAlternative msg _ exp ll -> "\n" ++ indent ++ "Alt("++Syntax.unparser Syntax.Elm exp ++") then [" ++ (case Lazy.force ll of
      Just u -> updateStackName_ (indent ++ " ") u ++ "]"
      Nothing -> "]"
    )
  UpdateError msg  -> "\n" ++ indent ++ "UpdateError " ++ msg

type UpdateStack = UpdateResult      Env Exp
                 | UpdateContinue    Env Exp PrevOutput Output NextAction
                 | UpdateContext     Env Exp PrevOutput Output
                 | UpdateResultAlternative String Env Exp (Lazy.Lazy (Maybe UpdateStack))
                 | UpdateError String
                 -- The new expression, the new output, what to add to the stack with the result of the update above.

type alias Output = Val
type alias PrevOutput = Val

outputToString = strVal

updateResults :  Env -> Exp -> (Lazy.Lazy (LazyList (Env, Exp))) -> UpdateStack
updateResults fEnv fOut lazyAlternatives =
  UpdateResultAlternative "fromUpdateREsult" fEnv fOut (lazyAlternatives |> Lazy.map (\alternatives ->
    case alternatives of
      LazyList.Nil -> Nothing
      LazyList.Cons (newEnv, newOut) lazyTail ->
        Just (updateResults newEnv newOut lazyTail)
  ))

