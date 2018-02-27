module UpdateStack exposing  (..)
import Lang exposing (..)
import Results exposing
  ( Results(..)
  , ok1, oks, okLazy
  , LazyList(..)
  , appendLazy, appendLazyLazy, mapLazy, andThenLazy, isLazyNil
  , lazyFromList
  , lazyCons2)
import Lazy
import Syntax
import ValUnparser exposing (strVal)

type NextAction = HandlePreviousResult String (Env -> Exp -> UpdateStack)
                | Fork String UpdateStack (LazyList NextAction)

nextActionsToString = nextActionsToString_ ""
nextActionsToString_ indent nextAction = case nextAction of
  HandlePreviousResult msg _ -> "\n" ++ indent ++ "Prev " ++ msg
  Fork msg u actions -> "\n" ++ indent ++ "Fork["++ updateStackName_ (indent ++ " ") u ++" => "  ++
      String.join ", " (List.map (nextActionsToString_ (indent ++ " ")) (Results.toList actions)) ++ "] " ++ msg

updateStackName = updateStackName_ ""
updateStackName_ indent u = case u of
  UpdateResult _ exp -> "\n" ++ indent ++ "Res(" ++Syntax.unparser Syntax.Elm exp ++ ")"
  UpdateResults _ _ _ -> "\n" ++ indent ++ "Ress"
  UpdateContinue _ exp _ o n -> "\n" ++ indent ++ "Contn(" ++Syntax.unparser Syntax.Elm exp ++ " <-- " ++ outputToString o ++ ")[" ++ nextActionsToString_ (indent ++ " ") n ++ "]"
  UpdateContext _ e _ o -> "\n" ++ indent ++ "Ctx " ++ Syntax.unparser Syntax.Elm e ++ "<--" ++ outputToString o
  UpdateAlternative msg _ exp ll -> "\n" ++ indent ++ "Alt("++Syntax.unparser Syntax.Elm exp ++") then [" ++ (case Lazy.force ll of
      Just u -> updateStackName_ (indent ++ " ") u ++ "]"
      Nothing -> "]"
    )
  UpdateError msg  -> "\n" ++ indent ++ "UpdateError " ++ msg

type UpdateStack = UpdateResult      Env Exp
                 | UpdateResults     Env Exp (Lazy.Lazy (LazyList (Env, Exp)))
                 | UpdateContinue    Env Exp Val Output NextAction
                 | UpdateContext     Env Exp Val Output
                 | UpdateAlternative String Env Exp (Lazy.Lazy (Maybe UpdateStack))
                 | UpdateError String
                 -- The new expression, the new output, what to add to the stack with the result of the update above.


type Output = Raw Val | Program Exp

outputToString o = case o of
  Raw v -> strVal v
  Program e -> "(prog)" ++ Syntax.unparser Syntax.Elm e