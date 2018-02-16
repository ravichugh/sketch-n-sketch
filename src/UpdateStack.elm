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