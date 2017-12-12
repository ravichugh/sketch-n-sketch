module Update where

import Lang exposing (..)

update : Env -> Exp -> Val -> Val -> (Exp, Env)
update env e oldVal newVal =
  case e.val.e__
  e
