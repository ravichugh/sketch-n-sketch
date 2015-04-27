module Eval where

import Lang exposing (..)
import LangParser exposing (prelude)

-- TODO move eval here

run : Exp -> Val
run e =
  let initEnv = snd (eval [] prelude) in
  eval_ initEnv e

