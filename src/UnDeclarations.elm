module UnDeclarations exposing
  ( World
  , Constraint
  , HoleFilling
  )

import Dict exposing (Dict)

import Lang exposing (Exp, HoleId)
import UnExp exposing (UnExp)
import Example exposing (Example)

type alias World =
  (UnExp.Env, Example)

type alias Constraint =
  (HoleId, World)

type alias HoleFilling =
  Dict HoleId (List Exp)
