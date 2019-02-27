module UnDeclarations exposing
  ( World
  , Worlds
  , Constraint
  , Constraints
  , HoleFilling
  )

import Dict exposing (Dict)

import Lang exposing (Exp, HoleId)
import UnExp exposing (UnExp)
import Example exposing (Example)

type alias World =
  (UnExp.Env, Example)

type alias Worlds =
  List World

type alias Constraint =
  (HoleId, World)

type alias Constraints =
  List Constraint

type alias HoleFilling =
  Dict HoleId Exp
