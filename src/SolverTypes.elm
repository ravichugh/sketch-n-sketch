module SolverTypes exposing (..)

import Dict exposing (Dict)

import Lang exposing (MathExp)


type alias Eqn = (MathExp, MathExp) -- LHS, RHS

type alias Problem = (List Eqn, List Int) -- System of equations, and varIds to solve for (usually a singleton).

type alias Solution = List (MathExp, Int)

type alias SolutionsCache =
  { eqnSystemSolutions : Dict Problem (List Solution)
  , simplifications    : Dict MathExp MathExp
  }


mapSolutionsExps : (MathExp -> MathExp) -> List Solution -> List Solution
mapSolutionsExps f solutions =
  solutions
  |> List.map (List.map (\(mathExp, varId) -> (f mathExp, varId)))

