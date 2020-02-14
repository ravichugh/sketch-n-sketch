module Core.Reference exposing
  ( benchmarkInputs
  )

import Random exposing (Generator)

import PBESuite
import Denotation
import Sample

--------------------------------------------------------------------------------
-- Suite Interface
--------------------------------------------------------------------------------

type alias SuiteInfo =
  { definitions : String
  , fullExamples : { code : String, count : Int }
  }

si : (String, String, Int, String, Int) -> SuiteInfo
si (definitions, fullExamples, fullExampleCount, _, _) =
  { definitions = definitions
  , fullExamples = { code = fullExamples, count = fullExampleCount }
  }

--------------------------------------------------------------------------------
-- References
--------------------------------------------------------------------------------

type alias Reference a b =
  { name : String
  , args : Int
  , suiteInfo : SuiteInfo
  , da : Denotation a
  , db : Denotation b
  , input : Generator a
  , func : a -> b
  }

list_stutter =
  { name = "list_stutter"
  , args = 1
  , suiteInfo = si PBE.list_stutter
  , da = Denotation.simpleList Denotation.int
  , db = Denotation.simpleList Denotation.int
  , input = Sample.natList
  , func =
      let
        f : List Int -> List Int
        f xs =
          case xs of
            [] ->
              []

            y :: ys ->
              y :: y :: list_stutter_ref ys
      in
        f
  }

--------------------------------------------------------------------------------
-- Benchmarking Interface
--------------------------------------------------------------------------------

type alias BenchmarkInput =
  { name : String
  , definitions : String
  , fullExamples : { code : String, count : Int }
  , restrictedExamples : Maybe { code : String, count : Int }
  }

examples :
  Int -> Denotation a -> Denotation b -> Generator (Set (Set (a, b)))
    -> Generator (List (Int, String))
examples args da db =
  let
    extract ios =
      ( List.length ios
      , "specifyFunction"
          ++ toString args
          ++ "\n[ "
          ++ String.join
               "\n, "
               (List.map ((\x, y) -> "(" ++ da x ++ ", " ++ da y ++ ")"))
          ++ "\n]"
      )
  in
    Random.map (Set.toList >> List.map (Set.toList >> extract))

createBenchmarkInput :
  Int -> Int -> Reference a b -> Generator (List BenchmarkInput)
createBenchmarkInput n k { name, args, suiteInfo, da, db, input, func } =
  Random.map
    ( \(exampleCount, exampleString) ->
        { name = name
        , definitions = suiteInfo.definitions
        , fullExamples = suiteInfo.fullExamples
        , restrictedExamples =
            Just { code = exampleString, count = exampleCount }
        }
    )
    ( examples args da db (trial n k func input)
    )

benchmarkInputs : Int -> Int -> Generator (List (List BenchmarkInput))
benchmarkInputs n k =
  Sample.sequence
    [ createBenchmarkInput n k list_stutter
    ]
