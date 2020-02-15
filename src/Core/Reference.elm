module Core.Reference exposing
  ( BenchmarkInput
  , benchmarkInputs
  )

import Random exposing (Generator)
import Set exposing (Set)

import PBESuite
import Core.Denotation as Denotation exposing (Denotation)
import Core.Sample as Sample

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
  , functionName : String
  , args : Int
  , suiteInfo : SuiteInfo
  , da : Denotation a
  , db : Denotation b
  , input : Generator a
  , func : a -> b
  }

list_stutter =
  { name = "list_stutter"
  , functionName = "listStutter"
  , args = 1
  , suiteInfo = si PBESuite.list_stutter
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
              y :: y :: f ys
      in
        f
  }

list_take =
  { name = "list_take"
  , functionName = "listTake"
  , args = 2
  , suiteInfo = si PBESuite.list_take
  , da = Denotation.args2 Denotation.int (Denotation.simpleList Denotation.int)
  , db = Denotation.simpleList Denotation.int
  , input = Random.pair Sample.nat Sample.natList
  , func =
      let
        f : (Int, List Int) -> List Int
        f (n, xs) =
          List.take n xs
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
  String -> Int -> Denotation a -> Denotation b -> Generator (Set (Set (a, b)))
    -> Generator (List (Int, String))
examples functionName args da db =
  let
    extract ios =
      ( List.length ios
      , Debug.log "Generated Examples:" <| "specifyFunction"
          ++ (if args == 1 then "" else toString args)
          ++ " "
          ++ functionName
          ++ "\n[ "
          ++ String.join
               "\n, "
               (List.map (\(x, y) -> "(" ++ da x ++ ", " ++ db y ++ ")") ios)
          ++ "\n, (0, [1,2], []), (2, [], [])]"
      )
  in
    Random.map (Set.toList >> List.map (Set.toList >> extract))

createBenchmarkInput :
  Int -> Int -> Reference a b -> Generator (List BenchmarkInput)
createBenchmarkInput
 n k { name, functionName, args, suiteInfo, da, db, input, func } =
  Random.map
    ( List.map <|
        \(exampleCount, exampleString) ->
          { name = name
          , definitions = suiteInfo.definitions
          , fullExamples = suiteInfo.fullExamples
          , restrictedExamples =
              Just { code = exampleString, count = exampleCount }
          }
    )
    ( examples functionName args da db (Sample.trial n k func input)
    )

benchmarkInputs : Int -> Int -> Generator (List (List BenchmarkInput))
benchmarkInputs n k =
  Sample.sequence
    [ createBenchmarkInput n k list_take
    ]
