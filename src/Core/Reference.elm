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
  , kMax : Int
  , suiteInfo : SuiteInfo
  , da : Denotation a
  , db : Denotation b
  , input : Generator a
  , baseCase : Generator a
  , func : a -> b
  }

list_stutter =
  { name = "list_stutter"
  , functionName = "listStutter"
  , args = 1
  , kMax = 5
  , suiteInfo = si PBESuite.list_stutter
  , da = Denotation.simpleList Denotation.int
  , db = Denotation.simpleList Denotation.int
  , input = Sample.natList
  , baseCase = Sample.constant []
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
  , kMax = 20
  , suiteInfo = si PBESuite.list_take
  , da = Denotation.args2 Denotation.int (Denotation.simpleList Denotation.int)
  , db = Denotation.simpleList Denotation.int
  , input = Random.pair Sample.nat Sample.natList
  , baseCase = Sample.constant (0, [])
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
          ++ "\n]"
      )
  in
    Random.map (Set.toList >> List.map (Set.toList >> extract))

createBenchmarkInput :
  Int -> Reference a b -> Generator (List (List BenchmarkInput))
createBenchmarkInput n
 { name, functionName, args, kMax, suiteInfo, da, db, input, baseCase, func } =
  List.range 1 kMax
    |> List.map
         ( \k ->
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
               ( examples
                   functionName
                   args
                   da
                   db
                   (Sample.trial n k func input baseCase)
               )
         )
  |> Sample.sequence

benchmarkInputs : Int -> Generator (List (List (List BenchmarkInput)))
benchmarkInputs n =
  Sample.sequence
    [ createBenchmarkInput n list_stutter
    , createBenchmarkInput n list_take
    ]
