module Core.Reference exposing
  ( BenchmarkInput
  , benchmarkInputs
  )

import MyRandom as Random exposing (Generator)
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
  , dIn : Denotation a
  , dOut : Denotation b
  , input : Generator a
  , baseCase : Maybe (Generator a)
  , func : a -> b
  }

-- Bool

bool_band =
  { name = "bool_band"
  , functionName = "and"
  , args = 2
  , kMax = 4
  , suiteInfo = si PBESuite.bool_band
  , dIn = Denotation.args2 Denotation.bool Denotation.bool
  , dOut = Denotation.bool
  , input = Random.pair Sample.bool Sample.bool
  , baseCase = Nothing
  , func =
      let
        f : (Bool, Bool) -> Bool
        f (x, y) =
          x && y
      in
        f
  }

bool_bor =
  { name = "bool_bor"
  , functionName = "or"
  , args = 2
  , kMax = 4
  , suiteInfo = si PBESuite.bool_bor
  , dIn = Denotation.args2 Denotation.bool Denotation.bool
  , dOut = Denotation.bool
  , input = Random.pair Sample.bool Sample.bool
  , baseCase = Nothing
  , func =
      let
        f : (Bool, Bool) -> Bool
        f (x, y) =
          x || y
      in
        f
  }

bool_impl =
  { name = "bool_impl"
  , functionName = "impl"
  , args = 2
  , kMax = 4
  , suiteInfo = si PBESuite.bool_impl
  , dIn = Denotation.args2 Denotation.bool Denotation.bool
  , dOut = Denotation.bool
  , input = Random.pair Sample.bool Sample.bool
  , baseCase = Nothing
  , func =
      let
        f : (Bool, Bool) -> Bool
        f (x, y) =
          (not x) || y
      in
        f
  }

bool_neg =
  { name = "bool_neg"
  , functionName = "neg"
  , args = 1
  , kMax = 2
  , suiteInfo = si PBESuite.bool_neg
  , dIn = Denotation.bool
  , dOut = Denotation.bool
  , input = Sample.bool
  , baseCase = Nothing
  , func =
      let
        f : Bool -> Bool
        f x =
          not x
      in
        f
  }

bool_xor =
  { name = "bool_xor"
  , functionName = "xor"
  , args = 2
  , kMax = 4
  , suiteInfo = si PBESuite.bool_xor
  , dIn = Denotation.args2 Denotation.bool Denotation.bool
  , dOut = Denotation.bool
  , input = Random.pair Sample.bool Sample.bool
  , baseCase = Nothing
  , func =
      let
        f : (Bool, Bool) -> Bool
        f (x, y) =
          x /= y
      in
        f
  }

-- List

list_stutter =
  { name = "list_stutter"
  , functionName = "listStutter"
  , args = 1
  , kMax = 5
  , suiteInfo = si PBESuite.list_stutter
  , dIn = Denotation.simpleList Denotation.int
  , dOut = Denotation.simpleList Denotation.int
  , input = Sample.natList
  , baseCase = Just (Random.constant [])
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
  , dIn = Denotation.args2 Denotation.int (Denotation.simpleList Denotation.int)
  , dOut = Denotation.simpleList Denotation.int
  , input = Random.pair Sample.nat Sample.natList
  , baseCase = Just (Random.constant (0, []))
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
  String -> Int -> Denotation a -> Denotation b -> Generator (List (Set (a, b)))
    -> Generator (List (Int, String))
examples functionName args dIn dOut =
  let
    extract ios =
      ( List.length ios
      , {- Debug.log "Generated Examples:" <| -} "specifyFunction"
          ++ (if args == 1 then "" else toString args)
          ++ " "
          ++ functionName
          ++ "\n[ "
          ++ String.join
               "\n, "
               (List.map (\(x, y) -> "(" ++ dIn x ++ ", " ++ dOut y ++ ")") ios)
          ++ "\n]"
      )
  in
    -- Random.map (Set.toList >> List.map (Set.toList >> extract))
    Random.map (List.map (Set.toList >> extract))

createBenchmarkInput :
  Int -> Reference a b -> Generator (List (List BenchmarkInput))
createBenchmarkInput n { name, functionName, args, kMax, suiteInfo, dIn, dOut,
 input, baseCase, func } =
  List.range 1 (min 5 kMax) -- TODO
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
                   dIn
                   dOut
                   (Sample.trial n k func input baseCase)
               )
         )
  |> Random.sequence

benchmarkInputs : Int -> Generator (List (List (List BenchmarkInput)))
benchmarkInputs n =
  Random.sequence
    [ createBenchmarkInput n bool_band
    , createBenchmarkInput n bool_bor
    , createBenchmarkInput n bool_impl
    , createBenchmarkInput n bool_neg
    , createBenchmarkInput n bool_xor

    , createBenchmarkInput n list_stutter
    , createBenchmarkInput n list_take
    ]
--    , createBenchmarkInput n list_append
--    , createBenchmarkInput n list_concat
--    , createBenchmarkInput n list_drop
--    , createBenchmarkInput n list_even_parity
--    , createBenchmarkInput n list_filter
--    , createBenchmarkInput n list_fold
--    , createBenchmarkInput n list_hd
--    , createBenchmarkInput n list_inc
--    , createBenchmarkInput n list_last
--    , createBenchmarkInput n list_length
--    , createBenchmarkInput n list_map
--    , createBenchmarkInput n list_nth
--    , createBenchmarkInput n list_pairwise_swap
--    , createBenchmarkInput n list_rev_append
--    , createBenchmarkInput n list_rev_fold
--    , createBenchmarkInput n list_rev_snoc
--    , createBenchmarkInput n list_rev_tailcall
--    , createBenchmarkInput n list_snoc
--    , createBenchmarkInput n list_sort_sorted_insert
--    , createBenchmarkInput n list_sorted_insert
--    , createBenchmarkInput n list_stutter
--    , createBenchmarkInput n list_sum
--    , createBenchmarkInput n list_take
--    , createBenchmarkInput n list_tl
--
--    , createBenchmarkInput n nat_iseven
--    , createBenchmarkInput n nat_max
--    , createBenchmarkInput n nat_pred
--    , createBenchmarkInput n nat_add
--
--    , createBenchmarkInput n tree_collect_leaves
--    , createBenchmarkInput n tree_count_leaves
--    , createBenchmarkInput n tree_count_nodes
--    , createBenchmarkInput n tree_inorder
--    , createBenchmarkInput n tree_map
--    , createBenchmarkInput n tree_preorder
