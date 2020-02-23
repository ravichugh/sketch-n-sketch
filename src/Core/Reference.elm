module Core.Reference exposing
  ( BenchmarkInput
  , benchmarkInputs
  , listPairwiseSwap
  , listEvenParity
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

list_append =
  { name = "list_append"
  , functionName = "append"
  , args = 2
  , kMax = 10
  , suiteInfo = si PBESuite.list_append
  , dIn =
      Denotation.args2
        (Denotation.simpleList Denotation.int)
        (Denotation.simpleList Denotation.int)
  , dOut = Denotation.simpleList Denotation.int
  , input = Random.pair Sample.natList Sample.natList
  , baseCase = Just (Random.constant ([], []))
  , func =
      let
        f : (List Int, List Int) -> List Int
        f (xs, ys) =
          xs ++ ys
      in
        f
  }

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

list_drop =
  { name = "list_drop"
  , functionName = "listDrop"
  , args = 2
  , kMax = 20
  , suiteInfo = si PBESuite.list_drop
  , dIn = Denotation.args2 (Denotation.simpleList Denotation.int) Denotation.int
  , dOut = Denotation.simpleList Denotation.int
  , input = Random.pair Sample.natList Sample.nat
  , baseCase = Just (Random.constant ([], 0))
  , func =
      let
        f : (List Int, Int) -> List Int
        f (xs, n) =
          List.drop n xs
      in
        f
  }

listEvenParity : List Bool -> Bool
listEvenParity bs =
  case bs of
    [] ->
      True

    head :: tail ->
      if head then
        not (listEvenParity tail)
      else
        listEvenParity tail

list_even_parity =
  { name = "list_even_parity"
  , functionName = "evenParity"
  , args = 1
  , kMax = 10
  , suiteInfo = si PBESuite.list_even_parity
  , dIn = Denotation.simpleList Denotation.bool
  , dOut = Denotation.bool
  , input = Sample.boolList
  , baseCase = Just (Random.constant [])
  , func =
      let
        f : List Bool -> Bool
        f bs =
          listEvenParity bs
      in
        f
  }

list_hd =
  { name = "list_hd"
  , functionName = "listHead"
  , args = 1
  , kMax = 5
  , suiteInfo = si PBESuite.list_hd
  , dIn = Denotation.simpleList Denotation.int
  , dOut = Denotation.int
  , input = Sample.natList
  , baseCase = Nothing
  , func =
      let
        f : List Int -> Int
        f xs =
          case xs of
            [] ->
              0

            head :: _ ->
              head
      in
        f
  }

list_inc =
  { name = "list_inc"
  , functionName = "listInc"
  , args = 1
  , kMax = 5
  , suiteInfo = si PBESuite.list_inc
  , dIn = Denotation.simpleList Denotation.int
  , dOut = Denotation.simpleList Denotation.int
  , input = Sample.natList
  , baseCase = Nothing
  , func =
      let
        f : List Int -> List Int
        f xs =
          List.map ((+) 1) xs
      in
        f
  }

list_last =
  { name = "list_last"
  , functionName = "listLast"
  , args = 1
  , kMax = 8
  , suiteInfo = si PBESuite.list_last
  , dIn = Denotation.simpleList Denotation.int
  , dOut = Denotation.opt Denotation.int
  , input = Sample.natList
  , baseCase = Just (Random.constant [])
  , func =
      let
        f : List Int -> Maybe Int
        f xs =
          case xs of
            [] ->
              Nothing

            [x] ->
              Just x

            _ :: tail ->
              f tail
      in
        f
  }

list_length =
  { name = "list_length"
  , functionName = "listLength"
  , args = 1
  , kMax = 5
  , suiteInfo = si PBESuite.list_length
  , dIn = Denotation.simpleList Denotation.int
  , dOut = Denotation.int
  , input = Sample.natList
  , baseCase = Just (Random.constant [])
  , func =
      let
        f : List Int -> Int
        f xs =
          List.length xs
      in
        f
  }

list_nth =
  { name = "list_nth"
  , functionName = "listNth"
  , args = 1
  , kMax = 20
  , suiteInfo = si PBESuite.list_nth
  , dIn = Denotation.args2 (Denotation.simpleList Denotation.int) Denotation.int
  , dOut = Denotation.int
  , input = Random.pair Sample.natList Sample.nat
  , baseCase = Just (Random.constant ([], 0))
  , func =
      let
        f : (List Int, Int) -> Int
        f (xs, n) =
          case xs of
            [] ->
              0

            head :: tail ->
              if n == 0 then
                head
              else
                f (tail, n - 1)
      in
        f
  }

listPairwiseSwap : List a -> Maybe (List a)
listPairwiseSwap xs =
  case xs of
    [] ->
      Just []

    [_] ->
      Nothing

    x1 :: x2 :: tail ->
      Maybe.map
        (\flipped -> x2 :: x1 :: flipped)
        (listPairwiseSwap tail)

list_pairwise_swap =
  { name = "list_pairwise_swap"
  , functionName = "listPairwiseSwap"
  , args = 1
  , kMax = 12
  , suiteInfo = si PBESuite.list_pairwise_swap
  , dIn = Denotation.simpleList Denotation.int
  , dOut = Denotation.simpleList Denotation.int
  , input = Sample.natList
  , baseCase = Just (Random.constant [])
  , func =
      let
        f : List Int -> List Int
        f xs =
          case listPairwiseSwap xs of
            Nothing ->
              []

            Just flipped ->
              flipped
      in
        f
  }

list_rev_append =
  { name = "list_rev_append"
  , functionName = "listRevAppend"
  , args = 1
  , kMax = 10
  , suiteInfo = si PBESuite.list_rev_append
  , dIn = Denotation.simpleList Denotation.int
  , dOut = Denotation.simpleList Denotation.int
  , input = Sample.natList
  , baseCase = Just (Random.constant [])
  , func =
      let
        f : List Int -> List Int
        f xs =
          List.reverse xs
      in
        f
  }

list_rev_fold =
  { name = "list_rev_fold"
  , functionName = "listRevFold"
  , args = 1
  , kMax = 10
  , suiteInfo = si PBESuite.list_rev_fold
  , dIn = Denotation.simpleList Denotation.int
  , dOut = Denotation.simpleList Denotation.int
  , input = Sample.natList
  , baseCase = Just (Random.constant [])
  , func =
      let
        f : List Int -> List Int
        f xs =
          List.reverse xs
      in
        f
  }

list_rev_snoc =
  { name = "list_rev_snoc"
  , functionName = "listRevSnoc"
  , args = 1
  , kMax = 10
  , suiteInfo = si PBESuite.list_rev_snoc
  , dIn = Denotation.simpleList Denotation.int
  , dOut = Denotation.simpleList Denotation.int
  , input = Sample.natList
  , baseCase = Just (Random.constant [])
  , func =
      let
        f : List Int -> List Int
        f xs =
          List.reverse xs
      in
        f
  }

list_rev_tailcall =
  { name = "list_rev_tailcall"
  , functionName = "listRevTailcall"
  , args = 1
  , kMax = 12
  , suiteInfo = si PBESuite.list_rev_tailcall
  , dIn = Denotation.simpleList Denotation.int
  , dOut = Denotation.simpleList Denotation.int
  , input = Sample.natList
  , baseCase = Just (Random.constant [])
  , func =
      let
        f : List Int -> List Int
        f xs =
          List.reverse xs
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
      let _ = Debug.log "Generated Examples:" ios in
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
    , createBenchmarkInput n list_append
--    , createBenchmarkInput n list_concat
    , createBenchmarkInput n list_drop
    , createBenchmarkInput n list_even_parity
--    , createBenchmarkInput n list_filter
--    , createBenchmarkInput n list_fold
    , createBenchmarkInput n list_hd
    , createBenchmarkInput n list_inc
    , createBenchmarkInput n list_last
    , createBenchmarkInput n list_length
--    , createBenchmarkInput n list_map
    , createBenchmarkInput n list_nth
    , createBenchmarkInput n list_pairwise_swap

    , createBenchmarkInput n list_rev_append
    , createBenchmarkInput n list_rev_fold
    , createBenchmarkInput n list_rev_snoc
    , createBenchmarkInput n list_rev_tailcall
    ]
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
