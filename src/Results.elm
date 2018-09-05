module Results exposing
  ( Results
  , withDefault1
  , ok1, oks, okLazy, errs
  , map, map2, map2withError, andThen, flatten, filter
  , toMaybe, fromMaybe, fromResult, mapErrors, projOk
  , fold, toList
  , firstResult
  , force
  , andAlso
  )

import Lazy

import Maybe exposing ( Maybe(Just, Nothing) )
import LazyList exposing (..)

{-| `Results` is either `Ok` meaning the computation succeeded, or it is an
`Err` meaning that there was some failure.
-}
type alias Results error values = Result error (LazyList values)

ok1: a -> Results e a
ok1 a = Ok (cons a Nil)

oks: (List a) -> Results e a
oks a = Ok (fromList a)

okLazy: a -> (() -> LazyList a) -> Results e a
okLazy head tailLazy =
  Ok <| Cons head <| Lazy.lazy tailLazy

errs msg = Err msg

filter: (a -> Bool) -> Results e a -> Results e a
filter pred r = case r of
  Err msg -> Err msg
  Ok l -> Ok (LazyList.filter pred l)

keepOks: LazyList (Results e a) -> LazyList a
keepOks l =
  case l of
    Nil -> Nil
    Cons (Err _) tailLazy -> keepOks (Lazy.force tailLazy)
    Cons (Ok ll) tailLazy -> appendLazy ll (Lazy.map keepOks tailLazy)

projOks: LazyList (Results e a) -> Results e a
projOks l =
  case l of
    Nil ->
      Ok Nil
    Cons (Ok (Nil)) tail ->
      projOks (Lazy.force tail)
    Cons (Ok (Cons vhead vtail)) tail -> -- At this point, we discard future errors since at least 1 worked.
      Ok <| Cons vhead
        <| Lazy.lazy (\_ -> appendLazy (Lazy.force vtail) <| Lazy.map keepOks tail)
    Cons (Err msg) tail ->
      case projOks <| Lazy.force tail of
        Err msgTail -> Err msg
        Ok Nil -> Err msg
        result -> result

projOk: List (Results e a) -> Results e (List a)
projOk l = case l of
  [] -> ok1 []
  head::tail -> head |> andThen (\a ->
    projOk tail |> map (\atail ->
      a::atail
    )
   )

fold: (e -> x) -> (LazyList a -> x) -> Results e a -> x
fold errsMap oksMap res = case res of
  Err e -> errsMap e
  Ok l -> oksMap l

withDefault1 : a -> Results x a -> a
withDefault1 def results =
  case results of
    Ok (LazyList.Cons value _) ->
      value
    _ ->
      def

firstResult: Results String a -> Result String a
firstResult r = case r of
  Err msg -> Err msg
  Ok LazyList.Nil -> Err "No result"
  Ok (LazyList.Cons head _) -> Ok head

{-| Apply a function to a results. If the results is `Ok`, it will be converted.
If the results is an `Err`, the same error value will propagate through.

    map sqrt (Ok 4.0)          == Ok 2.0
    map sqrt (Err "bad input") == Err "bad input"
-}
map : (a -> b) -> Results x a -> Results x b
map func ra = Result.map (LazyList.map func) ra

-- Performs the operation on every pair (a, b). Lists all values of b for every value of a
map2 : (a -> b -> value) -> Results x a -> Results x b -> Results x value
map2 func ra rb =
   ra |> andThen (\a ->
     rb |> map (func a)
   )

map2withError : ((x, x) -> x) -> ((a, b) -> value) -> Results x a -> Results x b -> Results x value
map2withError errorFunc func ra rb =
    case (ra,rb) of
      (Err x, Err y) -> Err (errorFunc (x, y))
      (Err x, _) -> Err x
      (_, Err x) -> Err x
      (Ok a, Ok b) -> Ok (LazyList.map func (cartesianProduct a b))



flatten: Results e (Results e a) -> Results e a
flatten r =
  case r of
    Err msg -> Err msg
    Ok ll -> projOks ll

{-| Chain together a sequence of computations that may fail. It is helpful
to see its definition:

    andThen : (a -> Results e b) -> Results e a -> Results e b
    andThen callback results =
        case results of
          Ok value -> callback value
          Err msg -> Err msg

This means we only continue with the callback if things are going well. For
example, say you need to use (`toInt : String -> Results String Int`) to parse
a month and make sure it is between 1 and 12:

    toValidMonth : Int -> Results String Int
    toValidMonth month =
        if month >= 1 && month <= 12
            then Ok month
            else Err "months must be between 1 and 12"

    toMonth : String -> Results String Int
    toMonth rawString =
        toInt rawString
          |> andThen toValidMonth

    -- toMonth "4" == Ok 4
    -- toMonth "9" == Ok 9
    -- toMonth "a" == Err "cannot parse to an Int"
    -- toMonth "0" == Err "months must be between 1 and 12"

This allows us to come out of a chain of operations with quite a specific error
message. It is often best to create a custom type that explicitly represents
the exact ways your computation may fail. This way it is easy to handle in your
code.
-}
andThen : (a -> Results x b) -> Results x a -> Results x b
andThen callback results =
    case results of
      Ok ll ->
        ll
        |> LazyList.map callback
        |> projOks

      Err msg ->
        Err msg


{-| Transform an `Err` value. For example, say the errors we get have too much
information:

    parseInt : String -> Results ParseErrors Int

    type alias ParseErrors =
        { message : String
        , code : Int
        , position : (Int,Int)
        }

    mapErrors .message (parseInt "123") == Ok 123
    mapErrors .message (parseInt "abc") == Err "char 'a' is not a number"
-}
mapErrors : (x -> y) -> Results x a -> Results y a
mapErrors f results =
    case results of
      Ok v ->
        Ok v

      Err e ->
        Err (f e)


{-| Convert to a simpler `Maybe` if the actual error message is not needed or
you need to interact with some code that primarily uses maybes.

    parseInt : String -> Results ParseErrors Int

    maybeParseInt : String -> Maybe Int
    maybeParseInt string =
        toMaybe (parseInt string)
-}
toMaybe : Results x a -> Maybe (LazyList a)
toMaybe results =
    case results of
      Ok  v -> Just v
      Err _ -> Nothing


{-| Convert from a simple `Maybe` to interact with some code that primarily
uses `Results`.

    parseInt : String -> Maybe Int

    resultsParseInt : String -> Results String Int
    resultsParseInt string =
        fromMaybe ("error parsing string: " ++ toString string) (parseInt string)
-}
fromMaybe : x -> Maybe (LazyList a) -> Results x a
fromMaybe err maybe =
    case maybe of
      Just v  -> Ok v
      Nothing -> Err err

fromResult: Result x a -> Results x a
fromResult res =
  case res of
    Err msg -> Err msg
    Ok a -> ok1 a

toList: Results e a -> List a
toList r = case r of
  Err msg -> []
  Ok ll -> LazyList.toList ll

force: Results e a -> Results e a
force r = case r of
  Err msg -> Err msg
  Ok ll -> Ok (LazyList.fromList (LazyList.toList ll))

andAlso: Results e a -> Results e a -> Results e a
andAlso other current =
  case (other, current) of
    (Err msg, _) -> Err msg
    (_, Err msg) -> Err msg
    (Ok l1, Ok l2) -> Ok (LazyList.append l1 l2)