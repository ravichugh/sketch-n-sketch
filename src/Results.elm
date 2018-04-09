module Results exposing
  ( Results(Oks, Errs)
  , withDefault, withDefault1
  , ok1, oks, okLazy, errs
  , map, map2, map2withError, andThen, flatten, filter
  , toMaybe, fromMaybe, fromResult, mapErrors, projOk
  , fold, toList
  , firstResult
  , force
  )

import Lazy

import Maybe exposing ( Maybe(Just, Nothing) )
import LazyList exposing (..)

{-| `Results` is either `Oks` meaning the computation succeeded, or it is an
`Errs` meaning that there was some failure.
-}
type Results error values
    = Oks (LazyList values)
    | Errs error

ok1: a -> Results e a
ok1 a = Oks (cons a Nil)

oks: (List a) -> Results e a
oks a = Oks (fromList a)

okLazy: a -> (() -> LazyList a) -> Results e a
okLazy head tailLazy =
  Oks <| Cons head <| Lazy.lazy tailLazy

errs msg = Errs msg

filter: (a -> Bool) -> Results e a -> Results e a
filter pred r = case r of
  Errs msg -> Errs msg
  Oks l -> Oks (LazyList.filter pred l)

keepOks: LazyList (Results e a) -> LazyList a
keepOks l =
  case l of
    Nil -> Nil
    Cons (Errs _) tailLazy -> keepOks (Lazy.force tailLazy)
    Cons (Oks ll) tailLazy -> appendLazy ll (Lazy.map keepOks tailLazy)

projOks: LazyList (Results e a) -> Results e a
projOks l =
  case l of
    Nil ->
      Oks Nil
    Cons (Oks (Nil)) tail ->
      projOks (Lazy.force tail)
    Cons (Oks (Cons vhead vtail)) tail -> -- At this point, we discard future errors since at least 1 worked.
      Oks <| Cons vhead
        <| Lazy.lazy (\_ -> appendLazy (Lazy.force vtail) <| Lazy.map keepOks tail)
    Cons (Errs msg) tail ->
      case projOks <| Lazy.force tail of
        Errs msgTail -> Errs msg
        Oks Nil -> Errs msg
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
  Errs e -> errsMap e
  Oks l -> oksMap l

{-| If the results is `Oks` return the value, but if the results is an `Errs` then
return a given default value.
-}
withDefault : LazyList a -> Results x a -> LazyList a
withDefault def results =
  case results of
    Oks value ->
        value

    Errs _ ->
        def

withDefault1 : a -> Results x a -> a
withDefault1 def results =
  case results of
    Oks (LazyList.Cons value _) ->
      value
    _ ->
      def

firstResult: Results String a -> Result String a
firstResult r = case r of
  Errs msg -> Err msg
  Oks LazyList.Nil -> Err "No result"
  Oks (LazyList.Cons head _) -> Ok head

{-| Apply a function to a results. If the results is `Oks`, it will be converted.
If the results is an `Errs`, the same error value will propagate through.

    map sqrt (Oks 4.0)          == Oks 2.0
    map sqrt (Errs "bad input") == Errs "bad input"
-}
map : (a -> b) -> Results x a -> Results x b
map func ra =
    case ra of
      Oks a -> Oks (LazyList.map func a)
      Errs e -> Errs e

-- Performs the operation on every pair (a, b). Lists all values of b for every value of a
map2 : ((a, b) -> value) -> Results x a -> Results x b -> Results x value
map2 func ra rb =
    case (ra,rb) of
      (Oks a, Oks b) -> Oks (LazyList.map func (cartesianProduct a b))
      (Errs x, _) -> Errs x
      (_, Errs x) -> Errs x

map2withError : ((x, x) -> x) -> ((a, b) -> value) -> Results x a -> Results x b -> Results x value
map2withError errorFunc func ra rb =
    case (ra,rb) of
      (Errs x, Errs y) -> Errs (errorFunc (x, y))
      (Errs x, _) -> Errs x
      (_, Errs x) -> Errs x
      (Oks a, Oks b) -> Oks (LazyList.map func (cartesianProduct a b))



flatten: Results e (Results e a) -> Results e a
flatten r =
  case r of
    Errs msg -> Errs msg
    Oks ll -> projOks ll

{-| Chain together a sequence of computations that may fail. It is helpful
to see its definition:

    andThen : (a -> Results e b) -> Results e a -> Results e b
    andThen callback results =
        case results of
          Oks value -> callback value
          Errs msg -> Errs msg

This means we only continue with the callback if things are going well. For
example, say you need to use (`toInt : String -> Results String Int`) to parse
a month and make sure it is between 1 and 12:

    toValidMonth : Int -> Results String Int
    toValidMonth month =
        if month >= 1 && month <= 12
            then Oks month
            else Errs "months must be between 1 and 12"

    toMonth : String -> Results String Int
    toMonth rawString =
        toInt rawString
          |> andThen toValidMonth

    -- toMonth "4" == Oks 4
    -- toMonth "9" == Oks 9
    -- toMonth "a" == Errs "cannot parse to an Int"
    -- toMonth "0" == Errs "months must be between 1 and 12"

This allows us to come out of a chain of operations with quite a specific error
message. It is often best to create a custom type that explicitly represents
the exact ways your computation may fail. This way it is easy to handle in your
code.
-}
andThen : (a -> Results x b) -> Results x a -> Results x b
andThen callback results =
    case results of
      Oks ll ->
        ll
        |> LazyList.map callback
        |> projOks

      Errs msg ->
        Errs msg


{-| Transform an `Errs` value. For example, say the errors we get have too much
information:

    parseInt : String -> Results ParseErrors Int

    type alias ParseErrors =
        { message : String
        , code : Int
        , position : (Int,Int)
        }

    mapErrors .message (parseInt "123") == Oks 123
    mapErrors .message (parseInt "abc") == Errs "char 'a' is not a number"
-}
mapErrors : (x -> y) -> Results x a -> Results y a
mapErrors f results =
    case results of
      Oks v ->
        Oks v

      Errs e ->
        Errs (f e)


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
      Oks  v -> Just v
      Errs _ -> Nothing


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
      Just v  -> Oks v
      Nothing -> Errs err

fromResult: Result x a -> Results x a
fromResult res =
  case res of
    Err msg -> Errs msg
    Ok a -> ok1 a

toList: Results e a -> List a
toList r = case r of
  Errs msg -> []
  Oks ll -> LazyList.toList ll

force: Results e a -> Results e a
force r = case r of
  Errs msg -> Errs msg
  Oks ll -> Oks (LazyList.fromList (LazyList.toList ll))
