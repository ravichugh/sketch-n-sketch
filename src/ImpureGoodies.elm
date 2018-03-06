module ImpureGoodies exposing (..)

import Native.ImpureGoodies


randomFloat : () -> Float
randomFloat () =
  Native.ImpureGoodies.randomFloat ()


randomInt : Int -> Int -> Int
randomInt low highNonInclusive =
  let range = highNonInclusive - low in
  randomFloat () * toFloat range + toFloat low |> floor


-- Runs thunk. If thunk crashes, log the error to the console and return Nothing.
crashToNothing : (() -> a) -> Maybe a
crashToNothing thunk =
  Native.ImpureGoodies.crashToNothing thunk


-- Runs thunk. If thunk crashes, returns an Err value. No built-in logging.
crashToError : (() -> a) -> Result String a
crashToError thunk =
  Native.ImpureGoodies.crashToError thunk


-- Exception can be any constructed value (exceptions are distinguished by constructor name).
throw : exception -> b
throw exception =
  Native.ImpureGoodies.throw exception


-- Give unqualified constructor name of exception to catch.
tryCatch : String -> (() -> a) -> (exception -> a) -> a
tryCatch exceptionConstructorName thunk catchThunk =
  Native.ImpureGoodies.tryCatch exceptionConstructorName thunk catchThunk


-- It's up to you to get the types right on this.
-- Also, you can create pointer cycles with this (that's kind of the point)
-- which can destroy your nice, otherwise total, functions.
mutateRecordField : a -> String -> b -> a
mutateRecordField record fieldName newValue =
  Native.ImpureGoodies.mutateRecordField record fieldName newValue

timedRun : (() -> a) -> (a, Float)
timedRun thunk =
  Native.ImpureGoodies.timedRun thunk


logTimedRun : String -> (() -> a) -> a
logTimedRun caption thunk =
  let (result, time) = timedRun thunk in
  let _ = Debug.log (caption ++ " milliseconds") time in
  result

stringCharAt: Int -> String -> Maybe Char
stringCharAt index string =
  Native.ImpureGoodies.stringCharAt index string

-- Used to cache a value inside an expression. Careful: the field name must be available and not overriding
putCache: record -> String -> value -> value
putCache record cacheName newValue =
  Native.ImpureGoodies.putCache record cacheName newValue

getCache: record -> String -> Maybe b
getCache record cacheName =
  Native.ImpureGoodies.getCache record cacheName

getOrUpdateCache: record -> String -> (() -> value) -> value
getOrUpdateCache record cacheName default =
  case getCache record cacheName of
    Just v -> v
    Nothing -> putCache record cacheName (default ())