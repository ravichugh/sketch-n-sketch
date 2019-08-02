module ImpureGoodies exposing (..)

import Native.ImpureGoodies

newPage : String -> ()
newPage content =
  Native.ImpureGoodies.newPage content

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


toggleGlobalBool : () -> Bool
toggleGlobalBool () =
  Native.ImpureGoodies.toggleGlobalBool ()


getCurrentTime : () -> Float
getCurrentTime () =
  Native.ImpureGoodies.getCurrentTime ()


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

evaluate: String -> a
evaluate s = Native.ImpureGoodies.evaluate s

log: a -> a
log s = Native.ImpureGoodies.log s

htmlunescape: String -> String
htmlunescape s = Native.ImpureGoodies.htmlunescape s

htmlescape: String -> String
htmlescape s = Native.ImpureGoodies.htmlescape s

emptyNativeRecord: () -> record
emptyNativeRecord = Native.ImpureGoodies.emptyNativeRecord

addPairToNativeRecord: (String, b) -> c -> c
addPairToNativeRecord (s, b) c = Native.ImpureGoodies.addPairToNativeRecord s b c

setValueToNativeRecord: String-> Maybe value -> record -> record
setValueToNativeRecord key mbValue record =
  Native.ImpureGoodies.setValueToNativeRecord key mbValue record

setValueToNativeIntRecord: Int-> Maybe value -> record -> record
setValueToNativeIntRecord key mbValue record =
  Native.ImpureGoodies.setValueToNativeRecord key mbValue record

updateNativeRecord: String -> (Maybe value -> Maybe value2) -> record -> record
updateNativeRecord key valueUpdater record =
  setValueToNativeRecord key (valueUpdater (nativeRecordGet key record)) record

updateNativeIntRecord: Int -> (Maybe value -> Maybe value2) -> record -> record
updateNativeIntRecord key valueUpdater record =
  setValueToNativeIntRecord key (valueUpdater (nativeIntRecordGet key record)) record

keyPairsToNativeRecord: List (String, b) -> c
keyPairsToNativeRecord l = List.foldl addPairToNativeRecord (emptyNativeRecord ()) l

keyPairsOfNativeRecord: c -> List (String, b)
keyPairsOfNativeRecord = Native.ImpureGoodies.keyPairsOfNativeRecord

nativeRecordGet: String -> record -> Maybe value
nativeRecordGet = Native.ImpureGoodies.nativeRecordGet

nativeIntRecordGet: Int -> record -> Maybe value
nativeIntRecordGet = Native.ImpureGoodies.nativeRecordGet

nativeRecordKeys: record -> List String
nativeRecordKeys = Native.ImpureGoodies.nativeRecordKeys

mapNativeRecord: (String -> value -> value2) -> record -> record
mapNativeRecord transformer record =
  nativeRecordKeys record |> List.foldl (\key record ->
    setValueToNativeRecord key (nativeRecordGet key record |> Maybe.map (transformer key)) record) record

nativeDict: {
  empty: () -> record,
  get: String -> record -> Maybe value,
  update: String -> (Maybe value -> Maybe value2) -> record -> record,
  insert: String -> value -> record -> record,
  map: (String -> value -> value2) -> record -> record
  }
nativeDict = {
  empty = emptyNativeRecord,
  get = nativeRecordGet,
  update = updateNativeRecord,
  insert = \key v record -> setValueToNativeRecord key (Just v) record,
  map = mapNativeRecord
  }

nativeIntDict: {
  empty: () -> record,
  get: Int -> record -> Maybe value,
  update: Int -> (Maybe value -> Maybe value2) -> record -> record,
  insert: Int -> value -> record -> record
  }
nativeIntDict = {
  empty = emptyNativeRecord,
  get = nativeIntRecordGet,
  update = updateNativeIntRecord,
  insert = \key v record -> setValueToNativeIntRecord key (Just v) record
  }

fromNative: a ->
  (String -> b) ->
  (Float -> b) ->
  (Bool -> b) ->
  (List c -> b) ->
  (List (String, c) -> b) ->
  ((arg -> res) -> b) -> b
fromNative =
  Native.ImpureGoodies.fromNative

toNativeArray: List a -> c
toNativeArray = Native.ImpureGoodies.toNativeArray

fromNativeArray: c -> List a
fromNativeArray = Native.ImpureGoodies.fromNativeArray

hideType: a -> b
hideType = Native.ImpureGoodies.hideType

fromHtmlEntity: String -> Maybe String
fromHtmlEntity = Native.ImpureGoodies.fromHtmlEntity

matchBaseValue: a -> (Bool -> b) -> (String -> b) -> (Float -> b) -> Result String b
matchBaseValue = Native.ImpureGoodies.matchBaseValue

whitespaceFromMetadata: metadata -> String
whitespaceFromMetadata = Native.ImpureGoodies.whitespaceFromMetadata

whitespaceToMetadata: String -> metadata
whitespaceToMetadata = Native.ImpureGoodies.whitespaceToMetadata

genCacheClear : () -> ()
genCacheClear =
  Native.ImpureGoodies.genCacheClear

genCachePut : string -> nonDetExp -> ()
genCachePut =
  Native.ImpureGoodies.genCachePut

genCacheGet : string -> Maybe nonDetExp
genCacheGet =
  Native.ImpureGoodies.genCacheGet
