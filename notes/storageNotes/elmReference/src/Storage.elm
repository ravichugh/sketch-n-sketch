module Storage where

import Json.Encode exposing (Value)
import Json.Decode exposing (Decoder, decodeValue)
import Task exposing (Task, succeed, fail, andThen)
import List
import Native.Storage


getItemAsJson : String -> Task String Value
getItemAsJson = Native.Storage.getItemAsJson

-- Do better error detection
getItem : String -> Decoder value -> Task String value
getItem key decoder =
  let decode value = case decodeValue decoder value of
    Ok v    -> succeed v
    Err err -> fail "Failed"
  in
    getItemAsJson key `andThen` decode

setItem : String -> Value -> Task String ()
setItem = Native.Storage.setItem

removeItem : String -> Task String ()
removeItem = Native.Storage.removeItem

clear : Task String ()
clear = Native.Storage.clear

keys : Task String (List String)
keys = Native.Storage.keys

-- keysArray : Task error (Array String)
-- keysArray : Native.Storage.keysArray


length : Task String Int
length = Native.Storage.length
