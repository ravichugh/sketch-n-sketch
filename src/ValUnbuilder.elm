module ValUnbuilder exposing (..)

import Lang exposing (..)
import LangUtils exposing (valToString)
import Dict exposing (Dict)
import Utils

list: (Val -> Result String b) -> Val -> Result String (List b)
list = \sub v -> case v.v_ of
  VList vs -> List.map sub vs |> Utils.projOk
  _ -> Err <| "Expected a list, got " ++ valToString v

tuple2: (Val -> Result String a) -> (Val -> Result String b)-> Val -> Result String (a, b)
tuple2 = \sub1 sub2 v -> case v.v_ of
  VList [v1, v2]-> Result.map2 (\a b -> (a, b)) (sub1 v1) (sub2 v2)
  _ -> Err <| "Expected a pair, got " ++ valToString v

string: Val -> Result String String
string = \v -> case v.v_ of
  VBase (VString s) -> Ok s
  _ -> Err <| "Expected a string, got " ++ valToString v

int: Val -> Result String Int
int = \v -> case v.v_ of
  VConst _ (n, _) -> Ok (floor n)
  _ -> Err <| "Expected a constant, got " ++ valToString v

dict: (Val -> Result String b) -> Val -> Result String (Dict (String, String) b)
dict = \sub v -> case v.v_ of
  VDict d -> Dict.foldl (\k v acc -> Result.map2 (\acc v -> Dict.insert k v acc) acc (sub v)) (Ok Dict.empty) d
  _ -> Err <| "Expected a dict, got " ++ valToString v

record: (Val -> Result String b) -> Val -> Result String (Dict (String) b)
record = \sub v -> case v.v_ of
  VRecord d -> Dict.foldl (\k v acc -> Result.map2 (\acc v -> Dict.insert k v acc) acc (sub v)) (Ok Dict.empty) d
  _ -> Err <| "Expected a record, got " ++ valToString v

constructor: (List Val -> Result String b) -> Val -> Result String (String, b)
constructor = \argwhat v -> case v.v_ of
  VList (vTag::vTail) -> case vTag.v_ of
    VBase (VString s) -> argwhat vTail |> Result.map (\x -> (s, x))
    _ -> Err <| "Expected a datatype starting with a string, got " ++ valToString v
  _ -> Err <| "Expected a datatype starting with a string, got " ++ valToString v