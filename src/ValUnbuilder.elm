module ValUnbuilder exposing (..)

import Lang exposing (..)
import LangUtils exposing (valToString)
import Dict exposing (Dict)
import Utils

list: (Val -> Result String b) -> Val -> Result String (List b)
list sub v = case v.v_ of
  VList vs -> List.map sub vs |> Utils.projOk
  _ -> Err <| "Expected a list, got " ++ valToString v

viewtuple2:  (Val -> Result String a) -> (Val -> Result String b) -> Val -> Result String (a, b)
viewtuple2 sub1 sub2 v = case v.v_ of
  VList [v1, v2] ->
    Result.map2 (\a b -> (a, b)) (sub1 v1) (sub2 v2)
  _ -> Err <| "Expected a 2-element list, got " ++ valToString v

tuple2: (Val -> Result String a) -> (Val -> Result String b)-> Val -> Result String (a, b)
tuple2 sub1 sub2 v = record Ok v |> Result.andThen (\d ->
    Dict.get "_1" d |> Result.fromMaybe ("Expected tuple, got " ++ valToString v) |> Result.andThen (\t1 ->
      Dict.get "_2" d |> Result.fromMaybe ("Expected tuple, got " ++ valToString v) |> Result.andThen (\t2 ->
        sub1 t1 |> Result.andThen (\a ->
          sub2 t2 |> Result.map (\b ->
            (a, b)
          )
        )
      )
    )
  )

innerTuple: Int -> Dict String Val -> List Val
innerTuple n dv =
  case Dict.get ("_" ++ toString n) dv of
    Nothing -> []
    Just v -> v :: innerTuple (n + 1) dv

string: Val -> Result String String
string v = case v.v_ of
  VBase (VString s) -> Ok s
  _ -> Err <| "Expected a string, got " ++ valToString v

int: Val -> Result String Int
int v = case v.v_ of
  VConst _ (n, _) -> Ok (floor n)
  _ -> Err <| "Expected a constant, got " ++ valToString v

num: Val -> Result String Num
num v = case v.v_ of
  VConst _ (n, _) -> Ok n
  _ -> Err <| "Expected a constant, got " ++ valToString v

bool: Val -> Result String Bool
bool v = case v.v_ of
  VBase (VBool b) -> Ok b
  _ -> Err <| "Expected a bool, got " ++ valToString v

dict: (Val -> Result String b) -> Val -> Result String (Dict (String, String) b)
dict sub v = case v.v_ of
  VDict d -> Dict.foldl (\k v acc -> Result.map2 (\acc v -> Dict.insert k v acc) acc (sub v)) (Ok Dict.empty) d
  _ -> Err <| "Expected a dict, got " ++ valToString v

record: (Val -> Result String b) -> Val -> Result String (Dict String b)
record sub v = case v.v_ of
  VRecord d -> Dict.foldl (\k v acc -> Result.map2 (\acc v -> Dict.insert k v acc) acc (sub v)) (Ok Dict.empty) d
  _ -> Err <| "Expected a record, got " ++ valToString v

constructor: (List Val -> Result String b) -> Val -> Result String (String, b)
constructor argwhat v = record Ok v |> Result.andThen (\d ->
    Dict.get (Lang.stringifyCtorKind Lang.DataTypeCtor) d |>
      Result.fromMaybe ("Datatype: " ++ Lang.stringifyCtorKind Lang.DataTypeCtor ++ " not found in record") |> Result.andThen (\tagNameVal ->
      string tagNameVal |> Result.andThen (\tagName ->
      Dict.get Lang.ctorArgs d |> Result.fromMaybe ("Datatype: " ++ ctorArgs ++ " not found in record") |> Result.andThen (\dargs ->
        record Ok dargs |> Result.andThen (\argDict ->
          argwhat (innerTuple 1 argDict) |> Result.map (\args ->
            (tagName, args)
          )
        )
      )
      )
    )
  )

identity: Val -> Result String Val
identity v = Ok v

dup: (Val -> Result String a) -> (Val -> Result String b) -> Val -> Result String (a, b)
dup sub1 sub2 v =
  sub1 v |> Result.andThen (\a ->
    sub2 v |> Result.map (\b ->
      (a, b)
    )
  )

-- Helpers to construct/deconstruct datatypes from Val
maybe: (Val -> Result String a)  -> Val -> Result String (Maybe a)
maybe subroutine v = case constructor Ok v of
  Ok ("Just", [x]) -> subroutine x |> Result.map Just
  Ok ("Nothing", []) -> Ok Nothing
  Ok _ -> Err <| "Expected Just or Nothing, got " ++ valToString v
  Err msg -> Err msg

result: (Val -> Result String a)  -> Val -> Result String (Result String a)
result subroutine v = case constructor Ok v of
  Ok ("Ok", [x]) -> subroutine x |> Result.map Ok
  Ok ("Err", [msg]) -> string msg |> Result.map Err
  Ok _ -> Err <| "Expected Ok or Err, got " ++ valToString v
  Err msg -> Err msg

