module ValBuilder exposing (..)

import Lang exposing (..)
import Dict exposing (Dict)

list: (Val -> a -> Val) -> Val -> List a -> Val
list sub v = replaceV_ v << VList << List.map (sub v)

tuple2:  (Val -> a -> Val) -> (Val -> b -> Val) -> Val -> (a, b) -> Val
tuple2 sub1 sub2 v (a, b) = replaceV_ v <| VList <| [sub1 v a, sub2 v b]

tuple3:  (Val -> a -> Val) -> (Val -> b -> Val) -> (Val -> c -> Val) -> Val -> (a, b, c) -> Val
tuple3 sub1 sub2 sub3 v (a, b, c) = replaceV_ v <| VList <| [sub1 v a, sub2 v b, sub3 v c]

string: Val -> String -> Val
string v= replaceV_ v << VBase << VString

int: Val -> Int -> Val
int v= replaceV_ v << VConst Nothing << (\i -> (toFloat i, dummyTrace))

dict: (Val -> a -> Val) -> Val -> Dict (String, String) a -> Val
dict sub v = replaceV_ v << VDict << Dict.map (\k a -> sub v a)

record: (Val -> a -> Val) -> Val -> Dict String a -> Val
record sub v = replaceV_ v << VRecord << Dict.map (\k a -> sub v a)

constructor: Val -> String -> List Val -> Val
constructor v tagname vals =
  vals
  |>  (\tail -> VList ((replaceV_ v <| VBase (VString tagname))::tail))
  |> replaceV_ v

identity: Val -> Val -> Val
identity v theVal = theVal