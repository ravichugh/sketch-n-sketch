module ValBuilder exposing (..)

import Lang exposing (..)
import Dict exposing (Dict)

list: Val -> (a -> Val) -> List a -> Val
list v sub = replaceV_ v << VList << List.map sub

tuple2: Val ->  (a -> Val) -> (b -> Val) -> (a, b) -> Val
tuple2 v sub1 sub2 (a, b) = replaceV_ v <| VList <| [sub1 a, sub2 b]

string: Val -> String -> Val
string v= replaceV_ v << VBase << VString

int: Val -> Int -> Val
int v= replaceV_ v << VConst Nothing << (\i -> (toFloat i, dummyTrace))

dict: Val -> (a -> Val) -> Dict (String, String) a -> Val
dict v sub = replaceV_ v << VDict << Dict.map (\k a -> sub a)

record: Val -> (a -> Val) -> Dict String a -> Val
record v sub = replaceV_ v << VRecord << Dict.map (\k a -> sub a)

constructor: Val -> String -> List Val -> Val
constructor v tagname vals =
  vals
  |>  (\tail -> VList ((replaceV_ v <| VBase (VString tagname))::tail))
  |> replaceV_ v
