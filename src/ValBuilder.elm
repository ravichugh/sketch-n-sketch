module ValBuilder exposing (..)

import Lang exposing (..)
import Dict exposing (Dict)
import Utils

type alias Vb = Val_ -> Val

fromVal: Val -> Vb
fromVal v = replaceV_ v

list: (Vb -> a -> Val) -> Vb -> List a -> Val
list sub vb = vb << VList << List.map (sub vb)

viewtuple2:  (Vb -> a -> Val) -> (Vb -> b -> Val) -> Vb -> (a, b) -> Val
viewtuple2 sub1 sub2 vb (a, b) = vb <| VList <| [sub1 vb a, sub2 vb b]

tuple2:  (Vb -> a -> Val) -> (Vb -> b -> Val) -> Vb -> (a, b) -> Val
tuple2 sub1 sub2 vb (a, b) =
   vb <| VRecord <| Dict.fromList [
     Lang.ctorVal (vb << VBase << VString) Lang.TupleCtor (Lang.ctorTupleName 2),
     ("_1", sub1 vb a),
     ("_2", sub2 vb b)]

viewtuple3:  (Vb -> a -> Val) -> (Vb -> b -> Val) -> (Vb -> c -> Val) -> Vb -> (a, b, c) -> Val
viewtuple3 sub1 sub2 sub3 vb (a, b, c) = vb <| VList <| [sub1 vb a, sub2 vb b, sub3 vb c]

tuple3:  (Vb -> a -> Val) -> (Vb -> b -> Val) -> (Vb -> c -> Val) -> Vb -> (a, b, c) -> Val
tuple3 sub1 sub2 sub3 vb (a, b, c) =
  vb <| VRecord <| Dict.fromList [
     Lang.ctorVal (vb << VBase << VString) Lang.TupleCtor (Lang.ctorTupleName 3),
     ("_1", sub1 vb a),
     ("_2", sub2 vb b),
     ("_3", sub3 vb c)]

string: Vb -> String -> Val
string vb= vb << VBase << VString

int: Vb -> Int -> Val
int vb= vb << VConst Nothing << (\i -> (toFloat i, dummyTrace))

const: Vb -> Float -> Val
const vb = vb << VConst Nothing << (\i -> (i, dummyTrace))

dict: (Vb -> a -> Val) -> Vb -> Dict (String, String) a -> Val
dict sub vb = vb << VDict << Dict.map (\k a -> sub vb a)

record: (Vb -> a -> Val) -> Vb -> Dict String a -> Val
record sub vb = vb << VRecord << Dict.map (\k a -> sub vb a)

constructor: Vb -> String -> List Val -> Val
constructor vb tagname vals =
  vb <| VRecord <| Dict.fromList [
    (Lang.stringifyCtorKind Lang.DataTypeCtor, string vb tagname),
    (Lang.ctorArgs, vb <| VRecord <| Dict.fromList (vals |> Utils.indexedMapFrom 1 Lang.numericalValEntry))]

identity: Vb -> Val -> Val
identity vb theVal = theVal


maybe: (Vb -> a -> Val) -> Vb -> Maybe a -> Val
maybe subroutine vb mba = case mba of
  Just x  -> constructor vb "Just"    [subroutine vb x]
  Nothing -> constructor vb "Nothing" []

result: (Vb -> a -> Val) -> Vb -> Result String a -> Val
result subroutine vb mba = case mba of
  Ok x  -> constructor vb "Ok"    [subroutine vb x]
  Err msg-> constructor vb "Err"  [string vb msg]
