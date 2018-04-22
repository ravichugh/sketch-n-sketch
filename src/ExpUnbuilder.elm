module ExpUnbuilder exposing (..)

import Lang exposing (..)
import LangUtils exposing (valToString)
import Dict exposing (Dict)
import Utils
import Syntax

list: (Exp -> Result String b) -> Exp -> Result String (List b)
list sub e = case e.val.e__ of
  EList _ elems _ Nothing _ -> List.map sub (Utils.listValues elems) |> Utils.projOk
  _ -> Err <| "Expected a list, got " ++ Syntax.unparser Syntax.Elm e

viewtuple2:  (Exp -> Result String a) -> (Exp-> Result String b) -> Exp-> Result String (a, b)
viewtuple2 sub1 sub2 e = case e.val.e__ of
  EList _ [(_, e1), (_, e2)] _ Nothing  _ ->
    Result.map2 (\a b -> (a, b)) (sub1 e1) (sub2 e2)
  _ -> Err <| "Expected a 2-element list, got " ++ Syntax.unparser Syntax.Elm e

string: Exp -> Result String String
string e = case e.val.e__ of
  EBase _ (EString _ s) -> Ok s
  _ -> Err <| "Expected a string, got " ++ Syntax.unparser Syntax.Elm e

int: Exp -> Result String Int
int e = case e.val.e__ of
  EConst _ n _ _ -> Ok (floor n)
  _ -> Err <| "Expected a constant, got " ++ Syntax.unparser Syntax.Elm e

identity: Exp -> Result String Exp
identity e = Ok e
