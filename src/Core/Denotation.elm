module Core.Denotation exposing
  ( ..
  )

import Utils

import Tree exposing (Tree)

type alias Denotation a =
  (a -> String)

bool : Denotation Bool
bool b =
  if b then
    "T ()"
  else
    "F ()"

int : Denotation Int
int =
  Utils.iterate
    (\acc -> "S (" ++ acc ++ ")")
    "Z ()"

opt : Denotation a -> Denotation (Maybe a)
opt da mx =
  case mx of
    Nothing ->
      "None ()"

    Just x ->
      "Some (" ++ da x ++ ")"

list : String -> String -> Denotation a -> Denotation (List a)
list cons nil da =
  List.foldr
    (\x acc -> cons ++ " (" ++ da x ++ ", " ++ acc ++ ")")
    (nil ++ " ()")

simpleList : Denotation a -> Denotation (List a)
simpleList =
  list "Cons" "Nil"

simpleNestedList : Denotation a -> Denotation (List (List a))
simpleNestedList da =
  list "LCons" "LNil" (simpleList da)

tree : Denotation a -> Denotation (Tree a)
tree da t =
  case t of
    Tree.Leaf ->
      "Leaf ()"

    Tree.Node left x right ->
      "Node (" ++ tree da left ++ ", " ++ da x ++ ", " ++ tree da right ++ ")"

args2 : Denotation a1 -> Denotation a2 -> Denotation (a1, a2)
args2 da1 da2 (x1, x2) =
  da1 x1 ++ ", " ++ da2 x2
