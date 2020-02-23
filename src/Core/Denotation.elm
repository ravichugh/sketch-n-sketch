module Core.Denotation exposing
  ( ..
  )

import Utils

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
  List.foldl
    (\x acc -> cons ++ " (" ++ da x ++ ", " ++ acc ++ ")")
    (nil ++ " ()")

simpleList : Denotation a -> Denotation (List a)
simpleList =
  list "Cons" "Nil"

args2 : Denotation a1 -> Denotation a2 -> Denotation (a1, a2)
args2 da1 da2 (x1, x2) =
  da1 x1 ++ ", " ++ da2 x2
