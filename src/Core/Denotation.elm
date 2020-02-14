module Denotation exposing
  ( ..
  )

type alias Denotation a =
  (a -> String)

int : Denotation Int
int =
  Util.iterate
    (\acc -> "S (" ++ acc ++ ")")
    "Z ()"

list : String -> String -> Denotation a -> Denotation (List a)
list cons nil da =
  List.foldl
    (\x acc -> cons ++ " (" ++ da x ++ ", " ++ acc ++ ")")
    (nil ++ " ()")

simpleList : Denotation a -> Denotation (List a)
simpleList =
  list "Cons" "Nil"
