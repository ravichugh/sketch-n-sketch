-- List definition, for reference.
--
-- List, Maybe, and Bool are actually baked in to our
-- core language because the Sketch-n-Sketch
-- surface language doesn't treat them correctly.
type List a = Nil
            | Cons a (List a)


toString : Num -> String
toString n = numToStringBuiltin n

toString : List a -> String
toString list =
  "[ " + elemsToString list + "\n]"

elemsToString : List a -> String
elemsToString list =
  case list of
    Nil            -> ""
    Cons head tail ->
      case tail of
        Nil      -> toString head
        Cons _ _ -> toString head + "\n, " + elemsToString tail

([1, 2, 3] : List Num)
