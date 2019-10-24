-- List definition, for reference. The Sketch-n-Sketch
-- surface language treats lists as a separate type
-- (not a datatype), so the following is actually ignored
-- and we have to bake the List datatype definition into
-- the core language.
type List a = Nil
            | Cons a (List a)


toString : Num -> String
toString n = numToStringBuiltin n

toString : List a -> String
toString list =
  "[" + elemsToString list + "]"

elemsToString : List a -> String
elemsToString list =
  case list of
    Nil            -> ""
    Cons head tail ->
      case tail of
        Nil      -> toString head
        Cons _ _ -> toString head + "," + elemsToString tail

([1, 2, 3] : List Num)
