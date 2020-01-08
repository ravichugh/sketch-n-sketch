-- List definition, for reference. The Sketch-n-Sketch
-- surface language treats lists as a separate type
-- (not a datatype), so the following is actually ignored
-- and we have to bake the List datatype definition into
-- the core language.
type List a = Nil
            | Cons a (List a)


map : (a -> b) -> List a -> List b
map f list =
  case list of
    Nil            -> Nil
    Cons head tail -> Cons (f head) (map f tail)

join : String -> List String -> String
join sep strs =
  case strs of
    Nil           -> ""
    Cons str rest ->
      let perhapsSep =
        case rest of
          Nil      -> ""
          Cons _ _ -> sep
      in
      str + perhapsSep + join sep rest

-- join : String -> List String -> String
-- join sep strs =
--   case strs of
--     Nil           -> ""
--     Cons str rest ->
--       case rest of
--         Nil      -> str + ""
--         Cons _ _ -> str + sep + join sep rest

toString : Num -> String
toString n = numToStringBuiltin n

toString : List a -> String
toString list =
  "[" + join "," (map toString list) + "]"

([1, 2, 3] : List Num)
