-- List definition, for reference.
--
-- List, Maybe, and Bool are actually baked in to our
-- core language because the Sketch-n-Sketch
-- surface language doesn't treat them correctly.
type List a = Nil
            | Cons a (List a)

type Tree a = Node a (List (Tree a))

toString : Num -> String
toString n = numToStringBuiltin n

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
      case rest of
        Nil      -> str
        Cons _ _ -> str + sep + join sep rest

toString : Tree a -> String
toString tree =
  case tree of
    Node x children ->
      case children of
        Nil      -> "(" + toString x + ")"
        Cons _ _ -> "(" + toString x + " " + join " " (map toString children) + ")"

-- START HERE why aren't inserts generating?

(Node 2 [Node 1 [], Node 4 [Node 3 [], Node 5 []]] : Tree Num)
