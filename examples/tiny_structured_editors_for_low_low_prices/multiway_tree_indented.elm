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
toString = treeToString ""

treeToString : String -> Tree a -> String
treeToString indent tree =
  case tree of
    Node x children ->
      let
        perhapsNewline = (if __strLength__ indent == 0 then "" else "\n")
        nextIndent = "  " + indent
        childStrs = map (treeToString nextIndent) children
      in
      perhapsNewline + indent + toString x + join "" childStrs

(Node 2 [Node 1 [], Node 4 [Node 3 [], Node 5 []]] : Tree Num)
