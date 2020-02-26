-- List definition, for reference.
--
-- List, Maybe, and Bool are actually baked in to our
-- core language because the Sketch-n-Sketch
-- surface language doesn't treat them correctly.
type List a = Nil
            | Cons a (List a)

type Tree = Node Num (List Tree)

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

toString : Tree -> String
toString tree =
  case tree of
    Node x children ->
      case children of
        Nil      -> "(" + toString x + ")"
        Cons _ _ -> "(" + toString x + " " + join " " (map toString children) + ")"

-- A.B.C. are missing inserts b/c we don't detect mutual recursion in types, only single recursion.
-- A.B.C. would be wrappers like so: Node 0 [existing]
-- (
-- -- A. ⛔️ MISSING
-- Node 2 [
--   -- 1. ✅
--   -- B. ⛔️ MISSING
--   Node 1 [
--     -- 2. ⛔️ UNREASONABLE, same x position as 3
--   ],
--   -- 3. ⛔️ UNREASONABLE, same x position as 2
--   -- C. ⛔️ MISSING
--   Node 4 [
--     -- 4. ✅
--     -- D. ⛔️ MISSING
--     Node 3 [
--       -- 5. ⛔️ UNREASONABLE, same x position as 6
--     ],
--     -- 6. ⛔️ UNREASONABLE, same x position as 5
--     -- E. ⛔️ MISSING
--     Node 5 [
--       -- 7. ⛔️ UNREASONABLE, same x position as 8
--     ]
--     -- 8. ⛔️ UNREASONABLE, same x position as 7
--   ]
--   -- 9. ⛔️ UNREASONABLE, appears at location 8
-- ] : Tree)

(Node 2 [Node 1 [], Node 4 [Node 3 [], Node 5 []]] : Tree)
