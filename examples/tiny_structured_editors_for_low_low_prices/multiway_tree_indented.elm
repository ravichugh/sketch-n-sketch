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
toString = treeToString ""

treeToString : String -> Tree -> String
treeToString indent tree =
  case tree of
    Node x children ->
      let
        perhapsNewline = (if __strLength__ indent == 0 then "" else "\n")
        nextIndent = "  " + indent
        childStrs = map (treeToString nextIndent) children
      in
      perhapsNewline + indent + toString x + join "" childStrs

-- A.B.C. are missing inserts b/c we don't detect mutual recursion in types, only single recursion.
-- A.B.C. would be wrappers like so: Node 0 [existing]
-- (
-- -- A. ⛔️ MISSING
-- Node 2 [
--   -- 1. ⛔️ UNREASONABLE, shared with 2
--   -- B. ⛔️ MISSING
--   Node 1 [
--     -- 2. ⛔️ UNREASONABLE, shared with 1
--   ],
--   -- 3. ✅
--   -- C. ⛔️ MISSING
--   Node 4 [
--     -- 4. ✅
--     -- D. ⛔️ MISSING
--     Node 3 [
--       -- 5. ✅
--     ],
--     -- 6. ⛔️ UNREASONABLE, shared with 7
--     -- E. ⛔️ MISSING
--     Node 5 [
--       -- 7. ⛔️ UNREASONABLE, shared with 6
--     ]
--     -- 8. ⛔️ UNREASONABLE, shared with 9
--   ]
--   -- 9. ⛔️ UNREASONABLE, shared with 8
-- ] : Tree)

(Node 2 [Node 1 [], Node 4 [Node 3 [], Node 5 []]] : Tree)
