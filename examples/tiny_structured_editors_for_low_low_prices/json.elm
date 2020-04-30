-- List definition, for reference.
--
-- List, Maybe, and Bool are actually baked in to our
-- core language because the Sketch-n-Sketch
-- surface language doesn't treat them correctly.
type List a = Nil
            | Cons a (List a)

-- Originally used pairs for dict entries, but our type inference is
-- incomplete (not TSE's fault) and so TSE can't instantiate the
-- a and b type variables and therefore can't generate insert actions.
-- type Pair a b = Pair a b

-- Concrete type.
type KeyVal = KeyVal String JSON

type JSON
  = JSONList (List JSON)
  | JSONDict (List KeyVal) -- | JSONDict (List (Pair String JSON))
  | JSONString String
  | JSONNumber Num

toString : Num -> String
toString n = numToStringBuiltin n

toString : JSON -> String
toString = jsonToString ""

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

stringToString : String -> String
stringToString string =
  basedOn string <| '"' + string + '"'

jsonToString : String -> JSON -> String
jsonToString indent json =
  let nextIndent     = "  " + indent in
  let joinWithCommaNewline strs = join (",\n" + nextIndent) strs in
  case json of
    JSONList jsons -> basedOn jsons <|
      "[\n" + nextIndent + joinWithCommaNewline (map (jsonToString nextIndent) jsons) + "\n" + indent + "]"
    JSONDict keyVals -> basedOn keyVals <|
      let keyValToString keyVal =
        case keyVal of
          KeyVal key val -> stringToString key + ": " + jsonToString nextIndent val
      in
      "{\n" + nextIndent + joinWithCommaNewline (map keyValToString keyVals) + "\n" + indent + "}"
    JSONString string -> stringToString string
    JSONNumber num    -> toString num


-- 16 Possible Insert Locations
--
-- (JSONList [
--   1. ✅
--   JSONDict [
--     2. ✅
--     KeyVal "num" (JSONNumber 3),
--     3. ✅
--     KeyVal "str" (JSONString "hi")
--     4. ✅
--   ],
--   5. ✅
--   JSONDict [
--     6. ✅
--     KeyVal "num" (JSONNumber 10),
--     7. ✅
--     KeyVal "str" (JSONString "bye")
--     8. ✅
--   ],
--   9. ✅
--   JSONList [
--     10. ✅
--     JSONList [
--       11. ✅
--       JSONString "one",
--       12. ✅
--       JSONString "two",
--       13. ✅
--       JSONString "three"
--       14. ⛔️ UNREASONABLE, shared with 15
--     ]
--     15. ⛔️ UNREASONABLE, shared with 14
--   ]
--   16. ⛔️ UNREASONABLE, appears at location 15
-- ] : JSON)


(JSONList [
  JSONDict [
    KeyVal "num" (JSONNumber 3),
    KeyVal "str" (JSONString "hi")
  ],
  JSONDict [
    KeyVal "num" (JSONNumber 10),
    KeyVal "str" (JSONString "bye")
  ],
  JSONList [
    JSONList [
      JSONString "one",
      JSONString "two",
      JSONString "three"
    ]
  ]
] : JSON)
