-- List definition, for reference. The Sketch-n-Sketch
-- surface language treats lists as a separate type
-- (not a datatype), so the following is actually ignored
-- and we have to bake the List datatype definition into
-- the core language.
type List a = Nil
            | Cons a (List a)

type Pair a b = Pair a b

type JSON
  = JSONList (List JSON)
  | JSONDict (List (Pair String JSON))
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
      str + case rest of -- Don't have the normalization step yet so let's move str outside
        Nil      -> ""
        Cons _ _ -> sep + join sep rest

jsonToString : String -> JSON -> String
jsonToString indent json =
  let nextIndent     = "  " + indent in
  let joinWithCommaNewline strs = join (",\n" + nextIndent) strs in
  case json of
    JSONList jsons ->
      "[\n" + nextIndent + joinWithCommaNewline (map (jsonToString nextIndent) jsons) + "\n" + indent + "]"
    JSONDict keyVals ->
      let keyValToString keyVal =
        case keyVal of
          Pair key val -> key + ": " + jsonToString nextIndent val
      in
      "{\n" + nextIndent + joinWithCommaNewline (map keyValToString keyVals) + "\n" + indent + "}"
    JSONString string -> '"' + string + '"'
    JSONNumber num    -> toString num


(JSONList [
  JSONDict [
    Pair "num" (JSONNumber 3),
    Pair "str" (JSONString "hi")
  ],
  JSONDict [
    Pair "num" (JSONNumber 10),
    Pair "str" (JSONString "bye")
  ]
] : JSON)
