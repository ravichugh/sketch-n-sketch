-- List definition, for reference.
--
-- List, Maybe, and Bool are actually baked in to our
-- core language because the Sketch-n-Sketch
-- surface language doesn't treat them correctly.
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
      case rest of
        Nil      -> str
        Cons _ _ -> str + sep + join sep rest

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
          Pair key val -> key + ": " + jsonToString nextIndent val
      in
      "{\n" + nextIndent + joinWithCommaNewline (map keyValToString keyVals) + "\n" + indent + "}"
    JSONString string -> basedOn string <| '"' + string + '"'
    JSONNumber num    -> toString num


(JSONList [
  JSONDict [
    Pair "num" (JSONNumber 3),
    Pair "str" (JSONString "hi")
  ],
  JSONDict [
    Pair "num" (JSONNumber 10),
    Pair "str" (JSONString "bye")
  ],
  JSONList [
    JSONList [
      JSONString "one",
      JSONString "two",
      JSONString "three"
    ]
  ]
] : JSON)
