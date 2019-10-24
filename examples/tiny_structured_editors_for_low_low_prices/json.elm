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

jsonToString : String -> JSON -> String
jsonToString indent json =
  let nextIndent = "  " + indent in
  case json of
    JSONList jsons ->
      let jsonsToString jsons =
        case jsons of
          Nil            -> ""
          Cons head tail ->
            case tail of
              Nil      -> jsonToString nextIndent head
              Cons _ _ -> jsonToString nextIndent head + ",\n" + indent + jsonsToString tail
      in
      "[" + jsonsToString jsons + "\n" + indent + "]\n"
    JSONDict keyVals ->
      let keyValToString keyVal =
        case keyVal of
          Pair key val -> key + ": " + jsonToString nextIndent val
      in
      let keyValsToString keyVals =
        case keyVals of
          Nil            -> ""
          Cons head tail ->
            case tail of
              Nil      -> keyValToString head
              Cons _ _ -> keyValToString head + ",\n" + indent + keyValsToString tail
      in
      "{" + keyValsToString keyVals + "\n" + indent + "}\n"
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
