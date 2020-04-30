-- List definition, for reference.
--
-- List, Maybe, and Bool are actually baked in to our
-- core language because the Sketch-n-Sketch
-- surface language doesn't treat them correctly.
type List a = Nil
            | Cons a (List a)

map : (a -> b) -> List a -> List b
map f list =
  case list of
    Nil            -> Nil
    Cons head tail -> Cons (f head) (map f tail)

singletonList : a -> List a
singletonList a = Cons a Nil

append : List a -> List a -> List a
append list1 list2 =
  case list1 of
    Nil              -> list2
    Cons head1 tail1 -> Cons head1 (append tail1 list2)

type Maybe a
  = Nothing
  | Just a

-- Originally used pairs for dict entries, but our type inference is
-- incomplete (not TSE's fault) and so TSE can't instantiate the
-- a and b type variables.
-- type Pair a b = Pair a b

-- Concrete type.
type KeyVal = KeyVal String JSON

type JSON
  = JSONList (List JSON)
  | JSONDict (List KeyVal) -- | JSONDict (List (Pair String JSON))
  | JSONString String
  | JSONNumber Num

-- Constructors aren't functions :/
wrapJSONList list  = JSONList list
wrapJSONString str = JSONString str
wrapJSONNumber num = JSONNumber num

toString : Num -> String
toString n = numToStringBuiltin n

toString : JSON -> String
toString = jsonToString ""

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


type DataRange = DataRange Num Num -- lower bound, upper bound
type DataType  = ORDINAL | NOMINAL | INTERVAL | RATIO
type Variable  = Variable String DataType (List String) (Maybe DataRange)  -- name, data type, ordinal/nominal categories, optional range

type Assumption
  = Alpha Num
  | SampleSize Num
  | GroupsNormal (List (List String))

type TeaSpec = TeaSpec (List Variable) (List Assumption)

toString : TeaSpec -> String
toString teaSpec = toString (teaSpecToJson teaSpec)

teaSpecToJson teaSpec =
  case teaSpec of
    TeaSpec variables assumptions ->
      JSONDict [
        KeyVal "variables" (JSONList (map variableToJson variables)),
        KeyVal "assumptions" (JSONDict (map assumptionToKeyVal assumptions))
      ]

variableToJson variable =
  case variable of
    Variable name dataType categories maybeDataRange ->
      let dataTypeString =
        case dataType of
          ORDINAL  -> "ordinal"
          NOMINAL  -> "nominal"
          INTERVAL -> "interval"
          RATIO    -> "ratio"
      in
      let categoriesKeyVals =
        case dataType of
          ORDINAL  -> singletonList (KeyVal "categories" (JSONList (map wrapJSONString categories)))
          NOMINAL  -> singletonList (KeyVal "categories" (JSONList (map wrapJSONString categories)))
          INTERVAL -> Nil
          RATIO    -> Nil
      in
      let rangeKeyVals =
        case maybeDataRange of
          Nothing -> Nil
          Just dataRange ->
            let low =
              case dataRange of
                DataRange low high -> low
            in
            let high =
              case dataRange of
                DataRange low high -> high
            in
            case dataType of
              ORDINAL  -> Nil
              NOMINAL  -> Nil
              INTERVAL -> Nil
              RATIO    -> singletonList (KeyVal "range" (JSONList (map wrapJSONNumber [low, high])))
      in
      JSONDict (append [KeyVal "name" (JSONString name), KeyVal "data type" (JSONString dataTypeString)] (append categoriesKeyVals rangeKeyVals))


-- avoid "as" pattern parser bug :D "as"sumption
assumptionToKeyVal : Assumption -> KeyVal
assumptionToKeyVal aSumption =
  case aSumption of
    Alpha threshold     -> KeyVal "alpha" (JSONNumber threshold)
    SampleSize n        -> KeyVal "sample size" (JSONNumber n)
    GroupsNormal groups -> KeyVal "groups normally distributed" (JSONList (map wrapJSONList (map (map wrapJSONString) groups)))


-- type DataRange = DataRange Num Num -- lower bound, upper bound
-- type DataType  = ORDINAL | NOMINAL | INTERVAL | RATIO
-- type Variable  = Variable String DataType (List String) (Maybe DataRange)  -- name, data type, ordinal/nominal categories, optional range
--
-- type Assumption
--   = Alpha Num
--   | SampleSize Num
--   | GroupsNormal (List (List String))
--
-- type TeaSpec = TeaSpec (List Variable) (List Assumption)

var1 = Variable "So" NOMINAL ["0", "1"] Nothing

-- variables = [
--     {
--         'name' : 'So',
--         'data type' : 'nominal',   # Options: 'nominal', 'ordinal', 'interval', 'ratio'
--         'categories' : ['0', '1']
--     },
--     {
--         'name' : 'Prob',
--         'data type' : 'ratio',   # Options: 'nominal', 'ordinal', 'interval', 'ratio'
--         'range' : [0,1]   # optional
--     }
-- ]
--
-- assumptions = {
--     'groups normally distributed': [['So', 'Prob']],
--     'Type I (False Positive) Error Rate': 0.05,
-- }

(TeaSpec [
  Variable "So"   NOMINAL ["0", "1"] Nothing,
  Variable "Prob" RATIO   []         (Just (DataRange 0 1))
] [
  GroupsNormal [["So", "Prob"]],
  Alpha 0.05
] : TeaSpec)
