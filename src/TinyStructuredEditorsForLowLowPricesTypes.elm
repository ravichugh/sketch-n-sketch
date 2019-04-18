module TinyStructuredEditorsForLowLowPricesTypes exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)
import String

import Lang


----------- State and Caching on the Model -----------

-- I hate caching but if we instead perform the work on
-- demand in the view then the GUI slows to a crawl.
type alias ModelState =
  { renderingFunctionNames                : List Ident
  , maybeRenderingFunctionName            : Maybe Ident
  , stringTaggedWithProjectionPathsResult : Result String StringTaggedWithProjectionPaths
  , stringProjectionPathToSpecificActions : Dict ProjectionPath (List SpecificAction)
  , selectedPaths                         : Set ProjectionPath
  }

initialModelState =
  { renderingFunctionNames                = []
  , maybeRenderingFunctionName            = Nothing
  , stringTaggedWithProjectionPathsResult = Err "No trace for toString call yet—need to run the code."
  , stringProjectionPathToSpecificActions = Dict.empty
  , selectedPaths                         = Set.empty
  }


----------- Basics -----------

type alias Nat   = Int
type alias Ident = String


----------- Core Language -----------

type Exp -- Expressions e :=
  = EFun Ident Exp -- λx.e
  | EVar Ident -- x
  | EApp Exp Exp -- e1 e2
  | ECtor Ident (List Exp) -- C (e1,...,en)
  | ECase Exp (List (Ident, TuplePattern, Exp)) -- case e of Ci pi -> ei
  | EString String -- s
  | EAppend Exp Exp -- e1 ++ e2
  | ENum Float
  | ENumToString Exp

type alias TuplePattern = List Ident -- Tuple Patterns p := (x1,...,xn)

type alias ProjectionPath = List Nat -- Projection Paths π := • | π.i

type alias Env = List (Ident, TaggedValue) -- Tagged Environments E := — | E,x↦w

type alias TaggedValue = { v : UntaggedPreValue, paths : Set ProjectionPath } -- (Tagged) Values w := v^{π1,...,πn}

noTag : UntaggedPreValue -> TaggedValue
noTag v = TaggedValue v Set.empty

type UntaggedPreValue -- (Untagged) Pre-Values v :=
  = VClosure Env Ident Exp -- [E]λx.e
  | VCtor Ident (List TaggedValue) -- C (w1,...,wn)
  | VString String -- s
  | VAppend TaggedValue TaggedValue -- w1 ++ w2
  | VNum Float

-- For debugging.
unparseToUntaggedString : TaggedValue -> String
unparseToUntaggedString taggedValue =
  let recurse = unparseToUntaggedString in
  case taggedValue.v of
    VClosure env fName fBody -> "[E]λx.e"
    VCtor ctorName argVals   -> ctorName ++ "(" ++ String.join ", " (List.map recurse argVals) ++ ")"
    VString string           -> toString string
    VAppend left right       -> recurse left ++ " ++ " ++ recurse right
    VNum number              -> toString number



----------- Workflow Functions -----------

type SpecificAction
  = Replace ProjectionPath TaggedValue
  | Scrub ProjectionPath

specificActionProjectionPath : SpecificAction -> ProjectionPath
specificActionProjectionPath specificAction =
  case specificAction of
    Replace projectionPath _ -> projectionPath
    Scrub projectionPath     -> projectionPath


type AppendedTaggedStrings t
  = TaggedString String t
  | TaggedStringAppend (AppendedTaggedStrings t) (AppendedTaggedStrings t) t

type alias StringTaggedWithProjectionPaths = AppendedTaggedStrings (Set ProjectionPath)
-- type alias StringTaggedWithSpecificActions = AppendedTaggedStrings (Set SpecificAction)

stringTag : AppendedTaggedStrings t -> t
stringTag appendedTaggedStrings =
  case appendedTaggedStrings of
    TaggedString _ tag         -> tag
    TaggedStringAppend _ _ tag -> tag

gatherStringTags : AppendedTaggedStrings t -> List t
gatherStringTags taggedString =
  let recurse = gatherStringTags in
  case taggedString of
    TaggedString string tag           -> [tag]
    TaggedStringAppend left right tag -> recurse left ++ [tag] ++ recurse right

-- Non-recursive.
mapStringTag : (t -> t) -> AppendedTaggedStrings t -> AppendedTaggedStrings t
mapStringTag f appendedTaggedStrings =
  case appendedTaggedStrings of
    TaggedString string tagSet           -> TaggedString string (f tagSet)
    TaggedStringAppend left right tagSet -> TaggedStringAppend left right (f tagSet)

-- Recursive
mapStringTags : (t1 -> t2) -> AppendedTaggedStrings t1 -> AppendedTaggedStrings t2
mapStringTags f taggedString =
  let recurse = mapStringTags f in
  case taggedString of
    TaggedString string tagSet           -> TaggedString string (f tagSet)
    TaggedStringAppend left right tagSet -> TaggedStringAppend (recurse left) (recurse right) (f tagSet)

-- replaceTagSet : t -> AppendedTaggedStrings t -> AppendedTaggedStrings t
-- replaceTagSet newTagSet appendedTaggedStrings =
--   mapTagSet (\_ -> newTagSet) appendedTaggedStrings
