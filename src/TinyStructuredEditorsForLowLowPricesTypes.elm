module TinyStructuredEditorsForLowLowPricesTypes exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)
import String

import Lang
import Sync
import Types2


----------- State and Caching on the Model -----------

-- I hate caching but if we instead perform the work on
-- demand in the view then the GUI slows to a crawl.
type alias ModelState =
  { renderingFunctionNames                : List Ident
  , dataTypeDefs                          : List Types2.DataTypeDef
  , maybeRenderingFunctionNameAndProgram  : Maybe { renderingFunctionName: Ident, multipleDispatchFunctions : MultipleDispatchFunctions, desugaredToStringProgram: Exp }
  , showWidgets                           : Bool
  , valueOfInterestTagged                 : TaggedValue
  , stringTaggedWithProjectionPathsResult : Result String StringTaggedWithProjectionPaths
  , stringProjectionPathToSpecificActions : Dict ProjectionPath (List SpecificAction)
  , selectedPaths                         : Set ProjectionPath
  , maybeTextEditingPathAndText           : Maybe (ProjectionPath, String)
  , maybeNewValueOptions                  : Maybe (List TaggedValue)
  , liveSyncInfo                          : Sync.LiveInfo
  }

initialModelState =
  { renderingFunctionNames                = []
  , dataTypeDefs                          = []
  , maybeRenderingFunctionNameAndProgram  = Nothing
  , showWidgets                           = False -- For creating new toString examples without seeing the generated TSEFLLP editor.
  , valueOfInterestTagged                 = noTag (VCtor "Nothing" [])
  , stringTaggedWithProjectionPathsResult = Err "No trace for toString call yet—need to run the code."
  , stringProjectionPathToSpecificActions = Dict.empty
  , selectedPaths                         = Set.empty
  , maybeTextEditingPathAndText           = Nothing
  , maybeNewValueOptions                  = Nothing
  , liveSyncInfo                          = { triggers = Dict.empty, initSubstPlus = Dict.empty }
  }


----------- Basics -----------

type alias Nat   = Int
type alias Ident = String


----------- Core Language -----------

-- Mutual recursion is handled during desugaring.
-- (Desugared into single recursions following https://caml.inria.fr/pub/docs/u3-ocaml/ocaml-ml.html#toc5).

-- Ad-hoc polymophism is handled by type-based multiple dispatch.
-- Certain function names are designated "dynamic" and may have multiple
-- implementations with different type signatures. At runtime, the argument
-- value is inspected and the implementation with the most specific matching
-- type signature is dispatched.
--
-- Implementation:
--
-- At desguaring time, functions with a dynamic name are identified and each
-- implementation binding is renamed to a fresh unique name. The sugaring
-- returns a list of (original name, type annotation, unique name) to facilitate
-- dynamic dispatch in the evaluator.
--
-- In the evaluator, variable uses matching a dynamic name (e.g. "toString")
-- evaluate to a special VClosureDynamic that simply records the name. When a
-- VClosureDynamic value is applied to an argument, the runtime inspects the
-- recored dynamic name, the type of the argument based on its runtime value,
-- and the list of (original name, type annotation, unique name) mappings to
-- choose a an implementation of the function to use; the implementation is
-- dispatched by looking up the unique name in the execution environment.
--
-- Additionally, on all function applications (regular or dynamic), any dynamic
-- implementations visible at the callsite are added to the execution
-- environment for the function body. Provided all dynamic implementations are
-- defined at the top level, and no top-level computation before the main
-- expression calls a dynamic name, then we can ensure that all dynamic
-- implementations are available at all dynamic calls.
--
-- Limitations:
--
-- 1. Because the runtime type is determined via inspecting the value rather
-- than via instantiating type dictionaries, polymorphic ADTs with constructors
-- that do not use all the type variables cannot be inferred specifically. For
-- example, the value "Nil" can only be interpreted as "List a", even if it might
-- be used in a context where the static type is "List Num". Consequently, the
-- dynamic dispatch does not attempt to instantiate type variables. That is,
-- dispatching on the argument "[1,2,3]" will look for a toString function of
-- type "List a -> String" rather than "List Num -> String".
--
-- 2. Only single argument functions are supported for now.
--
-- 3. Currently, the only dynamic name is "toString", although syntax
-- could be added to make the set of dynamic names user-defined.


type Exp -- Expressions e :=
  = EVar Ident -- x
  | EFun Ident Ident Exp -- f(x).e
  | EApp Exp Exp -- e1 e2
  | ECtor Ident (List Exp) -- C (e1,...,en)
  | ECase Exp (List (Ident, TuplePattern, Exp)) -- case e of Ci pi -> ei
  | EString String -- s
  | EAppend Exp Exp -- e1 ++ e2
  | ENum Float -- n
  | ENumToString Exp -- numToStr e

type alias MultipleDispatchFunctions = List (Ident, Lang.Type, Ident) -- Name, Type annotation, Desugared unique name

type alias TuplePattern = List Ident -- Tuple Patterns p := (x1,...,xn)

type alias ProjectionPath = List Nat -- Projection Paths π := • | π.i

type alias Env = List (Ident, TaggedValue) -- Tagged Environments E := — | E,x↦w

type alias TaggedValue = { v : UntaggedPreValue, paths : Set ProjectionPath } -- (Tagged) Values w := v^{π1,...,πn}

type UntaggedPreValue -- (Untagged) Pre-Values v :=
  = VClosure Env Ident Ident Exp -- [E]f(x).e
  | VClosureDynamic Ident -- Ad-hoc polymorphism via type-based dispatch of named function.
  | VCtor Ident (List TaggedValue) -- C (w1,...,wn)
  | VString String -- s
  | VAppend TaggedValue TaggedValue -- w1 ++ w2
  | VNum Float -- n

noTag : UntaggedPreValue -> TaggedValue
noTag v = TaggedValue v Set.empty

-- Bottom-up
mapTaggedValue : (TaggedValue -> TaggedValue) -> TaggedValue -> TaggedValue
mapTaggedValue f w =
  let recurse = mapTaggedValue f in
  case w.v of
    VClosure funcEnv fName varName body -> f w
    VClosureDynamic ident               -> f w
    VCtor ctorName ws                   -> f { w | v = VCtor ctorName (List.map recurse ws) }
    VString string                      -> f w
    VAppend w1 w2                       -> f { w | v = VAppend (recurse w1) (recurse w2) }
    VNum num                            -> f w

-- For debugging.
unparseToUntaggedString : TaggedValue -> String
unparseToUntaggedString taggedValue =
  let recurse = unparseToUntaggedString in
  case taggedValue.v of
    VClosure env fName varName fBody -> "[E]" ++ fName ++ "(" ++ varName ++ ").e"
    VClosureDynamic ident            -> "«" ++ ident ++ "»"
    VCtor ctorName argVals           -> ctorName ++ "(" ++ String.join ", " (List.map recurse argVals) ++ ")"
    VString string                   -> toString string
    VAppend left right               -> recurse left ++ " ++ " ++ recurse right
    VNum number                      -> toString number



----------- Workflow Functions -----------

type ChangeType
  = ChangeCtor
  | Remove
  | Insert

type SpecificAction
  = NewValue ChangeType ProjectionPath TaggedValue -- TaggedValue is a new whole value, starting from the root. ProjectionPath is just where in the current value to associate this action.
  | Scrub ProjectionPath                           -- ProjectionPath indicates both where this action should be associated and the location of the value to scrub.
  | EditText ProjectionPath                        -- ProjectionPath indicates both where this action should be associated and the location of the value to scrub.

specificActionProjectionPath : SpecificAction -> ProjectionPath
specificActionProjectionPath specificAction =
  case specificAction of
    NewValue _ projectionPath _ -> projectionPath
    Scrub projectionPath        -> projectionPath
    EditText projectionPath     -> projectionPath


specificActionMaybeChangeType : SpecificAction -> Maybe ChangeType
specificActionMaybeChangeType specificAction =
  case specificAction of
    NewValue changeType _ _ -> Just changeType
    _                       -> Nothing


specificActionMaybeNewValue : SpecificAction -> Maybe TaggedValue
specificActionMaybeNewValue specificAction =
  case specificAction of
    NewValue _ _ newValue -> Just newValue
    _                     -> Nothing

isScrubSpecificAction : SpecificAction -> Bool
isScrubSpecificAction specificAction =
  case specificAction of
    Scrub _ -> True
    _       -> False

isEditTextSpecificAction : SpecificAction -> Bool
isEditTextSpecificAction specificAction =
  case specificAction of
    EditText _ -> True
    _          -> False


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

