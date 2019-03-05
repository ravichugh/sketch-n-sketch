module TinyStructuredEditorsForLowLowPricesTypes exposing (..)

import Set exposing (Set)

import Lang

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

type UntaggedPreValue -- (Untagged) Pre-Values v :=
  = VClosure Env Ident Exp -- [E]λx.e
  | VCtor Ident (List TaggedValue) -- C (w1,...,wn)
  | VString String -- s
  | VAppend TaggedValue TaggedValue -- w1 ++ w2
  | VNum Float



----------- Workflow Functions -----------

type alias RenderingFunction = String

type SpecificAction
  = Transform ProjectionPath (Lang.Val -> Lang.Val)
  | Scrub Lang.Val

type AppendedTaggedStrings t
  = TaggedString String (Set t)
  | TaggedStringAppend (AppendedTaggedStrings t) (AppendedTaggedStrings t) (Set t)

type alias StringTaggedWithProjectionPaths = AppendedTaggedStrings ProjectionPath
type alias StringTaggedWithSpecificActions = AppendedTaggedStrings SpecificAction
