module Core.Encode exposing
  ( exp
  , resumptionAssertions
  , datatypeContext
  , holeContext
  )

import Json.Encode as E exposing (..)
import Core.Lang as C exposing (..)

ctor : String -> List E.Value -> E.Value
ctor name args =
  list (string name :: args)

option : (a -> E.Value) -> Maybe a -> E.Value
option encode mx =
  case mx of
    Just x ->
      encode x

    Nothing ->
      null

exp : Exp -> E.Value
exp e =
  case e of
    EFix f x body ->
      ctor "EFix" [option string f, string x, exp body]

    EApp e1 e2 ->
      ctor "EApp" [exp e1, exp e2]

    EVar x ->
      ctor "EVar" [string x]

    ETuple es ->
      ctor "ETuple" [list (List.map exp es)]

    EProj n i arg ->
      ctor "EProj" [int n, int i, exp arg]

    ECtor ctorName arg ->
      ctor "ECtor" [string ctorName, exp arg]

    ECase scrutinee branches ->
      let
        branch : (String, (String, Exp)) -> E.Value
        branch (ctorName, (argName, body)) =
          list [string ctorName, list [string argName, exp body]]
      in
        ctor "ECase"
          [ exp scrutinee
          , list (List.map branch branches)
          ]

    EHole holeName ->
      ctor "EHole" [int holeName]

    EAssert e1 e2 ->
      ctor "EAssert" [exp e1, exp e2]

typ : Typ -> E.Value
typ t =
  case t of
    TArr t1 t2 ->
      ctor "TArr" [typ t1, typ t2]

    TTuple ts ->
      ctor "TTuple" [list (List.map typ ts)]

    TData name ->
      ctor "TData" [string name]

res : Res -> E.Value
res r =
  case r of
    RFix en f x body ->
      ctor "RFix" [env en, option string f, string x, exp body]

    RTuple rs ->
      ctor "RTuple" [list (List.map res rs)]

    RCtor ctorName arg ->
      ctor "RCtor" [string ctorName, res arg]

    RHole en holeName ->
      ctor "RHole" [env en, int holeName]

    RApp r1 r2 ->
      ctor "RApp" [res r1, res r2]

    RProj n i arg ->
      ctor "RProj" [int n, int i, res arg]

    RCase en scrutinee branches ->
      let
        branch : (String, (String, Exp)) -> E.Value
        branch (ctorName, (argName, body)) =
          list [string ctorName, list [string argName, exp body]]
      in
        ctor "RCase"
          [ env en
          , res scrutinee
          , list (List.map branch branches)
          ]

env : Env -> E.Value
env =
  let
    envBinding : (String, Res) -> E.Value
    envBinding (x, r) =
      list [string x, res r]
  in
    List.map envBinding >> list

bindSpec : BindSpec -> E.Value
bindSpec bs =
  case bs of
    NoSpec ->
      ctor "NoSpec" []

    Rec f ->
      ctor "Rec" [string f]

    Arg f ->
      ctor "Arg" [string f]

    Dec f ->
      ctor "Dec" [string f]

typeContext : TypeContext -> E.Value
typeContext =
  let
    typeBinding : TypeBinding -> E.Value
    typeBinding (x, (tau, bs)) =
      list [string x, list [typ tau, bindSpec bs]]
  in
    List.map typeBinding >> list

datatypeContext : DatatypeContext -> E.Value
datatypeContext =
  let
    constructor : (String, Typ) -> E.Value
    constructor (ctorName, argType) =
      list [string ctorName, typ argType]

    datatypeBinding : (String, List (String, Typ)) -> E.Value
    datatypeBinding (datatypeName, constructors) =
      list [string datatypeName, list (List.map constructor constructors)]
  in
    List.map datatypeBinding >> list

holeContext : HoleContext -> E.Value
holeContext =
  let
    holeBinding : (HoleName, (TypeContext, Typ, BindSpec)) -> E.Value
    holeBinding (holeName, (gamma, tau, bs)) =
      list [int holeName, list [typeContext gamma, typ tau, bindSpec bs]]
  in
    List.map holeBinding >> list

value : C.Value -> E.Value
value v =
  case v of
    VTuple vs ->
      ctor "VTuple" [list (List.map value vs)]

    VCtor ctorName arg ->
      ctor "VCtor" [string ctorName, value arg]

resumptionAssertions : ResumptionAssertions -> E.Value
resumptionAssertions =
  let
    resumptionAssertion : ResumptionAssertion -> E.Value
    resumptionAssertion (r, v) =
      list [res r, value v]
  in
    List.map resumptionAssertion >> list
