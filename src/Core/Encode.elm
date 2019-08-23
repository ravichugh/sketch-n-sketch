module Core.Encode exposing
  ( exp
  , typ
  , res
  , datatypeContext
  )

import Json.Encode exposing (..)
import Core.Lang exposing (..)

ctor : String -> List Value -> Value
ctor name args =
  list (string name :: args)

option : (a -> Value) -> Maybe a -> Value
option encode mx =
  case mx of
    Just x ->
      encode x

    Nothing ->
      null

exp : Exp -> Value
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
        branch : (String, (String, Exp)) -> Value
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

typ : Typ -> Value
typ t =
  case t of
    TArr t1 t2 ->
      ctor "TArr" [typ t1, typ t2]

    TTuple ts ->
      ctor "TTuple" [list (List.map typ ts)]

    TData name ->
      ctor "TData" [string name]

res : Res -> Value
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
        branch : (String, (String, Exp)) -> Value
        branch (ctorName, (argName, body)) =
          list [string ctorName, list [string argName, exp body]]
      in
        ctor "RCase"
          [ env en
          , res scrutinee
          , list (List.map branch branches)
          ]

env : Env -> Value
env =
  let
    envBinding : (String, Res) -> Value
    envBinding (x, r) =
      list [string x, res r]
  in
    List.map envBinding >> list

datatypeContext : DatatypeContext -> Value
datatypeContext =
  let
    constructor : (String, Typ) -> Value
    constructor (ctorName, argType) =
      list [string ctorName, typ argType]

    datatypeBinding : (String, List (String, Typ)) -> Value
    datatypeBinding (datatypeName, constructors) =
      list [string datatypeName, list (List.map constructor constructors)]
  in
    List.map datatypeBinding >> list
