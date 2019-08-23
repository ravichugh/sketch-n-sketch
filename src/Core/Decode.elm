module Core.Decode exposing
  ( exp
  , typ
  , res
  )

import Json.Decode exposing (..)
import Core.Lang exposing (..)

ctor : (String -> Decoder a) -> Decoder a
ctor f =
  index 0 string |> andThen f

option : Decoder a -> Decoder (Maybe a)
option decode =
  oneOf
    [ map Just decode
    , null Nothing
    ]

exp : Decoder Exp
exp =
  ctor <| \ctorName ->
    case ctorName of
      "EFix" ->
        map3 EFix
          (index 1 (option string))
          (index 2 string)
          (index 3 exp)

      "EApp" ->
        map2 EApp
          (index 1 exp)
          (index 2 exp)

      "EVar" ->
        map EVar
          (index 1 string)

      "ETuple" ->
        map ETuple
          (index 1 (list exp))

      "EProj" ->
        map3 EProj
          (index 1 int)
          (index 2 int)
          (index 3 exp)

      "ECtor" ->
        map2 ECtor
          (index 1 string)
          (index 2 exp)

      "ECase" ->
        let
          branch : Decoder (String, (String, Exp))
          branch =
            map2 (,)
              ( index 1 string
              )
              ( index 2 <|
                  map2 (,)
                    (index 1 string)
                    (index 2 exp)
              )
        in
          map2 ECase
            (index 1 exp)
            (index 2 (list branch))

      "EHole" ->
        map EHole
          (index 1 int)

      "EAssert" ->
        map2 EAssert
          (index 1 exp)
          (index 2 exp)

      _ ->
        fail "Ill-formed expression"

typ : Decoder Typ
typ =
  ctor <| \ctorName ->
    case ctorName of
      "TArr" ->
        map2 TArr
          (index 1 typ)
          (index 2 typ)

      "TTuple" ->
        map TTuple
          (index 1 (list typ))

      "TData" ->
        map TData
          (index 1 string)

      _ ->
        fail "Ill-formed type"

res : Decoder Res
res =
  ctor <| \ctorName ->
    case ctorName of
      "RFix" ->
        map4 RFix
          (index 1 env)
          (index 2 (option string))
          (index 3 string)
          (index 4 exp)

      "RTuple" ->
        map RTuple
          (index 1 (list res))

      "RCtor" ->
        map2 RCtor
          (index 1 string)
          (index 2 res)

      "RHole" ->
        map2 RHole
          (index 1 env)
          (index 2 int)

      "RApp" ->
        map2 RApp
          (index 1 res)
          (index 2 res)

      "RProj" ->
        map3 RProj
          (index 1 int)
          (index 2 int)
          (index 3 res)

      "RCase" ->
        let
          branch : Decoder (String, (String, Exp))
          branch =
            map2 (,)
              ( index 1 string
              )
              ( index 2 <|
                  map2 (,)
                    (index 1 string)
                    (index 2 exp)
              )
        in
          map3 RCase
            (index 1 env)
            (index 2 res)
            (index 3 (list branch))

      _ ->
        fail "Ill-formed result"

env : Decoder Env
env =
  let
    envBinding : Decoder (String, Res)
    envBinding =
      map2 (,)
        (index 1 string)
        (index 2 res)
  in
    list envBinding
