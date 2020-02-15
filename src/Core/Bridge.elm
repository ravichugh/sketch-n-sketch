module Core.Bridge exposing
  ( Error(..)
  , Async
  , eval
  , synthesize
  )

import Task exposing (Task)

import Http
import Json.Encode as E
import Json.Decode as D

import Lang
import Core.Lang as C
import Core.Compile
import Core.Encode
import Core.Decode

import Syntax

url : String
url =
  "http://localhost:9090"

type Error
  = HttpError Http.Error
  | ServerError String

type alias Async a =
  Task Error a

resultDecoder : D.Decoder a -> D.Decoder e -> D.Decoder (Result e a)
resultDecoder ok err =
  Core.Decode.ctor <| \ctorName ->
    case ctorName of
      "Ok" ->
        D.map Ok (D.index 1 ok)

      "Error" ->
        D.map Err (D.index 1 err)

      _ ->
        D.fail "Ill-formed result (non-Ok, non-Error constructor)"

request :
  { action : String
  , encodedResponse : E.Value
  , responseDecoder : D.Decoder response
  } -> Async response
request {action, encodedResponse, responseDecoder} =
  { method = "POST"
  , headers = []
  , url = url ++ "/" ++ action
  , body = Http.jsonBody encodedResponse
  , expect = Http.expectJson (resultDecoder responseDecoder D.string)
  , timeout = Nothing
  , withCredentials = False
  }
    |> Http.request
    |> Http.toTask
    |> Task.mapError HttpError
    |> Task.andThen
         ( \result ->
             case result of
               Ok x ->
                 Task.succeed x

               Err e ->
                 Task.fail (ServerError e)
         )

eval : C.Exp -> Async (Result String (C.Res, C.ResumptionAssertions))
eval coreExp =
  request
    { action =
        "eval"

    , encodedResponse =
        Core.Encode.exp coreExp

    , responseDecoder =
        resultDecoder
          ( D.map2 (,)
              (D.field "res" Core.Decode.res)
              (D.field "assertions" Core.Decode.resumptionAssertions)
          )
          D.string
    }

synthesize :
  (C.HoleContext, C.DatatypeContext, C.ResumptionAssertions)
    -> Async (List C.HoleFilling, Float, Bool)
synthesize (delta, sigma, assertions) =
  request
    { action =
        "synthesize"

    , encodedResponse =
        E.object
          [ ("delta", Core.Encode.holeContext delta)
          , ("sigma", Core.Encode.datatypeContext sigma)
          , ("assertions", Core.Encode.resumptionAssertions assertions)
          ]

    , responseDecoder =
        D.map3 (,,)
          ( D.field "hole_fillings" <|
              D.list <|
                D.list <|
                  D.map2 (,)
                    (D.index 0 Core.Decode.holeName)
                    (D.index 1 Core.Decode.exp)
          )
          ( D.field "time_taken" D.float
          )
          ( D.field "timed_out" D.bool
          )
    }


