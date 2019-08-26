module Core.Bridge exposing
  ( compile
  , Failable(..)
  , eval
  , synthesize
  )

import Http
import Json.Encode as E
import Json.Decode as D

import Lang
import Core.Lang as C
import Core.Compile
import Core.Encode
import Core.Decode

import Syntax

--------------------------------------------------------------------------------
-- Synchronous functions
--------------------------------------------------------------------------------

compile : String -> Result String (C.Exp, C.DatatypeContext)
compile code =
  case Syntax.parser Syntax.Leo code of
    Ok lexp ->
      case Core.Compile.exp lexp of
        Ok cexp ->
          -- call Types2.getDataTypeDefs lexp
          Ok (cexp, [])

        Err err ->
          Err (toString err)

    Err err ->
      Err (toString err)

--------------------------------------------------------------------------------
-- Asynchronous functions
--------------------------------------------------------------------------------

url : String
url =
  "http://lvh.me:9090"

type Failable a
  = HttpError Http.Error
  | ServerError String
  | Success a

type alias Async a msg =
  (Failable a -> msg) -> Cmd msg

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

send : Http.Request (Result String response) -> Async response msg
send request handler =
  let
    httpHandler : Result Http.Error (Result String response) -> msg
    httpHandler r =
      case r of
        Err e ->
          handler (HttpError e)

        Ok (Err e) ->
          handler (ServerError e)

        Ok (Ok x) ->
          handler (Success x)
  in
    Http.send httpHandler request

request :
  { action : String
  , encodedResponse : E.Value
  , responseDecoder : D.Decoder response
  } -> Async response msg
request {action, encodedResponse, responseDecoder} =
  send <|
    Http.request
      { method = "POST"
      , headers = []
      , url = url ++ "/" ++ action
      , body = Http.jsonBody encodedResponse
      , expect = Http.expectJson (resultDecoder responseDecoder D.string)
      , timeout = Nothing
      , withCredentials = False
      }

eval : C.Exp -> Async (Result String (C.Res, C.ResumptionAssertions)) msg
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
    -> Async (List C.HoleFilling, Float) msg
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
        D.map2 (,)
          ( D.field "hole_fillings" <|
              D.list <|
                D.list <|
                  D.map2 (,) Core.Decode.holeName Core.Decode.exp
          )
          ( D.field "time_taken" D.float
          )
    }


