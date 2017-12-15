port module SyntaxHighlight exposing
  ( ExternalSyntaxHighlightMessage(..)
  , sendMessage
  )

import Json.Encode

import Syntax exposing (Syntax)

type ExternalSyntaxHighlightMessage
  = SetSyntax Syntax

type alias ExternalData =
  { tag : String
  , data : Json.Encode.Value
  }

sendMessage : ExternalSyntaxHighlightMessage -> Cmd msg
sendMessage em =
  case em of
    SetSyntax syntax ->
      externalSyntaxHighlightMessage
        { tag =
            "SetSyntax"
        , data =
            Syntax.encode syntax
        }

port externalSyntaxHighlightMessage : ExternalData -> Cmd msg
