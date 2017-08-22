module Whitespace exposing
  ( Whitespace
  , dummyWhitespace
  , whitespace_
  )

import Position exposing (dummyPosition)
import Range exposing (Ranged)

type alias Whitespace =
  Ranged
    { ws : String }

dummyWhitespace : Whitespace
dummyWhitespace =
  { start = dummyPosition
  , end = dummyPosition
  , ws = ""
  }

whitespace_ : String -> Whitespace
whitespace_ ws =
  { dummyWhitespace
      | ws = ws
  }
