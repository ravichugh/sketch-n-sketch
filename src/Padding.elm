module Padding exposing
  ( Padded
  )

import Whitespace exposing (Whitespace)

type alias Padded a =
  { a
      | before : Whitespace
      , after : Whitespace
  }
