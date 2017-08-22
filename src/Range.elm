module Range exposing
  ( Ranged
  )

import Position exposing (Position)

type alias Ranged a =
  { a
      | start : Position
      , end : Position
  }
