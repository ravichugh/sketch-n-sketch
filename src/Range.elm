module Range exposing
  ( Ranged
  , dummyRange
  )

import Position exposing
  ( Position
  , dummyPosition
  )

type alias Ranged a =
  { a
      | start : Position
      , end : Position
  }

dummyRange : Ranged {}
dummyRange =
  { start = dummyPosition
  , end = dummyPosition
  }
