module Pos exposing
  ( Pos
  , startPos
  , dummyPos
  , posFromRowCol
  , WithPos
  )

type alias Pos =
  { line : Int
  , col : Int
  }

type alias WithPos a =
  { val : a
  , pos : Pos
  }

startPos : Pos
startPos =
  { line = 1
  , col = 1
  }

dummyPos : Pos
dummyPos =
  { line = -1
  , col = -1
  }

posFromRowCol : (Int, Int) -> Pos
posFromRowCol (row, col) =
  { line = row
  , col = col
  }
