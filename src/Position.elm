module Position exposing
  ( Position
  , fromRowCol
  , dummyPosition
  )

type alias Position =
  { row : Int
  , col : Int
  }

fromRowCol : (Int, Int) -> Position
fromRowCol (row, col) =
  { row = row
  , col = col
  }

dummyPosition : Position
dummyPosition =
  { row = -1
  , col = -1
  }
