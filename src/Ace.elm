module Ace exposing (..)

-- these are per-line "annotations" in Ace
type alias Annotation =
  { row   : Int     -- 0-indexed
  , type_ : String  -- {"info", "warning", "error"}
  , text  : String
  }

-- these are "tooltips" when hovering over tokens in Ace
type alias Tooltip =
  { row   : Int     -- 0-indexed
  , col   : Int     -- 0-indexed
  , text  : String
  }

-- these are "markers" in Ace
type alias Highlight =
  { range : Range
  , color : String
  }

type alias Pos =
  { row : Int       -- 1-indexed ?
  , column : Int    -- 1-indexed ?
  }

type alias Range = { start : Pos, end : Pos }
