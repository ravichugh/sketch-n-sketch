module Ace where

-- these are per-line "annotations" in Ace
type alias Annotation =
  { row   : Int     -- 0-indexed
  , type_ : String  -- {"info", "warning", "error"}
  , text  : String
  }
