module Helpers.Utils exposing (..)

import Regex
import String

-- Replace all runs of whitespace with a single space
squish : String -> String
squish str =
  String.trim <|
    Regex.replace Regex.All (Regex.regex "\\s+") (\_ -> " ") str
