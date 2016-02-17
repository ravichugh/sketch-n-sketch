module Helpers.Utils where

import Regex

-- Replace all runs of whitespace with a single space
squish : String -> String
squish str =
  Regex.replace Regex.All (Regex.regex "\\s+") (\_ -> " ") str
