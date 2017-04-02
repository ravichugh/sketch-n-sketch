module Helpers.Matchers exposing (..)


expectEqual : a -> a -> String
expectEqual actual expected =
  if actual == expected then
    "ok"
  else
    "expected " ++ toString expected ++ "\nbut got  " ++ toString actual
