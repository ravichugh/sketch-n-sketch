module Constraints exposing
  ( assertEqual
  )

import UnLang exposing (..)

assertEqual : UnExp d -> UnExp d -> Constraints
assertEqual u1 u2 =
  [ (-1, ([], ExNum 2)) ]
