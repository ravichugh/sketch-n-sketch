module Javascript exposing (..)

import Native.Javascript


tripleEqualsOperator : a -> a -> Bool
tripleEqualsOperator a b =
  Native.Javascript.tripleEqualsOperator a b
