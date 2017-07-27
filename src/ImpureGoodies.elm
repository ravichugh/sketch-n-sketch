module ImpureGoodies exposing (..)

import Native.ImpureGoodies


randomFloat : () -> Float
randomFloat () =
  Native.ImpureGoodies.randomFloat ()


randomInt : Int -> Int -> Int
randomInt low highNonInclusive =
  let range = highNonInclusive - low in
  randomFloat () * toFloat range + toFloat low |> floor


-- Runs thunk. If thunk crashes, log the error to the console and return Nothing.
crashToNothing : (() -> a) -> Maybe a
crashToNothing thunk =
  Native.ImpureGoodies.crashToNothing thunk
