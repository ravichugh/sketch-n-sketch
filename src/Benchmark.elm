module Benchmark where

import Native.Benchmark


logDuration : String -> (() -> a) -> a
logDuration label f =
  let _ = Native.Benchmark.start label in
  let result = f () in
  let _ = Native.Benchmark.stop label in
  result


log : String -> a -> a
log label value =
  Native.Benchmark.log label value


silently f =
  let _ = Native.Benchmark.silence () in
  let result = f () in
  let _ = Native.Benchmark.unsilence () in
  result


setProgram : String -> String
setProgram exampleName =
  let _ = Native.Benchmark.setProgram exampleName in
  exampleName


setHeuristicsMode : Int -> Int
setHeuristicsMode mode =
  let modeName =
    case mode of
      0 -> "None"
      1 -> "Fair"
      2 -> "Biased"
      _ -> "Unknown"
  in
  let _ = Native.Benchmark.setHeuristicsMode modeName in
  mode


next : () -> ()
next () =
  let _ = Native.Benchmark.next () in
  ()
