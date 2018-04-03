// This runner looks for and runs somethingTest functions in
// TestSomething.elm files.
//
// Each somethingTest function can return either:
// 1. a string ("ok" test result or a failure message), or
// 2. more tests as a list of functions of () -> String
//    (which in turn might return a string or a list)
//
// Invoke this runner with the ./test.sh script.

const fs = require("fs");

// Load the Elm program into our namespace.
with (global) {
  eval(fs.readFileSync(__dirname + "/../build/benchmarks.js", "utf8"));
}