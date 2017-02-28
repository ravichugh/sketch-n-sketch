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
const Elm = require("../build/test.js")

// Load the Elm program into our namespace.
with (global) {
  eval(fs.readFileSync(__dirname + "/../build/test.js", "utf8"));
}


function red(str) {
  return "\x1b[31m" + str + "\x1b[0m";
}

// Apologies to the red/green colorblind.
function green(str) {
  return "\x1b[32m" + str + "\x1b[0m";
}

function yellow(str) {
  return "\x1b[33m" + str + "\x1b[0m";
}


var testsToRun = [];
var testNameFilter;

if (process.env["SNS_TESTS_FILTER"]) {
  testNameFilter = new RegExp(process.env["SNS_TESTS_FILTER"], "i");
  console.log("Running " + yellow(testNameFilter.toString()) + " tests. Unset SNS_TESTS_FILTER to run all tests.");
} else {
  testNameFilter = new RegExp();
  console.log("Running all tests. Set SNS_TESTS_FILTER to filter tests by name.");
}

// Look for all SomethingTest(s) modules...
for (moduleName in Elm) {
  if (moduleName.match(/Tests?$/)) {
    var compiledModule = Elm[moduleName].make(Elm);

    // Gather all test(s)/somethingTest(s)/something_test(s) functions in the module
    for (itemName in compiledModule) {
      var item = compiledModule[itemName];
      if (itemName.match(/(^t|_t|T)ests?$/) &&
          (itemName.match(testNameFilter) || moduleName.match(testNameFilter)) &&
          typeof item === "function") {
        testsToRun.push({"name": itemName, "run":item});
      }
    }
  }
}


var passCount = 0;
var failCount = 0;
var failures = [];

// Input:
// { ctor: '::',
// _0: [Function],
// _1:
//  { ctor: '::',
//    _0: [Function],
//    _1: { ctor: '::', _0: [Function], _1: [Object] } } }
//
// Output:
// [ {"name":[String], "run":[Function]}, {"name":[String], "run":[Function]}, ... ]
function arrayifyElmList(elmList, rootName) {
  var list = [];
  var head = elmList;
  var count = 0;
  while (head.ctor === "::") {
    list.unshift({
      "name":rootName + "_GENERATEDTEST_" + count,
      "run": head._0
    });
    head = head._1;
    count = count + 1;
  }
  return list;
}

function failTest(name, message) {
  failCount++;
  console._stdout.write(red("F"));
  failures.push({"name":name, "message":message});
}

while (testsToRun.length > 0) {

  var test = testsToRun.shift();
  var result = test.run();

  if (typeof result === "string") {

    // We have a test result!
    if (result === "ok") {
      passCount++;
      console._stdout.write(green("."));
    } else {
      failTest(test.name, result);
    }


  } else if (result.ctor === "::" && typeof result._0 === "function") {

    // We have a list of more tests to run!
    var newTests = arrayifyElmList(result, test.name);
    testsToRun = testsToRun.concat(newTests);

  } else {

    failTest(test.name, "Test did not return a string or list of functions: " + test.toString());

  }

}

// The .......FF.....F..F... stuff doesn't have a newline
if (passCount + failCount > 0) {
  console._stdout.write("\n");
}

// Output failure messages, if any.
if (failCount > 0) {
  console.error("");
  for (i in failures) {
    console.error(red(failures[i].name));
    console.error(failures[i].message);
    console.error("");
  }
}

// Summary goes to stdout if everything passes, otherwise to stderr.
var report = (failCount === 0 ? console.log : console.error);

report("" + (passCount + failCount) + " tests: " + passCount + " passed, " + failCount + " failed.");

// Exit with non-success if any tests fail.
if (failCount > 0) {
  process.exit(1);
} else if (passCount > 0) {
  console.log(green(" ____   __    __  _  _  __   _ "));
  console.log(green("(  _ \\ /  \\  /  \\( \\/ )/ _\\ / \\"));
  console.log(green(" ) _ ((  O )(  O ))  //    \\\\_/"));
  console.log(green("(____/ \\__/  \\__/(__/ \\_/\\_/(_)"));
}
