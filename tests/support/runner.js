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
        testsToRun.push(item);
      }
    }
  }
}


var passCount = 0;
var failCount = 0;
var failureMessages = [];

// Input:
// { ctor: '::',
// _0: [Function],
// _1:
//  { ctor: '::',
//    _0: [Function],
//    _1: { ctor: '::', _0: [Function], _1: [Object] } } }
//
// Output:
// [ [Function], [Function], ... ]
function arrayifyElmList(elmList) {
  var list = [];
  var head = elmList;
  while (head.ctor === "::") {
    list.unshift(head._0);
    head = head._1;
  }
  return list;
}

function failTest(failureMessage) {
  failCount++;
  console._stdout.write(red("F"));
  failureMessages.push(failureMessage);
}

while (testsToRun.length > 0) {

  var test = testsToRun.shift();
  var result = test();

  if (typeof result === "string") {

    // We have a test result!
    if (result === "ok") {
      passCount++;
      console._stdout.write(green("."));
    } else {
      failTest(result);
    }


  } else if (result.ctor === "::" && typeof result._0 === "function") {

    // We have a list of more tests to run!
    var newTests = arrayifyElmList(result);
    testsToRun = testsToRun.concat(newTests);

  } else {

    failTest("Test did not return a string or list of functions: " + test.toString());

  }

}

// The .......FF.....F..F... stuff doesn't have a newline
if (passCount + failCount > 0) {
  console._stdout.write("\n");
}

// Output failure messages, if any.
if (failCount > 0) {
  console.error("");
  for (i in failureMessages) {
    console.error(red(failureMessages[i]));
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
