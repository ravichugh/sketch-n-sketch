function existsParam(x) {
  return typeof process.argv.find(elem => elem == x) !== "undefined";
}
function getParam(x, defaultValue) {
  var param = process.argv.find(elem => elem.startsWith(x));
  if(typeof param !== "undefined") {
    return param.substring(x.length + 1);
  } else {
    return defaultValue;
  }
}

if(existsParam("--help") || existsParam("-h") || process.argv.length == 2) {
  console.log("Syntax:");
  console.log("node generate.js [--forward OR --backward] [--watch] [--autosync] [--input=dir]");
  console.log("");
  console.log("  -f, --forward   : Once, compute the outputs from the inputs");
  console.log("  -b, --backward  : Once, back-propagates changes from the outputs to the inputs");
  console.log("  -w, --watch     : Continually, watches for changes in the inputs and outputs");
  console.log("                If --backward is set, then it will immediately start to back-propagate changes.");
  console.log("                Else it regenerates the websites and listens to changes");
  console.log("  -a, --autosync  : If an ambiguity is found, choose the most likely solution");
  console.log("  --input=dir     : The directory from which to listen to changes in inputs. Default: .");
  return;
}
const watch = existsParam("--watch") || existsParam("-w");
const autosync = existsParam("--autosync") || existsParam("-a");
const forward = existsParam("--forward") || existsParam("-f");
const backward = existsParam("--backward") || existsParam("-b");
const inputDir = getParam("--input", ".")

const readline = require('readline');
const fs = require("fs")
const sns = require("sketch-n-sketch")
sns.params = sns.params || {};
sns.params.delayedWrite = true;
var generateElmScript = __dirname + "/generate.elm";

// Returns the set of files to be written, its representation as a Val, and the source of the script used
function computeForward(willwrite) {
  if(typeof willwrite == "undefined") willwrite = true;
  var source = fs.readFileSync(generateElmScript, "utf8");
  var valResult = sns.objEnv.string.evaluate({willwrite:willwrite})(source);
  var result = sns.process(valResult)(sns.valToNative);

  if(result.ctor == "Ok") {
    var filesToWrite = result._0;
    return [filesToWrite, valResult._0, source];
  } else {
    console.log("error while evaluating", result._0)
    return [false, false, false];
  }
}

function writeFiles(filesToWrite) {
  for(var i = 0; i < filesToWrite.length; i++) {
    var {_1: name, _2: content} = filesToWrite[i];
    fs.writeFileSync(name, content, "utf8");
  }
  console.log("Written all re-rendered results");
}

function computeAndWrite(willwrite) {
  var [filesToWrite, valFilesToWrite, source] = computeForward();
  if(filesToWrite) writeFiles(filesToWrite);
  return [filesToWrite, valFilesToWrite, source];
}

if(!watch && forward) {
  computeAndWrite();
  return;
}

// Apply the operations gathered by an update.
function applyUpdateOperations(operations) {
  for(var i = 0; i < operations.length; i++) {
    var [kind, action] = operations[i];
    if(kind == "write") {
      var {_1: name, _2: content} = action;
      fs.writeFileSync(name, content, "utf8");
    } else if (kind == "delete") {
      var name = action;
      fs.unlinkSync(name);
    }
  }
  // Now compute the pipeline forward
  return computeAndWrite();
}

// Given the output of the pipeline, reads the disk and computes [the new output, if the output has changed]
function getNewOutput(filesToWrite) {
  var newFilesToWrite = [];
  var hasChanged = false;
  for(var i = 0; i < filesToWrite.length; i++) {
    var {_1: name, _2: content} = filesToWrite[i];
    var newContent = fs.readFileSync(name, "utf8");
    if(newContent != content) hasChanged = true;
    newFilesToWrite.push({'$t_ctor': 'Tuple2', _1: name, _2: newContent});
  }
  return [newFilesToWrite, hasChanged];
}

function doUpdate(filesToWrite, valFilesToWrite, source) {
  sns.fileOperations = [];
  [filesToWrite, filesToWriteVal, source] = filesToWrite ? [filesToWrite, valFilesToWrite, source] : computeForward();
  if(!filesToWrite) return [false, false];
  var [newFilesToWrite, hasChanged] = getNewOutput(filesToWrite);
  if(!hasChanged) {
    console.log("Output website not modified.");
    if(!watch) {
      console.log("Finished!")
    }
    return [filesToWrite, filesToWriteVal, source];
  }
  var newFilesToWriteVal = sns.nativeToVal(newFilesToWrite);
  var resSolutions = sns.objEnv.string.updateWithOld({v:1})(source)(filesToWriteVal)(newFilesToWriteVal);
  if(resSolutions.ctor == "Err") {
    console.log("Error while updating: " + resSolutions._0);
    return [filesToWrite, filesToWriteVal, source];
  }
  var solutions = resSolutions._0;
  if(!sns.lazyList.nonEmpty(solutions)) {
    console.log("Error while updating, solution array is empty");
    return [filesToWrite, filesToWriteVal, source];
  }
  var {_0: newenv, _1: headSolution} = sns.lazyList.head(solutions);
  var headOperations = sns.fileOperations;
  if(headSolution != source) {
    headOperations.push(["write", {_1: generateElmScript, _2: headSolution}]);
  }
  // Check for ambiguity.
  console.log("Checking for ambiguity");
  sns.fileOperations = [];
  var tailSolutions = autosync ? {ctor: "Nil"} : sns.lazyList.tail(solutions);
  if(autosync || sns.lazyList.isEmpty(tailSolutions)) {
    console.log((autosync ? "--autosync not checking for ambiguities" : "No ambiguity found ") + "-- Applying the transformations");
    return applyUpdateOperations(headOperations);
  } else {
    console.log("Ambiguity found -- Computing the second solution");
    var {_0: newenv2, _1: headSolution2} = sns.lazyList.head(solutions);
    var headOperations2 = sns.fileOperations;
    sns.fileOperations = [];
    if(headSolution2 != source) {
      headOperations2.push(["write", {_1: generateElmScript, _2: headSolution2}]);
    }
    console.log("Ambiguity detected");
    console.log("Solution #1", headOperations);
    console.log("Solution #2", headOperations2);
    
    console.log("Which one should I apply? 1 or 2?");
    
    var rl = readline.createInterface({
      input: process.stdin,
      output: process.stdout,
      terminal: false
    });
    var returnValue;
    rl.on('line', function(line){
       if(line == "1") {
         returnValue = applyUpdateOperations(headOperations);
         rl.close();
       } else if(line == "2") {
         returnValue = applyUpdateOperations(headOperations2);
         rl.close();
       } else {
         console.log("Input not recognized. 1 or 2?");
       }
    })
    return returnValue;
  }
}

if(!watch && backward) {
  doUpdate();
  return;
}

// A timer use to trigger update only after a certain amount of time.
var changeTimer = false;
var watchers = [];

function unwatchEverything() {
  console.log("Unwatching files");
  for(var i = 0; i < watchers.length; i++) {
    watchers[i].close();
  }
  watchers = [];
}

function doWatch(filesToWrite, valFilesToWrite, source) {
  if(!filesToWrite) return;

  for(var i = 0; i < filesToWrite.length; i++) {
    var {_1: name, _2: content} = filesToWrite[i];
    var watcher =
      fs.watch(name, ((filesToWrite, valFilesToWrite, source) => (eventType, filename) => {
          if(eventType == "change") {
            if(changeTimer) {
              clearTimeout(changeTimer);
            }
            changeTimer =
              setTimeout(((filesToWrite, valFilesToWrite, source) => () => {
                changeTimer = false;
                unwatchEverything();
                [filesToWrite, valFilesToWrite, source] = doUpdate(filesToWrite, valFilesToWrite, source);
                doWatch(filesToWrite, valFilesToWrite, source);
              })(filesToWrite, valFilesToWrite, source), 100); // Time for all changes to be recorded
          }
      })(filesToWrite, valFilesToWrite, source));
    watchers.push(watcher);
  }
  watchers.push(fs.watch(inputDir, {recursive: true}, (eventType, generateElmScript) => {
    if(changeTimer) {
      clearTimeout(changeTimer);
    }
    changeTimer =
      setTimeout(() => {
        changeTimer = false;
        unwatchEverything();
        var [filesToWrite, source] = computeAndWrite();
        doWatch(filesToWrite, source);
      }, 100); // Time for all changes to be recorded
    
  }));
  console.log("Watching for changes on inputs or outputs...")
}


if(watch) {
  var filesToWrite;
  var source;
  var valFilesToWrite;
  if(backward && !forward) {
    [filesToWrite, valFilesToWrite, source] = doUpdate();
  }
  var [filesToWrite, valFilesToWrite, source] = backward && !forward ? [filesToWrite, valFilesToWrite, source] : computeForward();
  if((!backward || forward) && filesToWrite) { // Do the initial file write to make sure everything is consistent.
    writeFiles(filesToWrite);
  }
  console.log("watching with !filesToWrite", !filesToWrite);
  doWatch(filesToWrite, valFilesToWrite, source);
}
