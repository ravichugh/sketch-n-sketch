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
var generateElmScript = __dirname + "/generate.elm";

// Returns the set of files to be written, its representation as a Val, and the source of the script used
function computeForward(willwrite) {
  if(typeof willwrite == "undefined") willwrite = true;
  var source = fs.readFileSync(generateElmScript, "utf8");
  var valResult = sns.objEnv.string.evaluate({willwrite:willwrite, fileOperations:[]})(source);
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

function stringDiffSummary(oldString, newString, stringDiffs) {
  if(stringDiffs["$d_ctor"] == "Nothing") return "";
  var listStringDiffs = stringDiffs.args._1.args._1; // It's a VStringDiffs
  var offset = 0;
  var summary = "";
  for(var i = 0; i < listStringDiffs.length; i++) {
    var {args: {_1: start, _2: end, _3: replaced}} = listStringDiffs[i];
    var removed = oldString.substring(start, end);
    var inserted = newString.substring(start + offset, start + offset + replaced);
    var beforeRemoved = oldString.substring(0, start);
    var linesBeforeRemoved = beforeRemoved.split(/\r\n|\r|\n/);
    var lineNumber = linesBeforeRemoved.length;
    var charNumber = linesBeforeRemoved[linesBeforeRemoved.length - 1].length + 1;
    summary += "L" + lineNumber + "C" + charNumber + ", "
    if(removed == "")
      summary += "inserted '" + inserted + "'";
    else if(inserted == "")
      summary += "removed '" + removed + "'";
    else
      summary += "removed '" + removed + "', inserted '"+ inserted +"'";
  }
  return summary;
}

function fileOperationSummary(operations) {
  if(operations == null) return "";
  var summary = "";
  for(var i = 0; i < operations.length; i++) {
    var {_1: path, _2: action} = operations[i];
    if(summary != "") summary += "\n";
    if(action["$d_ctor"] == "Write") {
      summary += "Modify " + path + ", " + stringDiffSummary(action.args._1, action.args._2, action.args._3);
    } else if(action["$d_ctor"] == "Create") {
      summary += "Created " + path;
    } else if(action["$d_ctor"] == "Rename") {
      summary += "Renamed " + path + " to " + action.args._1;
    } else if(action["$d_ctor"] == "Delete") {
      summary += "Deleted " + path;
    } else {
      console.log("unrecognized action:", action);
    }
  }
  return summary;
}

// Apply the given operations to the file system. TODO: Merge different writes to a single file.
function applyOperations(operations) {
  for(var i = 0; i < operations.length; i++) {
    var {_1: path, _2: action} = operations[i];
    if(action["$d_ctor"] == "Write") {
      fs.writeFileSync(path, action.args._2, "utf8");
    } else if(action["$d_ctor"] == "Create") {
      // TODO: Create the path if necessary
      fs.writeFileSync(path, action.args._1, "utf8");
    } else if(action["$d_ctor"] == "Rename") {
      fs.renameSync(path, action.args._1);
    } else if(action["$d_ctor"] == "Delete") {
      fs.unlinkSync(path);
    } else {
      console.log("unrecognized action:", action);
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

function doUpdate(filesToWrite, valFilesToWrite, source, callback) {
  if(typeof callback == "undefined") {
    console.log("doUpdate should have a callback");
    return;
  }
  [filesToWrite, filesToWriteVal, source] = filesToWrite ? [filesToWrite, valFilesToWrite, source] : computeForward();
  if(!filesToWrite) return callback(false, false, false);
  var [newFilesToWrite, hasChanged] = getNewOutput(filesToWrite);
  if(!hasChanged) {
    console.log("Output website not modified.");
    if(!watch) {
      console.log("Finished!")
    }
    return callback(filesToWrite, filesToWriteVal, source);
  }
  var newFilesToWriteVal = sns.nativeToVal(newFilesToWrite);
  var resSolutions = sns.objEnv.string.updateWithOld({v:1, fileOperations: []})(source)(filesToWriteVal)(newFilesToWriteVal);
  console.log("finished to update");
  if(resSolutions.ctor == "Err") {
    console.log("Error while updating: " + resSolutions._0);
    return callback(filesToWrite, filesToWriteVal, source);
  }
  var solutions = resSolutions._0;
  if(!sns.lazyList.nonEmpty(solutions)) {
    console.log("Error while updating, solution array is empty");
    return callback(filesToWrite, filesToWriteVal, source);
  }
  var {_0: newEnv, _1: headSolution} = sns.lazyList.head(solutions);
  var headOperations = newEnv.fileOperations;
  function maybeAddGeneratorDiff(operations, oldSource, newSource) {
    if(newSource != oldSource) { // server file itself modified from the update method
      var d =
        sns.process(sns.objEnv.string.evaluate({x: oldSource, y: newSource})(`__diff__ x y`))(sns.valToNative)
      var diffssource = 
        d.ctor == "Ok" ? d._0 ? d._0.args ? d._0.args._1 ? d._0.args._1.args ? d._0.args._1.args._1 ? d._0.args._1.args._1 :
          false : false : false : false : false : false;

      operations.unshift(
        {"$t_ctor": "Tuple2",
         _1: serverFile,
         _2: {"$d_ctor": "Write",
         args: {_1: oldSource, _2: newSource, _3: diffssource}}
         });
    }
  }
  
  maybeAddGeneratorDiff(headOperations, source, headSolution);
  // Check for ambiguity.
  console.log("Checking for ambiguity");
  var tailSolutions = autosync ? {ctor: "Nil"} : sns.lazyList.tail(solutions);
  if(autosync || sns.lazyList.isEmpty(tailSolutions)) {
    console.log((autosync ? "--autosync not checking for ambiguities" : "No ambiguity found ") + "-- Applying the transformations");
    [a, b, c] = applyOperations(headOperations);
    return callback(a, b, c);
  } else {
    var solutions = [headOperations];
    console.log("Ambiguity found -- Computing the second solution");
    var solutionsRemaining = tailSolutions;
    var oneMoreSolution = () => {
      var {_0: alternativeEnv, _1: alternativeSolution} = sns.lazyList.head(solutionsRemaining);
      var alternativeOperations = alternativeEnv.fileOperations;
      maybeAddGeneratorDiff(alternativeOperations, source, alternativeSolution);
      solutions.push(alternativeOperations);
    }
    
    console.log("Ambiguity detected");
    
    function askSolutions() {
      for(var i = 0; i < solutions.length; i++) {
        console.log(`Solution #${i+1}`, fileOperationSummary(solutions[i]));
      }
      
      console.log(`Which solution number should I apply?${solutionsRemaining !== false ? ' Find [more] solutions?': ''} [cancel]?`);
      
      var rl = readline.createInterface({
        input: process.stdin,
        output: process.stdout,
        terminal: false
      });
      rl.on('line', function(line){
        if(line.toLowerCase() == "more" && solutionsRemaining !== false) {
          rl.close();
          solutionsRemaining = sns.lazyList.tail(solutionsRemaining);
          if(sns.lazyList.isEmpty(solutionsRemaining)) {
            console.log("No other solution found")
            solutionsRemaining = false;
          } else {
            oneMoreSolution();
          }
        } else if(line.toLowerCase() == "cancel") {
          rl.close();
          callback(filesToWrite, valFilesToWrite, source);
        } else {
          var selectedSolution = solutions[parseInt(line) - 1];
          if(selectedSolution != null) {
            rl.close();
            [a, b, c] = applyOperations(selectedSolution);
            callback(a, b, c);
          } else {
            console.log(`Input not recognized.
${solutions.map((elem, index) => "" + (index + 1)).join(", ")}${solutionsRemaining !== false ? ', [more]': ''} or 'cancel' accepted.`);
          }
        }
      })
    }
    return [];
  }
}

if(!watch && backward) {
  doUpdate(false, false, false, () => {});
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
                doUpdate(filesToWrite, valFilesToWrite, source, doWatch);
              })(filesToWrite, valFilesToWrite, source), 500); // Time for all changes to be recorded
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
      }, 500); // Time for all changes to be recorded
    
  }));
  console.log("Watching for changes on inputs or outputs...")
}


if(watch) {
  var filesToWrite;
  var source;
  var valFilesToWrite;
  var continuation = (filesToWrite, valFilesToWrite, source) => {
    var [filesToWrite, valFilesToWrite, source] = filesToWrite ? [filesToWrite, valFilesToWrite, source] : computeForward();
    if((!backward || forward) && filesToWrite) { // Do the initial file write to make sure everything is consistent.
      writeFiles(filesToWrite);
    }
    console.log("watching with !filesToWrite", !filesToWrite);
    doWatch(filesToWrite, valFilesToWrite, source);
  }
  if(backward && !forward) {
    doUpdate(false, false, false, continuation);
  } else {
    continuation(false, false, false);
  }
  
  
}
