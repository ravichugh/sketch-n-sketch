
const readline = require('readline');
const fs = require("fs")
const sns = require("sketch-n-sketch")
sns.params = sns.params || {};
sns.params.delayedWrite = true;

filename = __dirname + "/generate.elm";

var source = fs.readFileSync(filename, "utf8");

var result = sns.process(sns.objEnv.string.evaluate({write:"False"})(source))(
  sns.valToNative
)

function applyOperations(operations) {
  for(var i = 0; i < operations.length; i++) {
    var [kind, action] = operations[i];
    if(kind == "write") {
      var [name, content] = action;
      fs.writeFileSync(name, content, "utf8");
    } else if (kind == "delete") {
      var name = action;
      fs.unlinkSync(name);
    }
  }
  require("./forward.js")
}

if(result.ctor == "Ok") {
  sns.fileOperations = [];
  var writtenFiles = result._0;
  var newWrittenFiles = [];
  for(var i = 0; i < writtenFiles.length; i++) {
    var {_1: name, _2: content} = writtenFiles[i];
    var newContent = fs.readFileSync(name, "utf8");
    newWrittenFiles.push({'$t_ctor': 'Tuple2', _1: name, _2: newContent});
  }
  var newWrittenFilesVal = sns.nativeToVal(newWrittenFiles);
  var resSolutions = sns.objEnv.string.update({v:1})(source)(newWrittenFilesVal);
  if(resSolutions.ctor == "Ok") {
    var solutions = resSolutions._0;
    if(sns.lazyList.nonEmpty(solutions)) {
      var {_1: newenv, _2: headSolution} = sns.lazyList.head(solutions);
      var headOperations = sns.fileOperations;
      if(headSolution != source) {
        headOperations.push(["write", [filename, headSolution]]);
      }
      sns.fileOperations = [];
      // Check for ambiguity.
      var tailSolutions = sns.lazyList.tail(solutions);
      if(sns.lazyList.isEmpty(tailSolutions)) {
        console.log("No ambiguity found -- Applying the transformations");
        applyOperations(headOperations);
      } else {
        var {_1: newenv2, _2: headSolution2} = sns.lazyList.head(solutions);
        var headOperations2 = sns.fileOperations;
        if(headSolution2 != source) {
          headOperations2.push(["write", [filename, headSolution2]]);
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

        rl.on('line', function(line){
           if(line == "1") {
             applyOperations(headOperations);
             rl.close();
           } else if(line == "2") {
             applyOperations(headOperations2);
             rl.close();
           } else {
             console.log("Input not recognized. 1 or 2?");
           }
        })
      }
    } else {
      console.log("Error while updating, solution array is empty");
    }
  } else {
    console.log("Error while updating: " + resSolutions._0);
  }
} else {
  console.log("error", result._0)
}