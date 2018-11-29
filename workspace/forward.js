// Computes all the files related to SNS.

const fs = require("fs")
const sns = require("sketch-n-sketch")

var source = fs.readFileSync(__dirname + "/generate.elm", "utf8");
var result = sns.process(sns.objEnv.string.evaluate({v:1})(source))(sns.valToNative);

if(result.ctor == "Ok") {
  var writtenFiles = result._0;
  for(var i = 0; i < writtenFiles.length; i++) {
    var {_1: name, _2: content} = writtenFiles[i];
    var newContent = fs.writeFileSync(name, content, "utf8");
  }
  console.log("Written all re-rendered results");
} else {
  console.log("error", result._0)
}