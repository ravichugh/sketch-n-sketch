
const fs = require("fs")
const sns = require("sketch-n-sketch")
sns.params = sns.params || {};
sns.params.delayedWrite = true;

var source = fs.readFileSync(__dirname + "/generate.elm", "utf8") + `

toWriteRaw`;

var result = sns.process(sns.objEnv.string.evaluate({write:"False"})(source))(
  sns.valToNative
)

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
  sns.objEnv.string.update({v:1})(source)(newWrittenFilesVal);
  console.log("Done", sns.fileOperations);
} else {
  console.log("error", result._0)
}