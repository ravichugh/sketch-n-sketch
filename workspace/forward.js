
//	scripts/generatePosts.sh
//	scripts/generateTutorial.sh
//	scripts/generate.py

// TODO: Include this in front of each html file:
// <!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
// <html xmlns="http://www.w3.org/1999/xhtml">

const fs = require("fs")
const sns = require("sketch-n-sketch")

var source = fs.readFileSync(__dirname + "/generate.elm", "utf8");
var result = sns.evaluateEnv({v:1})(source)

if(result.ctor == "Ok") {
  console.log(sns.valToNative(result._0)._0)
} else {
  console.log("error", result._0)
}