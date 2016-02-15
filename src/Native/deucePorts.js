/*
window.initialPortValues["sourceCodeSignalFromJS"] = "DUMMY";
*/

window.initialPortValues["sourceCodeSignalFromJS"] = ["theSourceCode", "DUMMY"];

window.initializers.push(function (elmRuntime) {

  elmRuntime.ports.sourceCodeSignalToJS.subscribe(function (elementId) {
      var s;    
      if (elementId == "update") {
          s = combineNodes();
          //console.log(s);
      }
      else {
          s = window.document.getElementById("theSourceCode").textContent;
      }
      elmRuntime.ports.sourceCodeSignalFromJS.send([elementId, s]);
  });

});

function combineNodes() {
    var editor = window.document.getElementById("editor");
    var a = editor.childNodes;
    var i;
    var text;
    var sourceCode = window.document.getElementById("theSourceCode");
    text = sourceCode.textContent.replace(/[\t\r ]+/g, " ");
    //console.log(text);
    sourceCode.id = "temp";
    var node = window.document.getElementById("theSourceCode");
    while (node) {
        //console.log(a);
        text += "\n" + node.textContent.replace(/[\t\r ]+/g, " ");
        editor.removeChild(node);
        node = window.document.getElementById("theSourceCode");
    }
    //console.log(a);
    sourceCode.id = "theSourceCode";
    return text;
}

