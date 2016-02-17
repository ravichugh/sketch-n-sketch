/*
window.initialPortValues["sourceCodeSignalFromJS"] = "DUMMY";
*/

window.initialPortValues["sourceCodeSignalFromJS"] = ["theSourceCode", "DUMMY"];

window.initializers.push(function (elmRuntime) {

  elmRuntime.ports.sourceCodeSignalToJS.subscribe(function (elementId) {
      var s;
      console.log(elementId);
      if (elementId == "update") {
          //console.log("enter update");
          s = combineNodes();
      }
      else if (elementId == "edit" || elementId == "other") {
          s = window.document.getElementById("theSourceCode").textContent;
      }
      else {
          //console.log("here");
          s = window.document.getElementById(elementId).textContent;
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
    text = sourceCode.innerText.replace("\u00A0"," ");
    sourceCode.id = "temp";
    var node = window.document.getElementById("theSourceCode");
    while (node) {
        if (node.innerText == "\n") {
            text += "\n";
        }
        else {
            text += "\n" + node.innerText.replace("\u00A0", " ");
        }
        editor.removeChild(node);
        node = window.document.getElementById("theSourceCode");
    }
    sourceCode.id = "theSourceCode";
    var b = sourceCode.childNodes;
    var len = b.length;
    for (i = 0; i < len; i++) {
        if (i > 0) {
            sourceCode.removeChild(b[i]);
        }
    }
    //console.log(text);
    return text;
}

