/*
window.initialPortValues["sourceCodeSignalFromJS"] = "DUMMY";
*/

window.initialPortValues["sourceCodeSignalFromJS"] = ["theSourceCode", "DUMMY"];

window.initializers.push(function (elmRuntime) {

  elmRuntime.ports.sourceCodeSignalToJS.subscribe(function (elementId) {
    var s = window.document.getElementById("theSourceCode").textContent;
    elmRuntime.ports.sourceCodeSignalFromJS.send([elementId, s]);
/*
    var s = window.document.getElementById("theSourceCode").textContent;
    elmRuntime.ports.sourceCodeSignalFromJS.send(s);
*/
  });

});
