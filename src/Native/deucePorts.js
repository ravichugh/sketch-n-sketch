/*
window.initialPortValues["sourceCodeSignalFromJS"] = "DUMMY";
*/

window.initialPortValues["sourceCodeSignalFromJS"] = ["theSourceCode", "DUMMY"];

window.initializers.push(function (elmRuntime) {

  elmRuntime.ports.sourceCodeSignalToJS.subscribe(function (elementId) {
      var s = window.document.getElementById("theSourceCode").textContent;
      if (elementId == "update" || elementId == "edit") {
          elmRuntime.ports.sourceCodeSignalFromJS.send([elementId, s]);
      }
      else {
          alert(elementId);
      }
  });

});
