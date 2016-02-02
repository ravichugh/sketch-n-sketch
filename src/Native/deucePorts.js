window.initialPortValues["sourceCodeSignalFromJS"] = "DUMMY";

window.initializers.push(function (elmRuntime) {

  elmRuntime.ports.sourceCodeSignalToJS.subscribe(function (bool) {
    var s = window.document.getElementById("theSourceCode").textContent;
    elmRuntime.ports.sourceCodeSignalFromJS.send(s);
  });

});
