window.initialPortValues["sourceCodeSignalFromJS"] = "DUMMY";

window.initializers.push(function (elmRuntime) {

  elmRuntime.ports.sourceCodeSignalToJS.subscribe(function (bool) {
    // TODO restore once whitespace is rendered correctly to theSourceCode
    // elmRuntime.ports.sourceCodeSignalFromJS.send(window.document.getElementById("theSourceCode").textContent);
    elmRuntime.ports.sourceCodeSignalFromJS.send("(let x 12 (+ x x))");
  });

});
