api = undefined;
programdefObserver = undefined;
outputObserver = undefined;
delayTimeoutOutputObserver = 1000;
timeoutOutputObserver = undefined;

importLib("sns.js").then(() => {
  api = Elm.EvalUpdate.api;
  programdef = document.querySelector("#programdef");
  programview = require("#viewhandler").getProgramView();
  function handleMutationsToProgram() {
    viewhandler = require("#viewhandler");
    viewhandler.render();
  }
  function handleMutationsToOutput() {
    viewhandler = require("#viewhandler");
    if(typeof timeoutOutputObserver == "number") {
      clearTimeout(timeoutOutputObserver);
    }
    timeoutOutputObserver = setTimeout(function() {
      timeoutOutputObserver = undefined;
      viewhandler.unrender();
    }, delayTimeoutOutputObserver);
    
  }
  if (typeof programdefObserver !== "undefined" && programdefObserver !== null) {
    // console.log("outputValueObserver.disconnect()");
    programdefObserver.disconnect();
  }
  if (typeof outputObserver !== "undefined" && outputObserver !== null) {
    outputObserver.disconnect();
  }
  programdefObserver = new MutationObserver(handleMutationsToProgram);
  outputObserver = new MutationObserver(handleMutationsToOutput);
  programdefObserver.observe(programdef,
    { attributes: true,
      childList: true,
      characterData: true,
      attributeOldValue: true,
      characterDataOldValue: true,
      subtree: true
  });
  handleMutationsToProgram();
  outputObserver.observe(programview,
    { attributes: true,
      childList: true,
      characterData: true,
      attributeOldValue: true,
      characterDataOldValue: true,
      subtree: true
  });
});