//////////////////////////////////////////////////////////////////////

var outputCanvas;

function getOutputCanvasState() {
  var info =
    { scrollTop : outputCanvas.scrollTop
    , scrollLeft : outputCanvas.scrollLeft
    };
  return info;
}

function initializeOutputCanvas() {
  outputCanvas = document.getElementById("outputCanvas");

  if (outputCanvas === undefined || outputCanvas === null) {
    console.log("[outputCanvas.js] element not found: outputCanvas");

  } else {
    outputCanvas.onscroll = function() {
      var info = getOutputCanvasState();
      app.ports.receiveOutputCanvasState.send(info);
    };
  }
}

app.ports.outputCanvasCmd.subscribe(function(cmd) {
  var message = cmd.message;

  if (message == "initialize") {
    initializeOutputCanvas();

  } else if (message == "resetScroll") {
    outputCanvas.scrollLeft = 0;
    outputCanvas.scrollTop = 0;

  } else {
    console.log("[outputCanvas.js] unexpected message: " + message);
  }

});
