var outputCanvas;


//////////////////////////////////////////////////////////////////////
// Scroll State

function getOutputCanvasState() {
  var info =
    { scrollTop : outputCanvas.scrollTop
    , scrollLeft : outputCanvas.scrollLeft
    };
  return info;
}


//////////////////////////////////////////////////////////////////////
// Mutation Observers

// https://developer.mozilla.org/en-US/docs/Web/API/MutationObserver
// https://blog.sessionstack.com/how-to-track-changes-in-the-dom-using-mutation-observer-bafdac65bca5
// https://davidwalsh.name/mutationobserver-api

// Listen for changes to outputCanvas.data-canvas-count attribute.
// Each time it changes, then listen for all changes to .outputValue
// and .outputValueWithText nodes. Not simply observing all descendants
// of outputCanvas, to avoid widget layer.
//
// If want to debug the SnS UI by watching all changes to the DOM, observe
// document.documentElement with config.subtree = true.

function dataValueIdOf(mutationTarget) {
  return mutationTarget.attributes.getNamedItem("data-value-id").value;
}

function listenForUpdatesToCanvasCount() {

  function handleMutations(mutations) {
    mutations.forEach(function(mutation) {

      if (mutation.type == "attributes") {
        var attr = mutation.target.getAttributeNode(mutation.attributeName);
        var newAttrValue;
        if (attr) {
          newAttrValue = attr.value;
        } else {
          newAttrValue = "???";
        }
        console.log
          ( "Id: "        + mutation.target.id + "; "
          + "Attribute: " + mutation.attributeName + "; "
          + "New Value: " + newAttrValue
          );

        if (mutation.attributeName == "data-canvas-count") {
          listenForUpdatesToOutputValues();
        }
      }

    });
  }

  var outputCanvasObserver = new MutationObserver(handleMutations);
  var config = { attributes: true };
  outputCanvasObserver.observe(outputCanvas, config);

}

var outputValueObserver;

function listenForUpdatesToOutputValues() {

  function handleMutations(mutations) {
    mutations.forEach(function(mutation) {

      if (mutation.type == "attributes") {

        var attr = mutation.target.getAttributeNode(mutation.attributeName);
          // getAttributeNode() avoids differences between cssText, etc.

        var newAttrValue;

        if (attr) {
          newAttrValue = attr.value;
        } else {
          newAttrValue = "???";
        }

        console.log
          ( "Value Id: "  + dataValueIdOf(mutation.target) + "; "
          + "Attribute: " + mutation.attributeName + "; "
          + "Old Value: " + mutation.oldValue + "; "
          + "New Value: " + newAttrValue
          );

        app.ports.receiveAttributeValueUpdate.send
          ([ parseInt(dataValueIdOf(mutation.target))
           , mutation.attributeName
           , newAttrValue
           ]);

      } else if (mutation.type == "characterData") {

        var parentValueId = dataValueIdOf(mutation.target.parentNode);

        console.log
          ( "Value Id: "  + parentValueId + "; "
          + "Old Text: "  + mutation.oldValue + "; "
          + "New Text : " + mutation.target.textContent // innerHtml?
          );

        app.ports.receiveTextValueUpdate.send
          ([ parseInt(parentValueId)
           , mutation.target.textContent
           ]);

      // https://www.w3schools.com/jsref/prop_node_nodename.asp

      } else if (mutation.type == "childList" &&
                 mutation.removedNodes.length == 1 &&
                 mutation.removedNodes[0].nodeName == "#text") {

        console.log
          ( "Value Id: " + dataValueIdOf(mutation.target) + "; "
          + "Text Child Removed"
          );

      } else if (mutation.type == "childList" &&
                 mutation.addedNodes.length == 1 &&
                 mutation.addedNodes[0].nodeName == "#text") {

        console.log
          ( "Value Id: " + dataValueIdOf(mutation.target) + "; "
          + "Text Child Added"
          );

      } else {

        console.log
          ( "Value Id: "      + dataValueIdOf(mutation.target) + "; "
          + "mutation.type: " + mutation.type
          );
        console.log(mutation);

      }

    });
  }

  if (outputValueObserver) {
    // console.log("outputValueObserver.disconnect()");
    outputValueObserver.disconnect();
  }

  outputValueObserver = new MutationObserver(handleMutations);

  var outputValues = document.getElementsByClassName("_outputValue");

  for (i = 0; i < outputValues.length; i++) {
    // console.log("outputValue " + i);
    outputValueObserver.observe
      ( outputValues[i]
      , { attributes: true
        , childList: true
        , characterData: true
        , attributeOldValue: true
        , characterDataOldValue: true
        , subtree: false
        }
      );
  };

  var outputValuesWithText = document.getElementsByClassName("_outputValueWithText");

  for (i = 0; i < outputValuesWithText.length; i++) {
    // console.log("outputValueWithText " + i);
    outputValueObserver.observe
      ( outputValuesWithText[i]
      , { attributes: true
        , childList: true
        , characterData: true
        , attributeOldValue: true
        , characterDataOldValue: true
        , subtree: true // in order to observe #text children
        }
      );
  };

}


//////////////////////////////////////////////////////////////////////
// Initialization and Ports

function initializeOutputCanvas() {
  outputCanvas = document.getElementById("outputCanvas");

  if (outputCanvas === undefined || outputCanvas === null) {
    console.log("[outputCanvas.js] element not found: outputCanvas");

  } else {

    outputCanvas.onscroll = function() {
      var info = getOutputCanvasState();
      app.ports.receiveOutputCanvasState.send(info);
    };

    listenForUpdatesToCanvasCount();

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
