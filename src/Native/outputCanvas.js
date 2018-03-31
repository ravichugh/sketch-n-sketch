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
          newAttrValue = "NULL";
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
  function whichChild(elem){
    var  i= 0;
    while((elem=elem.previousSibling)!=null) ++i;
    return i;
  }
  
  function getPathUntilOutput(htmlElem) {
    if(htmlElem.parentNode && htmlElem.parentNode.getAttribute && htmlElem.parentNode.getAttribute("id") == "outputCanvas") // Single child !
      return []
    else
      //2 is for the children of a node (encoded [tag, attributes, children])
      var res = getPathUntilOutput(htmlElem.parentNode)
      res.push(2)
      res.push(whichChild(htmlElem))
      return res;
  }
  
  function encodeAttributes(attrs) {
    var attributes = [];
    for(var i = 0; i < attrs.length; i++) {
      attributes.push([attrs[i].name, attrs[i].value])
    }
    return attributes;
  }
  
  function encodeNode(node) {
    if (node.nodeType == 3) return node.textContent
    var children = []
    for(var i = 0; i < node.childNodes.length; i++) {
      children.push(encodeNode(node.childNodes[i]))
    }
    return [node.tagName, encodeAttributes(node.attributes), children]
  }

  function handleMutations(mutations) {
    mutations.forEach(function(mutation) {
      if (mutation.type == "attributes") {
        var path = getPathUntilOutput(mutation.target)
        path.push(1) // 1 for the attributes. We can be more precise if we now there are no insertion.
        
        var attrs = encodeAttributes(mutation.target.attributes)
      
        app.ports.receiveValueUpdate.send([path, attrs])
      } else {
        
        var path = getPathUntilOutput(mutation.target) // We change the whole node.
        var encodedNode = encodeNode(mutation.target);
        console.log("encoded node", encodedNode)
        app.ports.receiveValueUpdate.send([path, encodedNode])

      // https://www.w3schools.com/jsref/prop_node_nodename.asp
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
    var currentNode = outputValues[i];
    // console.log("outputValue " + i);
    outputValueObserver.observe
      ( currentNode
      , { attributes: true
        , childList: true
        , characterData: true
        , attributeOldValue: true
        , characterDataOldValue: true
        , subtree: false
        }
      );
    // We listen to its children text nodes too
    for (j = 0; j < currentNode.childNodes.length; j++) {
      var child = currentNode.childNodes[j];
      if (child.nodeType == 3) {
        outputValueObserver.observe
        ( child
        , { characterData: true
          , attributeOldValue: true
          , characterDataOldValue: true
          , subtree: false
          }
        );
      }
    }
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
