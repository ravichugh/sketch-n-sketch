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
// Caret State
function getCaretPositionIn(m) {
  var sel = window.getSelection()
  if(sel.rangeCount == 0) return -1
  var range = sel.getRangeAt(0);
  var start = range.startOffset;
  var tmp = range.startContainer;
  while(tmp != m && tmp != null) {
    while (tmp.previousSibling != null) {
      tmp = tmp.previousSibling
      start = start + (tmp.textContent != null ? tmp.textContent.length : 0);
    }
    tmp = tmp.parentNode;
  }
  return start
}

function setCaretPositionIn(m, a) {
  if(a == -1) return;
  if (a <= m.textContent.length) {
    if (m.nodeType == 3) { // Text nodes
      var sel  = window.getSelection()
      return sel.collapse(m, a)
    } else {
      m = m.firstChild;
      while(m != null && a > m.textContent.length) {
        a = a - m.textContent.length
        m = m.nextSibling
      }
      if(m != null) {
        setCaretPositionIn(m, a)
      } else {
        console.log("Could not find position. Reach node " + n + " and position " + p)
      }
    }
  }
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

var outputCanvasObserver;

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
        /*console.log
          ( "Id: "        + mutation.target.id + "; "
          + "Attribute: " + mutation.attributeName + "; "
          + "New Value: " + newAttrValue
          );*/

        if (mutation.attributeName == "data-canvas-count") {
          listenForUpdatesToOutputValues();
        }
      }

    });
  }

  outputCanvasObserver = new MutationObserver(handleMutations);
  var config = { attributes: true };
  outputCanvasObserver.observe(outputCanvas, config);

}

var outputValueObserver;

var msBeforeAutoSync = 1000;

var timerAutoSync = undefined;

var enableAutoUpdate = false;
  // Model.elm: set initModel.syncMode = ValueBackprop True/False to match

var previewMode = false;

var lastCaretPosition = -1;

function triggerAutoUpdate() {
  if(!enableAutoUpdate || previewMode) return;
  if(typeof timerAutoSync !== "undefined") clearTimeout(timerAutoSync);
  timerAutoSync = setTimeout(function() {
    timerAutoSync = undefined;
    if(!enableAutoUpdate || previewMode) return;
    lastCaretPosition = getCaretPositionIn(document.getElementById("outputCanvas"));
    /*console.log("Sending caret position: " + lastCaretPosition);*/
    app.ports.maybeAutoSync.send(lastCaretPosition);
  }, msBeforeAutoSync)
}

function listenForUpdatesToOutputValues() {
  function isTransientElement(elem) {
    return elem.nodeType == 1 && elem.tagName == "TRANSIENT";
  }

  function whichChild(elem){
    var  i= 0;
    while((elem=elem.previousSibling)!=null) {
      if(!(isTransientElement(elem))) ++i;
    }
    return i;
  }

  function isContainer(htmlElem) {
    return htmlElem.parentNode &&
      htmlElem.parentNode.getAttribute &&
      (htmlElem.parentNode.getAttribute("id") == "outputCanvas" ||
        htmlElem.parentNode.getAttribute("id") == "svgOutputCanvas")
  }

  function hasTransientAncestor(htmlElem) {
    if(htmlElem == null) return true;
    if(isContainer(htmlElem)) return false;
    if(isTransientElement(htmlElem)) return true;
    return hasTransientAncestor(htmlElem.parentNode);
  }
  
  function getPathUntilOutput(htmlElem) {
    if(htmlElem == null) return null;
    if(isContainer(htmlElem))
      return []
    else
      //2 is for the children of a node (encoded [tag, attributes, children])
      var res = getPathUntilOutput(htmlElem.parentNode);
      if(res == null) return null;
      if(htmlElem.parentNode.getAttribute("ignore-except-first-child") == "true") {

      } else {
        res.push(2)
        res.push(whichChild(htmlElem))
      }
      return res;
  }

  function isAttributeTransient(name) {
    return name.startsWith("transient-");
  }

  function isAttributeIgnored(name) {
    return name.startsWith("ignore-") || name == "value";
  }
  
  function encodeAttributes(attrs, node) {
    var attributes = [];
    for(var i = 0; i < attrs.length; i++) {
      var name = attrs[i].name;
      var value = attrs[i].value;
      if((name != "contenteditable" || i > 0) && (name != "data-value-id")) {
        if(name == "style") { // styles are encoded as list of (name, value)
           var styles = []
           var nodeStyles = value.split(/; ?/);
           for(var j = 0; j < nodeStyles.length; j++) {
             var nameVal = nodeStyles[j].split(/: ?/);
             if(nameVal[0] != "") {
               styles.push([nameVal[0], typeof nameVal[1] == "undefined" ? "" : nameVal[1]]);
             }
           }
           value = styles;
        }
        if(!isAttributeTransient(name)) {
          attributes.push([name, value]);
        }
      }
    }
    return attributes;
  }
  
  function encodeNode(node) {
    if (node.nodeType == 3) return node.textContent
    var children = []
    for(var i = 0; i < node.childNodes.length; i++) {
      if(!(isTransientElement(node.childNodes[i])))
        children.push(encodeNode(node.childNodes[i]))
    }
    return [node.tagName.toLowerCase(), encodeAttributes(node.attributes, node), children]
  }

  function handleMutations(mutations) {
    if(previewMode) {
      /*console.log("Handling mutations in preview mode ")*/
      // Only remove nodes which have been inserted manually
      mutations.forEach(function(mutation) {
        /*console.log("Checking if a mutation added nodes ", mutation.addedNodes)*/
        for (var i = 0; i < mutation.addedNodes.length; i++) {
          var domNode = mutation.addedNodes[i];
          while(domNode.previousSibling != null) {
            /*console.log("During preview, rewiinding ", domNode)*/
            domNode = domNode.previousSibling;
          }
          while(domNode != null) {
            /*console.log("During preview, we remove inserted node. Checking ", domNode)*/
            if(domNode.manuallyInsertedNode) {
              /*console.log("Manually inserted, we remove it.")*/
              var next = domNode.nextSibling;
              domNode.remove();
              domNode = next;
            } else {
              /*console.log("Not manually inserted")*/
              domNode = domNode.nextSibling
            }
          }
        }
      });
    }
    mutations.forEach(function(mutation) {
      for (var i = 0; i < mutation.addedNodes.length; i++) {
        var domNode = mutation.addedNodes[i];
        domNode.manuallyInsertedNode = true;
        if(domNode.nodeType == 1) { // In case it thinks that the node was a text node.
          domNode.replaceData = (function(self) { // Hides this node for now, 
             return function(start, end, data) {
               // Here self is totally detached from any parents. So the only thing we can do is to hid it.
               self.style.display = "none";
               /*if(self.previousSibling && self.previousSibling.manuallyInsertedNode) {
                 self.previousSibling.remove();
               }
               self.remove();// We hid it from the diff and apply the change to the next sibling.
               self.nextSibling.replaceData(0, nextSibling.length, data);*/
             }
          })(domNode);
        } else {
        }
      }
			
      if (mutation.type == "attributes") {
        if(!isAttributeTransient(mutation.attributeName) && !isAttributeIgnored(mutation.attributeName)) {
          var path = getPathUntilOutput(mutation.target)
          if(path != null) {
            path.push(1) // 1 for the attributes. We can be more precise if we know there are no insertion.
            var attrs = encodeAttributes(mutation.target.attributes, mutation.target)
            var valueUpdateType = 1;
            app.ports.receiveValueUpdate.send([valueUpdateType, path, attrs])
            triggerAutoUpdate()
          }
        }
      } else {
        /*console.log("mutation type:", mutation.type);*/
        for (var k = 0; k < mutation.addedNodes.length; k ++) { // We add the callback to each added node.
          function walkAllChildren(currentNode) {
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
            for (var j = 0; j < currentNode.childNodes.length; j++) {
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
              } else if (child.nodeType == 1) {
                walkAllChildren(child)
              }
            }
          }
          if(!hasTransientAncestor(mutation.addedNodes[k])) {
            walkAllChildren(mutation.addedNodes[k])
          }
        }
        var path = getPathUntilOutput(mutation.target) // We change the whole node.
        if(path != null && !hasTransientAncestor(mutation.target)) {
          var encodedNode = encodeNode(mutation.target);
          var valueUpdateType = 0;
          if(mutation.type == "childList") {
            encodedNode = encodedNode[2];
            path.push(2);
            valueUpdateType = 2;
          }
          /*console.log("path", path);
          console.log("encoded node", encodedNode);*/
          app.ports.receiveValueUpdate.send([valueUpdateType, path, encodedNode])
          triggerAutoUpdate()
        }
      // https://www.w3schools.com/jsref/prop_node_nodename.asp
      }
    });
  }

  if (outputValueObserver) {
    // console.log("outputValueObserver.disconnect()");
    outputValueObserver.disconnect();
  }

  outputValueObserver = new MutationObserver(handleMutations);

  var outputValues = document.querySelectorAll("[data-value-id]");

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
// DOM Updates from SnS Direct Manipulation UI

app.ports.setDomShapeAttribute.subscribe(function(args) {
  // console.log("setDomNumAttribute:" + args.nodeId + " / " + args.attrName + " / " + args.attrValue);
  var e = document.querySelector('[data-value-id="' + args.nodeId + '"]');
  if (e) {
    e.setAttribute(args.attrName, args.attrValue);
  } else {
    console.log("couldn't find data-value-id: " + args.nodeId);
  }
});


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

  } else if (message == "stopDomListener") {
    // console.log("stop listening to DOM changes");
    outputCanvasObserver.disconnect();
    outputValueObserver.disconnect();

  } else if (message == "startDomListener") {
    // console.log("start listening to DOM changes");
    listenForUpdatesToCanvasCount();

  } else {
    console.log("[outputCanvas.js] unexpected message: " + message);
  }

});

app.ports.enableAutoSync.subscribe(function(b) {
  enableAutoUpdate = b;
})

app.ports.setAutoSyncDelay.subscribe(function(n) {
  msBeforeAutoSync = n;
})

app.ports.setPreviewMode.subscribe(function(b) {
  previewMode = b;
})

var timerClearDiff = undefined;
app.ports.setDiffTimer.subscribe(function(b) {
  if(b.activate) {
    if(typeof timerClearDiff !== "undefined" ) {
      clearTimeout(timerClearDiff)
    }
    timerClearDiff = setTimeout(function() {
      timerClearDiff = undefined;
      app.ports.clearPreviewDiff.send(1)
    }, b.delay);
  }
})

app.ports.setCaretPosition.subscribe(function(position) {
  setTimeout(function() {
    /*console.log("Setting caret position: " + position);*/
    setCaretPositionIn(document.getElementById("outputCanvas"), position)
  }, 0);
})