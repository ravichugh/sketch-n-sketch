var browser = window.navigator.userAgent;
var currentProgram = "";
var heuristicsMode = "";
var heuristicsModes = ["Fair", "Biased", "None"];
var beSilent = false;
var examplesToOutput = [];

function mouseEventOn(domNode, eventName, x, y) {
  var mouseEvent =
    new MouseEvent(eventName, {
      view: window,
      bubbles: true,
      cancelable: true,
      clientX: x,
      clientY: y
    });

  if (!beSilent) { console.log(mouseEvent); }

  domNode.dispatchEvent(mouseEvent);
}

function mouseDownOn(domNode, x, y) {
  mouseEventOn(domNode, 'mousedown', x, y);
}

function mouseMoveOn(domNode, x, y) {
  mouseEventOn(domNode, 'mousemove', x, y);
}

function mouseUpOn(domNode, x, y) {
  mouseEventOn(domNode, 'mouseup', x, y);
}

function mouseClickOn(domNode, x, y) {
  mouseDownOn(domNode, x, y);
  mouseUpOn(domNode, x, y);
}

function getCanvas() {
  return document.getElementById("canvas");
}

function gatherNodes(node, predicate) {
  var result;

  if (predicate(node)) {
    result = [node];
  } else {
    result = []
  }

  var children = node.children;

  for (var i in children) {
    var passingDescendants = gatherNodes(children[i], predicate);
    result = result.concat(passingDescendants);
  }

  return result;
}

function findNodeContaining(str) {
  return findNode(function (node) {
    if (node.text && node.text.includes(str)) {
      return true;
    }

    var children = node.childNodes;

    for (var i in children) {
      if (children[i].nodeType == 3) {
        var textChild = children[i];

        if (textChild.data.includes(str)) { return true; }
      }
    }

    return false;
  });
}

function findNode(predicate) {
  return findNode_(document.body, predicate);
}

function findNode_(node, predicate) {
  if (predicate(node)) {
    return node;
  }

  var children = node.children;

  for (var i in children) {
    var child = children[i];
    var found = findNode_(child, predicate);

    if (typeof found !== 'undefined') {
      return found;
    }
  }
}

function svgZoneNodes() {
  var isZoneNode = function (node) {
    // In the canvas, only Zone nodes define "cursor"
    return node.attributes && node.attributes["cursor"];
  };

  return gatherNodes(getCanvas(), isZoneNode);
}

function exampleSelectElement() {
  return document.querySelector("select");
}

function examples() {
  var options = exampleSelectElement().children;

  var examples = [];
  var inExamples = false;

  for (var i in options) {
    var option = options[i];

    if (option["value"] === "") {
      inExamples = false;
    }

    if (inExamples) {
      // Despite efforts, can't seem to get Cult of Lambda to not
      // stack overflow on Chrome; even though it can run normally
      if (option["value"] !== "Cult of Lambda") {
        examples.push(option["value"])
      }
    }

    if (option["value"] === "*Prelude*") {
      inExamples = true;
    }
  }

  return examples;
}

function selectExample(exampleName) {
  var options = exampleSelectElement().children;
  var targetI;

  for (var i in options) {
    if (options[i]["value"] === exampleName) {
      targetI = i;
    }
  }

  if (typeof targetI !== 'undefined') {
    exampleSelectElement().selectedIndex = targetI;
    var changeEvent =
      new Event("change", {
        bubbles: true,
        cancelable: true,
      });

    exampleSelectElement().dispatchEvent(changeEvent);
    currentProgram = exampleName;
  }
}

function heuristicsModeButton() {
  return findNodeContaining("Heuristics");
}

function readHeuristicsModeFromButton() {
  heuristicsMode = heuristicsModeButton().childNodes[0].textContent.replace("[Heuristics] ","");
  return heuristicsMode;
}

function changeHeuristicsMode() {
  mouseClickOn(heuristicsModeButton());
}

function selectHeuristicsMode(targetMode) {
  var triesRemaining = 100;

  if (targetMode !== readHeuristicsModeFromButton()) {
    changeHeuristicsMode();
    triesRemaining--;

    if (triesRemaining > 0) {
      window.setTimeout(function () {
        selectHeuristicsMode(targetMode);
      }, 100);
    }
  }

  return readHeuristicsModeFromButton();
}

function undo() {
  window.undoButton = window.undoButton || findNodeContaining("Undo");
  mouseClickOn(undoButton, 0, 0);
}

function outputAllExampleStats() {
  heuristicModesToOutput = ["Fair"]; //, "Biased", "None"];

  changeHeuristicsModeAndOutputNextExample();
}


function changeHeuristicsModeAndOutputNextExample () {

  newTodoList().do(function () {
    beSilent = true;
    selectHeuristicsMode(heuristicModesToOutput.shift());
  }).thenWait(1000).do(function () {
    beSilent = false;
    examplesToOutput = examples();
  }).do(outputNextExample).go();

}
// todoList = newTodoList();
//
// examples().forEach(function (exampleName) {
//
//   todoList.do(function() {
//     selectExample(exampleName);
//   }).thenWait(3000);
//
//   heuristicsModes.forEach(function (heuristicsModeName) {
//
//     todoList.do(function () {
//       selectHeuristicsMode(heuristicsModeName);


function outputNextExample() {
  // Elm will call back to us to output the next one, and so on.
  if (examplesToOutput.length > 0) {
    runtime.ports.benchmarkExample.send(examplesToOutput.pop());
  } else if (heuristicModesToOutput.length > 0) {
    changeHeuristicsModeAndOutputNextExample();
  }
}

function outputProgramStats() {
  runtime.ports.programStats.send([]);
}

function newTodoList() {
  var todoList = [];

  todoList.do = function (func) {
    todoList.push(func);
    return todoList;
  };

  todoList.prioritize = function (item) {
    todoList.unshift(item);
    // funcs.reverse().forEach(function (func) {
    //   todoList.unshift(func);
    // })
    return todoList;
  };

  todoList.thenWait = function (milliseconds) {
    todoList.push(milliseconds);
    return todoList;
  };

  todoList.go = function () {
    var item = todoList.shift();

    if (typeof item === "number") {
      window.setTimeout(todoList.go, item);
    } else if (typeof item !== "undefined" && typeof (item.go) === "function") {
      // Nested todoList. Complete it first.
      // Have it resume our execution when done.
      item.do(todoList.go);
      item.go();
    } else if (typeof item === "function") {
      item();
      window.setTimeout(todoList.go, 0);
    }
  };

  return todoList;
}

function runInteractionBenchmarks() {
  todoList = newTodoList();

  examples().forEach(function (exampleName) {

    todoList.do(function() {
      selectExample(exampleName);
    }).thenWait(3000);

    heuristicsModes.forEach(function (heuristicsModeName) {

      todoList.do(function () {
        selectHeuristicsMode(heuristicsModeName);
      }).thenWait(500).do(function () {
        outputProgramStats()
      }).thenWait(200).do(function () {
        nudgeZonesTodoList = newTodoList();

        svgZoneNodes().forEach(function (zone) {
          nudgeZonesTodoList.do(function () {
            mouseMoveOn(zone, 0, 0)
          }).thenWait(20).do(function () {
            mouseDownOn(zone, 0, 0)
          }).thenWait(20).do(function () {
            // Semibug in SnS: callback not created until
            // first move event after click.
            //
            // Create callback.
            mouseMoveOn(zone, 0, 0)
          }).thenWait(20).do(function () {
            // Exercise callback.
            mouseMoveOn(zone, 1, 1)
          }).thenWait(200).do(function () {
            mouseUpOn(zone, 1, 1)
          }).thenWait(200).do(undo).thenWait(200);
        });

        todoList.prioritize(nudgeZonesTodoList);
      });

    });

  });

  todoList.go();
}

Elm.Native.Benchmark = {};
Elm.Native.Benchmark.make = function(elm) {

  elm.Native = elm.Native || {};
  elm.Native.Benchmark = elm.Native.Benchmark || {};

  if (elm.Native.Benchmark.values) {
    return elm.Native.Benchmark.values;
  }

  elm.Native.Benchmark.timers = elm.Native.Benchmark.timers || {};

  var timers = elm.Native.Benchmark.timers;

  return elm.Native.Benchmark.values = {

    // tick: function (label) {
    //   console.log(label + "," + (Date.now() / 1000));
    //   return label;
    // },

    silence: function () {
      beSilent = true;
      return beSilent;
    },

    unsilence: function () {
      beSilent = false;
      return beSilent;
    },

    setProgram: function (programName) {
      currentProgram = programName;
      return programName;
    },

    setHeuristicsMode: function (modeName) {
      heuristicsMode = modeName;
      return modeName;
    },

    log: function (label) {
      return function (value) {
        if (!beSilent) {
          console.log(browser + "," + currentProgram + "," + heuristicsMode + "," + label + "," + value);
        }
        return value;
      }
    },

    // Run next benchmark
    next: function (unit) {
      window.setTimeout(outputNextExample, 200);
      return;
    },

    start: function (label) {
      timers[label] = new Date();
      return timers[label].getTime();
    },

    stop: function (label) {

      if (timers[label]) {
        var duration = (new Date()) - timers[label];
        delete timers[label];
        return elm.Native.Benchmark.values.log(label)(duration / 1000);
      } else {
        return -1;
      }

    }

  };
};
