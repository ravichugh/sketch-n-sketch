function errorNode(msg) {
  var r = document.createElement("span");
  r.classList.add("errorconversion");
  r.setATtribute("title", "msg");
  r.innerHTML = "??";
}

function nativeValueToDomNode(v) {
  if(typeof v == "object") {
    if(v.length == 2) {
      if (typeof v[1] == "string") {
        return document.createTextNode(v[1]);
      } else {
        return errorNode("Got a 2-element list, but the second element was not a string but a " + typeof v[1]);
      }
    } else if(v.length == 3) {
      if(typeof v[0] == "string") {
        var r = document.createElement(v[0]);
        var attributes = v[1];
        for(var attrIndex = 0; attrIndex < attributes.length; attrIndex++) {
          var key = attributes[attrIndex][0];
          var value = attributes[attrIndex][1];
          if(key == "style" && typeof value == "object") {
            r.setAttribute(key, value.map(v => v[0] + ":" + v[1]).join(";"));
          } else {
            r.setAttribute(key, value);
          }
        }
        var children = v[2];
        for(var childIndex = 0; childIndex < children.length; childIndex++) {
          r.appendChild(nativeValueToDomNode(children[childIndex]));
        }
        return r;
      } else {
        return errorNode("For a 3-element list, the first argument should be a string, got " + (typeof v[0]));
      }
    } else {
      return errorNode("Expected a 2- or 3-element list, got " + v.length);
    }
  }
}

function domNodeToNativeValue(n) {
  if(n.nodeType == "3") {
    return ["TEXT", n.textContent];
  } else {
    var attributes = [];
    for(var i = 0; i < n.attributes.length; i++) {
      var key = n.attributes[i].name;
      var value = n.attributes[i].value;
      if(key == "style") {
        value = value.split(";").map(x => x.split(":")).filter(x => x.length == 2);
      }
      attributes.push([key, value]);
    }
    var children = [];
    for(i = 0; i < n.childNodes.length; i++) {
      children.push(domNodeToNativeValue(n.childNodes[i]));
    }
    return [n.tagName.toLowerCase(), attributes, children];
  }
}

removeErrorTimeout = undefined;

function reportError(msg, maybeTimeout) {
  var programerrorview = document.querySelector("#programerrorview");
  if(typeof programerrorview == "undefined" || programerrorview === null) {
    var programerroroutput = document.querySelector("#programerroroutput");
    programerrorview = document.createElement("transient");
    programerrorview.setAttribute("id", "programerrorview");
    programerroroutput.appendChild(programerrorview);
  }
  if(msg === "") {
    programerrorview.parentNode.classList.add("error-hidden");
  } else {
    programerrorview.parentNode.classList.remove("error-hidden");
    programerrorview.innerText = msg;
  }
  if(typeof maybeTimeout == "number") {
    if(removeErrorTimeout !== "undefined") {
      clearTimeout(removeErrorTimeout);
    }
    removeErrorTimeout = setTimeout(function() {
      reportError("");
      removeErrorTimeout = undefined;
    }, maybeTimeout);
  }
}

function getProgramView() {
  var programview = document.querySelector("#programview");
  if(typeof programview == "undefined" || programview === null) {
    var programoutput = document.querySelector("#programoutput");
    programview = document.createElement("transient");
    programview.setAttribute("id", "programview");
    programoutput.appendChild(programview);
  }
  return programview;
}

function getProgramSource() {
  return document.querySelector("#programdef").innerText;
}

function setProgramSource(newSource) {
  document.querySelector("#programdef").innerText = newSource;
}

function render(potentialNewSource) {
  var progSource = typeof potentialNewSource == "string" ? potentialNewSource : getProgramSource();
  var justRefresh = progSource == global_src;
  var programview = getProgramView();
  var resProgExp = justRefresh ? { ctor: "Ok", _0: global_exp} : api.parse(progSource);
  if(resProgExp.ctor == "Ok") {
    var progExp = resProgExp._0;
    var resEval = justRefresh ? { ctor: "Ok", _0: global_val} : api.evalExp(progExp);
    if(resEval.ctor == "Ok") {
      var result = resEval._0;
      var resNativeResult = justRefresh ? { ctor: "Ok", _0: global_native_val} : api.valToNative(result);
      if(resNativeResult.ctor == "Ok") {
        var nativeResult = resNativeResult._0;
        var node = nativeValueToDomNode(nativeResult);
        programview.innerHTML = "";
        programview.appendChild(node);
        global_src = potentialNewSource;
        global_exp = progExp;
        global_val = result;
        global_native_val = nativeResult;
        if(typeof potentialNewSource == "string") {
          setProgramSource(potentialNewSource);
        }
        reportError("");
      }else {
        reportError("The result failed to convert to a native value: " + resNativeResult._0);
      }
      
    } else {
      reportError("The program failed to evaluate: " + resEval._0);
    }
    
  } else {
    reportError("The program failed to parse: " + resProgExp._0);
  }
}

function unrender() {
  var programview = getProgramView();
  var nodeToUnrender = programview.childNodes[0];
  var nativeValue = domNodeToNativeValue(nodeToUnrender);
  var new_val = api.nativeToVal(nativeValue);
  var programs = api.updateExp(global_exp)(global_val)(new_val);
  if(programs.ctor == "Oks") {
    var programLists = programs._0;
    var firstProgram = programLists._0;
    var newProgramSource = api.unparse(firstProgram);
    if(getProgramSource() !== newProgramSource) {
      render(newProgramSource);
    }
  } else {
    render();
    reportError("Not taking changes into account because got an error while updating: " + programs._0, 5000);
  }
}

exports.getProgramView = getProgramView;

exports.render = render;

exports.unrender = unrender;