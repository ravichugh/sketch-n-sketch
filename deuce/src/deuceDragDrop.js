
/*** MODEL ******************************************************************/

// littleProgram.js defines: topDefs, dependencies

var wait   = function()  { return ["WAITING"    ]; };
var select = function(x) { return ["SELECTED", x]; };

var state = wait();

var allMarkers = {};

function clearMarkers() {
  for (var markerId in allMarkers) {
    editor.session.removeMarker(markerId);
  }
  // TODO
  allMarkers = {};
}


/*** CONTROLLER *************************************************************/

function between(i, x, j) { return i <= x && x <= j; }

function betweenPos(start, p, end) {
  if (p.row < start.row)       { return false; }
  else if (p.row > end.row)    { return false; }
  else if (p.row == start.row) { return between(start.column, p.column, end.column - 1); }
  else if (p.row == end.row)   { return p.column < end.column; }
  else                         { return true; }
}

function moveDefTo(source, target) {
  console.log("MOVE " + source + " TO " + target);
  topDefs = topDefs.slice(0, target)
              .concat([topDefs[source]])
              .concat(topDefs.slice(target, source))
              .concat(topDefs.slice(source + 1));
  // clearMarkers();
  // displayProgram();
}

function makeHandler(eventKind) {

  return function handler(e) {

    var editor = e.editor;
    var pos = editor.getCursorPosition();

    var handled  = false;
    var oldState = state;

    for (markerId in allMarkers) {
      var thisMarker = allMarkers[markerId];
      if (thisMarker === undefined) {
        console.log("WARNING: markerId not found: " + markerId);
        continue;
      }
      var range = thisMarker.range;
      var defId = thisMarker.defId;

      if (betweenPos(range.start, pos, range.end)) {
        if (eventKind == "click") {

          if (state[0] == "WAITING") {
            state = select(defId);
            handled = true;

          } else if (state[0] == "SELECTED" && state[1] == defId) {
            state = wait();
            handled = true;

          } else if (state[0] == "SELECTED") {
            var source = state[1];
            var target = defId;
            if (source > target) {
              moveDefTo(source, target);
            }
            state = wait();
            handled = true;

          } else {
            console.log("TODO blah");
          }

        } else if (eventKind == "mousedown") {

        } else if (eventKind == "mouseup") {

        } else {

        }
      }
    }

    if (state != oldState) {
      console.log("updated state: " + state.toString());
    }

    if (handled) { // redraw whenever model is changed
      displayProgram();
    } else {
      console.log("not handled: " + eventKind + " pos: " + pos.row + " " + pos.column);
    }

    // "redraw" (i.e. choose marker colors) after each event
    // displayProgram();
  }
}


/*** VIEW *******************************************************************/

var Range = require("ace/range").Range, markerId

function displayProgram() {

  var code = "";
  clearMarkers();

  for (var i = 0; i < topDefs.length; i++) {
    var def   = topDefs[i];
    var start = { row: i, column: def.ws_before_open_paren.length + 1 };
    var end   = { row: i, column: start.column + "def".length };
    var range = { start: start, end: end };

    var color;
/*
    color = 'ace_highlight-marker';
*/
    if (state[0] == "SELECTED" && state[1] == i) {
      color = 'deuce_selected-def';
    } else if (state[0] == "SELECTED" && i < state[1]) {
      color = 'deuce_green-drop';
    } else {
      color = 'ace_highlight-marker';
    }
    var newMarkerId =
      editor.session.addMarker
        (new Range(start.row, start.column, end.row, end.column), color);

    console.log("Added new markerId " + newMarkerId + " for defId " + i);
    allMarkers[newMarkerId] = { defId: i, range: range };

    // for now, assuming each topDef is on a single line, so
    // adding \n here

    code +=
      def.ws_before_open_paren
        + "(def" + def.ws_before_ident + def.ident
        + def.exp + def.ws_before_close_paren + ")\n";

  }
  editor.setValue(code);
  editor.clearSelection();
}

