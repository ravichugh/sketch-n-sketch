(function() {
"use strict";

/******************************************************************************
 * Core Functions
 ******************************************************************************/

function setSyntax(syntax) {
  if (syntax === "Leo") {
    editor.getSession().setMode("ace/mode/elm");
  } else if (syntax === "Little") {
    editor.getSession().setMode("ace/mode/little");
  } else {
    console.error(
      "WARN (syntaxHighlight.js): unknown syntax name '" + syntax + "'"
    );
  }
}

/******************************************************************************
 * Message Delegator
 ******************************************************************************/

app.ports.externalSyntaxHighlightMessage.subscribe(function(msg) {
  if (msg.tag === "SetSyntax") {
    var syntax = msg.data;
    setSyntax(syntax);
  } else {
    console.error(
      "WARN (syntaxHighlight.js): unknown external syntax highlight message '"
        + msg.tag
        + "' with data: "
        + msg.data.toString()
    );
  }
});
})();
