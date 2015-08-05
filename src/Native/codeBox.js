// This is the JavaScript interface between CodeBox.elm and ace.js.

// Relevant Ace API functions:
//  new EditSession(String text, TextMode mode)
//  new Document(String)
//  Document.setValue(String text)
//    - probably what will be needed to assert contents of the box
//  Document.on("change", function(Object e))
//    - probably what will be needed to send CodeEvents
//  new Editor(VirtualRenderer renderer, EditSession session)
//    - The main entry point into Ace
//
// Relevant Ports:
//   aceInTheHole -- Is where new Models to be rendered come from
//   theTurn      -- Is where what become Events for things like code updates go
//   theRiver     -- Is where new totally rendered Html for the code box goes
//
// This means that things like typing in a new character into the code box will
// send an event to update the model, which will send an update to the
// aceInTheHole, which will (probably, to start with) rerender the code box,
// which will be sent to theRiver, which will display the newly rendered box
// (for a second time?). We could maybe filter out updates to aceInTheHole if
// the last event was a codeUpdate and Ace already has rendered it.
//
// Big things to do: Figure out how to get a given instance of Ace working with
// the correct dimensions. Figure out how Html looks when sent over a port.
// Figure out how to turn that Html into a Graphics.Element.Element. Wire up the
// Model signal and grab the code contents to render.
//
// With that, we should be able to at least load the page.
//
// Then: Figure out the appropriate signal handlers to send what will be events over
// theTurn. Interpret the events appropriately so that sigModel is updated.
// Update the model to remember the last event, and don't send a new model down
// the aceInTheHole unless the rerender actually has to happen (it might!). Be
// careful with things like cursor information, which might need to preserved
// (the field in the model other than code might be a big JSON block of state
// information for Ace that can just be poked at where need be).
//
// Then, we should have a usable replacement for what we currently have. So,
// more or less where we started.
//
// Then: Figure out how to add syntax highlighting (likely just a static file
// pointed to when the Editor is set up). Figure out how to set other options,
// like Vim keybindings and color themes. Add these option choices to the model.
// Add an options dialog to choose between these. Figure out how to highlight
// certain tokens. Add information about what should currently be highlighted
// and how to the model. Implement said highlighting.
//
// With this, we'll have all the really nice things from Ace that we wanted with
// the potential to extend it even further.

// For convenience during development, the page controller is here
var runtime = Elm.fullscreen(Elm.Main, { 
    theTurn : { evt : "init"
              , strArg : "" 
              , cursorArg : { row : 0 , column : 0 }
              , selectionArg : []
              }
});

window.oncrash = function(msg, url, linenumber) {
  var s = '';
  s += 'We crashed... Sorry! Hit OK to restart.\n\n';
  s += 'Error message: ' + msg + '\n\n';
  s += 'The JS error console may contain more info.';
  alert(s);
  location.reload();
}

//The total object that we'll be returning
//var editorElement = document.createElement("div");
//editorElement.id = "editor";

//content = document.createTextNode("(svg [])");
//editorElement.appendChild(content);

//codebox = document.getElementById("codebox");
//codebox.appendChild(content);

//console.log(codebox)

var Range = ace.require('ace/range').Range

var editor = ace.edit("editor");
editor.$blockScrolling = Infinity;
editor.setTheme("ace/theme/chrome");
editor.setFontSize(14);
editor.getSession().setMode("ace/mode/lisp");
editor.getSession().getDocument().on("change", maybeSendUpdate);
editor.getSession().selection.on("changeCursor", maybeSendUpdate);
editor.getSession().selection.on("changeSelection", maybeSendUpdate);

var updateWasFromElm = false;

runtime.ports.aceInTheHole.subscribe(function(codeBoxInfo) {
    //Set our flag so that our event handlers don't update on any updates from
    // Elm
    updateWasFromElm = true;
    //Resize the editor window if the div size has changed
    editor.resize();

    //console.log("Got upd:");
    //console.log(codeBoxInfo);
    //editor.destroy();
    //Need to remove the old event listeners
    //copyWithNoEventListeners = editor.container.cloneNode(true);
    //removeAllChildren(editor.container);
    //editor.container.remove();
    //editor.container.parentNode.replaceChild(document., editor.container);
    //console.log("Got code:\n" + codeBoxInfo.code);
    //editor = ace.edit("editor");
    
    //Set the value of the code editor with what we got from Elm
    editor.getSession().setValue(codeBoxInfo.code, 0);
    //Set the cursor position to what we got from Elm
    editor.moveCursorTo(codeBoxInfo.cursorPos.row, codeBoxInfo.cursorPos.column);
    //Set the selections appropriately
    //Note that we need to turn the Elm Ranges into Ace Range Objects
    editor.selection.clearSelection();
    //We need to treat the first one differently, so we must iterate
    //This is undocumented outside of the tests, see:
    // https://github.com/ajaxorg/ace/blob/0a3b002e285a41a4884ce42f94d2f68673e43b30/lib/ace/multi_select_test.js#L168
    //For the special namespacing, see:
    // http://stackoverflow.com/questions/10452869/when-i-try-to-create-a-range-object-in-ace-js-an-illegal-constructor-error-is
    var numRanges = codeBoxInfo.selections.length;
    for (var i = 0; i < numRanges; i++) {
        elmRange = codeBoxInfo.selections[i];
        var Range = ace.require('ace/range').Range,
            aceRange = new Range(elmRange.start.row,
                                 elmRange.start.column,
                                 elmRange.end.row,
                                 elmRange.end.column);
            console.log(aceRange);
        if (i === 0) {
            //We only allow backwards selections if there is just one selection
            //We deduce if it's backwards by comparing the cursor position to
            // the end of the selection (which always orients itself such that
            // it's > the start in the process of constucting the Range)
            //Note that we use compareEnd here because the compare functions are
            // a little wonky - compareStart returns -1, for instance, if the
            // cursor is at the start.
            if (numRanges === 1) {
                if (aceRange.compareEnd(codeBoxInfo.cursorPos.row,
                                        codeBoxInfo.cursorPos.column) === 1) {
                    var backwards = false;
                } else {
                    var backwards = true;
                }
                editor.selection.setSelectionRange(aceRange, backwards);
            } else {
                editor.selection.fromOrientedRange(aceRange);
            }
        } else {
            editor.selection.addRange(aceRange, false);
        }
    }

    //Add the special syntax highlighting for changes and such
    //These are 'markers', see:
    //http://ace.c9.io/#nav=api&api=edit_session
    //ctl+f addMarker
    //

    //editorCopy = editor.container.cloneNode(true);
    //console.log(editorCopy);
    
    //If the div rerendered (kept track of with a special attribute) then we
    // should copy the editor back into it
    var editorDiv = document.getElementById("editor");
    if (editorDiv.getAttribute("rendered") !== "true") {
      editorDiv.parentNode.replaceChild(editor.container, editorDiv);
      editorDiv.setAttribute("rendered","true");
    }

    //editor.resize(true);
    //var editorDiv = document.getElementById("editor");
    //editorDiv.parentNode.replaceChild(editor.container, editorDiv);
    
    //Set the contents to the correct manipulable state
    if (!codeBoxInfo.manipulable) {
        editor.container.style.zIndex = "0";
        editor.container.style.pointerEvents = "auto";
        editor.setReadOnly(true);
    } else {
        editor.container.style.zIndex = "1";
        editor.container.style.pointerEvents = "auto";
        editor.setReadOnly(false);
    }

    //editor.getSession().setMode("ace/mode/lisp");
    //editor.resize(true);
    
    //Set our flag back to enable the event handlers again
    updateWasFromElm = false;
});

function maybeSendUpdate(e) {
    //console.log(e);
    //console.log(updateWasFromElm);
    //console.log(editor.getCursorPosition());
    if (!updateWasFromElm) {
      runtime.ports.theTurn.send(
        { evt : "AceCodeUpdate"
        , strArg : editor.getSession().getDocument().getValue()
        , cursorArg : editor.getCursorPosition()
        , selectionArg : editor.selection.getAllRanges()
        }
      );
    }
}

function removeAllChildren(container) {
    var nodes = container.childNodes;
    for (var i = 0; i < nodes.length; i++) {
        nodes[i].remove();
    }
}
    

//console.log(document.getElementById("editor"));
//codebox.innerHTML = editor;

// runtime is defined in the html wrapper
//runtime.ports.theRiver.send(editor);
//console.log(editor);
