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
var runtime = Elm.fullscreen(Elm.Main, { testtest : "" });

window.onerror = function(msg, url, linenumber) {
  var s = '';
  s += 'We crashed... Sorry! Hit OK to restart.\n\n';
  s += 'Error message: ' + msg + '\n\n';
  s += 'The JS error console may contain more info.';
  alert(s);
  location.reload();
}


//The total object that we'll be returning
var editorElement = document.createElement("div");
editorElement.id = "editor";

content = document.createTextNode("(svg [])");
editorElement.appendChild(content);

var editor = ace.edit(editorElement);


// runtime is defined in the html wrapper
runtime.ports.testtest.send(string(editor));
console.log(editorElement);
