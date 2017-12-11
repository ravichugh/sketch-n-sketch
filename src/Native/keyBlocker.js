window.addEventListener("keydown", blockCommands);

// Prevent commands that we capture (e.g. Command-D) from being interpreted by Chrome.
//
// For reasons entirely unclear to me, in Chrome, hitting Command-D will send a keydown but not a keyup event for D.
//
// I do not think this code is at fault. Ctrl-D works as expected.
function blockCommands(event) {
  if (event.metaKey || event.ctrlKey) {
    if (event.keyCode == 68) { event.preventDefault(); } // Command-D
    if (event.keyCode == 13) { event.preventDefault(); } // Command-Enter
  }
}
