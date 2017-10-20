window.addEventListener("keydown", blockCommands);

// Prevent commands that we capture (e.g. Command-D) from being interpreted by Chrome.
function blockCommands(event) {
  // Command-D
  if (event.keyCode == 68) {
    event.preventDefault();
  }
}
