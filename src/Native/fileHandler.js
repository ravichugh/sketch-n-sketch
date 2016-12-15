function save(code) {
  localStorage.setItem("file", code);
}

function load() {
  var code = localStorage.getItem("file");
  if (code === null) {
    code = "(blobs [\n])"; // same as BLANK template
  }
  return code;
}

app.ports.save.subscribe(function(code) {
  save(code);
  app.ports.hasSaved.send(null);
});

app.ports.requestLoad.subscribe(function() {
    var code = load();
    app.ports.receiveLoad.send(code);
});
