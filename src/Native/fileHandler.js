function save(code) {
  localStorage.setItem("file", code);
}

function load() {
    return localStorage.getItem("file");
}

app.ports.save.subscribe(function(code) {
  save(code);
  app.ports.hasSaved.send(null);
});

app.ports.requestLoad.subscribe(function() {
    var code = load();
    app.ports.receiveLoad.send(code);
});
