function write(file) {
  var filename = file.filename;
  var code = file.code;
  localStorage.setItem(filename, code);
}

function read(filename) {
  var code = localStorage.getItem(filename);
  if (code === null) {
    code = "(blobs [\n])"; // same as BLANK template
  }
  return code;
}

app.ports.write.subscribe(function(file) {
  write(file);
  app.ports.writeConfirmations.send(file.filename);
});

app.ports.requestFile.subscribe(function(filename) {
  var code = read(filename);
  var file = {
    filename: filename,
    code: code
  }
  app.ports.requestedFiles.send(file);
});
