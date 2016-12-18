function getFiles() {
  return JSON.parse(localStorage.getItem("sketch-files")) || {};
}

function setFiles(files) {
  localStorage.setItem("sketch-files", JSON.stringify(files));
}

function write(file) {
  var files = getFiles();
  files[file.filename] = {
    code: file.code
  };
  setFiles(files);
}

function read(filename) {
  var files = getFiles();
  var file = files[filename];
  var code;
  if (file === undefined) {
    code = "(blobs [\n])"; // same as BLANK template
  } else {
    code = file.code;
  }
  return code;
}

app.ports.write.subscribe(function(file) {
  write(file);
  app.ports.writeConfirmation.send(file.filename);
});

app.ports.requestFile.subscribe(function(filename) {
  var code = read(filename);
  var file = {
    filename: filename,
    code: code
  }
  app.ports.receiveFile.send(file);
});

app.ports.requestFileIndex.subscribe(function() {
  var files = getFiles();
  var fileIndex = Object.keys(files)
  app.ports.receiveFileIndex.send(fileIndex);
});
