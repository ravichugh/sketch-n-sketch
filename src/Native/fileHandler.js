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

// http://stackoverflow.com/questions/3665115/create-a-file-in-memory-for-user-to-download-not-through-server
function download(filename, text) {
  var downloadLink = document.createElement('a');

  downloadLink.setAttribute(
    "href", "data:text/plain;charset=utf-8," + encodeURIComponent(text)
  );
  downloadLink.setAttribute(
    "download", filename
  );

  downloadLink.style.display = 'none';
  document.body.appendChild(downloadLink);

  downloadLink.click();

  document.body.removeChild(downloadLink);
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

app.ports.download.subscribe(function(downloadInfo) {
  var filename = downloadInfo.filename;
  var text = downloadInfo.text;
  download(filename, text);
})
