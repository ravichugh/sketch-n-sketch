(function() {
"use strict";

/******************************************************************************
 * Helpers
 ******************************************************************************/

function stripExtension(str) {
  var lastDotIndex = str.lastIndexOf(".");
  if (lastDotIndex === -1) {
      return str;
  } else {
      return str.substring(0, lastDotIndex);
  }
}

/******************************************************************************
 * Constants
 ******************************************************************************/

var STORAGE_NAME = "sns-files";

/******************************************************************************
 * Core Functions
 ******************************************************************************/

function getFiles() {
  return JSON.parse(localStorage.getItem(STORAGE_NAME)) || {};
}

function setFiles(files) {
  localStorage.setItem(STORAGE_NAME, JSON.stringify(files));
}

function fileUID(name, extension) {
  return name + "." + extension;
}

function writeFile(name, extension, contents) {
  var files = getFiles();
  var uid = fileUID(name, extension);
  files[uid] = {
    "name": name,
    "extension": extension,
    "contents": contents
  };
  setFiles(files);
}

function readFile(name, extension) {
  var files = getFiles();
  var uid = fileUID(name, extension);
  var file = files[uid];
  var contents = file === undefined ? "" : file.contents;
  return contents;
}

function deleteFile(name, extension) {
  var files = getFiles();
  var uid = fileUID(name, extension);
  delete files[uid];
  setFiles(files);
}

// http://stackoverflow.com/questions/3665115/create-a-file-in-memory-for-user-to-download-not-through-server
function downloadTextAsFile(title, text) {
  var downloadLink = document.createElement("a");

  downloadLink.setAttribute(
    "href", "data:text/plain;charset=utf-8," + encodeURIComponent(text)
  );
  downloadLink.setAttribute(
    "download", title
  );

  downloadLink.style.display = "none";
  document.body.appendChild(downloadLink);

  downloadLink.click();

  document.body.removeChild(downloadLink);
}

/******************************************************************************
 * Message Delegator
 ******************************************************************************/

function sendMessage(tag, data) {
  app.ports.internalFileMessage.send({ tag: tag, data: data });
}

function sendFileIndex() {
  var files = getFiles();
  var uids = Object.keys(files);
  var fileIndex = [];
  for (var i = 0; i < uids.length; i++) {
    var uid = uids[i];
    fileIndex.push({
      "name": files[uid].name,
      "extension": files[uid].extension,
    });
  }
  sendMessage("ReceiveFileIndex", fileIndex);
}

function sendFile(file, needsSave) {
  sendMessage("ReceiveFile", {
    "file": file,
    "needsSave": needsSave
  });
}

app.ports.externalFileMessage.subscribe(function(msg) {
  if (msg.tag === "Write") {
    var filename = msg.data.filename;
    var contents = msg.data.contents;
    writeFile(
      filename.name,
      filename.extension,
      contents
    );
    sendFileIndex();
    sendMessage("ConfirmWrite", filename);
  } else if (msg.tag === "Delete") {
    var filename = msg.data;
    deleteFile(
      filename.name,
      filename.extension
    );
    sendFileIndex();
    sendMessage("ConfirmDelete", filename);
  } else if (msg.tag === "Download") {
    var title = msg.data.title;
    var contents = msg.data.contents;
    downloadTextAsFile(title, contents);
  } else if (msg.tag === "RequestFile") {
    var filename = msg.data;
    var contents = readFile(filename.name, filename.extension);
    var file = {
      "filename": filename,
      "contents": contents
    };
    sendFile(file, false);
  } else if (msg.tag === "RequestIcon") {
    var name = msg.data.iconName;
    // In order of precedence
    var extensions = msg.data.iconExtensionPrecedences;
    for (var i = 0; i < extensions.length; i++) {
      var contents = readFile(name, extensions[i]);
      if (contents !== "") {
        var file = {
          "filename": {
            "name": name,
            "extension": extensions[i]
          },
          "contents": contents
        };
        console.log("ICON GET");
        sendMessage("ReceiveIcon", file);
        return;
      }
    }
    sendMessage("ReceiveIcon", {
      "filename": {
        "name": name,
        "extension": extensions[0]
      },
      "contents": ""
    });
  } else if (msg.tag === "RequestUploadedFile") {
    var inputId = msg.data;
    var input = document.getElementById(inputId);

    if (input.files.length === 0) {
        return;
    }

    var realFile = input.files[0];
    var reader = new FileReader();

    reader.onload = function(event) {
      var name = stripExtension(realFile.name);
      var extension = "elm"; // TODO don't assume extension
      var contents = event.target.result;
      var file = {
        "filename": {
          name: name,
          extension: extension
        },
        "contents": contents
      };

      input.value = "";
      sendFile(file, true);
    };

    reader.readAsText(realFile, "UTF-8");
  } else if (msg.tag === "RequestFileIndex") {
    sendFileIndex();
  } else {
    console.error(
      "WARN (fileHandler.js): unknown external file message '"
        + msg.tag
        + "' with data: "
        + msg.data.toString()
    );
  }
});
})();
