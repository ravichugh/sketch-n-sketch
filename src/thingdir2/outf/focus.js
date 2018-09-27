app.ports.doFocusJustShownRenameBox.subscribe(function(_) {

  var tryFocus = function () {
    var box = document.getElementById("rename-box");
    if (box) {
      box.focus();
      box.select();
    } else {
      window.setTimeout(tryFocus, 10);
    }
  };

  tryFocus();

});

app.ports.doFocusAsNeeded.subscribe(function(idToFocus) {

  var tryFocus = function () {
    var box = document.getElementById(idToFocus);
    if (box) {
      box.focus();
      box.select();
    } else {
      window.setTimeout(tryFocus, 10);
    }
  };

  window.setTimeout(tryFocus, 15);

});
