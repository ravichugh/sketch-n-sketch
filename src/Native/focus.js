app.ports.doFocusJustShownRenameBox.subscribe(function(_) {

  var tryFocus = function () {
    var box = document.getElementById("rename_box");
    if (box) {
      box.focus();
      box.select();
    } else {
      window.setTimeout(tryFocus, 10);
    }
  };

  tryFocus();

});
