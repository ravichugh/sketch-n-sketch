"use strict";

app.ports.resetProseScroll.subscribe(function (_) {
  var proses = document.getElementsByClassName("prose-panel");
  for (var i = 0; i < proses.length; i++) {
    proses[i].scrollTop = 0;
  }
});
