"use strict";

app.ports.requestDeucePopupPanelInfo.subscribe(function (_) {
  // Enough delay so that the menu will actually appear and have height
  window.setTimeout(function() {
    var deucePopupPanel = document.getElementsByClassName("deuce-popup-panel")[0];
    if (deucePopupPanel) {
      var info =
        { "height":
            deucePopupPanel.offsetHeight
        }
      app.ports.receiveDeucePopupPanelInfo.send(info);
    }
  }, 10);
});
