app.ports.showAlert.subscribe(function() {
  alert("Hello!");
  app.ports.alertDone.send(null);
});
