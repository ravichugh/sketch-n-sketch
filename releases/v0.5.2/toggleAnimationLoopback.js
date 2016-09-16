//
// For toggling the animation ticker signal on and off for efficiency.
//
// This is a simple loopback so that a change in the model can trigger an
// event that becomes a loopback signal.
//

window.initialPortValues["animateSignal"] = true;

window.initializers.push(function (elmRuntime) {

  elmRuntime.ports.modelRunAnimation.subscribe(function (bool) {
    elmRuntime.ports.animateSignal.send(bool);
  });

});
