var lastTime = undefined;

function handleFrame(newTime) {
  tickDelta = newTime - lastTime;
  lastTime  = newTime;
  app.ports.receiveFrame.send(tickDelta);

    // sending tickDeltas (rather than newTimes) to Elm
    // so that the animation logic there can remain the
    // same as when using the old Time.fps signal
}

app.ports.requestFrame.subscribe(function() {
  if (lastTime == undefined) {
    lastTime = performance.now();
  }
  window.requestAnimationFrame(handleFrame);
});
