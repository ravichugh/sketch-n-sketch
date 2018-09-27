
app.ports.renderDotGraph.subscribe(function(string) {

  image = Viz(string, { format: "png-image-element" });

  // HACK: temporarily add image to the DOM so that gets
  // converted to a data URI. there may be other ways
  // (e.g. through the canvas API or other libraries) for
  // converting directly.

  image.id = "tempImage";
  image.style.position = "fixed";
  document.body.appendChild(image);

  image.onload = function() {
    var tempImage = document.getElementById("tempImage");
    var dataURI = tempImage.src;
    document.body.removeChild(tempImage);
    app.ports.receiveImage.send(dataURI);
  }

});
