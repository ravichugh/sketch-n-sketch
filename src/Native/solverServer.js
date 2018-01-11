// Open a connection to the server, and send and receive data from it.
//
// Looks for a local server first ($ make run in the solver_server directory).
// Otherwise, tries to connect to a global server.

function forwardReduceResponseToElm(event) {
  console.log("REDUCE reply: " + event.data);
  app.ports.reduceResponse.send(event.data);
};

window.solverServerSocket = new WebSocket("ws://localhost:7444/reduce");
window.solverServerSocket.onmessage = forwardReduceResponseToElm
window.solverServerSocket.onerror = function (event) {
  console.log("No local solver server, attempting to connect to tormenta.cs.uchicago.edu...");
  window.solverServerSocket = new WebSocket("ws://tormenta.cs.uchicago.edu:7444/reduce");
  window.solverServerSocket.onmessage = forwardReduceResponseToElm
  window.solverServerSocket.onerror = function (event) {
    console.log("Could not connect to tormenta.cs.uchicago.edu. Cannot solve novel equations during this session.")
  }
};

app.ports.queryReduce.subscribe(function(query) {
  console.log("Querying REDUCE: " + query);
  window.solverServerSocket.send(query);
});
