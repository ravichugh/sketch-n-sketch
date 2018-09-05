// Open a connection to the server, and send and receive data from it.
//
// Looks for a local server first ($ make run_server in the solver_server directory).
// Otherwise, tries to connect to a global server.
//
// I'm sure there's some race condition in here between the interaction of long queries,
// server disconnects, and the internals of the WebSocket browser implementation.

window.reconnectPauseSeconds = 5;

function setupConnection(address, onError) {
  if (window.solverServerSocket && (window.solverServerSocket.readyState == WebSocket.CONNECTING || window.solverServerSocket.readyState == WebSocket.OPEN)) {
    return;
  }
  window.solverServerSocket           = new WebSocket(address);
  window.solverServerSocket.onmessage = forwardReduceResponseToElm;
  window.solverServerSocket.onerror   = onError;
  window.solverServerSocket.onopen    = function (event) {
    console.log("Connected to solver server at " + address);
    window.reconnectPauseSeconds = 5;
    window.solverServerSocket.onclose = function (event) {
      console.log("Solver server connection closed. Attempting to reconnect...");
      connectToSolverServer();
    };
    resendOutstandingQueries();
  };
}

function connectToSolverServer() {
  setupConnection("ws://localhost:7444/reduce", function (event) {
    console.log("No local solver server (start local server with $ cd solver_server && make run_server). Attempting to connect to tormenta.cs.uchicago.edu...");
    setupConnection("ws://tormenta.cs.uchicago.edu:7444/reduce", function (event) {
      console.log("Could not connect to tormenta.cs.uchicago.edu. Cannot solve novel equations while not connected. Re-attempting connection in " + reconnectPauseSeconds + " seconds.");
      window.setTimeout(connectToSolverServer, reconnectPauseSeconds * 1000);
      reconnectPauseSeconds = Math.min(reconnectPauseSeconds*2, 300);
    });
  });
}

// Keep track of outstanding queries in case of connection failure.
window.solverOutstandingQueries = []

function forwardReduceResponseToElm(event) {
  console.log("REDUCE reply: " + event.data);
  window.solverOutstandingQueries.shift();
  app.ports.reduceResponse.send(event.data);
};

function resendOutstandingQueries() {
  for (i in window.solverOutstandingQueries) {
    sendQuery(window.solverOutstandingQueries[i])
  }
}

function sendQuery(query) {
  if (window.solverServerSocket.readyState == WebSocket.CONNECTING) {
    console.log("Waiting for connection to solver server...");
  } else if (window.solverServerSocket.readyState == WebSocket.CLOSING || window.solverServerSocket.readyState == WebSocket.CLOSED) {
    console.log("No server connection. Attempting to reconnect...");
    connectToSolverServer();
  } else {
    console.log("Querying REDUCE: " + query);
    window.solverServerSocket.send(query);
  }
}

app.ports.queryReduce.subscribe(function(query) {
  window.solverOutstandingQueries.push(query);
  sendQuery(query);
});

connectToSolverServer();

// window.solverServerSocket.onclose   = function(event) {
//   console.log("Solver server connection was closed. " + event.reason + " Attempting to reconnect...");
//   connectToSolverServer();
// }
