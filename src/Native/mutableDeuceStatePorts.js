"use strict";

// Global state defined in index.html

app.ports.setHashesHovered.subscribe(function(hashes) {
  GLOBAL_hashesHovered = hashes;
});

app.ports.setHashesSelected.subscribe(function(hashes) {
  GLOBAL_hashesSelected = hashes;
});
