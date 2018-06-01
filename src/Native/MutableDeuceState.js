"use strict";

// Global state defined in index.html

var _user$project$Native_MutableDeuceState = {
  isHovered : function(hash) {
    console.log("is hovered")
    return GLOBAL_hashesHovered.includes(hash);
  },

  isSelected : function(hash) {
    return GLOBAL_hashesSelected.includes(hash);
  }
};
