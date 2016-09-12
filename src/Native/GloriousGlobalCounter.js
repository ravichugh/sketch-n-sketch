window.gloriousGlobalCounter = 0;

Elm.Native = Elm.Native || {};
Elm.Native.GloriousGlobalCounter = {};
Elm.Native.GloriousGlobalCounter.make = function(elm) {

  elm.Native = elm.Native || {};
  elm.Native.GloriousGlobalCounter = elm.Native.GloriousGlobalCounter || {};

  if (elm.Native.GloriousGlobalCounter.values) {
    return elm.Native.GloriousGlobalCounter.values;
  }

  return elm.Native.GloriousGlobalCounter.values = {

    next: function () {
      window.gloriousGlobalCounter += 1;
      return window.gloriousGlobalCounter;
    },

  };
};