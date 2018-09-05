"use strict";

function setUiColorScheme(selectedColorSchemeName) {
  // Disable all color schemes
  var allColorSchemeElements = document.querySelectorAll("[data-color-scheme]");
  for (var i = 0; i < allColorSchemeElements.length; i++) {
    allColorSchemeElements[i].disabled = true;
  }

  // Enable selected color scheme
  var selectedColorSchemeElement =
    document.querySelectorAll(
      "[data-color-scheme=" + selectedColorSchemeName + "]"
    )[0];
  selectedColorSchemeElement.disabled = false;
}

function setAceColorScheme(selectedColorSchemeName) {
  if (selectedColorSchemeName === "light") {
    editor.setTheme("ace/theme/chrome");
  } else if (selectedColorSchemeName === "dark") {
    editor.setTheme("ace/theme/tomorrow_night");
  } else {
    console.error(
      "WARN (colorScheme.js): unknown color scheme name '" +
      selectedColorSchemeName +
      "'"
    );
  }
}

app.ports.updateColorSchemeByName.subscribe(function(selectedColorSchemeName) {
  setUiColorScheme(selectedColorSchemeName);
  setAceColorScheme(selectedColorSchemeName);
});
