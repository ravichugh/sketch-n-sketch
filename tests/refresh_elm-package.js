// The test elm-package.json needs to be nearly identical to
// the main elm-package.json.

// This script copies the main elm-package.json, adds the necessary
// additions, then runs `elm-package` to prompt you for the
// installation of any required packages.

// Run like so:
// $ node refresh_elm-package.js

const fs = require("fs");
const child_process = require("child_process");

var main_elm_package_str = fs.readFileSync(__dirname + "/../src/elm-package.json", 'utf8');
var elm_package_json     = JSON.parse(main_elm_package_str);

// Tell Elm to look in /src for source files
elm_package_json["source-directories"].push("../src");

// elm_package_json["dependencies"]["laszlopandy/elm-console"] = "1.1.0 <= v < 2.0.0";

var test_elm_package_str = JSON.stringify(elm_package_json, null, "    ");

fs.writeFileSync(__dirname + "/elm-package.json", test_elm_package_str, 'utf8');

child_process.spawnSync("elm-package", ["install"], {stdio: 'inherit'});
