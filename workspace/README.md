# Sketch-n-Sketch website generator

Inside this folder, make sure to run `npm install` to correctly install Sketch-n-sketch.

## Website bigeneration

To regenerate the website and listen to changes in output or input, run:

    make

`make` or `make watch` actually runs the command:

    node generate.js --watch --autosync

### Specific launches

    make output
    
is a shortcut for `node generate.js --forward`, it re-generate the website only once.

    make input

is a shortcut for `node generate.js --backward`, it propagates the changes from the output to the inputs only once

### Advanced usage

If you want to start watching changes from either sides *after* back-propagating changes, run

    node backward.js --watch --backward --autosync

## Bonus: change the website visually

Install editor with `npm install -g http-server-editor`.
Then, in the folder above, containing `index.html`, run

    editor

Editor allows to back-propage visual changes to the HTML generated files.
To further back-propagate these changes to the real source files, open a new command line and follow the instructions below:



