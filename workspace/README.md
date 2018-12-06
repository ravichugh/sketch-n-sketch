# Sketch-n-Sketch website generator

## Quick launch

    make

It will first regenerate the website, and then automatically propagate changes from the inputs to the outputs and vice-versa.
`make` or `make watch` actually runs the command:

    node generate.js --watch --autosync

## Specific launches

    make output
    
is a shortcut for `node generate.js --forward`, it re-generate the website only once.

    make input

is a shortcut for `node generate.js --backward`, it propagates the changes from the output to the inputs only once

## Advanced usage

If you want to start watching changes from either sides *after* back-propagating changes, run

    node backward.js --watch --backward --autosync




