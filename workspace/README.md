# Sketch-n-Sketch website generator

To install the dependencies, run Editor, the generation pipeline once and listen to the change, just type

    make

For more precise commands, see below.

## Installation instructions

    make install

This will install Editor globally and all the dependencies of this project.

## Website bigeneration

To just propagate the changes to/from inputs/outputs (without Editor), run

    make watch

`make watch` actually runs the command:

    node generate.js --watch --autosync

### Specific launches

To regenerate the website once, run:

    make output
    
which is a shortcut for `node generate.js --forward`.  
To back-propagate once modifications of the outputs to the inputs, run:

    make input

which is a shortcut for `node generate.js --backward`

