#!/usr/bin/env bash

# http://stackoverflow.com/questions/59895/can-a-bash-script-tell-what-directory-its-stored-in
FILE_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

cd $FILE_DIR

/usr/bin/time sh -c 'elm-make *Test*.elm --output build/test.js && node --stack_size=2048 support/runner.js'
