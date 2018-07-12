#!/usr/bin/env bash

# http://stackoverflow.com/questions/59895/can-a-bash-script-tell-what-directory-its-stored-in
FILE_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

cd $FILE_DIR

if [ $# -eq 0 ]
  then
  if [ ! -f pathtobenchmark.txt ]; then
    echo "Hint: Create the file tests/pathtobenchmark.txt and put inside the relative or absolute path to the TeX file where the benchmarks will be written to. pathtobenchmark.txt is ignored by git. Alternatively, run ./benchmarks.sh --tests to not display this hint."
    elm-make UpdateBenchmarks.elm --output build/benchmarks.js && node support/runnerBenchmark.js
  else
    elm-make UpdateBenchmarks.elm --output build/benchmarks.js && (node support/runnerBenchmark.js | tee "$(< pathtobenchmark.txt)")
    echo "Content written to $(< pathtobenchmark.txt)"
  fi;
else
  elm-make UpdateBenchmarks.elm --output build/benchmarks.js && node  support/runnerBenchmark.js
fi
