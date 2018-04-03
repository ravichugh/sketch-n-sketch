#!/usr/bin/env bash

# http://stackoverflow.com/questions/59895/can-a-bash-script-tell-what-directory-its-stored-in
FILE_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

cd $FILE_DIR

if [ ! -f pathtobenchmark.txt ]; then
  echo "tests/pathtobenchmark.txt not found. Create it and put inside the relative or absolute path to the TeX file where the benchmarks will be written to. This file is ignored by git."
else
  elm-make UpdateBenchmarks.elm --output build/benchmarks.js && (node --stack_size=4096 support/runnerBenchmark.js | tee "$(< pathtobenchmark.txt)")
  echo "Content written to $(< pathtobenchmark.txt)"
fi

