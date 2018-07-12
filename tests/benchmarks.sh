#!/usr/bin/env bash

# http://stackoverflow.com/questions/59895/can-a-bash-script-tell-what-directory-its-stored-in
FILE_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

cd $FILE_DIR

if [ $# -eq 0 ]
  then
  if [ ! -f pathtobenchmark.txt ]; then
    echo "tests/pathtobenchmark.txt not found. Create it and put inside the relative or absolute path to the TeX file where the benchmarks will be written to. This file is ignored by git. Alternatively, run ./benchmarks.sh --tests to only display the result"
  else
    elm-make UpdateBenchmarks.elm --output build/benchmarks.js && (node --stack-size=4096 support/runnerBenchmark.js | tee "$(< pathtobenchmark.txt)")
    echo "Content written to $(< pathtobenchmark.txt)"
  fi;
else
  elm-make UpdateBenchmarks.elm --output build/benchmarks.js && node  --max-old-space-size=4076 --stack-size=4096 support/runnerBenchmark.js #&& ../../../../../Program\ Files\ \(x86\)/VideoLAN/VLC/vlc.exe ../../../Dropbox/Musiques\ à\ partager/All\ by\ Myself.m4a
fi
