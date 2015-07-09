#!/usr/bin/python

import sys
from littleReader import *

inn = open("PreludeTemplate.elm")
out = open("PreludeGenerated.elm", "w+")

for s in inn:
  s = trimNewline(s)
  toks = s.split(" ")
  if toks[0] == "LITTLE_TO_ELM":
    if len(toks) != 2:
      print "Bad line:\n" + s
      sys.exit()
    else:
      name = toks[1]
      for t in readLittle(name): write(out, t)
  else:
    writeLn(out, s)

print "Wrote to PreludeGenerated.elm"
