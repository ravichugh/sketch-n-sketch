#!/usr/bin/python

import sys
from littleReader import *

if len(sys.argv) < 2 or len(sys.argv) > 3:
  print("Usage: expandTemplate.py BASENAME [SOURCEFOLDER]")
  sys.exit()

base = sys.argv[1]
baseTemplate = base + 'Template.elm'
baseGenerated = base + 'Generated.elm'

try:
  sourceFolder = sys.argv[2]
except IndexError:
  sourceFolder = "../examples/"

inn = open(baseTemplate)
out = open(baseGenerated, "w+")

for s in inn:
  s = trimNewline(s)
  toks = s.split(" ")
  if toks[0] == "LITTLE_TO_ELM":
    if len(toks) != 2:
      print("Bad line:\n" + s)
      sys.exit()
    else:
      name = toks[1]
      for t in readLittle(name, sourceFolder): write(out, t)
  else:
    writeLn(out, s)

print("Wrote to " + baseGenerated)
