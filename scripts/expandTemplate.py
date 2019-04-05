#!/usr/bin/python

import sys
from littleReader import *
from leoReader import *

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

inn = io.open(baseTemplate,encoding="utf8")

generatedStr = u""

for s in inn:
  s = trimNewline(s)
  toks = s.split(u" ")
  if toks[0] == u"LITTLE_TO_ELM":
    if len(toks) != 2:
      print("Bad line:\n" + s)
      sys.exit()
    else:
      name = toks[1]
      for t in readLittle(name, sourceFolder): generatedStr += t
  elif toks[0] == u"LEO_TO_ELM":
    if len(toks) != 2:
      print("Bad line:\n" + s)
      sys.exit()
    else:
      name = toks[1]
      for t in readLeo(name, sourceFolder): generatedStr += t
  else:
    generatedStr += s + "\n"

oldGenerated = io.open(baseGenerated, "r", encoding="utf8")
oldGeneratedStr = oldGenerated.read()
oldGenerated.close()

if generatedStr != oldGeneratedStr:
  write(io.open(baseGenerated, "w+", encoding="utf8"), generatedStr)
  print("Wrote to " + baseGenerated)
else:
  print(baseGenerated + " unchanged")
