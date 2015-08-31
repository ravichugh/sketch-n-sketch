#!/usr/bin/python

import re

def trimNewline(s):
  return s[:-1]

def writeLn(f, s):
  f.write(s + "\n")

def copyFromToWithIndent(rootDir, fromFilename, outFile, indent):
  def comment(s):
    writeLn (outFile, "%s<!-- %s LOADED_SRC %s -->" % (indent, s, fromFilename))

  comment("BEGIN")
  for s in open(fromFilename):
    s = trimNewline(s)
    s = s.replace("__ROOT__", rootDir)
    writeLn (outFile, indent + s)
  comment("END")

def expandSkeleton(rootDir, inFilename, outFilename):

  inn = open(inFilename)
  out = open(outFilename, "w+")

  for s in inn:
    s = trimNewline(s)
    s = s.replace("__ROOT__", rootDir)

    m = re.match("^( *)<!-- LOAD_SRC (.*) --> *$", s)
    if m:
      indent = m.group(1)
      filename = m.group(2)
      copyFromToWithIndent(rootDir, filename, out, indent)
      continue

    # fall-thru
    writeLn(out, s)

  print(outFilename)


expandSkeleton ( "."   , "src/index.src.html"                   , "../index.html"                     )
expandSkeleton ( ".."  , "src/releases/index.src.html"          , "../releases/index.html"            )
expandSkeleton ( ".."  , "src/blog/index.src.html"              , "../blog/index.html"                )
expandSkeleton ( ".."  , "src/blog/00-preview.src.html"         , "../blog/00-preview.html"           )
expandSkeleton ( ".."  , "src/blog/01-case-studies.src.html"    , "../blog/01-case-studies.html"      )
expandSkeleton ( ".."  , "src/blog/02-user-study.src.html"      , "../blog/02-user-study.html"        )
expandSkeleton ( ".."  , "src/tutorial/index.src.html"          , "../tutorial/index.html"            )
expandSkeleton ( ".."  , "src/tutorial/entrance.src.html"       , "../tutorial/entrance.html"         )
expandSkeleton ( ".."  , "src/tutorial/01.src.html"             , "../tutorial/01.html"               )
expandSkeleton ( ".."  , "src/tutorial/02.src.html"             , "../tutorial/02.html"               )
expandSkeleton ( ".."  , "src/tutorial/03.src.html"             , "../tutorial/03.html"               )
expandSkeleton ( ".."  , "src/tutorial/04.src.html"             , "../tutorial/04.html"               )
expandSkeleton ( ".."  , "src/tutorial/05.src.html"             , "../tutorial/05.html"               )
expandSkeleton ( ".."  , "src/tutorial/exit.src.html"           , "../tutorial/exit.html"             )

