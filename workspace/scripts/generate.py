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


expandSkeleton ( "."   , "src/index.src.html"                             , "../index.html"                     )
expandSkeleton ( ".."  , "src/releases/index.src.html"                    , "../releases/index.html"            )
expandSkeleton ( ".."  , "src/blog/index.src.html"                        , "../blog/index.html"                )
expandSkeleton ( ".."  , "src/blog/00-preview.src-generated.html"         , "../blog/00-preview.html"           )
expandSkeleton ( ".."  , "src/blog/01-case-studies.src-generated.html"    , "../blog/01-case-studies.html"      )
expandSkeleton ( ".."  , "src/blog/02-user-study.src-generated.html"      , "../blog/02-user-study.html"        )
expandSkeleton ( ".."  , "src/blog/03-user-study-videos.src-generated.html" , "../blog/03-user-study-videos.html" )
expandSkeleton ( ".."  , "src/blog/04-coffee.src-generated.html"          , "../blog/04-coffee.html" )
expandSkeleton ( ".."  , "src/blog/05-deuce.src-generated.html"           , "../blog/05-deuce.html" )
expandSkeleton ( ".."  , "src/tutorial/index.src.html"                    , "../tutorial/index.html"            )
expandSkeleton ( ".."  , "src/tutorial/entrance.src-generated.html"       , "../tutorial/entrance.html"         )
expandSkeleton ( ".."  , "src/tutorial/01.src-generated.html"             , "../tutorial/01.html"               )
expandSkeleton ( ".."  , "src/tutorial/02.src-generated.html"             , "../tutorial/02.html"               )
expandSkeleton ( ".."  , "src/tutorial/03.src-generated.html"             , "../tutorial/03.html"               )
expandSkeleton ( ".."  , "src/tutorial/04.src-generated.html"             , "../tutorial/04.html"               )
expandSkeleton ( ".."  , "src/tutorial/05.src-generated.html"             , "../tutorial/05.html"               )
expandSkeleton ( ".."  , "src/tutorial/exit.src-generated.html"           , "../tutorial/exit.html"             )

expandSkeleton ( "../.." , "src/misc/pldi2016-artifact.src.html" , "../releases/v0.4.1/pldi2016-artifact.html" )

