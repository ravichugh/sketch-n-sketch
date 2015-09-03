#!/bin/bash

cd src/tutorial/posts
echo cd src/tutorial/posts

for f in `ls *.md`
do
  fHtml=../../tutorial/`echo $f | sed 's/\(.*\).../\1/'`.src-generated.html
  pandoc -H ../post-header.html -B ../post-before.html -A ../post-after.html $f > $fHtml
  echo Wrote to: $fHtml
done

echo
