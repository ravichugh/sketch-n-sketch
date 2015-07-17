#!/bin/bash

cd src/blog/posts
echo cd src/blog/posts

for f in `ls *.md`
do
  fHtml=../../blog/`echo $f | sed 's/\(.*\).../\1/'`.src.html
  pandoc -H ../post-header.html -B ../post-before.html -A ../post-after.html $f > $fHtml
  echo Wrote to: $fHtml
done

echo
