#!/bin/bash

# This script can be used for generating *.scala files from the *.md files in the
# ./src directory.
#
# The script runs on all markdown files in the ./src directory and deletes
# everything which isn't inbetween scala codefences, e.g.
#
# ```scala
# val x = 1;
# ```
#
echo "Creating .scala files..."
for d in $(find ./src -name "*.md" -type f ! -name SUMMARY.md ! -name preface.md ! -name furtherreading.md)
do
  echo "Create .scala file for $d..."
  sed -n '/```scala/,/```/{/```\(scala\)\?/n;p}' $d > "$(dirname $d)/$(basename -- "$d" .md).scala"
done
