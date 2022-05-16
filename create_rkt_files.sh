#!/bin/bash

# This script can be used for generating *.rkt files from the *.md files in the
# ./src directory.
#
# The script runs on all markdown files in the ./src directory and deletes
# everything which isn't inbetween racket codefences.
#
echo "Creating .rkt files..."
for d in $(find ./src -name "monadic-reflection.md" -o -name "letcc.md")
do
  echo "Create .rkt file for $d..."
  sed -n '/```racket/,/```/{/```\(racket\)\?/n;p}' $d > "$(dirname $d)/$(basename -- "$d" .md).rkt"
done
