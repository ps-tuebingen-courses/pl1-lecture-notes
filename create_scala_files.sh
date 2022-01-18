#!/bin/bash
echo "Creating .scala files..."
for d in $(find ./src -name "*.md" -type f)
do
  #Do something, the directory is accessible with $d:
  echo "Create .scala file for $d..."
  sed -n '/```scala/,/```/{/```\(scala\)\?/n;p}' $d > "$(dirname $d)/$(basename -- "$d" .md).scala"
done