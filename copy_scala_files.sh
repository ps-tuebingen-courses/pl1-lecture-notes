#!/bin/bash

# This script is used for copying the generated scala files from the ./src
# directory to the ./out directory.
#
echo "Copying .scala files..."
for d in $(find ./src -name "*.scala" -type f)
do
  echo "Copy .scala file $d..."
  cp $d $(echo $d | sed -r 's;^\./src;./out;')

done
