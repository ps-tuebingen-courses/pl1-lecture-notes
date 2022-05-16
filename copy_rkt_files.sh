#!/bin/bash

# This script is used for copying the generated rkt files from the ./src
# directory to the ./out directory.
#
echo "Copying .rkt files..."
for d in $(find ./src -name "*.rkt" -type f)
do
  echo "Copy .rkt file $d..."
  cp $d $(echo $d | sed -r 's;^\./src;./out;')
done
