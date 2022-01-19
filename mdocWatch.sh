#!/bin/bash
## If coursier is not installed, install it
if ! [ -f ./cs ]
then
  echo "Downloading coursier"
  curl -fLo cs https://git.io/coursier-cli-"$(uname | tr LD ld)"
  chmod +x cs
fi

./cs launch org.scalameta:mdoc_2.12:2.2.24 -- --watch --in src/ --out out/
