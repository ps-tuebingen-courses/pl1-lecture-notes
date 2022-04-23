#!/bin/bash
## If coursier is not installed, install it
if ! [ -f ./cs ]
then
  echo "Downloading coursier"
  curl -fLo cs https://git.io/coursier-cli-"$(uname | tr LD ld)"
  chmod +x cs
fi

./cs launch org.scalameta:mdoc_3:2.3.2 -- --watch --in src/ --out out/
