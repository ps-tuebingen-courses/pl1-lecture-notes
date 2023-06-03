#!/bin/bash
## If coursier is not installed, install it
if ! [ -f ./cs ]
then
  echo "Downloading coursier"
  curl -fL "https://github.com/coursier/launchers/raw/master/cs-x86_64-pc-linux.gz" | gzip -d > cs
  chmod +x cs
fi

./cs launch org.scalameta:mdoc_3:2.3.7 -- --in src/ --out out/
