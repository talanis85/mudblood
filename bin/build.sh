#!/bin/sh

MAINFILE="$1"
shift
MUDBLOOD_DIR="$1"
shift
GHC_OPTS=$@

mkdir build
ghc -O1 -threaded -hidir build -odir build --make "-i$MUDBLOOD_DIR" "-i$(dirname $MAINFILE)" $GHC_OPTS $MAINFILE
