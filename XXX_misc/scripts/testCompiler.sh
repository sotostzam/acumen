#!/bin/sh

set -e
sbt "run-main acumen.Main "$1" compile"
g++ -O model.cpp -lm
time ./a.out last > model.res
time ./a.out last > model.res
time ./a.out last > model.res
cat model.res
sbt "run-main acumen.Main "$1" time"
#sbt "run-main acumen.Main "$1" time"
#sbt "run-main acumen.Main "$1" time"

