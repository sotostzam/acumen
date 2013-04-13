#!/bin/sh

set -e
sbt "run-main acumen.Main "$1" compile"
gcc model.c -lm
./a.out > model.res
head model.res
tail model.res
sbt "run-main acumen.Main "$1" last"
gnuplot model.gpl
