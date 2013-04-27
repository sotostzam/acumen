#!/bin/sh

set -e
sbt "run-main acumen.Main "$1" compile"
g++ -O model.cpp -lm
time ./a.out > model.res
time ./a.out > model.res
time ./a.out > model.res
head model.res
tail model.res
sbt "run-main acumen.Main "$1" time"
sbt "run-main acumen.Main "$1" time"
sbt "run-main acumen.Main "$1" time"
gnuplot model.gpl
