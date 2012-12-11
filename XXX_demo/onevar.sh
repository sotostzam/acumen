#!/bin/sh

ORIG=$PWD
FILENAME=$ORIG/$2

cd ../target/scala_2.8.1.RC2/classes/
scala acumen.Main trace < $FILENAME | python $ORIG/onevar.py $1
