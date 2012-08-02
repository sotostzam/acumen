#!/bin/sh

FILENAME=$PWD/$1
ORIG=$PWD

cd ../target/scala_2.8.1.RC2/classes/
scala acumen.Main 2d < $FILENAME > ../../../demo/log.data
cd $ORIG
python disp2d.py < log.data
