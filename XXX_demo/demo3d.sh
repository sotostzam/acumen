#!/bin/sh

FILENAME=$PWD/$1
ORIG=$PWD

cd ../target/scala_2.8.1.RC2/classes/
scala acumen.Main 3d < $FILENAME > ../../demo/log.data
cd $ORIG
python disp3d.py < log.data
