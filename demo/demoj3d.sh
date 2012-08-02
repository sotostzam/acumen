#!/bin/sh

FILENAME=$PWD/$1
J3D=$PWD/../lib/j3d
EXT=$J3D/ext

export CLASSPATH=$EXT/j3dcore.jar:$EXT/j3dutils.jar:$EXT/vecmath.jar:$CLASSPATH
export LD_LIBRARY_PATH=$J3D/i386:$LD_LIBRARY_PATH

cd ../target/scala_2.8.1.RC2/classes/
echo $CLASSPATH
scala acumen.Main java3d < $FILENAME
