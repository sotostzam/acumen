#!/bin/sh

FILENAME=`readlink -f $1`

cd ../target/scala_2.8.1.RC2/classes/
scala acumen.Main trace < $FILENAME 
