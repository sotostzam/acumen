#!/bin/sh

FILENAME=$1

cd ../target/scala_2.8.1.RC2/classes/
scala acumen.Main bench < $FILENAME $2 $3
