#!/bin/sh

sbt proguard
echo You can find the jar file in target/scala-2.10 directory
echo No examples are included in the jar file, you need to copy them manually