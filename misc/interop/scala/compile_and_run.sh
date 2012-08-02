#!/bin/sh

ROOT=../../..
CP=.:$ROOT/target/scala_2.8.1.RC4/classes

for j in `find $ROOT -name '*.jar' | grep 2\.8\.1\.RC4`; do
 CP=$CP:$j
done

# call the scala compiler
java -cp $CP scala.tools.nsc.Main -cp $CP Test.scala
# launch the example
java -cp $CP Test < glucose.acm

