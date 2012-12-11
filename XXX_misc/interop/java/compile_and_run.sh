#!/bin/sh

javac -cp $SCALA_HOME/lib/scala-library.jar:../../../target/scala_2.8.1.RC4/classes/:. Test.java
java -cp $SCALA_HOME/lib/scala-library.jar:../../../target/scala_2.8.1.RC4/classes/:.  Test  < ../../../src/main/resources/acumen/examples/bouncing_ball_1d.acm 

