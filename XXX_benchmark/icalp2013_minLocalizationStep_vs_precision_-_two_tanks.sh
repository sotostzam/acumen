#!/bin/sh

# This script measures the effect of the simulator.minLocalizationStep parameter 
# on the end-time precision when simulating a model of the two tanks hybrid system.
#
# Experiment
#
# Model: examples/9_Experimental/1_Enclosures/3_Zeno/08_Two_Tanks_Overconstrained_Helper_Variable.acm
#
# Description: 
# The minComputationImprovement simulator parameter has been set to 0 in
# order to force the algorithm to improve the solution if this is possible.
# The simulator parameter minLocalizationStep is given the following values: 
#  
#   0.1 / 2^n, 0 <= n < 8.
#
# The end time has been set to 4 to allow the enclosure to stabilize for large
# values of minLocalizationStep. The simulator parameter minSolverStep is given 
# the value of the simulation end time since the continuous behaviour of the 
# system is linear.


set -e

title="Two tanks Overconstrained with helper variable, minSolverStep versus end-time precision"
model=examples/9_Experimental/1_Enclosures/3_Zeno/08_Two_Tanks_Overconstrained_Helper_Variable.acm
resultDir=benchmark-res
prefix=Two_Tanks_Overconstrained_Helper_Variable-minLocalizationStep
repeat=3
parm=minLocalizationStep
values=0.1,5.0e-2,2.5e-2,1.25e-2,6.25e-3,3.125e-3,1.5625e-3,7.8125e-4,3.90625e-4

PREFIX="./$resultDir/$prefix"

mkdir -p $resultDir

sbt "run-main acumen.Main --semantics enclosure $model bench-enclosures $PREFIX $repeat $parm=$values endTime=4 minSolverStep=4 minComputationImprovement=0"

gnuplot <<EOF
set terminal png
set output "$PREFIX-runtime.png"
set title "$title"
set xlabel "$parm"
set ylabel "runtime"
unset key # don't display a legend
set logscale x
plot "$PREFIX-runtime.dat" using 1:6 with lines, \
     "$PREFIX-runtime.dat" using 1:6:8 with errorbars
EOF

gnuplot <<EOF
set terminal png
set output "$PREFIX-precision.png"
set title "$title"
set xlabel "$parm"
set ylabel "aggregate enclosure width at end time"
unset key # don't display a legend
#set logscale x
plot "$PREFIX-precision-norm.dat" using 1:6 with linespoints
EOF

