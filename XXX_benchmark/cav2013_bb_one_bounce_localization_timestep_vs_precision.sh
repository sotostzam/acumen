#!/bin/sh

# This script measures the effect of the simulator.minLocalizationStep parameter on the end-
# time precision when simulating an over-constrained model of a bouncing ball.
#
# Experiment
#
# Model: examples/9_Experimental/3_Zeno/11_Bouncing_Ball_Overconstrained_Helper_Variable.acm
#
# Description: The endTime simulator parameter has been overwritten to be equal to 1.5, the 
# time at which the ball's velocity changes sign. The simulator parameter minSolverStep is 
# set to 0.15, and localizationTimeStep is given the following values:
# 
#   minSolverStep / 2^n, 0 <= n <= 10.
#
# The minComputationImprovement simulator parameter has been set to 0 in order to force
# the algorithm to improve the solution if this is possible.

set -e

title="Zeno Half Overconstrained, minLocalizationStep versus end-time precision"
model=examples/9_Experimental/3_Zeno/11_Bouncing_Ball_Overconstrained_Helper_Variable.acm
resultDir=benchmark-res
prefix=Zeno_Half_Overconstrained-minLocalizationStep
repeat=3
parm=minLocalizationStep
values=0.15,7.5e-2,3.75e-2,1.875e-2,9.375e-3,4.6875e-3,2.34375e-3,1.171875e-3

PREFIX="./$resultDir/$prefix"

mkdir -p $resultDir

sbt "run-main acumen.Main --semantics enclosure $model bench-enclosures $PREFIX $repeat $parm=$values endTime=1.5 minComputationImprovement=0 minSolverStep=0.5"

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
plot "$PREFIX-precision-norm.dat" using 1:6 with lines
EOF

