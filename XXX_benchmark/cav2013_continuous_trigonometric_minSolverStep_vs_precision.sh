#!/bin/sh

# This script measures the effect of the simulator.minSolverStep parameter 
# on the end-time precision when simulating a purely continuous model, 
# whose solution is of the form x(t) = a*sin(t) + b*cos(t).
#
# Experiment
#
# Model: examples/9_Experimental/1_Continuous/05_Trigonometrics.acm
#
# Description: 
# The minComputationImprovement simulator parameter has been set to 0 in
# order to force the algorithm to improve the solution if this is possible.
# The simulator parameter minSolverStep is given the following values: 
#  
#   0.5 / 2^n, 0 <= n < 10.
#
# The simulator parameter minLocalizationStep is given the minimum value of 
# minSolverStep, i.e. 9.765625e-4, to ensure that is has a valid value, 
# although it is never used during simulation as this is a continuous system.

set -e

title="Trigonometrics, minSolverStep versus end-time precision"
model=examples/9_Experimental/1_Continuous/05_Trigonometrics.acm
resultDir=benchmark-res
prefix=Trigonometrics-minSolverStep
repeat=3
parm=minSolverStep
values=0.5,0.25,0.125,6.25e-2,3.125e-2,1.5625e-2,7.8125e-3,3.90625e-3,1.953125e-3,9.765625e-4

PREFIX="./$resultDir/$prefix"

mkdir -p $resultDir

sbt "run-main acumen.Main --semantics enclosure $model bench-enclosures $PREFIX $repeat $parm=$values endTime=1.5 minComputationImprovement=0 minLocalizationStep=9.765625e-4"

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
set ylabel "bigDecimalDigits"
unset key # don't display a legend
#set logscale x
plot "$PREFIX-precision-norm.dat" using 1:6 with lines
EOF

