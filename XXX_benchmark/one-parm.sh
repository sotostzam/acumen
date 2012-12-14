#!/bin/sh

#
# Example script to run enclosure benchmarks varying one parameter.
#
# Run this scipt in the root of the source tree, not in the
# benchmark directory.  Its usage will be:
#   sh benchmark/one-parm.sh
# the results by default will go in a new dir: benchmark-res
#
# The main products will be two graphs
#   $prefix-runtime.png
#   $prefix-precision.png
# You will also notice lots of pdf files generated in the form of
#   $prefix-###.pdf
# these are the actual results of running the model with the
# paramter set to ###
# A bunch of raw data files are also generated
#
# Hint: To tweak the graphs comment out the sbt line to avoid
# regenerating the data 

set -e

title="Zeno Half Overconstrained"
model=examples/XXX_internal/enclosure/zeno/02_Zeno_Half_Overconstrained.acm
resultDir=benchmark-res
prefix=Zeno_Half_Overconstrained-minImprovment
repeat=3
parm=minComputationImprovement
values=0.00001,0.0001,0.001,0.005

PREFIX="./$resultDir/$prefix"

mkdir -p $resultDir

sbt "run-main acumen.Main --semantics enclosure $model bench-enclosures $PREFIX $repeat $parm=$values"

gnuplot <<EOF
set terminal png
set output "$PREFIX-runtime.png"
set title "$title"
set xlabel "$parm"
set ylabel "runtime"
unset key # don't display a legend
set logscale x
plot "$PREFIX-runtime.dat" using 1:3 with lines, \
     "$PREFIX-runtime.dat" using 1:3:5 with errorbars
EOF

gnuplot <<EOF
set terminal png
set output "$PREFIX-precision.png"
set title "$title"
set xlabel "$parm"
set ylabel "bigDecimalDigits"
unset key # don't display a legend
set logscale x
plot "$PREFIX-precision-norm.dat" using 1:3 with lines
EOF

