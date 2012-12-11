#!/bin/sh

set -e

#
# Example script to run enclosure benchmarks varying two parameter
#
# Like one-parm.sh but varies two and attempts to display them
# using gnuplot, although not very well
#

title="Zeno Half Overconstrained"
model=examples/XXX_internal/enclosure/zeno/02_Zeno_Half_Overconstrained.acm
resultDir=benchmark-res
prefix=Zeno_Half_Overconstrained-minImprovment,minTimeStep
repeat=3
parm1=minImprovement
values1=0.00001,0.0001,0.001,0.005
parm2=minTimeStep
values2=0.0001,0.001,0.01,0.1

PREFIX="./$resultDir/$prefix"

mkdir -p $resultDir

sbt "run-main acumen.Main --semantics enclosure $model bench-enclosures $PREFIX $repeat $parm1=$values1 $parm2=$values2"

gnuplot <<EOF
set terminal png
set output "$PREFIX-runtime.png"
set title "$title"
set xlabel "$parm1"
set ylabel "$parm2"
set zlabel "runtime"
unset key # don't display a legend
set logscale x
set logscale y
splot "$PREFIX-runtime.dat" using 1:2:4
EOF

gnuplot <<EOF
set terminal png
set output "$PREFIX-precision.png"
set title "$title"
set xlabel "$parm1"
set ylabel "$parm2"
set zlabel "runtime"
unset key # don't display a legend
set logscale x
set logscale y
splot "$PREFIX-precision-norm.dat" using 1:2:4
EOF

