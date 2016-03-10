#!/usr/bin/env python
#############################################
# Filter the columns of an Acumen enclosure
# simulation output table, exported using
# File > Export Table.
#############################################

import csv
import sys
import argparse
import re

parser = argparse.ArgumentParser(
  formatter_class=argparse.RawTextHelpFormatter, 
  description=
    '''
    Filter columns of an Acumen enclosure simulation output table, 
    exported using File > Export Table in the Aucmen IDE.
    
    This can be used to create either plots of one or more variables
    against time, or a 2D phase-space plot of two variables against
    one another.
    
    Example usage for time-based plot (of x1 and x2 against time):
    
      filterplot.py --term svg exportedTable.tsv tmp/out.tsv "$@" \\
        "(#0.0 : Simulator).time"  "(#0 : Main).x1"  "(#0 : Main).x2"
      gnuplot < tmp/plot.gp > tmp/plot.svg     
    
    Example usage for phase-space plot (of x1 against x2):
    
      filterplot.py --term svg exportedTable.tsv tmp/out.tsv "$@" \\
        "(#0 : Main).x1"  "(#0 : Main).x2"
      gnuplot < tmp/plot.gp > tmp/plot.svg          
    ''')
parser.add_argument('--objectNames', action='store_true',
                    help="include object names in legend")
parser.add_argument('--xDomain', type=float, nargs=2,
                    help="x axis bounds")
parser.add_argument('--yDomain', type=float, nargs=2,
                    help="y axis bounds")
parser.add_argument('--term', dest='terminal',
                    choices=['png','svg','tikz'],
                    help="gnuplot terminal")
parser.add_argument('--xLabel', default="Time")
parser.add_argument('--yLabel', default="")
parser.add_argument('inFile',
                    help="input TSV file exported using File > Export Table")
parser.add_argument('outFile',
                    help="output CSV, to be used with the output plot.gp")
parser.add_argument('columns', nargs='+',
                    help="columns (varibles) in the input TSV file, on the form (CId : Model).name")
args = parser.parse_args()

cols = args.columns

isTimePlot = "(#0.0 : Simulator).time" in cols

if not isTimePlot and len(cols) != 2:
    print("Phase space plots only works for exactly two variables!")
    exit()

maxTime = 0

# Python regex for Acumen intervals. NOTE: Note same as gnuplot pattern furhter down!
intervalRegex = "\[(\-?\d*(\.\d*)?)\.\.(-?\d*(\.\d*))?\]"

with open(args.inFile,'rb') as tsvin, open(args.outFile, 'wb') as csvout:
    for c in cols:
        csvout.write('"' + c + '"\t'),
    csvout.write(""),
    tsvin = csv.reader(tsvin, delimiter='\t')
    for r,row in enumerate(tsvin):
        er = zip(range(len(row)),row)
        if r == 0:
            outColIndexes = [ k for k,c in er if c in cols ]
            if isTimePlot:
                timeCol = row.index("(#0.0 : Simulator).time")
            csvout.write("\n")
        else:
            if isTimePlot:
                csvout.write(row[timeCol] + "\t")
                if float(row[timeCol]) > maxTime:
                    maxTime = float(row[timeCol])
                for i,c in er:
                    if i in outColIndexes and i != timeCol:
                        csvout.write(c + "\t"),
                csvout.write("\n")
            else: # Phase-space plot
                for i,c in er:
                    try:
                        xMatch = re.match(intervalRegex, row[outColIndexes[0]])
                        yMatch = re.match(intervalRegex, row[outColIndexes[1]])
                        x1 = xMatch.group(1)
                        x2 = xMatch.group(3)
                        y1 = yMatch.group(1)
                        y2 = yMatch.group(3)
                        # print (((row[outColIndexes[0]],x1,x2),(row[outColIndexes[1]],y1,y2)))
                        csvout.write(x1 + " " + y1 + "\n")
                        csvout.write(x2 + " " + y1 + "\n")
                        csvout.write(x2 + " " + y2 + "\n\n")
                        csvout.write(x1 + " " + y2 + "\n")
                    except:
                        print(("Could not parse ",row[outColIndexes[0]],row[outColIndexes[1]]))
lc = len(cols)
rlc = range(lc+1)
gpout = open('tmp/plot.gp','w')
if args.terminal == 'tikz':
    gpout.write("set term tikz size 8cm,5cm\n")
    gpout.write("set output 'tmp/out.tex'\n")
else: 
    if args.terminal == 'svg':
        gpout.write("set term svg size 600,300\n")
        gpout.write("set output 'tmp/out.svg'\n")
    else: # args.term == 'png'
        gpout.write("set term png size 500,300\n")
        gpout.write("set output 'tmp/out.png'\n")
if args.xDomain is not None:
    xMin = args.xDomain[0]
    xMax = args.xDomain[1]
else:
    xMin = 0
    xMax = maxTime

gpout.write("""
# Palette
# Color definitions
# Axes
set style line 11 lc rgb '#000000' lt 1
set border 3 back ls 11
set tics nomirror in scale 0.5
set arrow from graph 1,0 to graph 1.05,0 size screen 0.025,15,60 filled ls 11
set arrow from graph 0,1 to graph 0,1.05 size screen 0.025,15,60 filled ls 11
""")
gpout.write("set xrange [" + str(xMin) + ":" + str(xMax) + "]\n")
if args.yDomain is not None:
    vMin = args.yDomain[0]
    vMax = args.yDomain[1]
    gpout.write("set yrange [" + str(vMin) + ":" + str(vMax) + "]\n")
gpout.write("""
# Colors
set linetype 1 lc rgb '#444444'
set linetype 2 lc rgb '#AAAAAA'
# Legend
# set key bottom left
unset key
# Axis labels
""")
gpout.write('set xlabel "' + args.xLabel + '" offset 0,0.3\n')
gpout.write('set ylabel "' + args.yLabel + '" \n')

si = "\[%*lf\.\.%*lf\]\\t" # pattern to skip an interval

if isTimePlot: # Plot against time
    for i,c in zip(range(len(cols)),cols):
        if i < lc-1:
            e = ', \\\n'
        else:
            e = '\n'
        if i == 0:
            f = "plot '" + args.outFile + "'"
        else:
            f = "''"
        intervalSkipPattern = si*(i-1)
        gpout.write(f + ' u ' + ":".join([str(i+1) for i in range(len(cols))]) + ' "') 
        # parse time
        gpout.write('%lf\\t')
        # skip initial intervals
        gpout.write(intervalSkipPattern)
        # parse interval (in column i)
        gpout.write("\[%lf\.\.%lf\]" + '" with filledcurves closed title "')
        if args.objectNames:
            n = c
        else:
            n = c.split(".")[-1]
        if args.terminal == 'tikz':
            gpout.write('$' + n + '$' + '"' + e)
        else:
            gpout.write(n + '"' + e)
else: # Phase-space plot
    cs = len(cols)
    f = "plot '" + args.outFile + "'"
    columnList = ":".join([str(i+1) for i in range(cs)])
    gpout.write(f + ' u ' + columnList + ' with filledcurves notitle "')