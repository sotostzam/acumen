 #!/bin/bash
 #parameter to manually change
pngFolder=$3/pictures/withMin_2Sig
datFolder=$3/data/withMin_2Sig

gnuplotFile=$3/$1.plot

#Folders creation if not exist
pngFile=$pngFolder"/$1.png"
if [ ! -d "$pngFolder" ]; then
mkdir "$pngFolder"
fi

datFile=$datFolder"/$1.dat"
if [ ! -d "$datFolder" ]; then
mkdir "$datFolder"
fi


nbline=$(cat $2/* | grep -A 7 $1 | grep Total| wc -l)
cat $2/* 2>/dev/null | grep -A 7 $1 | grep Total | awk '{print 1+i++ ", "  $2 ", " $5 ", " $6 ", 0.1"}' > "$datFile"
medOfmin=$(cat $2/* | grep -A 7 $1 | grep Total | sort |awk '{med[i++]=$2;}END{if (i%2 == 1 ){print med[(i-1)/2] }else{ print (med[i/2-1] + med[i/2])/2}}')

#display inspired by gnuplot example of http://www.gnuplot.info/demo/mgr.html
echo -n "set terminal pngcairo font \"arial,10\" 
set output '$pngFile'
set xrange [0:$(($nbline+1))]

set datafile separator \",\"

plot '$datFile' using 1:2:4:3 title '$1' with yerrorbars , '$datFile' using 1:2:5 ls 1 lw 0.3 with circles notitle, $medOfmin notitle" > $gnuplotFile

gnuplot $gnuplotFile
rm $gnuplotFile
eog "$pngFile"

