 #!/bin/bash
 #parameter to manually change
pngFolder=$3/Bench_results
datFolder=$3/dataFiles

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
cat $2/* | grep -A 7 $1 | grep Total | awk '{med[i++]=$2; dist[j++]=$4; q1[k++]=$7; q3[l++]=$8}END{for (ind=0; ind< i; ind++)  print ind+1", " med[ind]", "dist[ind]", "q1[ind]", " q3[ind];exit med[i/2]}' > "$datFile"
medOfmed=$(cat $2/* | grep -A 7 $1 | grep Total | sort |awk '{med[i++]=$2;}END{if (i%2 == 1 ){print med[(i-1)/2] }else{ print med[i/2]}}')

echo -n "set terminal pngcairo font \"arial,10\" 
set output '$pngFile'
set xrange [0:$(($nbline+1))]
set datafile separator \",\"
plot '$datFile' using 1:2:5:4 title '$1' with errorbars , $medOfmed notitle" > $gnuplotFile

gnuplot $gnuplotFile
rm $gnuplotFile
eog "$pngFile"

