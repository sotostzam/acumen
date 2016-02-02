#!/bin/sh

# Generate statistics
#
# Depends on "gitstats": http://gitstats.sourceforge.net/
# 
# Run without parameters, this will generate a directory "stats"
# with the default output of gitstats.
# To generate line stats, comparing line numbers in Acumen versus
# implementation (Scala, Java), pass anything as a parameter. 

# Check that gitstats is installed
if ! type "gitstats" > /dev/null; then
  echo "Please install gitstats: http://gitstats.sourceforge.net/."
fi

EXCLUDE_BASE="00_Basic,01_Braking,01_First,02_Braking,02_angles,03_Basic,03_Quantization,03_Vehicle,03_angles,04_Two,05_Lorenz,05_more,08_Two,11_floating,12_show,13_simple,21_Challenge,2D,3D,3ds,AUTHORS,Bleu,EXTERNALS,First,Group,Groups,LICENSE,LICENSE-AIC,LICENSE-Rice,Makefile,Matelasse,README,README-developers,README~,TESTERS,TODO,Tissue,With,XXX,XXX_EXTERNALS,XXX_HISTORY,XXX_HISTORY_STUB,XXX_README,XXX_TODO,XXX_basic_routine,XXX_creating_releases,XXX_main_workflow,bench,blend,blend1,class,cpp,exp,gif,gitignore,gitmodules,hpp,hs,html,jpg,mdown,ml,mtl,numbers,obj,pdf,pl,png,properties,py,res,result,sbt,sh,tex,txt,xml"
EXCLUDE_ACM=acm,$EXCLUDE_BASE
EXCLUDE_SCALAJAVA=scala,java,$EXCLUDE_BASE
GIT_DIR=../../
OUTPUT_DIR=stats

rm -rf stats
mkdir stats

if [ $# -eq 0 ] 
then
  echo "Generating default gitstats"
  gitstats $GIT_DIR $OUTPUT_DIR
else
  echo "Generating line number stats, ACM versus SCALA+JAVA."
  gitstats -c exclude_extensions=$EXCLUDE_SCALAJAVA $GIT_DIR $OUTPUT_DIR/outputACM
  gitstats -c exclude_extensions=$EXCLUDE_ACM $GIT_DIR $OUTPUT_DIR/outputSCALAJAVA
  # Output plotting file
  echo 'set terminal png transparent size 800,450
set size 1.0,1.0
set output "lines_of_code.png"
set key top left
set xdata time
set timefmt "%s"
set format x "%Y-%m"
set grid y
set xtics rotate
set bmargin 6
set ylabel "Lines of code in version control"

plot "'$OUTPUT_DIR'/outputSCALAJAVA/lines_of_code.dat" using 1:2 w lines title "Scala + Java Code", \
     "'$OUTPUT_DIR'/outputACM/lines_of_code.dat" using 1:2 w lines title "Acumen Code"
' > $OUTPUT_DIR/lines_of_code.plot
  gnuplot < $OUTPUT_DIR/lines_of_code.plot
fi
  
