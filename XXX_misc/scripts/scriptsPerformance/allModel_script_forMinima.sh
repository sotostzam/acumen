#!/bin/bash
model=$(cat $1/* |grep .acm | sed 's/.*\/\(.*\).acm.*/\1/g' |sort|uniq)
for mod in $model 
do
	echo $mod
	bash script_forMinima.sh "$mod" $1 $2 &
done
