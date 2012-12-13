#!/bin/sh

if [ -z "$1" -o -z "$2" ]; then
  echo "usage: run-from-dir.sh DIR RESDIR [SEMANTICS]"
  exit 1
fi

DIR=$1
RESDIR=$2
SEMANTICS=$3

if [ -e $RESDIR ]; then
  echo "$RESDIR exists, existing"
  exit 1
fi

if [ -z "$SEMANTICS" ]; then
    SEMANTICS=enclosure
fi

cp -a $DIR $RESDIR

models=`find $RESDIR -name '*.acm'`

for f in $models; do
    dn=`dirname "$f"`
    bn=`basename "$f" .acm`
    prefix="$dn/$bn$EXTRA"
    CMD="run-main acumen.Main --semantics $SEMANTICS $f bench-enclosures $prefix 1"
    echo "***$CMD"
    sbt "$CMD"
done

