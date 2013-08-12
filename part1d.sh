#!/bin/sh

if [ $# -lt 2 ] ; then
	exit
fi

NUMPROC=$1
DIM=$2

for i in `seq 1 $NUMPROC` ; do
	SI=`expr \( $i - 1 \) \* $DIM / $NUMPROC + 1`
	EI=`expr $i \* $DIM / $NUMPROC`
	expr $EI - $SI + 1
	seq $SI $EI
done
