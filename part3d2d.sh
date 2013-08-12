#!/bin/sh

if [ $# -lt 2 ] ; then
	exit
fi

NUMPROC=$1
DIM=$2

for j in `seq 1 $NUMPROC` ; do
for i in `seq 1 $NUMPROC` ; do
	SI=`expr \( $i - 1 \) \* $DIM / $NUMPROC + 1`
	EI=`expr $i \* $DIM / $NUMPROC`
	SJ=`expr \( $j - 1 \) \* $DIM / $NUMPROC + 1`
	EJ=`expr $j \* $DIM / $NUMPROC`
	expr \( $EI - $SI + 1 \) \* \( $EJ - $SJ + 1 \) \* $DIM
	for z in `seq 1 $DIM` ; do
	for y in `seq $SJ $EJ` ; do
	for x in `seq $SI $EI` ; do
		expr \( $z - 1 \) \* $DIM \* $DIM + \( $y - 1 \) \* $DIM + $x
	done
	done
	done
done
done
