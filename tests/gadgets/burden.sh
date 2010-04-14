#!/bin/bash
LINES=`cat $1 | wc -l`
FUNCS=`cat $1 | grep function | wc -l`
BURDEN=`echo "scale=2; $FUNCS*100/$LINES" | bc`
echo "$FUNCS funcs / $LINES lines = $BURDEN% overhead"
