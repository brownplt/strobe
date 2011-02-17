#!/bin/bash

P=`dirname $0`
FILE=$1

BASE=`echo $FILE | cut -d'.' -f1`

RESULT=`$P/../../tcg -env $BASE.env -tc $FILE 2>&1`
if [ $? -ne 0 ]; then
    echo "$FILE: aborted with exit code $?; output was:\n$RESULT\n"
fi

