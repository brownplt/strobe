#!/bin/bash

P=`dirname $0`
FILE=$1

RESULT=`$P/../../tc -env $FILE.env -sb $FILE -noflows 2>&1`
if [ $? -ne 0 ]; then
  echo -e "$FILE: aborted with exit code $?; output was:\n$RESULT\n"
fi
