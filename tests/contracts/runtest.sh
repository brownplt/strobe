#!/bin/bash

SRC=$1
DEST=`mktemp`

set -o pipefail

cat ../../data/contracts.js > $DEST
../../build/main.d.byte -contracts $SRC >> $DEST

if [[ $(head -n1 $SRC) == "// succeeds" ]]; then
  OUTPUT=`./v8 -f $DEST 2>&1`
  if [ $? -ne 0 ]; then
    echo -e "[ FAILED ] $SRC aborted with exit code $?\nResult was $OUTPUT"
    exit 0
  fi
else
  OUTPUT=$(./v8 -f $DEST 2>&1 | head -n1)
  EXITCODE=$?
  if [ $EXITCODE -ne 1 ]; then
    echo "[ FAILED ] $SRC aborted with exit code $EXITCODE"
    echo $OUTPUT
    exit 0
  fi

  EXPECTED=$(grep -o "contract violation: .*" $SRC | head -n1)
  OUTPUT_LINE1=$(echo $OUTPUT | grep -o "contract violation: .*")
  if [[ $OUTPUT_LINE1 == $EXPECTED ]]; then
    true
  else
    echo "[ FAILED ] $SRC produced:"
    echo $OUTPUT_LINE1
    echo "expected:"
    echo $EXPECTED
  fi
fi


