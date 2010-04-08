#!/bin/bash

P=`dirname $0`
FILE=$1
echo "Running on $FILE"

#get the environment from the XML:
BASE=`echo $FILE | cut -d'.' -f1`

XML=$BASE.xml
ENVCMD=
if [ -f $XML ]
then
  ENV=`mktemp`
  trap 'rm -f $ENV' EXIT
  $P/../../env-scrapers/google-desktop-gadget-env.ss < $XML > $ENV
  ENVCMD="-env $ENV"
fi

STRXML="$BASE.strings.xml"
STRENVCMD=
if [ -f $STRXML ]
then
  STRENV=`mktemp`
  trap 'rm -f $STRENV' EXIT
  $P/../../env-scrapers/google-desktop-gadget-strings.ss < $STRXML > $STRENV
  STRENVCMD="-env $STRENV"
fi

RESULT=`$P/../../tcg $ENVCMD $STRENVCMD -tc $FILE 2>&1`
if [ $? -ne 0 ]; then
    echo "$FILE: aborted with exit code $?; output was:\n$RESULT\n"
fi

