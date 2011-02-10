#!/bin/bash

tmpfile=`mktemp -t lint.XXXX`
lintresult=`mktemp -t lint.XXXX`

sed '/<script>/d' $1 | 
sed '/<div id/d' | 
sed '/<\/div>/d' | 
sed '/<\/script>/d' > $tmpfile

echo -e $1
result=`../../tca -lint -noflows $tmpfile 2>&1`
if [ $? -ne 0 ]; then
    echo -e "$file: aborted with exit code $?; output was:\n$result\n"
fi


jslint=jslint-lib.js

hasdiv=`grep "<div id=" $1`
if [[ $hasdiv != "" ]]; then
    jslint=jslint.js
fi

result1=`rhino ./$jslint $1 &> $lintresult`
result2=`grep "No problems" $lintresult`
  # There should be a lint error here
if [[ $result2 == "" ]]; then
    echo -e "$file: JSlint found problems but we type it"
    echo -e "It reported: `cat $lintresult`"
    echo $result2
fi

echo -e "$1 done"