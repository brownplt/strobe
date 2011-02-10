#!/bin/bash


for file in ./untypable/*.js
do
  echo -e $file
  result=`../../tca -lint -noflows $file 2>&1`
  exitcode=$?
  if [ $exitcode -ne 2 ]; then
      echo -e "$file: expected a type-error error; exit code was $exitcode\n"
      continue
  fi
  result2=`rhino ./jslint.js $file 2>&1 | grep "No problems"`
  # There should be a lint error here
  if [[ $result2 != "" ]]; then
      echo -e "$file: JSlint found no problems but we don't type it"
      echo $result2
      continue
  fi
done

for file in ./typable/*.js
do
    ./single-test.sh $file
done
