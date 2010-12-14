#!/bin/bash


for file in ./tests/lint/untypable/*.js
do
  echo -e $file
  result=`./tca -lint -noflows $file 2>&1`
  exitcode=$?
  if [ $exitcode -ne 2 ]; then
      echo -e "$file: expected a type-error error; exit code was $exitcode\n"
      continue
  fi
done

for file in ./tests/lint/typable/*.js
do
  echo -e $file
  result=`./tca -lint -noflows $file 2>&1`
  if [ $? -ne 0 ]; then
      echo -e "$file: aborted with exit code $?; output was:\n$result\n"
  fi
done
