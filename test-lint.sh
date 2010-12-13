#!/bin/bash



for file in ./tests/lint/typable/*.js
do
  result=`./tca -lint -noflows $file 2>&1`
  if [ $? -ne 0 ]; then
      echo -e "$file: aborted with exit code $?; output was:\n$result\n"
  fi
done
