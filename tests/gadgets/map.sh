#!/bin/bash

CMD=$1
shift

for file in $*
do
  echo -n "$file: "
  $CMD $file
done

