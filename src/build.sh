#!/bin/bash

if [ $# -gt 1 ]; then
  echo "Invalid cmdline args"
  exit 10
fi

OCAMLBUILD="ocamlbuild -build-dir ../obj -quiet"

ACTION="build"

case $1 in
'opt') SUFFIX=".native";;
'prof') SUFFIX=".p.native";;
'') SUFFIX=".d.byte";;
'doc') ACTION="doc";;
'clean') ACTION="clean";;
*) echo "invalid argument"; exit 10;;
esac

if [[ -d LambdaJS ]]; then
  true
else
  echo "symlink to LambdaJS is missing"
  exit 10
fi

if [[ -d ../obj ]]; then
  true
else
  mkdir ../obj
fi

case $ACTION in
'build') $OCAMLBUILD main$SUFFIX; $OCAMLBUILD insinf$SUFFIX;;
'doc') $OCAMLBUILD doc.docdir/index.html;;
'clean') rm -rf ../obj;;
'') echo "shell script error--invalid action"; exit 10;;
esac
