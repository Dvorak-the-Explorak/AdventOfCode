#!/bin/bash
# This script should be sourced from within a shell
# and not executed. For instance with:
# 
#   . script
#        or
#   source script

folder=$1
useParsec=$2

mkdir $1
if [[ "$useParsec" == "parsec" ]]; then
	cp ../template_parsec.hs ./$1/main.hs
else
	cp ../template.hs ./$1/main.hs
fi
cp ../util/Helpers.hs ./$1/

cd $1
touch test_input.txt
touch input.txt

subl main.hs
subl test_input.txt
subl input.txt