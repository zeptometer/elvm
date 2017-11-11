#!/bin/bash

set -e

dir=$(mktemp -d)
mkdir -p $dir
cp $1 $dir/$(basename $1).scm
infile=${dir}/input.scm

gosh ${dir}/$(basename $1).scm
rm -fr $dir
