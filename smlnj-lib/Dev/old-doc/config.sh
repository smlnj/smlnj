#!/bin/sh
#
# This script creates the Makefile for building the documentation.  You
# will need to have installed the ML-Doc tools and have mk-mldoc-makefile
# program in your path.

find ML-Doc -name "*.mldoc" -print | mk-mldoc-makefile

mkDirTree () {
  base=$1
  if test ! -d $base ; then
    mkdir $base || (echo "unable to create $base"; exit 1)
  fi
  for i in ML-Doc/* ; do
    if test -d $i ; then
      f=$base/$(basename $i)
      if test ! -d $f; then
        echo " creating $f"
        mkdir $f || (echo "unable to create $f"; exit 1)
      fi
    fi
  done
}

mkDirTree Info
mkDirTree HTML
mkDirTree Hardcopy

