#!/bin/sh
#
# Copyright (c) 2022 The Fellowship of SML/NJ (https://smlnj.org)
#
# build script for ml-ulex
#
# options:
#   -o image		-- specify the name of the heap image, "ml-ulex"
#			   is the default.

CMD=$0

ROOT="ml-ulex"
HEAP_IMAGE=""
SMLNJROOT=`pwd`/../../..
BIN=${INSTALLDIR:-$SMLNJROOT}/bin
LIB=${INSTALLDIR:-$SMLNJROOT}/lib
BUILD=$BIN/ml-build
SML=$BIN/sml

#
# process command-line options
#
while [ "$#" != "0" ] ; do
  arg=$1
  shift
  case $arg in
    -o)
      if [ "$#" = "0" ]; then
        echo "$CMD: must supply image name for -o option"
        exit 1
      fi
      HEAP_IMAGE=$1; shift
      ;;
    -64) ;; # ignore size specification for compatibility with 2021.1
    *)
      echo $CMD: invalid argument: $arg
      exit 1
      ;;
  esac
done

if [ "$HEAP_IMAGE" = "" ]; then
  HEAP_IMAGE="$ROOT"  # this is the normal case
fi

#
# Build the ml-ulex standalone program:
# ml-build <-D args> (root=sources.cm), (main=Main.main), (heap="ml-ulex")
"$BUILD" -DNO_ML_ANTLR -DNO_ML_LEX -DNO_ML_YACC sources.cm Main.main $HEAP_IMAGE

exit 0
