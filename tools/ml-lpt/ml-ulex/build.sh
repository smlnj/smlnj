#!/bin/sh
#
# Copyright (c) 2018 The Fellowship of SML/NJ (https://smlnj.org)
#
# build script for ml-ulex
#
# options:
#   -o image		-- specify the name of the heap image, "ml-ulex"
#			   is the default.

CMD=$0

ROOT="ml-ulex"
HEAP_IMAGE=""
SMLNJROOT=`pwd`/../..
BIN=${INSTALLDIR:-$SMLNJROOT}/bin
LIB=${INSTALLDIR:-$SMLNJROOT}/lib
BUILD=$BIN/ml-build
SML=$BIN/sml
SIZE_OPT="-32"

#
# process command-line options
#
while [ "$#" != "0" ] ; do
    arg=$1
    shift
    case $arg in
	-32) SIZE_OPT=$arg ;;
	-64) SIZE_OPT=$arg ;;
	-o)
	    if [ "$#" = "0" ]; then
		echo "$CMD: must supply image name for -o option"
		exit 1
	    fi
	    HEAP_IMAGE=$1; shift
	    ;;
	*)
	    echo $CMD: invalid argument: $arg
	    exit 1
	    ;;
    esac
done

if [ "$HEAP_IMAGE" = "" ]; then
    HEAP_IMAGE="$ROOT"
fi

#
# Build the ml-ulex standalone program:
"$BUILD" $SIZE_OPT -DNO_ML_ANTLR -DNO_ML_LEX -DNO_ML_YACC sources.cm Main.main $HEAP_IMAGE

exit 0
