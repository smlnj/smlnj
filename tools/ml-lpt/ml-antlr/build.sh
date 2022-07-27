#!/bin/sh
#
# Copyright (c) 2018 The Fellowship of SML/NJ (https://smlnj.org)
#
# build script for ml-antlr
#
# options:
#   -o image		-- specify the name of the heap image, "ml-antlr"
#			   is the default.

CMD=$0

ROOT="ml-antlr"
HEAP_IMAGE=""
SMLNJROOT=`pwd`/../..
BIN=${INSTALLDIR:-$SMLNJROOT}/bin
BUILD=$BIN/ml-build
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
# Build the ml-antlr standalone program:
"$BUILD" $SIZE_OPT -DNO_ML_ULEX -DNO_ML_ANTLR sources.cm Main.main $HEAP_IMAGE

exit 0
