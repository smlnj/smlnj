#!/bin/sh
#
# Copyright (c) 2022 The Fellowship of SML/NJ (https://smlnj.org)
#
# build script for nowhere
#
# options:
#   -o image		-- specify the name of the heap image, "nowhere"
#			   is the default.

CMD=$0

ROOT="nowhere"
HEAP_IMAGE=""
SMLNJROOT=`pwd`/../../..
BIN=${INSTALLDIR:-$SMLNJROOT}/bin
BUILD=$BIN/ml-build

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
	*)
	    echo $CMD: invalid argument: $arg
	    exit 1
	    ;;
    esac
done

if [ "$HEAP_IMAGE" = "" ]; then
    HEAP_IMAGE="$ROOT"
fi

$BUILD nowhere.cm NoWhere.main $HEAP_IMAGE
