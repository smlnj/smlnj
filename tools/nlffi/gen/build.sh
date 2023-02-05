#!/bin/sh
#
# Copyright (c) 2022 The Fellowship of SML/NJ (https://smlnj.org)
#
# build script for ml-nlffigen
#
# options:
#   -o image		-- specify the name of the heap image, "ml-nlffigen"
#			   is the default.

CMD=$0

ROOT="ml-nlffigen"
HEAP_IMAGE=""
TWOUP=`pwd`/../..
BIN=${INSTALLDIR:-$TWOUP}/bin
BUILD="$BIN/ml-build"

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
  HEAP_IMAGE="$ROOT"
fi

exec "$BUILD" ml-nlffigen.cm Main.main "$HEAP_IMAGE"
