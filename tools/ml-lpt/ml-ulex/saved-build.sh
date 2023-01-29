#!/bin/sh
#
# Copyright (c) 2022 The Fellowship of SML/NJ (https://smlnj.org)
#
# build script for ml-ulex
#
# location: $SMLNJ/tools/ml-lpt/ml-ulex/build.sh
# options: None

CMD=$0

SMLNJROOT=`pwd`/../../..  # $SMLNJ
BIN_DIR=${INSTALLDIR:-$SMLNJROOT}/bin
BUILD=$BIN_DIR/tool-build

#
# check for unexpected command-line options
#
if [ "$#" != "0" ] ; then
  echo $CMD: invalid argument: $1
  exit 1
fi

# change the time for export.sml so that it will get compiled
# and executed by sml @CMbuild.
touch export.sml

# Build the ml-ulex tool as a standalone program:
# tool-build [sml options] CDFname toolname
"$BUILD" -DNO_ML_ANTLR -DNO_ML_LEX -DNO_ML_YACC "export.cm" "sources.cm" "ml-ulex"

exit 0
