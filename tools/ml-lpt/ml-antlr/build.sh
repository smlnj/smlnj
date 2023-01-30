#!/bin/sh
#
# Copyright (c) 2023 The Fellowship of SML/NJ (https://smlnj.org)
#
# tool build script for: ml-antlr
#
# location: $SMLNJ/tools/ml-lpt/ml-antlr/build.sh
# options: None

CMD=$0

# cleanup trap: delete BOOTLIST and XXXX files output by (sml @CMbuild), and also
# the bin file .cm/*/export.sml, because this will force compilation and execution
# of export.sml at the next call of build.sh.
trap 'rm -f BOOTLIST XXXX .cm/*/export.sml' 0 1 2 3 15

SMLNJROOT=`pwd`/../../..  # $SMLNJ (e.g. = ~/sml/Dev/github/smlnj in my case)
BIN_DIR=$SMLNJROOT/bin
SML=$BIN_DIR/sml

# define ARCH and OPSYS using bin/.arch-n-opsys
ARCH_N_OPSYS=`"$BIN_DIR/.arch-n-opsys"`
if [ "$?" != "0" ]; then
  echo "$CMD: unable to determine architecture/operating system"
  exit 1
fi
eval $ARCH_N_OPSYS

RUN=$BIN_DIR/.run/run.$ARCH-$OPSYS

# ml-ulex/build.sh does not take command line options, so no option processing is done.

# REDUNDANT: update the modification time for export.sml so that it will get compiled
# and executed by export.cm (invoked through the $SML @CMbuild command). This is redundant,
# because the trap will delelet .cm/*/export.sml, and this also causes CM to
# compile and execute "export.sml" when this script is run next.
touch export.sml

# Build the tool as a standalone program - step 1:
# The 5 positional arguments (no setup) after @CMbuild are: project, wrapper, target, listfile,
# linkargsfile in the terminology of mlbuild in CM/main/cm-boot.sml. mlbuild expects its first
# two positional arguments to be CM description files, namely "project" and "wrapper".
# We use the generated BOOTLIST file in the $RUN command below, but the XXXX file (listargsfile) is not used.
$SML -DNO_ML_ULEX -DNO_ML_ANTLR @CMbuild "sources.cm" "export.cm" "ml-antlr" BOOTLIST XXXX

# Build the tool - step 2: call $RUN with the BOOTLIST produced by the previous command.
exec $RUN @SMLboot=BOOTLIST @SMLheap=sml
