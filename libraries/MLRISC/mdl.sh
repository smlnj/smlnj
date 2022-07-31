#!/bin/sh
#
# COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
# All rights reserved.
#
# A script for running the MDL tool on a specification file.
#
# usage: mdl.sh <file>
#

if [ $# -ne 1 ] ; then
  echo "usage: mdl.sh <file>"
  exit 1
fi
TARGET=$1

src="$TARGET/$TARGET.mdl"

if [ ! -r "$src" ] ; then
  echo "mdl.sh: unknown target"
  exit 1
fi

sml <<XXXX
val _ = List.app (fn f => #set(CM.Anchor.anchor f) (SOME "cm")) [
	  "Control.cm", "Lib.cm", "Graphs.cm", "MLRISC.cm", "MLTREE.cm"
	];
CM.make "Tools/MDL/sources.cm";
MDLGen.gen "$src";
XXXX
