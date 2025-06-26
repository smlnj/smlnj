#!/bin/sh
#
# COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
# All rights reserved.
#
# A script to generate the pickler/unpickler code for both the compiler
# and C++ code generator.
#
# usage: gen.sh
#

cmd=gen.sh
src=cfg.asdl

if [ ! -f $src ] ; then
  echo "$cmd: cfg.asdl is missing; are you in the right directory?"
  exit 1
fi
HERE=$(pwd)

if [ ! -x ../../../bin/asdlgen ] ; then
  echo "$cmd: cannot find asdlgen tool"
  exit 1
fi

cd ../../../bin
BINDIR=$(pwd)
cd "$HERE"

ASDL="$BINDIR/asdlgen"

echo "$cmd: generating SML code"
$ASDL sml $src || exit 1

echo "$cmd: generating C++ code"
$ASDL c++ $src || exit 1

exit 0
