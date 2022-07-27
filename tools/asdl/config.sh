#!/bin/sh
#
# Copyright (c) 2018 The Fellowship of SML/NJ (https://smlnj.org)
#
# wrapper script for configuring ASDL when using the SML/NJ installer
#
# usage:
#	config.sh [-32 | -64] <install-dir>
#
# where <install-dir> is the root of the SML/NJ installation.
#

if [ x"$1" = x-32 ] ; then
  SIZE_OPT="--enable-32-bit"
  shift
elif [ x"$1" = x-64 ] ; then
  SIZE_OPT="--enable-64-bit"
  shift
fi

if [ x"$1" = x ] ; then
  if [ x"$INSTALLDIR" = x ] ; then
    echo "config.sh: either specify the install directory as an argument"
    echo "           or define INSTALLDIR in the environment"
    echo "usage: config.sh [ -32 | -64 ] [ <install-dir> ]"
    exit 1
  fi
else
  INSTALLDIR=$1
fi

# make sure that the configure script is present
#
if [ ! ./configure -nt ./configure.ac ] ; then
  autoheader -Iconfig
  autoconf -Iconfig
  rm -rf autom4te.cache
fi

SMLNJ_CMD=$INSTALLDIR/bin/sml
export SMLNJ_CMD

./configure $SIZE_OPT --prefix=$INSTALLDIR
