#!/bin/sh
#
# Script for adding a signature description file to an existing
# library documentation directory.
#
# usage: scripts/prepare-sig.sh <dir> <sign>
#

function usage {
  echo "usage: scripts/prepare-sig.sh <dir> <sign>"
  exit 1
}

if [ ! -x scripts/prepare-sig.sh ] ; then
  usage
fi

if [ $# -ne 2 ] ; then
  usage
fi

d=$1 ; shift
name=$1 ; shift

if [ ! -d "$d" ] ; then
  echo "$0: missing library directory $d"
  exit 1
fi

lib=$(basename $d/*-lib.adoc .adoc)

template=Templates/sig.adoc
stem="sig-$name"
mod_adoc="$d/$stem.adoc"

if [ -f $mod_adoc ] ; then
  echo "$0: file '$mod_adoc' already exists"
  exit 1
fi

echo "creating $mod_adoc"

## create the placeholder for the module
sed -e "s/@DIR@/$d/" -e "s/@LIBRARY@/$lib/" -e "s/@NAME@/$name/" $template > $mod_adoc

exit 0
