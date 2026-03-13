#!/bin/sh
#
# COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
# All rights reserved.
#
# usage: fetch.sh <rootdir> <file>
#
# NOTE: this script was extracted from the old "unpack" script and deals with
# the fetching of files.

this="$0"
ROOT="$1"
shift

LIBS="$ROOT"/libraries
TOOLS="$ROOT"/tools

CONFIGDIR="$ROOT/config"

VERSION=`cat "$CONFIGDIR"/version`
. "$CONFIGDIR"/srcarchiveurl

SUFFIXES="tgz tar.gz tar.Z tz tar tar.bz2"

vsay() {
    if [ x${INSTALL_DEBUG} = xtrue ] ; then
	echo "$@"
    elif [ x${INSTALL_QUIETLY} = xtrue ] ; then
	:
    else
	echo "$@"
    fi
}

#
# Function for asking user to fetch source archive.
#   $1 - descriptive name
#   $2 - base name without extension, without version, and without dir
#   $3 - remote directory
#
askurl() {
    echo "$this: Please, fetch $1 archive"
    echo ' ('$2.'*' or $VERSION-$2.'*)'
    echo " from $3"
    echo " and then re-run this script!"
    exit 1
}

#
# Function for fetching source archives automatically using wget or lynx.
#   $1 - command to actually get the stuff
#   $2 - descriptive name
#   $3 - base name without extension and without dir
#   $4 - remote directory
#
fetchurl() {
    getter=$1 ; shift
    vsay $this: Fetching $1 from $3. Please stand by...
    fetched=no
    for base in "$2" "$VERSION-$2" ; do
	for ext in $SUFFIXES ; do
	    try=$base.$ext
	    vsay $this: Trying $try ...
	    if "$getter" "$3"/"$try" "$ROOT"/"$try" ; then
		fetched=yes
		vsay $this: Fetching $try was a success.
		break 2		# get out of both for-loops
	    else
		rm -f "$ROOT"/"$try"
	    fi
	done
    done
    if [ $fetched = no ] ; then
	echo $this: Fetching $try was no success.
	echo '  ' You should try to do it manually now.
	askurl "$1" "$2" "$3"
    fi
}

# wrapper for wget
usewget() {
    wget -nv -O "$2" "$1"
}

# wrapper for lynx
uselynx() {
    lynx -source "$1" >"$2"
}

# wrapper for curl
usecurl() {
    curl -s --fail "$1" >"$2"
}

testurlgetter() {
    (exec >/dev/null 2>&1 ; exec $*)
}

#
# Function to check whether wget or lynx is available.
# Set URLGETTER accordingly.  URLGETTER can be set externally
# to either 'wget' or 'curl' or 'lynx' -- in which case the
# corresponding command will be used (properly wrapped).  Any
# other external setting will be passed directly to fetchurl (without
# wrapping -- meaning it must take precisely two argumets: source and
# destination, in that order).
#
urlgetter() {
    case ${URLGETTER:-unknown} in
	fetchurl*)
	    ;;
	unknown)
	    # automatically figure out which wrapper to use
	    if testurlgetter wget --help ; then
		URLGETTER="fetchurl usewget"
	    elif testurlgetter curl -s file:///dev/null -o /dev/null ; then
		URLGETTER="fetchurl usecurl"
	    elif testurlgetter lynx -help ; then
		URLGETTER="fetchurl uselynx"
	    else
		URLGETTER="askurl"
	    fi
	    ;;
	wget|curl|lynx)
	    # special getters we know how to wrap
	    URLGETTER="fetchurl use${URLGETTER}"
	    ;;
	*)
	    # other -- must be able to work without wrapper
	    URLGETTER="fetchurl ${URLGETTER}"
	    ;;
    esac
}

base="$1"

# does the file already exist?
#
for ext in $SUFFIXES ; do
  if [ -r "$base.$ext" ] ; then
    vsay "$base.$ext is already available; skipping download"
    exit 0
  fi
done

# download the file
#
urlgetter
$URLGETTER "$base" "$base" "$SRCARCHIVEURL"
