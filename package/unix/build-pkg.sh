#!/bin/sh
#
# Script to build a single tarball for Unix/Linux systems on x86-64
#
# usage:
#	build-pkg.sh <version>
#

CMD="build-pkg.sh"
GITHUB_URL="git@github.com:smlnj/smlnj.git"
DISTROOT=smlnj
ARCH=""
VERSION=none
VERBOSE=""
CLEANUP=no

ROOT=$(pwd)

cleanup () {
  if [ x"$CLEANUP" = xyes ] ; then
    cd $ROOT
    rm -rf $DISTROOT
  fi
}

usage() {
  echo "usage: build-pkg.sh [ options ] [ <version> ]"
  echo "  options:"
  echo "    -h            -- print this message"
  echo "    -verbose      -- enable messages that document the packaging process"
  echo "    -arch <arch>  -- specify architecture for boot files"
  exit $1
}

complain() {
  echo "$CMD [Error]: $@"
  cleanup
  exit 1
}

vsay() {
  if [ x"$VERBOSE" = x-verbose ] ; then
    echo "$CMD: $@"
  fi
}

make_tarball() {
  if [ x"$OPSYS" = xdarwin ] ; then
     TARFLAGS="--no-mac-metadata $TARFLAGS"
  fi
  tar $TARFLAGS -czf "$1" "$2" || exit 1
}

# process command-line arguments
#
while [ "$#" != "0" ] ; do
  arg=$1; shift
  case $arg in
    -h) usage 0 ;;
    -verbose) VERBOSE="-verbose" ;;
    -arch)
      if [ "$#" = "0" ] ; then
        usage 1
      fi
      case $1 in
        arm|arm64|aarch64) ARCH="arm64" ;;
        i386|x86-64|x86_64|amd64) ARCH="amd64" ;;
        *) complain "unknown architecture '$1'" ;;
      esac
      shift
      ;;
    -*) usage 1 ;;
    *) VERSION=$arg ; break ;;
  esac
done

if [ $# != 0 ] ; then
  usage 1
fi

# determine the architecture (if necessary)
#
if [ x"$ARCH" = xnone ] ; then
  case `uname -p` in
    i386) ARCH=amd64 ;;
    arm) ARCH=arm64 ;;
    *)
      complain "unknown architecture"
    ;;
  esac
fi
BOOT_ARCHIVE=boot.$ARCH-unix

# first we need to download the source from GitHub
#
if [ -d $DISTROOT ] ; then
  complain "please remove $DISTROOT first"
fi

# clone the sources
if [ x"$VERSION" != xnone ] ; then
  BRANCH="--branch v$VERSION"
fi
vsay "git clone --depth 1 --recurse-submodules $BRANCH $GITHUB_URL"
git clone --depth 1 --recurse-submodules $BRANCH $GITHUB_URL
if [ "$?" != 0 ] ; then
  complain "unable to download source from GitHub"
fi

# switch to the cloned source directory
#
cd $DISTROOT

# remove stuff that we do not need
#
rm -rf .git .gitignore .gitmodules .github package
rm -rf runtime/llvm*/.git runtime/llvm*/.gitignore runtime/llvm*/.github

# get the version from the source code
#
if [ ! -r config/version ] ; then
  complain "config/version is missing"
fi
CONFIG_VERSION=$(cat config/version)

if [ x"$VERSION" = xnone ] ; then
  # get version number from source
  VERSION=$CONFIG_VERSION
elif [ x"$VERSION" != x"$CONFIG_VERSION" ] ; then
  echo "$CMD [Error]: version in config/version is $CONFIG_VERSION"
  cd $HERE
  rm -rf $DISTROOT
  exit 1
fi

# get the bootfiles
#
config/unpack . "$BOOT_ARCHIVE"

# package up the source tree as a compressed tar file
#
cd $ROOT
make_tarball smlnj-$ARCH-unix-$VERSION.tgz smlnj

cleanup
