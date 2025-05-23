#!/bin/sh
#
# COPYRIGHT (c) 2024 The Fellowship of SML/NJ (http://www.smlnj.org)
# All rights reserved.
#
# Build and installation script for SML/NJ System.
#
# usage: build.sh [ options ]
#
# TODO:
#    add support for fetching the boot files
#    dump output from build process to a log (instead of to the terminal)

cmd=$0
here=$(pwd)

complain() {
  echo "$cmd: !!! $@"
  exit 1
}

usage() {
  echo "usage: build.sh [ options ]"
  echo "options:"
  echo "    -h,-help      print this message and exit"
  echo "    -nolib        skip building libraries/tools"
  echo "    -verbose      emit feedback messages"
  echo "    -doc          generate documentation"
  echo "    -debug        debug installation (enables verbose mode)"
  echo "    -dev          developer install (includes cross compiler support)"
  echo "developer options:"
  echo "    -debug-llvm   build a debug version of the LLVM libraries"
  echo "    -llvmdir dir  specify the path to the LLVM directory"
  exit 1
}

# specifying the LLVM subdirectory (which is a submodule)
#
LLVM_DIRNAME=llvm10
LLVMDIR_OPTION=

# process options
NOLIB=no
QUIET=yes
INSTALL_DEBUG=no
INSTALL_DEV=no
MAKE_DOC=no
BUILD_LLVM_FLAGS=""
while [ "$#" != "0" ] ; do
  arg=$1; shift
  case $arg in
    -help|-h) usage ;;
    -nolib) NOLIB=yes ;;
    -verbose) QUIET=no ;;
    -debug) INSTALL_DEBUG=yes ; QUIET=no ;;
    -dev)
      INSTALL_DEV=yes;
      BUILD_LLVM_FLAGS="-all-targets $BUILD_LLVM_FLAGS"
    ;;
    -doc) MAKE_DOC=yes ;;
    -debug-llvm) BUILD_LLVM_FLAGS="-debug $BUILD_LLVM_FLAGS" ;;
    -llvmdir)
      if [[ $# -gt 0 ]] ; then
        LLVMDIR_OPTION=$1; shift
      else
        usage
      fi ;;
    *) usage ;;
  esac
done

# feedback messages for verbose mode
#
vsay() {
  if [ x${QUIET} = xno ] ; then
    echo "$@"
  fi
}

# feedback messages for debug mode
#
dsay() {
  if [ x${INSTALL_DEBUG} = xyes ] ; then
    echo "$@"
  fi
}

export CM_VERBOSE
if [ x${QUIET} = xyes ] ; then
  CM_VERBOSE=false
else
  CM_VERBOSE=true
fi

#
# create the preloads.standard file
#
if [ ! -r config/preloads ]; then
  complain "File config/preloads is missing."
fi
cp config/preloads preloads.standard

SHELL=/bin/sh
dsay "$cmd: Using shell $SHELL."

#
# set the SML root directory
#

cd $(dirname $cmd)
SMLNJ_ROOT=$(pwd)
vsay "$cmd: SML root is $SMLNJ_ROOT."

cd $here
cd "${INSTALLDIR:=$SMLNJ_ROOT}"
INSTALLDIR=`pwd`
cd "$SMLNJ_ROOT"
vsay "$cmd: Installation directory is ${INSTALLDIR}."

#
# set the various directory and file pathname variables
#
CONFIGDIR="$SMLNJ_ROOT/config"
RUNTIMEDIR="$SMLNJ_ROOT/runtime"
if [[ x"$LLVMDIR_OPTION" != x ]] ; then
  LLVMDIR="$LLVMDIR_OPTION"
  # check the validity of the path specified by the user
  if [[ ! -x "$LLVMDIR/build-llvm.sh" ]] ; then
    complain "invalid LLVM directory: build-llvm.sh script is missing"
  fi
else
  LLVMDIR="$RUNTIMEDIR/$LLVM_DIRNAME"
fi

#
# installation directories
#
BINDIR=$INSTALLDIR/bin		# main dir for binary stuff
HEAPDIR=$BINDIR/.heap		# where heap images live
RUNDIR=$BINDIR/.run		# where executables (i.e., the RTS) live
LIBDIR=$INSTALLDIR/lib		# where libraries live

export SMLNJ_ROOT INSTALLDIR CONFIGDIR BINDIR LLVMDIR

#
# old root environment variable (for compatibility)
#
ROOT=$SMLNJ_ROOT
export ROOT

#
# files to be deleted after we are done...
#
tmpfiles=""
tmpfiles="$tmpfiles preloads.standard"
#
# make sure we always clean up after ourselves...
#
trap 'cd "$SMLNJ_ROOT"; rm -f $tmpfiles' 0 1 2 3 15

#
# set the CM configuration variables (these are environment variables
# that will be queried by the bootstrap code)
# Especially important is CM_PATHCONFIG.
#
export CM_PATHCONFIG
CM_PATHCONFIG=$LIBDIR/pathconfig
#
# the release version that we are installing
#
VERSION=$(cat "$CONFIGDIR/version")
vsay "$cmd: Installing version $VERSION."

#
# the URL for the (usually remote) source archive
#
SRCARCHIVEURL=https://smlnj.cs.uchicago.edu/dist/working/${VERSION}/
vsay "$cmd: URL of source archive is $SRCARCHIVEURL."

######################################################################
## UTILITY SCRIPTS
######################################################################

#
# Function to install a "driver" script...
#   This takes care of patching the source of the script with the SHELL,
#   BINDIR, and VERSION variables to use.
#
installdriver() {
  vsay "$cmd: installing $BINDIR/$2"
  dsrc=$1
  ddst=$2
  rm -f "$BINDIR"/"$ddst"
  cat "$CONFIGDIR"/"$dsrc" | \
  sed -e "s,@SHELL@,$SHELL,g" \
      -e "s,@BINDIR@,$BINDIR," \
      -e "s,@LIBDIR@,$LIBDIR," \
      -e "s,@VERSION@,$VERSION," \
      -e "s,@CMDIRARC@,${CM_DIR_ARC:-dummy},"\
    > "$BINDIR"/"$ddst"
  chmod 555 "$BINDIR"/"$ddst"
  if [ ! -x "$BINDIR"/"$ddst" ]; then
    complain "Installation of $BINDIR/${ddst} failed."
  fi
}

#
# Fish out the CM metadata directory name from library files
# and store it in ORIG_CM_DIR_ARC.
# The single argument is the name of the directory containing
# a single subdirectory which is a CM metadata directory:
#
fish() {
  cd "$1"
  ORIG_CM_DIR_ARC=unknown
  for i in * .[a-zA-Z0-9]* ; do
    if [ -d $i ] ; then
      ORIG_CM_DIR_ARC=$i
      break
    fi
  done
  if [ $ORIG_CM_DIR_ARC = unknown ] ; then
    complain "Could not determine CM metadata directory name"
  else
    vsay "$cmd: CM metadata directory name is \"${ORIG_CM_DIR_ARC}\""
  fi
}


# A function to move all stable library files to a parallel directory
# hierarchy.
# The first argument must be a simple path (no / inside), and
# the second argument must be an absolute path.
move() {
  if [ -L "$1" ] ; then
    rm -f "$1"	     # remove symbolic link made by diracs (see below)
  elif [ -d "$1" ] ; then
    if [ ! -d "$2" ] ; then
      if [ -f "$2" ] ; then
        complain "$2 exists as a non-directory."
      fi
      mkdir "$2"
    fi
    cd "$1"
    for i in * .[a-zA-Z0-9]* ; do
      move "$i" "$2"/"$i"
    done
    cd ..
  elif [ -f "$1" ] ; then
    rm -f "$2"
    mv "$1" "$2"
  fi
}

#
# Traverse the directory tree rooted at $3 (must be single arc!).
# Find all directories named $1, rename them into $2 and make
# and establish $1 as a symbolic link to $2:
#
dirarcs() {
  if [ -d "$3" ] ; then
    if [ "$3" = "$1" ] ; then
      mv "$1" "$2"
      ln -s "$2" "$1"
    else
      cd "$3"
      for d in * .[a-zA-Z0-9]* ; do
        dirarcs "$1" "$2" "$d"
      done
      cd ..
    fi
  fi
}

######################################################################

#
# create the various sub directories
#
for dir in "$BINDIR" "$HEAPDIR" "$RUNDIR" "$LIBDIR" ; do
  if [ x"$QUIET" = xyes ] ; then
    mkdir -p "$dir" || exit 1
  else
    mkdir -p -v "$dir" || exit 1
  fi
done

#
# install the script that tests architecture and os...
#
installdriver _arch-n-opsys .arch-n-opsys

#
# run it to figure out what architecture and os we are using, define
# corresponding variables...
#
ARCH_N_OPSYS=`"$BINDIR"/.arch-n-opsys`
if [ "$?" != "0" ]; then
  complain "$BINDIR/.arch-n-opsys fails on this machine; please patch by hand and repeat the installation."
  exit 2
else
  vsay "$cmd: Script $BINDIR/.arch-n-opsys reports $ARCH_N_OPSYS."
fi
eval $ARCH_N_OPSYS

#
# now install most of the other driver scripts
#  (except ml-build, since we don't know $CM_DIR_ARC yet)
#
installdriver _run-sml .run-sml
installdriver _link-sml .link-sml
installdriver _ml-makedepend ml-makedepend
## TODO: install-sml-wrapper script

#
# we optimistically install heap2exec, but will remove it if heap2asm
# is not installed
#
installdriver _heap2exec heap2exec

#
# set allocation size; for the x86, this gets reset in .run-sml
#
ALLOC=1M

# OS-specific things for building the runtime system
#
RT_MAKEFILE=mk.$ARCH-$OPSYS
case $OPSYS in
  darwin)
    EXTRA_DEFS="AS_ACCEPTS_SDK=yes"
    ;;
  linux)
    EXTRA_DEFS=$("$CONFIGDIR/chk-global-names.sh")
    if [ "$?" != "0" ]; then
      complain "Problems checking for underscores in asm names."
    fi
    EXTRA_DEFS="XDEFS=$EXTRA_DEFS"
    ;;
esac

#
# the name of the bin files directory
#
# FIXME: should make these file names more consistent!!
#
BOOT_ARCHIVE=boot.$ARCH-unix.tgz
BOOT_FILES=sml.boot.$ARCH-unix

#
# build the run-time system
#
if [ -x "$RUNDIR"/run.$ARCH-$OPSYS ]; then
  vsay $cmd: Run-time system already exists.
else
  #
  # first we configure and build the LLVM and CFGCodeGen libraries.  Note that
  # if the "-dev" option was given, then we rebuild LLVM even if it is already
  # built, since we want to assure that the cross compiler is supported.
  #
  BUILD_LLVM_FLAGS="-install $RUNTIMEDIR $BUILD_LLVM_FLAGS"
  if [ x"$INSTALL_DEV" = xyes ] ; then
    vsay $cmd: Building LLVM for all targets in $LLVMDIR
    cd "$LLVMDIR"
    dsay ./build-llvm.sh $BUILD_LLVM_FLAGS
    ./build-llvm.sh $BUILD_LLVM_FLAGS || complain "Unable to build LLVM"
  elif [ ! -x "$RUNTIMEDIR/bin/llvm-config" ] ; then
    vsay $cmd: Building LLVM in $LLVMDIR
    cd "$LLVMDIR"
    dsay ./build-llvm.sh $BUILD_LLVM_FLAGS
    ./build-llvm.sh $BUILD_LLVM_FLAGS || complain "Unable to build LLVM"
  fi
  cd "$RUNTIMEDIR/objs"
  vsay $cmd: Compiling the run-time system.
  make -f $RT_MAKEFILE $EXTRA_DEFS
  if [ -x run.$ARCH-$OPSYS ]; then
    mv run.$ARCH-$OPSYS "$RUNDIR"
    if [ -f runx.$ARCH-$OPSYS ]; then
      mv runx.$ARCH-$OPSYS "$RUNDIR"
    fi
    if [ -f run.$ARCH-$OPSYS.so ]; then
      mv run.$ARCH-$OPSYS.so "$RUNDIR"
    fi
    if [ -f run.$ARCH-$OPSYS.a ]; then
      mv run.$ARCH-$OPSYS.a "$RUNDIR"
    fi
    make MAKE=make clean
  else
    complain "Run-time system build failed for some reason."
  fi
fi
cd "$SMLNJ_ROOT"

#
# boot the base SML system
#
if [ -r "$HEAPDIR"/sml.$HEAP_SUFFIX ]; then
  vsay "$cmd: Heap image $HEAPDIR/sml.$HEAP_SUFFIX already exists."
  fish "$LIBDIR"/smlnj/basis
  # ignore requested arc name since we have to live with what is there:
  export CM_DIR_ARC
  CM_DIR_ARC=$ORIG_CM_DIR_ARC
  # now re-dump the heap image:
  vsay "$cmd: Re-creating a (customized) heap image..."
  "$BINDIR"/sml @CMredump "$SMLNJ_ROOT"/sml
  cd "$SMLNJ_ROOT"
  if [ -r sml.$HEAP_SUFFIX ]; then
    mv sml.$HEAP_SUFFIX "$HEAPDIR"
  else
    complain "Unable to re-create heap image (sml.$HEAP_SUFFIX)."
  fi
else
  cd "$SMLNJ_ROOT"

  if [ ! -d "$BOOT_FILES"/smlnj/basis ] ; then
    if [ -f "$BOOT_ARCHIVE" ] ; then
      vsay "$cmd: unpacking the boot files"
      tar -xzf "$BOOT_ARCHIVE"
    else
      complain "Missing $BOOT_ARCHIVE"
    fi
  else
    vsay "$cmd: boot files are already unpacked"
  fi

  fish "$SMLNJ_ROOT"/"$BOOT_FILES"/smlnj/basis

  # Target arc:
  export CM_DIR_ARC
  CM_DIR_ARC=${CM_DIR_ARC:-".cm"}

  if [ $CM_DIR_ARC != $ORIG_CM_DIR_ARC ] ; then
    # now we have to make a symbolic link for each occurrence of
    # $ORIG_CM_DIR_ARC to $CM_DIR_ARC
    dirarcs "$ORIG_CM_DIR_ARC" "$CM_DIR_ARC" "$BOOT_FILES"
  fi

  cd "$SMLNJ_ROOT"/"$BOOT_FILES"

  # now link (boot) the system and let it initialize itself...
  dsay "$BINDIR"/.link-sml @SMLheap="$SMLNJ_ROOT"/sml @SMLboot=BOOTLIST @SMLalloc=$ALLOC
  if "$BINDIR"/.link-sml @SMLheap="$SMLNJ_ROOT"/sml @SMLboot=BOOTLIST @SMLalloc=$ALLOC ; then
    cd "$SMLNJ_ROOT"
    if [ -r sml.$HEAP_SUFFIX ]; then
      mv sml.$HEAP_SUFFIX "$HEAPDIR"
      cd "$BINDIR"
      ln -s .run-sml sml
      #
      # Now move all stable libraries to $LIBDIR and generate
      # the pathconfig file.
      #
      cd "$SMLNJ_ROOT"/"$BOOT_FILES"
      for anchor in * ; do
        if [ -d $anchor ] ; then
          dsay "move $anchor to $LIBDIR"
          echo $anchor $anchor >> $CM_PATHCONFIG
          move $anchor "$LIBDIR"/$anchor
        fi
      done
      cd "$SMLNJ_ROOT"
      # $BOOT_FILES is now only an empty skeleton, let's get rid of it.
      rm -rf "$BOOT_FILES"
    else
      complain "No heap image generated (sml.$HEAP_SUFFIX)."
    fi
  else
    complain "Boot code failed, no heap image (sml.$HEAP_SUFFIX)."
  fi
fi

#
# now that we know CM_DIR_ARC we can install the ml-build driver...
#
installdriver _ml-build ml-build

#
# Now do all the rest using the precompiled installer
# (see system/smlnj/installer for details)
#
if [ x"$NOLIB" = xno ] ; then
  vsay "$cmd: Installing other libraries and programs:"
  CM_TOLERATE_TOOL_FAILURES=true
  export CM_TOLERATE_TOOL_FAILURES
  if "$BINDIR"/sml -m \$smlnj/installer.cm ; then
    # because we create heap2exec without knowing if heap2asm is going
    # to be installed, we need this hack to remove heap2exec when heap2asm
    # is not available
    if [ ! -x "$BINDIR"/heap2asm ] ; then
      rm -f "$BINDIR"/heap2exec
    fi
    vsay $cmd: Installation complete.
  else
    complain "Installation of libraries and programs failed."
  fi
fi

#
# generate the documentation and manual pages (if requested)
#
if [ x"$MAKE_DOC" = xyes ] ; then
  vsay "$cmd: Generating documentation."
  #
  # first we clear CM related shell variables so that the documentation tool
  # builds are not confused.
  #
  unset CM_PATHCONFIG CM_DIR_ARC CM_TOLERATE_TOOL_FAILURES
  export SMLNJ_HOME
  SMLNJ_HOME=$here      # gives access to the version of SML/NJ that we are building
  cd doc
  if autoconf -Iconfig ; then
    :
  else
    complain "Error configuring documentation."
  fi

  ./configure

  if make doc && make distclean ; then
    vsay $cmd: Documentation generation complete.
  else
    complain "Error generating documentation."
  fi
fi

exit 0
