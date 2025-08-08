#!/bin/sh
#
# Script to build the installer package for macOS
#
# usage:
#	build-pkg.sh [ -verbose ] [ -no-sign ] [ <version> ]
#

CMD="build-pkg.sh"
GITHUB_URL="git@github.com:smlnj/smlnj.git"
DISTROOT=smlnj
VERBOSE=""
SIGNER="$USER"
VERSION=none
CLEANUP=yes

usage() {
  echo "usage: build-pkg.sh [ options ] [ <version> ]"
  echo "  options:"
  echo "    -verbose   -- enable messages that document the packaging process"
  echo "    -no-sign   -- build an unsigned package"
  echo "    -no-clean  -- do not remove the distribution tree after packaging"
  exit $1
}

complain() {
    echo "$CMD [Error]: $@"
    exit 1
}

vsay() {
  if [ x"$VERBOSE" = x-verbose ] ; then
    echo "$CMD: $@"
  fi
}

# get the path to the script's directory and to the root of the source
# distribution
#
HERE=$(pwd)

# process command-line arguments
#
while [ "$#" != "0" ] ; do
  arg=$1; shift
  case $arg in
    -h) usage 0 ;;
    -no-clean) CLEANUP="no" ;;
    -no-sign) SIGNER="none" ;;
    -verbose) VERBOSE="-verbose" ;;
    -*) usage 1 ;;
    *) VERSION=$arg ; break ;;
  esac
done

if [ $# != 0 ] ; then
  usage 1
fi

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
rm -rf .gitignore .gitmodules .github package
rm -rf runtime/llvm*/.gitignore runtime/llvm18/.github

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

# is this a release candidate?
#
case "$VERSION" in
  *-rc*)
    VERSION_STEM=$(echo $VERSION | sed -e 's/\([^-]*\)-\(.*\)/\1/')
    RC=$(echo $VERSION | sed -e 's/\([^-]*\)-\(.*\)/\2/')
    ;;
  *)
    VERSION_STEM=$VERSION
    RC=""
    ;;
esac

# determine the architecture
#
case `uname -p` in
  i386) ARCH=amd64 ; PRODUCT_ARCH=x86_64 ;;
  arm) ARCH=arm64 ; PRODUCT_ARCH=arm64 ;;
  *)
    complain "unknown architecture"
  ;;
esac

if [ x"$RC" = x ] ; then
  echo "$CMD: building release $VERSION for $ARCH"
else
  echo "$CMD: building release-candidate $VERSION_STEM ($RC) for $ARCH"
fi

ID=org.smlnj.$ARCH.pkg
RSRC=Resources

# you need a developer ID to sign the final package;
#
case x"$SIGNER" in
  xjhr) SIGN="Developer ID Installer: John Reppy (8A296SNBSN)" ;;
  xnone) SIGN=none ;;
  *)
    echo "$CMD [Warning]: unknown user, so package will not be signed!"
    SIGN=none
  ;;
esac

# build the distribution (note that this assumes that config/targets is what we want!)
#
./build.sh -doc $VERBOSE
if [ "$?" != 0 ] ; then
  complain "problem building SML/NJ"
fi

#
# replace the document source tree with the generated documentation
#
mv doc/doc tmp-doc
rm -rf doc
if [ -d doc ] ; then
  complain "unable to remove documentation source"
fi
mv tmp-doc doc

# cleanup
#
rm *tgz

# back up to the root
#
cd $HERE

# configure the distribution file
#
sed \
  -e "s/@VERSION@/$VERSION/g" \
  -e "s/@PRODUCT_ARCH@/$PRODUCT_ARCH/g" \
  -e "s/@ARCH@/$ARCH/g" \
  components/distribution_xml.in \
    > distribution.xml

# TODO: here is probably where we should use codesign to enable the hardened runtime
# for the runtime executable.  Something like the following command:
#
#    codesign --force --options runtime --sign $SIGN bin/.run/run.amd64-darwin
#
# we may also need to add the --entitlements flag to enable things like executing
# the code that we generate

# create the resources directory and fill it
#
if [ -d $RSRC ] ; then
  rm -rf $RSRC
fi
mkdir $RSRC
cp -p components/smlnj-background.jpg $RSRC/background.jpg
sed -e "s/@VERSION@/$VERSION/g" -e "s/@ARCH@/$ARCH/g" \
  components/welcome_html.in \
    > $RSRC/welcome.html
cp -p components/license.html $RSRC/license.html
cp -p components/conclusion.html $RSRC/conclusion.html

# copy the readme, while adjusting the fontsize for the installer panel
# NOTE: this command relies on the fact that there is only one absolute
# font-size command in the README file (the others are relative)
#
if [ ! -f "$DISTROOT/doc/html/readme/$VERSION_STEM-README.html" ] ; then
  complain "cannot find $DISTROOT/doc/html/readme/$VERSION_STEM-README.html"
  exit 1
fi
sed -E 's/font-size: [0-9]+pt;/font-size: 9pt;/' \
  $DISTROOT/doc/html/readme/$VERSION_STEM-README.html > $RSRC/readme.html

# build package
#
PKG_OPTS="--identifier $ID --version $VERSION --scripts components/scripts/ \
  --install-location /usr/local/smlnj --root $DISTROOT"
vsay "$CMD: pkgbuild $PKG_OPTS smlnj.pkg"
pkgbuild $PKG_OPTS smlnj.pkg

# build distribution package
#
BUILD_OPTS="--package-path components --resources $RSRC \
  --distribution distribution.xml ./smlnj-$ARCH-$VERSION.pkg"
if [ x"$SIGN" = xnone ] ; then
  echo "$CMD: building unsigned package smlnj-$ARCH-$VERSION.pkg"
  vsay "$CMD: productbuild $BUILD_OPTS"
  productbuild $BUILD_OPTS
else
  echo "$CMD: building signed package smlnj-$ARCH-$VERSION.pkg"
  vsay "$CMD: productbuild --sign \"$SIGN\" $BUILD_OPTS"
  productbuild --sign "$SIGN" $BUILD_OPTS
fi

# cleanup
#
if [ x"$CLEANUP" = xyes ] ; then
  rm -rf $RSRC $DISTROOT smlnj.pkg distribution.xml
else
  rm -rf smlnj.pkg
fi