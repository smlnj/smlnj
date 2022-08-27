#!/bin/sh
#
# Script to build the installer package for macOS
#
# usage:
#	build-pkg.sh [ -verbose ] [ <version> ]
#

CMD="build-pkg.sh"
GITHUB_URL="git@github.com:smlnj/smlnj.git"
DISTROOT=smlnj

# get the path to the script's directory and to the root of the source
# distribution
#
HERE=$(pwd)

if [ x"$1" = x-verbose ] ; then
  VERBOSE="-verbose"
  shift
else
  VERBOSE=""
fi

# get the version number
#
if [ x"$1" = x ] ; then
  # get version from sources
  VERSION=none
else
  VERSION=$1
  shift
fi

if [ $# != 0 ] ; then
  echo "usage: $CMD [ -verbose ] [ <version> ]"
  exit 1
fi

# you need a developer ID to sign the final package;
#
case x"$USER" in
  xjhr) SIGN="Developer ID Installer: John Reppy" ;;
  *)
    echo "$CMD [Warning]: unknown user, so package will not be signed!"
    SIGN=none
  ;;
esac

# first we need to download the source from GitHub
#
if [ -d $DISTROOT ] ; then
  echo "$CMD [Error]: please remove $DISTROOT first"
  exit 1
fi
git clone $GITHUB_URL
if [ "$?" != 0 ] ; then
  echo "$CMD [Error]: unable to download source from GitHub"
  exit 1
fi

# switch to the cloned source directory
#
cd $DISTROOT

# remove stuff that we do not need
#
rm -rf .gitignore .github package

# get the version from the source code
#
if [ ! -r config/version ] ; then
  echo "$CMD [Error]: config/version is missing"
  exit 1
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

# determine the architecture
#
case `uname -p` in
  i386) ARCH=amd64 ;;
  arm) ARCH=arm ;;
  *)
    echo "$CMD [Error]: unknown architecture"
    exit 1
  ;;
esac

echo "$CMD: building version $VERSION for $ARCH"

BOOTURL=http://smlnj.org/dist/working/$VERSION/boot.$ARCH-unix.tgz
ID=org.smlnj.$ARCH.pkg
RSRC=Resources

# get the boot files
#
curl -s -S -O $BOOTURL

# build the distribution (note that this assumes that config/targets is what we want!)
#
./build.sh -doc $VERBOSE
if [ "$?" != 0 ] ; then
  echo "$CMD [Error]: problem building SML/NJ"
  exit 1
fi

#
# replace the document source tree with the generated documentation
#
mv doc/doc tmp-doc
rm -rf doc
if [ -d doc ] ; then
  echo "$CMD [Error]: unable to remove documentation source"
  exit 1
fi
mv tmp-doc doc

# cleanup
#
rm *tgz

# back up to the root
#
cd $HERE

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
sed -e "s/VERSION/$VERSION/g" components/distribution_xml.in > $RSRC/distribution.xml
cp -p components/smlnj-background.jpg $RSRC/background.jpg
sed -e "s/VERSION/$VERSION/g" components/welcome_html.in > $RSRC/welcome.html
cp -p components/license.html $RSRC/license.html
cp -p components/conclusion.html $RSRC/conclusion.html

# copy the readme, while adjusting the fontsize for the installer panel
# NOTE: this command relies on the fact that there is only one absolute
# font-size command in the README file (the others are relative)
#
sed -E 's/font-size: [0-9]+pt;/font-size: 9pt;/' \
  $DISTROOT/doc/html/readme/$VERSION-README.html > $RSRC/readme.html

# build package
#
PKG_OPTS="--identifier $ID --version $VERSION --scripts components/scripts/ \
  --install-location /usr/local/smlnj --root $DISTROOT"
pkgbuild $PKG_OPTS smlnj.pkg

# build distribution package
#
BUILD_OPTS="--package-path components --resources $RSRC \
  --distribution $RSRC/distribution.xml ./smlnj-$ARCH-$VERSION.pkg"
if [ x"$SIGN" = xnone ] ; then
  echo "$CMD: building unsigned package smlnj-$ARCH-$VERSION.pkg"
  productbuild $BUILD_OPTS
else
  echo "$CMD: building signed package smlnj-$ARCH-$VERSION.pkg"
  productbuild --sign "$SIGN" $BUILD_OPTS
fi

# cleanup
#
rm -rf $RSRC $DISTROOT smlnj.pkg
