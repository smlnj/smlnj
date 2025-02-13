#!/bin/sh
#
# Script to sign an installer package that was created by build-pkg.sh
# using the "-no-sign" flag.
#
# usage:
#	sign-pkg.sh <pkg>
#

CMD="sign-pkg.sh"
ROOT=$(pwd)

SIGNER="$USER"

usage() {
  echo "usage: sign-pkg.sh <pkg>"
  exit $1
}

if [ $# != "1" ] ; then
  usage
fi
PKG=$1

if [ ! -r $PKG ] ; then
  echo "$CMD: package '$PKG' does not exist"
  exit 1
fi

# you need a developer ID to sign the final package;
#
case x"$SIGNER" in
  xjhr) SIGN="Developer ID Installer: John Reppy (8A296SNBSN)" ;;
  *)
    echo "$CMD [Error]: unknown user, so package will not be signed!"
    exit 1
  ;;
esac

TMP_PKG="tmp$$.pkg"

echo "$CMD: signing package '$PKG'"
productsign --sign "$SIGN" "$PKG" ./$TMP_PKG
if [ "$?" = "0" ] ; then
  mv $TMP_PKG $PKG
fi
