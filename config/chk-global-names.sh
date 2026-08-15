#!/bin/sh
#
# COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
# All rights reserved.
#
# Check to see if "_" is prepended to global names in the symbol table.
#

set -eu

CC=${CC:-cc}

TMP_DIR=$(mktemp -d)

trap 'rm -rf "$TMP_DIR"' EXIT

TMP_FILE=$TMP_DIR/smlConfig
TMP_FILE_C=$TMP_FILE.c

WITNESS="w3E_4Ew3E_4Rrr_56TtT"

cat > "$TMP_FILE_C" <<XXXX
void $WITNESS () {}
XXXX

$CC -c -o "$TMP_FILE" "$TMP_FILE_C"

if nm "$TMP_FILE" | grep -q "_$WITNESS"; then
    echo "-DGLOBALS_HAVE_UNDERSCORE"
fi

exit 0
