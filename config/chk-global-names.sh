#!/bin/sh
#
# COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
# All rights reserved.
#
# Check to see if "_" is prepended to global names in the symbol table.
#

CC=${CC:-cc}

TMP_FILE=/tmp/smlConfig-$$
TMP_FILE_C=$TMP_FILE.c

WITNESS="w3E_4Ew3E_4Rrr_56TtT"

cat > $TMP_FILE_C <<XXXX
void $WITNESS () {}
XXXX

$CC -c -o $TMP_FILE $TMP_FILE_C
if [ "$?" != "0" ]; then
    rm -f $TMP_FILE $TMP_FILE_C
    exit 1
fi

if `nm $TMP_FILE | grep -q "_$WITNESS"`
  then echo "-DGLOBALS_HAVE_UNDERSCORE"
fi

rm -f $TMP_FILE $TMP_FILE_C

exit 0
