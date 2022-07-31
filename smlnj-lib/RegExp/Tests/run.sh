#!/bin/sh
#
# Run the RE tests
#

sml sources.cm <<XXXX
ThompsonRE.doTests();
DfaRE.doTests();
BackTrackRE.doTests();
XXXX

