# CMake configuration for Concurrent ML
#
# COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
# All rights reserved.
#

cml lib cml core-cml.cm     libraries/cml/src
cml lib cml cml-internal.cm libraries/cml/src
cml lib cml cml.cm          libraries/cml/src
cml lib cml basis.cm        libraries/cml/src
cml-lib  lib cml-lib      trace-cml.cm libraries/cml/cml-lib/cm-descr
cml-lib  lib cml-lib      smlnj-lib.cm libraries/cml/cml-lib/cm-descr
cml-lib  lib cml          trace-cml.cm libraries/cml/src
cml-lib  lib cml          smlnj-lib.cm libraries/cml/src
cml-lib  lib cml          inet-lib.cm  libraries/cml/src
cml-lib ulib cml          unix-lib.cm  libraries/cml/src
cml-lib  lib cml          cml-lib.cm   libraries/cml/src
# dependencies (from config/dependencies)
cml-lib cml
cml smlnj-lib
