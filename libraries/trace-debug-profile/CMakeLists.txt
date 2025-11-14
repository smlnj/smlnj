# CMake configuration for the trace-debug-profile LIBRARY
#
# COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
# All rights reserved.
#

tdp-util lib smlnj-tdp plugins.cm       libraries/trace-debug-profile
tdp-util lib smlnj-tdp back-trace.cm    libraries/trace-debug-profile
tdp-util lib smlnj-tdp coverage.cm      libraries/trace-debug-profile

smlnj_add_library(tdp-lib
    ANCHOR tdp-lib.cm
    DEPENDS ml-yacc ml-ulex ml-ulex-mllex-ext)

# what about installation?
