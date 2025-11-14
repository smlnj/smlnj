# CMake configuration for CKit
#
# COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
# All rights reserved.
#

smlnj_add_library(ckit-lib
    ANCHOR ckit-lib.cm
    DEPENDS ml-yacc ml-ulex ml-ulex-mllex-ext)

# what about installation?
