# CMake configuration for SML/NJ tools
#
# COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
# All rights reserved.
#

add_subdirectory(heap2asm)

add_subdirectory(ml-lpt)

if (SMLNJ_PROG_ML_YACC)
  add_subdirectory(ml-yacc)
endif()

add_subdirectory(asdl)

if (SMLNJ_PROG_ML_BURG)
  add_subdirectory(ml-burg)
endif()

add_subdirectory(nlffi)

if (SMLNJ_PROG_PRINT_CFG)
  add_subdirectory(print-cfg)
endif()
