# smlnj-targets.cmake_host_system_information
#
# COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
# All rights reserved.
#
# Options to enable components of the system (these replace the old "config/targets"
# file).
#

include(CMakeDependentOption)

# option(<variable> "<help_text>" [value])
# cmake_dependent_option(<variable> <help> <value> <condition> <else-value>)


option(
  SMLNJ_LIB_OLD_BASIS
  "include backward-compatible versions of the SML Basis Library"
  OFF)
option(
  SMLNJ_TOOL_ML_ULEX
  "build the ml-ulex lexer generator"
  ON)
option(
  SMLNJ_TOOL_ML_YACC
  "build the ml-yacc LALR(1) parser generator"
  ON)
option(
  SMLNJ_TOOL_ML_ANTLR
  "build the ml-antlr LL(k) parser generator"
  ON)
cmake_dependent_option(
  SMLNJ_LIB_ML_LPT
  "include the ml-lpt library even though the ml-lpt tools are disabled"
  OFF
  [[NOT (SMLNJ_TOOL_ML_ULEX OR SMLNJ_TOOL_ANTLR)]]
  ON)
cmake_dependent_option(
  SMLNJ_TOOL_ASDLGEN
  "build the asdlgen tool"
  ON
  [[NOT (CMAKE_SYSTEM_NAME STREQUAL "Windows")]]
  OFF)
cmake_dependent_option(
  SMLNJ_LIB_ASDL
  "include the ASDL library even though asdlgen is disabled"
  OFF
  [[NOT SMLNJ_TOOL_ASDLGEN]]
  ON)
option(
  SMLNJ_LIB_PGRAPH
  "include CM's portable-graph-utility library"
  OFF)
option(
  SMLNJ_LIB_TDP
  "include the Trace-Debug-Profile utility library"
  ON)
option(
  SMLNJ_TOOL_ML_BURG
  "build the ml-burg bottom-up tree-parser generator"
  ON)
option(
  SMLNJ_LIB_MLRISC
  "include the MLRisc code generation library"
  OFF)
option(
  SMLNJ_LIB_CKIT
  "include the CKit library"
  ON)
option(
  SMLNJ_TOOL_ML_NLFFIGEN
  "build the ml-nlffigen foreign function glue-code generator"
  OFF)
cmake_dependent_option(
  SMLNJ_LIB_ML_NLFFI
  "include the ml-nlffi library even though ml-nlffigen is disabled"
  OFF
  [[NOT SMLNJ_TOOL_ML_NLFFIGEN]]
  ON)
option(
  SMLNJ_ENABLE_CML
  "enable support for Concurrent ML"
  ON)
option(
  SMLNJ_DOCUMENTATION
  "generate the manpages and other documentation for the SML/NJ system"
  OFF)
option(
  SMLNJ_TOOL_CFGC
  "build the cfgc command-line tool"
  OFF)
mark_as_advanced(SMLNJ_TOOL_CFGC)
option(
  SMLNJ_TOOL_PRINT_CFG
  "build the print-cfg command-line tool"
  OFF)
mark_as_advanced(SMLNJ_TOOL_PRINT_CFG)

#request mlrisc-tools
#request nowhere

# override the values of certain options based on other choices to ensure
# consistency
#
if(NOT SMLNJ_LIB_CKIT AND SMLNJ_TOOL_ML_NLFFIGEN)
  message(STATUS "ml-nlffigen requires CKit")
  set(SMLNJ_LIB_CKIT ON)
endif()

# CM configuration options
#
cmake_dependent_option(
  SMLNJ_LEX_EXT_IS_ML_LEX
  "recognize '.lex' files as using the old ml-lex syntax (instead of ml-ulex syntax)"
  ON
  SMLNJ_TOOL_ML_ULEX
  ON)
mark_as_advanced(SMLNJ_LEX_EXT_IS_ML_LEX)
cmake_dependent_option(
  SMLNJ_GRM_EXT_IS_ML_ANTLR
  "recognize '.grm' files as ml-antlr files (instead of ml-yacc)"
  OFF
  SMLNJ_TOOL_ML_ANTLR
  OFF)
mark_as_advanced(SMLNJ_GRM_EXT_IS_ML_ANTLR)
