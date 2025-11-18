# CMake configuration for the ML LPT tools and library
#
# COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
# All rights reserved.
#

if (SMLNJ_LIB_ML_LPT)
  ml-lpt-lib 	   lib 	 ml-lpt-lib.cm ml-lpt-lib.cm tools/ml-lpt/lib
endif()

if (SMLNJ_TOOL_ML_ULEX)
  ml-ulex    	        dprog   ml-ulex         -               tools/ml-lpt/ml-ulex
  ml-ulex               lib 	ml-ulex-tool.cm ml-ulex-tool.cm tools/ml-lpt/ml-ulex/tool
  ml-ulex-mllex-tool    lib 	mllex-tool.cm   mllex-tool.cm   tools/ml-lpt/ml-ulex/mllex-tool
  #ml-ulex-lex-ext      lib 	lex-ext.cm      lex-ext.cm      tools/ml-lpt/ml-ulex/tool
  ml-ulex-mllex-ext     lib 	lex-ext.cm      lex-ext.cm      tools/ml-lpt/ml-ulex/mllex-tool
  # dependencies (from config/dependencies)
  ml-ulex smlnj-lib ml-lpt-lib
endif()

if (SMLNJ_TOOL_ML_ANTLR)
  ml-antlr                dprog   ml-antlr         -                      tools/ml-lpt/ml-antlr
  ml-antlr                lib     ml-antlr-tool.cm ml-antlr-tool.cm       tools/ml-lpt/ml-antlr/tool
  ml-antlr-grm-ext        lib     grm-ext.cm       grm-ext.cm             tools/ml-lpt/ml-antlr/tool
  # dependencies (from config/dependencies)
  ml-antlr smlnj-lib ml-lpt-lib
endif()
