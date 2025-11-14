# smlnj-actions.cmake
#
# COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
# All rights reserved.
#
# This file replaces the old "config/actions" file that specified the
# rules for building the various targets defined in the config/targets
# file.

# TODO: write CMake code to implement the various kinds of actions
# We need the following commands:
#
#       smlnj_add_library(<target>
#           ANCHOR <anchor.cm>
#           [ UNIX | WINDOWS ]
#           [ ALT_ANCHOR <anchor.cm> ]
#           [ DEPENDS <targets> ])
#       smlnj_add_prog(<target> DEPENDS <targets>)

# QUESTION: should we check that the smlnj-targets.cmake file has been
# processed first?

# QUESTION: should these be distributed across the CMakeLists.txt files in the
# source tree or can we split things into code to specify which targets to build
# in this directory and then info on how to build them in the CMakeLists.txt files?

# Backwards compatible views of the SML Basis Library
#
if (SMLNJ_LIB_OLD_BASIS)
  old-basis lib basis-2004.cm basis-2004.cm libraries/old-basis/2004
endif()

# SML/NJ Libraries; these are always included
#
smlnj-lib ulib unix-lib.cm      unix-lib.cm      smlnj-lib/Unix
smlnj-lib  lib hash-cons-lib.cm hash-cons-lib.cm smlnj-lib/HashCons
smlnj-lib  lib html-lib.cm      html-lib.cm      smlnj-lib/HTML
smlnj-lib  lib html4-lib.cm     html4-lib.cm     smlnj-lib/HTML4
smlnj-lib  lib inet-lib.cm      inet-lib.cm      smlnj-lib/INet
smlnj-lib  lib json-lib.cm      json-lib.cm      smlnj-lib/JSON
smlnj-lib  lib pp-extras-lib.cm pp-extras-lib.cm smlnj-lib/PP
smlnj-lib  lib regexp-lib.cm    regexp-lib.cm    smlnj-lib/RegExp
smlnj-lib  lib reactive-lib.cm  reactive-lib.cm  smlnj-lib/Reactive
smlnj-lib  lib sexp-lib.cm      sexp-lib.cm      smlnj-lib/SExp
smlnj-lib  lib uuid-lib.cm      uuid-lib.cm      smlnj-lib/UUID
smlnj-lib  lib xml-lib.cm       xml-lib.cm       smlnj-lib/XML


# new (unicode-capable) lexer generator:
#
if (SMLNJ_TOOL_ML_ULEX)
  ml-ulex    	        dprog   ml-ulex         -               tools/ml-lpt/ml-ulex
  ml-ulex               lib 	ml-ulex-tool.cm ml-ulex-tool.cm tools/ml-lpt/ml-ulex/tool
  ml-ulex-mllex-tool    lib 	mllex-tool.cm   mllex-tool.cm   tools/ml-lpt/ml-ulex/mllex-tool
  #ml-ulex-lex-ext      lib 	lex-ext.cm      lex-ext.cm      tools/ml-lpt/ml-ulex/tool
  ml-ulex-mllex-ext     lib 	lex-ext.cm      lex-ext.cm      tools/ml-lpt/ml-ulex/mllex-tool
  # dependencies (from config/dependencies)
  ml-ulex smlnj-lib ml-lpt-lib
endif()

# LALR(1) parser generator:
#
if (SMLNJ_TOOL_ML_YACC)
  ml-yacc         prog    ml-yacc         src             tools/ml-yacc
  ml-yacc         lib     mlyacc-tool.cm  mlyacc-tool.cm  tools/ml-yacc/tool
  ml-yacc-grm-ext lib     grm-ext.cm      grm-ext.cm      tools/ml-yacc/tool
  # dependencies (from config/dependencies)
  ml-yacc ml-ulex
endif()

# LL(k) parser generator:
#
if (SMLNJ_TOOL_ML_ANTLR)
  ml-antlr                dprog   ml-antlr         -                      tools/ml-lpt/ml-antlr
  ml-antlr                lib     ml-antlr-tool.cm ml-antlr-tool.cm       tools/ml-lpt/ml-antlr/tool
  ml-antlr-grm-ext        lib     grm-ext.cm       grm-ext.cm             tools/ml-lpt/ml-antlr/tool
  # dependencies (from config/dependencies)
  ml-antlr smlnj-lib ml-lpt-lib
endif()

# support library for ml-ulex and ml-antlr:
#
if (SMLNJ_LIB_ML_LPT)
  ml-lpt-lib 	   lib 	 ml-lpt-lib.cm ml-lpt-lib.cm tools/ml-lpt/lib
endif()

# ASDL generator
#
if (SMLNJ_TOOL_ASDLGEN)
  asdl    config  tools/asdl
  asdl    dprog   asdlgen         src/asdlgen     tools/asdl
  asdl    lib     asdlgen-tool.cm asdlgen-tool.cm tools/asdl/tool
  asdl    lib     asdl-ext.cm     asdl-ext.cm     tools/asdl/tool
  # dependencies (from config/dependencies)
  asdl smlnj-lib ml-lpt-lib
endif()

# ASDL support library
#
if (SMLNJ_LIB_ASDL)
  asdl    lib     asdl-lib.cm     asdl-lib.cm     tools/asdl/src/lib/sml
endif()

# portable dependency graph library:
#
if (SMLNJ_LIB_PGRAPH)
  pgraph-util lib pgraph-util.cm pgraph-util.cm libraries/pgraph
endif()

# tracing/debugging/profiling:
#
if (SMLNJ_LIB_TDP)
  tdp-util lib smlnj-tdp plugins.cm       libraries/trace-debug-profile
  tdp-util lib smlnj-tdp back-trace.cm    libraries/trace-debug-profile
  tdp-util lib smlnj-tdp coverage.cm      libraries/trace-debug-profile
endif()

# bottom-up rewrite code generator generator:
#
if (SMLNJ_TOOL_ML_BURG)
  ml-burg prog    ml-burg         -               tools/ml-burg
  ml-burg lib     mlburg-tool.cm  mlburg-tool.cm  tools/ml-burg/tool
  ml-burg lib     burg-ext.cm     burg-ext.cm     tools/ml-burg/tool
  # dependencies (from config/dependencies)
  ml-burg ml-yacc ml-ulex
endif()

# MLRISC libraries:
#
if (SMLNJ_LIB_MLRISC)
  mlrisc libanchor Control.cm SMLNJ-MLRISC
  mlrisc libanchor Lib.cm     SMLNJ-MLRISC
  mlrisc libanchor Visual.cm  SMLNJ-MLRISC
  mlrisc libanchor MLRISC.cm  SMLNJ-MLRISC
  mlrisc libanchor MLTREE.cm  SMLNJ-MLRISC
  mlrisc libanchor Graphs.cm  SMLNJ-MLRISC
  mlrisc libanchor IA32.cm    SMLNJ-MLRISC
  mlrisc libanchor AMD64.cm   SMLNJ-MLRISC
  mlrisc libanchor SPARC.cm   SMLNJ-MLRISC

  #mlrisc libanchor StagedAlloc.cm SMLNJ-MLRISC
  #mlrisc libanchor CCall.cm SMLNJ-MLRISC
  #mlrisc libanchor CCall-x86-64.cm SMLNJ-MLRISC
  #mlrisc libanchor CCall-x86.cm SMLNJ-MLRISC
  #mlrisc libanchor CCall-sparc.cm SMLNJ-MLRISC
  #mlrisc libanchor CCall-Vararg.cm SMLNJ-MLRISC
  #mlrisc libanchor CCall-VarargCall.cm SMLNJ-MLRISC
  #mlrisc libanchor CCall-VarargInterp.cm SMLNJ-MLRISC

  mlrisc anchor RA.cm libraries/MLRISC/cm
  #mlrisc anchor SPARC.cm libraries/MLRISC/cm
  mlrisc anchor Peephole.cm libraries/MLRISC/cm
  mlrisc anchor StagedAlloc.cm libraries/MLRISC/cm
  #mlrisc anchor IA32.cm libraries/MLRISC/cm
  mlrisc anchor AMD64.cm libraries/MLRISC/cm
  mlrisc anchor CCall.cm libraries/MLRISC/cm
  mlrisc anchor CCall-x86-64.cm libraries/MLRISC/cm
  mlrisc anchor CCall-x86.cm libraries/MLRISC/cm
  mlrisc anchor CCall-sparc.cm libraries/MLRISC/cm
  mlrisc anchor CCall-Vararg.cm libraries/MLRISC/cm
  mlrisc anchor CCall-VarargCall.cm libraries/MLRISC/cm
  mlrisc anchor CCall-VarargInterp.cmlibraries/ MLRISC/cm

  mlrisc lib OTHER-MLRISC RA.cm                   libraries/MLRISC/cm SMLNJ-MLRISC
  mlrisc lib OTHER-MLRISC Peephole.cm             libraries/MLRISC/cm SMLNJ-MLRISC
  #mlrisc lib OTHER-MLRISC IA32.cm                libraries/MLRISC/cm SMLNJ-MLRISC
  mlrisc lib OTHER-MLRISC IA32-Peephole.cm        libraries/MLRISC/cm SMLNJ-MLRISC
  mlrisc lib OTHER-MLRISC AMD64.cm                libraries/MLRISC/cm SMLNJ-MLRISC
  mlrisc lib OTHER-MLRISC AMD64-Peephole.cm       libraries/MLRISC/cm SMLNJ-MLRISC
  #mlrisc lib OTHER-MLRISC SPARC.cm               libraries/MLRISC/cm SMLNJ-MLRISC
  mlrisc lib OTHER-MLRISC StagedAlloc.cm          libraries/MLRISC/cm SMLNJ-MLRISC
  mlrisc lib OTHER-MLRISC CCall.cm                libraries/MLRISC/cm SMLNJ-MLRISC
  mlrisc lib OTHER-MLRISC CCall-x86-64.cm         libraries/MLRISC/cm SMLNJ-MLRISC
  mlrisc lib OTHER-MLRISC CCall-x86.cm            libraries/MLRISC/cm SMLNJ-MLRISC
  mlrisc lib OTHER-MLRISC CCall-sparc.cm          libraries/MLRISC/cm SMLNJ-MLRISC
  mlrisc lib OTHER-MLRISC CCall-Vararg.cm         libraries/MLRISC/cm SMLNJ-MLRISC
endif()

# C Kit:
#
if (SMLNJ_LIB_CKIT)
  ckit lib ckit-lib.cm ckit-lib.cm libraries/ckit/src
  # dependencies (from config/dependencies)
  ckit ml-yacc ml-ulex ml-ulex-mllex-ext
endif()

# NLFFI foreign function interface generator:
#
if (SMLNJ_TOOL_ML_NLFFIGEN)
  ml-nlffigen dprog ml-nlffigen - tools/nlffi/gen
  # dependencies (from config/dependencies)
  ml-nlffigen ckit smlnj-lib
endif()

# NLFFI foreign function interface library
#
if (SMLNJ_LIB_ML_NLFFI)
  ml-nlffi-lib lib c memory/memory.cm   tools/nlffi/lib
  ml-nlffi-lib lib c internals/c-int.cm tools/nlffi/lib
  ml-nlffi-lib lib c c.cm               tools/nlffi/lib
endif()

# Concurrent ML:
#
if (SMLNJ_ENABLE_CML)
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
endif()

#
if (SMLNJ_DOCUMENTATION)
endif()

#
if (SMLNJ_TOOL_CFGC)
endif()

# tool to print CFG files
#
if (SMLNJ_TOOL_PRINT_CFG)
  print-cfg dprog print-cfg       -               tools/print-cfg
  # dependencies (from config/dependencies)
  print-cfg smlnj-lib asdl
endif()
