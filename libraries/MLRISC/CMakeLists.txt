# CMake configuration for the MLRisc libraries
#
# COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
# All rights reserved.
#

# MLRISC libraries (those that are not already included in the compiler):
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
