NCG is a renamed version of NewCodeGen, which has since been renamed to CodeGen in the main branch.
The renaming to NCG was to prevent modifying the directory when merging main into newpp.

NCG, unlike CodeGen[main] has a codegen.cm and a MAP-CodeGen.txt file.

NCG/cps-to-cfg/cluster.sml is a new, incomplete replacement for
CPS/main/cluster.sml.  When it is complete and tested, it should be
moved to CPS/main.  It is therefore not to be exported from
NCP/codegen.cm.



