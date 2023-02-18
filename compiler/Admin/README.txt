Admin/README.txt

This is a proposed new Admin directory, which will be at the bottom of the compiler
library heirarchy (or dependency graph). The idea is that these facilities are not
properly part of the compiler itself, but provide "administrative" support of various
kinds that are used in the compiler.  So these files should not depend on things that
are part of the compiler "itself", if you see what I mean. admin.cm should therefore
not need to import anything other than basis.cm (for TextIO used in PrintControl).

Subdirectories or Admin are:

compiler/
  -- moved from Basics/compiler, but need to find a new home for compileexn.sml

control/
  -- gathers the control structures/files from the other component directories
     and simplifies by using plain ref flags (containing bools, ints, strings, etc.),
     so they don't use the smlnj-lib/Controls library.  Naming of the control
     structures and their signatures has been made consistent.
     [There is one value, FLINTControl.recover, that refers to LambdaVar and
     needs to be moved elsewhere (e.g. FLINT/main/flintcomp.sml).]

errormsg/
  -- moved with minimal necessary changes from Basics

source/
  -- moved with mininal necessary changes from Basics

stats/
  -- moved with minimal necessary changes from Basics


CM description file: Admin/admin.cm
