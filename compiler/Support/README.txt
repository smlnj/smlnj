Support/README.txt

This is a proposed new compiler/Support directory, which should be at the bottom of the
compiler library dependency heirarchy. The idea is that these facilities are not properly
part of the compiler itself, but provide "administrative" support of various kinds that
are used in the compiler.  So these files should not depend on things that are part of the
compiler "itself", if you see what I mean. admin.cm should therefore not need to import
anything other than basis.cm (for TextIO used in PrintControl).

Support and a new Basic (no s) directory replace Basics, ElabData/basics, and Library.

Subdirectories or Support are:

config/
  -- Moved from Basics/compiler, but need to find a new home for compileexn.sml
     This contains the target signature and target32.sml and target64.sml files, as
     well as files for endianess configuration.

control/
  -- gathers the control structures/files from the other component directories
     and simplifies them by using plain ref flags (containing bools, ints, strings, etc.),
     so they no longer depend on the smlnj-lib/Controls library.  Naming of the control
     structures and their signatures has been made consistent.
     [There is one element, FLINTControl.recover, that refers to LambdaVar and
     needs to be reconsidered and possibly moved elsewhere (e.g. FLINT/main/flintcomp.sml).]

     An alternate approach is to define component-specific Control structures in component
     directories (e.g. ElabData/control/elabdatacontrol.sml) and have a "high-level"
     Control component that uses these to construct the overall Control structure that is
     now defined in TopLevel/main/control.{sig,sml}. This _combined_ control structure
     would be intended for convenient user access.

     We should also separate "debugging" flags from user-facing controls, as much as possible.

errormsg/
  -- moved with minimal necessary changes from Basics

source/
  -- moved with mininal necessary changes from Basics

stats/
  -- moved with minimal necessary changes from Basics


CM description file: Support/support.cm
MAP file: MAP-Support.txt

Need to consider alternative names (JHR objects to Support). Possibilities:

  Infrastructure (No -- too long)
  Config (No -- not quite appropriate)
  General
  Common
  Support [**tentative winner**]

But want to keep this stuff separate from what we want to put into "Basic", which are
facilities that actually are used in compilation, per se, such as Symbols.

------------------------------------------------------------------------------------------
Changes:

1. FLINTControl.recover is removed because its type is not appropriate for a control
flat. We have to consider how and where to restore that functionality if it is still
needed.
