## ElabData/pickle

The pickler for static environments

* used by CM
* used to support _cutoff recompilation_
* pickles are stored in compiled (object) files

Uses the Library/pickles general library for pickling data structures.

The pickler facilities can handle sharing (dag structures) but not
cycles.

Static constructs (types, modules, bindings, etc.) that make up
static environments are therefore designed to be cycle free.

This pickler is likely to be replaced by a new pickling approach
based on a general-purpose pickler for ASDL-defined data structures.
This would entail translating the static environment data into
an ASDL scheme.

	files: README.md  -- this file
		   rehash.sml
		   pickle-sym-pid.sml
		   unpickle-sym-pid.sml
		   pickmod.sml
		   unpickmod.sml
		 

-------------------------
Document History
[DBM, 2020.04.26] created, V. 110.97
