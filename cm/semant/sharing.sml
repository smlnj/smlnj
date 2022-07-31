(*
 * Module-level value-sharing in CM.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
structure Sharing = struct

    (* the "request" corresponds to the class specified in the .cm file *)
    datatype request = PRIVATE | SHARED | DONTCARE

    (* the "mode" (i.e., what CM actually uses) is determined by
     * taking both "request" and the modes of predecessors in the
     * dependency graph into account. *)
    datatype mode =
	SHARE of bool			(* true: warn if sharing is broken *)
      | DONTSHARE
end
