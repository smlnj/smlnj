(* fetch-cm.sml
 *
 * COPYRIGHT (c) 2022 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Defining the top-level structure CM by fetching it from CM0.
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)

local
  (* These two are here just pro-forma to prevent CM from optimizing
   * the two libraries away before they can be used for pickling.
   * See the comment in full.cm.
   *)
  structure SrcPath = SrcPath		(* ref to $cm/paths/srcpath-lib.cm *)
  structure String = String		(* ref to $/basis.cm *)
in

structure CM :> CM = CM0.CM

end
