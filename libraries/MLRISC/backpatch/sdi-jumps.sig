(* sdi-jumps.sig --- specification of target information to resolve jumps. 
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

signature SDI_JUMPS = sig
  structure I : INSTRUCTIONS
  structure C : CELLS
    sharing I.C = C

  val branchDelayedArch : bool

  val isSdi : I.instruction -> bool
  val minSize : I.instruction -> int
  val maxSize : I.instruction -> int
      (* minSize and maxSize are not restricted to SDIs but 
       * instructions that may require NOPs after them, etc. 
       *)

  val sdiSize : I.instruction * (Label.label -> int) * int -> int
      (* sdiSize(instr, regmaps, labMap, loc) -- return the size of
       * instr at location loc, assuming an assignment of labels
       * given by labMap.
       *)

  val expand : I.instruction * int * int -> I.instruction list
      (* expand(instr,size,loc) - expands sdi instruction instr,
       *  into size bytes at postion loc.
       *)

end


