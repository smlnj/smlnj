(* pseudo-ops-basis-typ.sml
 *
 * COPYRIGHT (c) 2001 Bell Labs, Lucent Technologies
 *
 * Representation of pseudo-ops.
 *)

structure PseudoOpsBasisTyp = struct
  datatype ('labexp, 'ext) pseudo_op = 

	   (*
	    * ALIGN_SZ aligns on a 2^n boundary.
	    *
	    * ALIGN_ENTRY forces alignment on an instruction cache line 
	    * boundary, and ALIGN_LABEL is used for internal labels 
	    * (such as loops) and may only align if a small (architecture 
	    * determined) number of nops are required 
	    *)		      
     ALIGN_SZ of int			
   | ALIGN_ENTRY			
   | ALIGN_LABEL

	   (* 
	    * Labels for data pseudo-ops.
	    * All code labels should not be generated as pseudo-ops.
	    *)
   | DATA_LABEL of Label.label

           (*
	    * The usual text and data sections. 
	    * Sections are not allowed inside a text segment 
            *)
   | DATA_READ_ONLY
   | DATA 
   | BSS	(* data initialized to zero *)
   | TEXT
   | SECTION of Atom.atom 
   
	  (*
	   * May have to rethink this one!
	   * For now, all instructions following a NOREORDER pseudo-op
           * are preserved in the order they were generated, until 
	   * a REORDER pseudo-op is seen.
           *
           * Perhaps what we also want a BARRIER pseudo-op that says
           * no instructions must be moved above or below the barrier.
	   *)
   | REORDER
   | NOREORDER

          (*
           * Constant integral data
           *)
   | INT of {sz : int, i: 'labexp list}
	     
	 (*
          * Strings and zero terminated strings
	  *)
   | ASCII of string
   | ASCIIZ of string

         (* 
          * allocate uninitialized data space with size in bytes
	  *)
   | SPACE of int 			

	 (*
          * Constant real data
	  *)
   | FLOAT of {sz : int, f : string list}

	(*
	 * Import and export identifiers
         *)
   | IMPORT of Label.label list
   | EXPORT of Label.label list
   | COMMENT of string
	(*
         * Client specific pseudo-ops
	 * All these pseudo-ops must be related to data
	 * and not code!
	 *)
   | EXT of 'ext
end