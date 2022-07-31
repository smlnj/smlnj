 (* staged-allocation-sig.sml
 *
 * This code implements the Staged Allocation technique for calling conventions.
 * You can find the POPL06 paper describing this technique at
 * http://www.eecs.harvard.edu/~nr/pubs/staged-abstract.html
 *
 * Mike Rainey (mrainey@cs.uchicago.edu)
 * 
 *
 * Terminology for staged allocation (see the paper for more details):
 *   counter - stores of current the number of bits allocated to the call
 *   location - a mechanism for passing a parameter, e.g., machine registers, stack locations, etc.
 *   req - corresponds to a parameter
 *   alignment - alignment in bits for a location
 *   width - width in bits for a location
 *   stage - one rule for specifying calling conventions
 *
 *)

signature STAGED_ALLOCATION = 
  sig

    type loc_kind                                (* gprs, fprs, stack locations, etc. *)
    type width = int                             (* bit width *)

    type req = (width * loc_kind * int)          (* the last field is the alignment *)

  (* locations consist of machine registers, offsets in to overflow blocks, combinations of
   * locations, and narrowed locations.
   *)
    type reg_id
    type reg = (int * loc_kind * reg_id)
    datatype loc 
      = REG of reg
      | BLOCK_OFFSET of (width * loc_kind * int)
      | COMBINE of (loc * loc)  
      | NARROW of (loc * width * loc_kind)          (* specifies a coercion to the given width and kind *)

    type counter                                 (* abstract counter for a convention *)
    type store                                   (* counter -> "bit offset" *)

    datatype block_direction = UP | DOWN         (* direction in which the overflow block grows *)
			 
  (* language for specifying calling conventions *)
    datatype stage 
      = OVERFLOW of {                                (* overflow block (usually corresponds to a runtime stack) *)
	     counter : counter,
	     blockDirection : block_direction,
	     maxAlign : int 
        }
      | WIDEN of (width -> width)      
      | CHOICE of ( (req -> bool) * stage) list      (* choose the first stage whose corresponding 
						      * predicate is true. *)
      | REGS_BY_ARGS of (counter * reg list)         (* the first n arguments go into the first n
						      * registers *)
      | ARGCOUNTER of counter
      | REGS_BY_BITS of (counter * reg list)         (* the first n bits arguments go into the first 
						      * n bits of registers *)
      | BITCOUNTER of counter                        
      | SEQ of stage list                            (* sequence of stages *)
      | PAD of counter                               (* specifies an alignment (this rule applies even 
						      * for registers) *)      
      | ALIGN_TO of (width -> width)                 (* specifies an alignment *)

    exception StagedAlloc of string

  (* bit width of a machine location *)
    val width : loc -> int

  (* Create a counter. *)
    val freshCounter : unit -> counter

  (* helper function that creates a counter c, and returns the sequence:
   * [BITCOUNTER c, REGS_BY_BITS (c, regs)] (this function is taken from 
   * the paper). 
   *)
    val useRegs : reg list -> (counter * stage)

  (* find the value stored at a counter. *)
    val find : (store * counter) -> int

  (* initialize a list of counters for a calling convention. *)
    val init : counter list -> store

  (* takes an spec and automaton and allocates a location *)
    val allocate : stage list -> (req * store)
		       -> (loc * store)

  (* allocate lifted to sequences of requests *)
    val allocateSeq : stage list -> (req list * store)
		       -> (loc list * store)

  (* allocateSeq lifted to sequences of sequences of requests *)
    val allocateSeqs : stage list -> (req list list * store)
		       -> (loc list list * store)

  (* takes an automaton (the first two parameters) and returns the overflow block and the set of
   * registers used by previous calls to allocate 
   *)
    val freeze : (stage list * store) 
		     -> {overflowBlock : loc option, allocatedRegs : loc list}

  (* extract the kind of a location *)
    val kindOfLoc : loc -> loc_kind

  end (* STAGED_ALLOCATION *)
