(* staged-allocation-fn.sml
 *
 * This code implements the Staged Allocation technique for calling conventions.
 * You can find the POPL06 paper describing this technique at
 * http://www.eecs.harvard.edu/~nr/pubs/staged-abstract.html
 * 
 * Mike Rainey (mrainey@cs.uchicago.edu)
 *
 *)

functor StagedAllocationFn (
    type reg_id
    type loc_kind
    val memSize : int     (* number of bytes addressable in the target machine *)
  ) :> STAGED_ALLOCATION 
	   where type loc_kind = loc_kind
           where type reg_id = reg_id
  = 
  struct

    exception StagedAlloc of string

    type loc_kind = loc_kind
    type width = int

    type req = (width * loc_kind * int)

  (* locations consist of machine registers, offsets in to overflow blocks, combinations of
   * locations, and narrowed locations (Figure 3).
   *)
    type reg_id = reg_id
    type reg = (int * loc_kind * reg_id)
    datatype loc 
      = REG of reg
      | BLOCK_OFFSET of (width * loc_kind * int)
      | COMBINE of (loc * loc)  
      | NARROW of (loc * width * loc_kind)          (* specifies a coercion to the given width and kind *)

  (* the store
   *   the store keeps three pieces of information
   *      - a map from counters to their values
   *      - the overflow block
   *      - the list of allocated registers
   *)
    type counter = int
    structure Store = IntBinaryMap
    type store = (int Store.map * loc option * loc list)
    fun insert ((store, ob, regs), c, n) = (Store.insert (store, c, n), ob, regs)
    fun init cs = List.foldl (fn (c, store) => insert(store, c, 0)) (Store.empty, NONE, []) cs
    fun find ((store, _, _), c) = (case Store.find (store, c)
          of SOME v => v
	   | NONE => raise StagedAlloc "missing store location"
          (* end case *))
    fun setOverflowBlock ((store, _, regs), ob) = (store, SOME ob, regs)
    fun addReg ((store, ob, regs), reg) = (store, ob, reg :: regs)

    datatype block_direction = UP | DOWN
			 
  (* language for specifying calling conventions (Figure 7) *)
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

  (* source for globally unique counter values *)
    local
	val globalCounter = ref 0
    in
    fun freshCounter () = let
	val c = !globalCounter
        in
	  globalCounter := c + 1;
	  c
        end
    end (* local *)

  (* bit width of a machine location *)
    fun width (REG (w, _, _)) = w
      | width (BLOCK_OFFSET (w, _, _)) = w
      | width (COMBINE (l1, l2)) = width l1 + width l2
      | width (NARROW (_, w, _)) = w

    fun useRegs rs = let
	val c = freshCounter ()
        in
	    (c, SEQ [BITCOUNTER c, REGS_BY_BITS (c, rs)])
        end

    fun divides (x, y) = Int.mod (x, y) = 0
    fun toMemSize sz = sz div memSize
    val roundUp = Int.max

  (* Figure 8 *)
    fun dropBits (0, rs) = rs
      | dropBits (n, []) = []
      | dropBits (n, r as (w, _, _) :: rs) = if (n >= w)
	     then dropBits (n - w, rs) 
	     else rs

  (* Figure 8 *)
    fun drop (0, rs) = rs
      | drop (n, []) = []
      | drop (n, r :: rs) = drop (n - 1, rs)

  (* Figure 6: allocator machine  *)
    fun step stages ((w, k, al), store) = (case stages
            of [] => (NONE, store)
	   (* allocate upwards on the overflow block *)
	     | OVERFLOW{counter, blockDirection=UP, maxAlign} :: stages =>
	           if (divides(maxAlign, al) andalso divides(w, memSize))
		      then let
			 val n = find(store, counter)
			 val n' = roundUp(n, al)
			 val store = insert(store, counter, n + toMemSize w)
			 val ob = BLOCK_OFFSET (w, k, n)
			 val store = setOverflowBlock(store, ob)
		         in
			    (SOME ob, store)
			 end
		   else raise StagedAlloc "overflow up"
	   (* allocate downwards on the overflow block *)
	     | OVERFLOW{counter, blockDirection=DOWN, maxAlign} :: stages =>
	           if (divides(maxAlign, al) andalso divides(w, memSize))
		      then let
			 val n = find(store, counter)
			 val n' = roundUp(n, al) + w div memSize
			 val store = insert(store, counter, n')
			 val ob = BLOCK_OFFSET (w, k, n)
			 val store = setOverflowBlock(store, ob)
		         in
			    (SOME ob, store)
			 end
		   else raise StagedAlloc "overflow down"
           (* widen a location *)
	     | WIDEN f :: stages =>
	           if (w <= f w) 
		      then let
			val (SOME loc, store') = step stages ((f w, k, al), store)
			val loc' = if w = f w 
				   then loc   (* eliminate unnecessary narrowed locations *)
				   else NARROW(loc, w, k)
		        in
			   (SOME loc', store')
			end
		   else raise StagedAlloc "widen"
           (* choose the first stage whose corresponding predicate is true. *)
	     | CHOICE choices :: stages => let
	           fun choose [] = raise StagedAlloc "choose"
		     | choose ((p, c) :: choices) = if (p (w, k, al))
                         then c
                         else choose choices
		   val choice = choose choices
	           in
		       step (choice :: stages) ((w, k, al), store)
		   end
           (* the first n arguments go into the first n registers *)
	     | REGS_BY_ARGS (c, rs) :: stages => let
		   val n = find(store, c)
		   val rs' = drop(n, rs)
	           in
		       case rs'
			of [] => step stages ((w, k, al), store)
			 | (r as (w', _, _)) :: _ => if (w' = w)
				         then let
					     val loc = REG r
					     val store = addReg(store, loc)
					     in
					         (SOME loc, store)
					     end
				         else raise StagedAlloc "regs by args"
		   end
           (* increment the argument counter *)
	     | ARGCOUNTER c :: stages => let
		   val (SOME loc, store) = step stages ((w, k, al), store)
		   val n = find(store, c)
		   val store = insert(store, c, n + 1)
	           in
		       (SOME loc, store)
		   end
           (* the first n bits arguments go into the first n bits of registers *)
	     | REGS_BY_BITS (c, rs) :: stages => let
		   val n = find(store, c)
		   val rs' = dropBits(n, rs)
	           in
		     case rs'
		      of [] => (* insufficient bits *) 
			 step stages ((w, k, al), store)
		       | (r as (w', _, _)) :: _ => if (w' = w)
		            then let (* the arg fits into the regs *) 
                               val loc = REG r
			       val store = addReg(store, loc)
                               in
                                  (SOME loc, store)
                               end
 		            else if w' < w
				then let (* some of the arg's bits fit into the regs *)
				  val store = insert (store, c, n + w')
				  val loc = REG r
				  val store = addReg(store, loc)
				  val (SOME loc', store) =
					  step (REGS_BY_BITS (c, rs) :: stages) ((w - w', k, al), store)
				  val store = addReg(store, loc')
				  val loc'' = COMBINE (loc, loc')
				  val n' = find(store, c)
				  val store = insert(store, c, n' - w')
				  in
				     (SOME loc'', store)
				  end
			    else raise Fail "incorrect number of bits"
		   end
	     | BITCOUNTER c :: stages => let
		   val (SOME loc, store) = step stages ((w, k, al), store)
		   val n = find(store, c)
		   val store = insert(store, c, n + w)
	           in
		       (SOME loc, store)
		   end
	     | SEQ ss :: stages => step (ss @ stages) ((w, k, al), store)
	     | PAD c :: stages => let
		   val n = find(store, c)
		   val n' = roundUp(n, al * memSize)
		   val store = insert(store, c, n')
		   val (SOME loc, store) = step stages ((w, k, al), store)
		   in
		       (SOME loc, store)
		   end
	     | ALIGN_TO f :: stages => step stages ((w, k, f al), store)
	    (* end case *))

    fun allocate stages (req, store) = let
	    val (SOME loc, store) = step stages (req, store)
            in
	       (loc, store)
	    end
	handle Match => raise StagedAlloc "failed to allocate"

    fun allocate' stages (req, (locs, store)) = let
	        val (loc, store) = allocate stages (req, store)
                in
	          (loc :: locs, store)
	        end

    fun allocateSeq stages (reqs, store) = let
	  val (locs, store') = List.foldl (allocate' stages) ([], store) reqs
          in
	    (List.rev locs, store')
	  end

    fun allocateSeqs stages (reqss, store) = let
	  fun alloc (reqs, (locss, store)) = let
	        val (locs, store) = allocateSeq stages (reqs, store)
	        in
	            (locs :: locss, store)
	        end
	  val (locss, store') = List.foldl alloc ([], store) reqss
          in
	     (List.rev locss, store')
	  end

    fun freeze (stages, (_, ob, regs)) = 
	    {overflowBlock=ob, allocatedRegs=regs}

  (* extract the kind of a location *)
    fun kindOfLoc (REG(_, k, _)) = k
      | kindOfLoc (BLOCK_OFFSET(_, k, _)) = k
      | kindOfLoc (COMBINE(l1, l2)) = kindOfLoc l1
      | kindOfLoc (NARROW(_, _, k)) = k
  
  end (* StagedAllocationFn *)
