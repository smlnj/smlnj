(* NOTE on xchg on the x86
 *
 * From Allen Leung:
 * Here's why I didn't use xchg: 
 * 
 * o  According to the optimization guide xchg mem, reg is complex,
 *    cannot be pipelined or paired at all. xchg reg, reg requires 3 uops.
 *    In contrast, mov mem, reg requires 1 or 2 uops.  
 *    So xchgs loses out, at least on paper.  
 *    [I haven't done any measurements though] 
 * 
 * o  Secondly, unlike other architectures, parallel copies are split 
 *    into individual copies during instruction selection.  Here's why
 *    I did this:  I found that more copies are retained and more spills 
 *    are generated when keeping the parallel copies.   My guess on this is 
 *    that the copy temporary for parallel copies create addition 
 *    interferences [even when they are not needed.]  
 *    This is not a problem on RISC machines, because of plentiful registers.
 *   
 * o  Spilling of parallel copies is also a very complex business when
 *    memory coalescing is turned on.  I think I have implemented a solution
 *    to this, but not using parallel copies keep life simple.   This problem
 *    could be simpler with xchg...but I haven't thought about it much.
 * 
 * From Fermin Reig:
 * In the java@gcc.gnu.org, GC  mailing lists there's been a discussion about
 * the costs of xcgh. Here's some extracts of it:
 * 
 * ----------------
 * > From: Emery Berger [mailto:emery@cs.utexas.edu]
 * > 
 * > http://developer.intel.com/design/pentium4/manuals/24547203.pdf
 * > 
 * > See Chapter 7.1. "For the P6 family processors, locked 
 * > operations serialize
 * > all outstanding load and store operations (that is, wait for them to
 * > complete). This rule is also true for the Pentium 4 
 * > processor, with one
 * > exception: load operations that reference weakly ordered 
 * > memory types (such
 * > as the WC memory type) may not be serialized. "
 * > 
 * -----------------
 * I just tried this on a 500 MHz Pentium III.  I get about 23 cycles for
 *  
 * lock; cmpxchg
 *  
 * :
 * and about 19 or 20 cycles for xchg (which has an implicit lock prefix).
 *  
 * I got consistent results by timing a loop and by looking at an instruction
 * level profile.  Putting other stuff in the loop didn't seem to affect the
 * time taken by xchg much.  Here's the code in case someone else wants to try.
 * (This requires Linux/gcc)
 * -------------------
 * Chris Dodd pointed out on the GC mailing list that on recent Intel X86
 * processors:
 *  
 * - cmpxchg without a lock prefix is much faster (roughly 3x or close to 15
 * cycles by my measurements) than either xchg (implied lock prefix) or lock;
 * cmpxchg .
 *  
 * - cmpxchg without the lock prefix is atomic on uniprocessors, i.e. it's not
 * interruptable.
 *  
 * As far as I can tell, none of the GNU libraries currently take advantage of
 * this fact.  Should they?
 *  
 * This argues, for example, that I could get noticable additional speedup from
 * Java hash synchronization on X86 by overwriting a few strategic "lock"
 * prefixes with "nop"s when I notice that there's only one processor
 *
 *
 * From John Reppy:
 *
 * Disregard what I said.  The xchg instruction has an implicit lock prefix,
 * so it is not useful for normal programming tasks.
 *)

functor X86Shuffle(I : X86INSTR) : X86SHUFFLE =
struct
  structure I = I
  structure C = I.C
  structure CB = CellsBasis
  structure Shuffle = Shuffle(I)

  type t = {tmp:I.operand option, dst:CellsBasis.cell list, src:CellsBasis.cell list}

  exception foo
  val shuffle =
    Shuffle.shuffle
        {mvInstr=fn{dst, src} => [I.move{mvOp=I.MOVL, src=src, dst=dst}],
	 ea=I.Direct}

  (*
   * These assume that the ''registers'' are mapped onto the memory
   *)

  (* Note, this only works with double precision floating point *) 
  val shufflefpNormalAndSlow = 
    Shuffle.shuffle
        {mvInstr=fn{dst, src} => [I.fldl src, I.fstpl dst],
	 ea = I.FDirect}

  (* 
   * This version makes use of the x86 floating point stack for hardware
   * renaming! 
   *)
  fun shufflefpNormal{tmp, src, dst} = let
    val n =  length src
  in 
    if n <= 7 then let 
	fun gen(s::ss, d::ds, pushes, pops) = 
  	    if CB.sameColor(s,d) then gen(ss, ds, pushes, pops)
	    else 
	      gen(ss, ds, 
		    I.fldl(I.FDirect s)::pushes,
		    I.fstpl(I.FDirect d)::pops)
	  | gen(_, _, pushes, pops) = List.revAppend(pushes, pops)
      in  gen(src, dst, [], []) 
      end
    else shufflefpNormalAndSlow{tmp=tmp, src=src, dst=dst}
  end

  (*
   * These assume that the ''registers'' are mapped onto the pseudo 
   * %fpr register.  Only works with double precision floating point for 
   * now...
   *)
  val shufflefpFast = 
       Shuffle.shuffle
         {mvInstr=fn{dst, src} => [I.fmove{fsize=I.FP64,src=src, dst=dst}],
	  ea = I.FPR}

  fun shufflefp(x as {tmp=SOME(I.FPR _), ...}) = shufflefpFast x
    | shufflefp x = shufflefpNormal x

end

