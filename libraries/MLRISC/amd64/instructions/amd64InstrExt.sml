(* amd64InstrExt.sml
 *
 * COPYRIGHT (c) 2007 The Fellowship of SML/NJ (http://smlnj.org)
 *
 * emit code for extensions to the amd64 instruction set.
 *)

structure AMD64InstrExt =
  struct

    datatype fsz = SINGLE | DOUBLE

    datatype ('s, 'r, 'f, 'c) sext 
	= PUSHQ of 'r
 	| POP of 'r 
        | LEAVE
        | RET of 'r
    (* atomic fetch and add operations
     *   XADD (src, dst)            tmp = src + dst
     *                              src = dst
     *                              dst := tmp
     *)
        | LOCK_XADDL of ('r * 'r)	(* 32-bit atomic fetch and add *)
        | LOCK_XADDQ of ('r * 'r)	(* 64-bit atomic fetch and add *)
    (* atomic compare and exchange instructions.
     *   CMPXCHG(src, dst)	compares dst with %eax (or %rax) register.
     *				if they are equal, then the ZF flag is set and
     *				src is stored in dst; otherwise dst is loaded
     *				into %eax (or %rax) and ZF is cleared.
     *)
        | LOCK_CMPXCHGL of ('r * 'r)	(* 32-bit compare and exchange *)
        | LOCK_CMPXCHGQ of ('r * 'r)	(* 64-bit compare and exchange *)
    (* atomic exchange instructions.
     *   XCHG(src, dst)	       tmp = *dst
     *                         *dst := src
     *                         src := tmp
     *)
        | LOCK_XCHGL of ('r * 'r)	(* 32-bit exchange *)
        | LOCK_XCHGQ of ('r * 'r)	(* 64-bit exchange *)
	| PAUSE                         (* improves performance of spin-wait loops *) 
    (* performs a serializing operation on all load-from-memory and store-to-memory 
     * operations issued prior to the mfence instruction.
     *)
	| MFENCE
    (* performs a serializing operation on all load-to-memory operations issued prior to
     * the lfence instruction.
     *)
	| LFENCE
    (* performs a serializing operation on all store-to-memory operations issued prior to
     * the sfence instruction.
     *)
	| SFENCE
    (* time-stamp counter (TSC) operations
     *    RDTSC         edx := <high-order 32 bits of the processor's time-stamp counter>
     *                  eax := <low-order 32 bits of the processor's time-stamp counter>
     *
     *    RDTSCP        edx := <high-order 32 bits of the processor's time-stamp counter>
     *                  eax := <low-order 32 bits of the processor's time-stamp counter>
     *                  ecx := <processor's cpu id>
     *                  *** waits until all previous instructions have been executed 
     *                  before the reading the counter
     *)
	| RDTSC
	| RDTSCP

  end (* AMD64InstrExt *)
