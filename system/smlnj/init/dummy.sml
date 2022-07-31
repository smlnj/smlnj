(* dummy.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Dummy implementation of the Assembly structure.  The actual
 * implementation of Assembly.A is in assembly code.  The records
 * that represent the Assembly and Assembly.A structures are allocated
 * in the runtime system (runtime/kernel/globals.c).
 *)

structure Assembly :> ASSEMBLYBOXED =
  struct
    type object = object
    datatype 'a option = NONE | SOME of 'a

   (*
    * Declarations whose right handside is a primOp do not
    * generate any code. This is a hack, and should be cleaned
    * in the future. (ZHONG)
    *)
    val cast : 'a -> 'b = InLine.cast

    structure A =
      struct
	type c_function = c_function
	type word8array = word8array
	type real64array = real64array
	type spin_lock = spin_lock

	fun array (x: object): object = cast x
	fun bind_cfun (x: object): object = cast x
	fun callc (x: object): object = cast x
	fun create_b (x: object) : word8array = cast x
	fun create_r (x: object) : real64array = cast x
	fun create_s (x: object): string = cast x
	fun create_v (x: object): object = cast x
	fun floor (x: object): object = cast x
	fun logb (x: object): object = cast x			(* DEPRECATED *)
	fun scalb (x: object): object = cast x
	fun try_lock (x: spin_lock): object = cast x
	fun unlock (x: spin_lock): object = cast x
      end

    exception Div
    exception Overflow
    exception SysErr of (string * int option)

    val vector0 : 'a vector = cast()
    val profCurrent : int ref = cast()
    val pollEvent : bool ref = cast()
    val pollFreq : int ref = cast()
    val pollHandler : (unit cont -> unit cont) ref = cast()
    val activeProcs : int ref = cast()
    val pstruct : object ref = cast()
    val sighandler : ((int * int * unit cont) -> unit cont) ref = cast()

 end (* abstraction Assembly *)
