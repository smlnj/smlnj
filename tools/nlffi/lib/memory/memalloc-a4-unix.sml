(* memalloc-a4-unix.sml
 *
 *   Memory allocation (via malloc) for Unix.
 *   Size of address: 4 bytes.
 *
 * Copyright (c) 2004 by The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (blume@tti-c.org)
 *)
structure CMemAlloc : CMEMALLOC = struct

    exception OutOfMemory

    type addr = Word32.word
    type addr' = addr

    structure DL = DynLinkage

    fun main's s = DL.lib_symbol (DL.main_lib, s)
    val malloc_h = main's "malloc"
    val free_h = main's "free"

    fun sys_malloc (n : Word32.word) =
	let val w_p = RawMemInlineT.rawccall :
		      Word32.word * Word32.word * (unit * word -> string) list
		      -> Word32.word
	    val a = w_p (DL.addr malloc_h, n, [])
	in if a = 0w0 then raise OutOfMemory else a
	end

    fun sys_free (a : Word32.word) =
	let val p_u = RawMemInlineT.rawccall :
		      Word32.word * Word32.word * (unit * string -> unit) list
		      -> unit
	in p_u (DL.addr free_h, a, [])
	end

    fun alloc bytes = sys_malloc (Word32.fromLarge(Word.toLarge bytes))
    fun free a = sys_free a
end
