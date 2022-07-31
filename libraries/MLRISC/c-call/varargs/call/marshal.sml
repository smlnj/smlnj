(* marshal.sml
 *
 * Marshaling for located arguments.
 *)

structure Marshal =
  struct

    structure DL = DynLinkage
    structure Consts = VarargConstants
    structure V = Vararg
    structure W = VarargConstants.W

    datatype argument = I of int | R of real | B of bool | S of string

    fun main's s = DL.lib_symbol (DL.main_lib, s)
    val malloc_h = main's "malloc"
    val free_h = main's "free"

    exception OutOfMemory

    fun sys_malloc (n : W.word) =
	let val w_p = RawMemInlineT.rawccall :
		      W.word * W.word * (unit * word -> string) list
		      -> W.word
	    val a = w_p (DL.addr malloc_h, n, [])
	in if a = 0w0 then raise OutOfMemory else a
	end

    fun sys_free (a : W.word) =
	let val p_u = RawMemInlineT.rawccall :
		      W.word * W.word * (unit * string -> unit) list
		      -> unit
	in p_u (DL.addr free_h, a, [])
	end

    fun alloc bytes = sys_malloc (W.toLargeWord bytes)
    fun free a = sys_free a

    type addr = W.word
    infix ++ 
    fun (a: addr) ++ i = a + W.fromInt i

    fun set' (p, w) = RawMemInlineT.w32s (p, w)
    fun nxt' p = p ++ 1

    fun cpML' { from, to } = let
	val n = String.size from
	fun loop (i, p) =
	    if i >= n then set' (p, 0w0)
	    else (set' (p, W.fromInt (Char.ord
					  (String.sub (from, i))));
		  loop (i+1, nxt' p))
    in
	loop (0, to)
    end

    fun dupML' s = let
	    val z = alloc (W.fromInt (String.size s + 1))
	in
	    cpML' { from = s, to = z };
	    z
	end

    fun set (p, off, v) = set'(p + off, v)

  (* track strings allocated for the call *)
    local
	val allocatedStrs = ref ([] : W.word list)
    in
	fun freeStrs () = (
	       List.app free (!allocatedStrs);
	       allocatedStrs := [])
	fun addStr s = allocatedStrs := s :: !allocatedStrs
    end

  (* marshal the argument field *)
    fun marshalArg (locdArgsArr, V.SINT_ARG i) = set(locdArgsArr, Consts.argOffB, W.fromInt i)
      | marshalArg (locdArgsArr, V.STRING_ARG s) = let
	    val strPtr = dupML' s
	    in 
	       addStr strPtr;
	       set(locdArgsArr, Consts.argOffB, strPtr)
	    end
      | marshalArg (locdArgsArr, V.PTR_ARG p) = set(locdArgsArr, Consts.argOffB, p)
      | marshalArg (locdArgsArr, V.DOUBLE_ARG r) = RawMemInlineT.f64s (locdArgsArr + Consts.argOffB, r)

  (* marshal a located argument *)
    fun marshalLocdArg ({arg, k, width, narrowing, loc, offset}, locdArgsArr) = (
	    set(locdArgsArr, Consts.kindOffB, Consts.kind k);
	    set(locdArgsArr, Consts.widthOffB, W.fromInt width);
	    set(locdArgsArr, Consts.narrowingOffB, W.fromInt (Option.getOpt(narrowing, width)));
	    set(locdArgsArr, Consts.locOffB, W.fromInt loc);
	    set(locdArgsArr, Consts.offsetOffB, W.fromInt offset);
	    marshalArg(locdArgsArr, arg);
	  (* advance the pointer by one located argument *)
	    locdArgsArr + Consts.locdArgSzB
        )

  (* marshal an array of located arguments *)
    fun marshalLocdArgs locdArgs = let
	    val argsSzB = W.fromInt (List.length locdArgs) * Consts.locdArgSzB
	    val locdArgsArr = alloc argsSzB
            in
	        List.foldl marshalLocdArg locdArgsArr locdArgs;
	        {startLocdArgs=locdArgsArr, endLocdArgs=argsSzB + locdArgsArr}
	    end

  end
