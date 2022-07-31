(* target32-core.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This is the implementation of the Core structure for 32-bit targets.
 *
 * Core assumes that the following bindings are already in the static
 * environment:
 *
 *   1. Built-in structures, defined in PrimTypes (env/prim.sml):
 *        PrimTypes InLine
 *
 *   2. Built-in type constructors, defined in PrimTypes (env/prim.sml):
 *        int string bool unit real list array ref exn
 *
 *   3. Built-in data constructors, also from PrimTypes (env/prim.sml):
 *        :: nil ref true false
 *
 *   4. Built-in primitive operators, defined in InLine (env/prim.sml).
 *      The InLine structure is not typed (all values have type alpha, this
 *      will change in the future though !).
 *
 *   5. The Assembly structure: its static semantics is defined by elaborating
 *      the boot/dummy.sml file, and its dynamic semantics is directly coming
 *      the implementation module provided by the runtime system.
 *
 * In addition, all matches in this file should be exhaustive; the match and
 * bind exceptions are not defined at this stage of bootup, so any uncaught
 * match will cause an unpredictable error.
 *
 *)

structure Core =
  struct
  (*
   * We build an Assembly structure from the implementation module provided
   * from the runtime systems. The coercions are implemented via InLine.cast,
   * a primitive operator hardwired inside the compiler. In the future, the
   * linkage should be done safely without using cast (ZHONG).
   *
   * Note: in the future, the Assembly.A substructure may be replaced by
   * a dynamic run vector (JHR).
   *)
    structure Assembly : ASSEMBLY =
      struct
	open Assembly

	val cast : 'a -> 'b = InLine.cast
        datatype ('a, 'b) pair = PAIR of 'a * 'b

        structure A =
          struct
	    structure AA = Assembly.A

	    type c_function = AA.c_function
	    type word8array = AA.word8array
	    type real64array = AA.word8array
	    type spin_lock = AA.spin_lock

            val arrayP : (int, 'a) pair -> 'a array = cast AA.array
	    val array : int * 'a -> 'a array = fn x => arrayP(PAIR x)

            val bind_cfunP : (string, string) pair -> c_function =
                     cast AA.bind_cfun
	    val bind_cfun : (string * string) -> c_function =
                     fn x => bind_cfunP (PAIR x)

            val callcP : (c_function, 'a) pair -> 'c = cast AA.callc
	    val callc : (c_function * 'a) -> 'c = fn x => callcP (PAIR x)

	    val create_b : int -> word8array = cast AA.create_b
	    val create_r : int -> real64array = cast AA.create_r
	    val create_s : int -> string = cast AA.create_s
            val create_vP : (int, 'a list) pair -> 'a vector = cast AA.create_v
	    val create_v : int * 'a list -> 'a vector =
                     fn x => create_vP(PAIR x)

	    val floor : real -> int = cast AA.floor
	    val logb : real -> int = cast AA.logb			(* DEPRECATED *)
            val scalbP : (real, int) pair -> real = cast AA.scalb
	    val scalb : real * int -> real = fn x => scalbP(PAIR x)

	    val try_lock : spin_lock -> bool = cast AA.try_lock
	    val unlock : spin_lock -> unit = cast AA.unlock

	  end (* structure A *)

	  val vector0 : 'a vector = cast vector0

      end (* structure Assembly *)

    infix 7  * / quot mod rem div
    infix 6 ^ + -
    infix 3 := o
    infix 4 > < >= <=
    infixr 5 :: @
    infix 0 before

    exception Bind
    exception Match

    exception Domain
    exception Range      	(* for word8array update *)
    exception Subscript  	(* for all bounds checking *)
    exception Size
    exception Chr

    local exception NoProfiler
    in val profile_register =
      ref(fn s:string => (raise NoProfiler):int*int array*int ref)
    end

    local val ieql : int * int -> bool = InLine.int_eql
          val peql : 'a * 'a -> bool = InLine.ptr_eql
          val ineq : int * int -> bool = InLine.int_neq
	  val i32eq : int32 * int32 -> bool = InLine.int32_eql
	  val i64eq : int64 * int64 -> bool = InLine.int64_eql
          val boxed : 'a -> bool = InLine.boxed
          val op + : int * int -> int = InLine.int_add
          val op - : int * int -> int = InLine.int_sub
          val op * : int * int -> int = InLine.int_mul
	  val op := : 'a ref * 'a -> unit = InLine.:=
          val ordof : string * int -> char = InLine.char_vec_unsafe_sub
          val cast : 'a -> 'b = InLine.cast
          val getObjTag : 'a -> int = InLine.gettag
          val getObjLen : 'a -> int = InLine.objlength
	  val getData : 'a -> 'b = InLine.seq_data
	  val recSub : ('a * int) -> 'b = InLine.recordSub
          val vecLen : 'a -> int = InLine.seq_length
          val vecSub : 'a vector * int -> 'a = InLine.vec_unsafe_sub
          val andb : int * int -> int = InLine.int_andb
	  val fast_add : int * int -> int = InLine.int_unsafe_add
	  val fast_sub : int * int -> int = InLine.int_unsafe_sub

	  val width_tags = 0w7  (* 5 tag bits plus "10" *)

        (* the type annotation is just to work around an bug - sm *)
          val ltu : int * int -> bool = InLine.int_ltu

    in

     (* limit of array, string, etc. element count is one greater than
      * the maximum length field value (sign bit should be 0).
      *)
       val max_length = let
	    val op - = InLine.word_sub
	    infix << val op << = InLine.word_lshift
	    val int = InLine.signed_word_to_int
	    in
	      int ((0w1 << (0w31 - width_tags)) - 0w1)
	    end

       fun mkNormArray (n, init) =
             if ieql(n, 0) then InLine.newArray0()
             else if ltu(max_length, n) then raise Size
                  else Assembly.A.array (n, init)

       val mkrarray : int -> real array = InLine.cast Assembly.A.create_r
       fun mkRealArray (n : int, v : real) : real array =
             if ieql(n, 0) then InLine.newArray0()
             else if ltu(max_length, n) then raise Size
                  else let val x = mkrarray n
                           fun init i =
                             if ieql(i,n) then x
    			     else (InLine.real64_arr_unsafe_update(x,i,v);
                                   init (fast_add (i, 1)))
                        in init 0
                       end

       val vector0 = Assembly.vector0  (* needed to compile ``#[]'' *)


      (* LAZY: The following definitions are essentially stolen from
       *  SMLofNJ.Susp.  Unfortunately, they had to be copied here in
       *  order to implement lazyness (in particular, in order to be
       *  able to compute pids for them.) *)

       local
	   structure Susp :> sig
	       type 'a susp
	       val delay : (unit -> 'a) -> 'a susp
	       val force : 'a susp -> 'a
	   end = struct
	   (* LAZY:  The following is hard-wired and needs to track the object
	    * descriptor definitions.
	    *)
	   val TSUS = 0;  (* == ObjectDesc.special_unevaled_susp *)
	   val TSES = 1;  (* == ObjectDesc.special_evaled_susp *)

	   (* Just a hack for bootstrapping: *)
	   datatype 'a susp = Something of 'a

	   fun delay (f : unit -> 'a) = InLine.mkspecial(TSUS,f): 'a susp
	   fun force (x : 'a susp) =
	       if InLine.int_eql((InLine.getspecial x),TSUS)
	       then let val y : 'a = recSub (InLine.cast x, 0) ()
		    in InLine.cast x := y;
		       InLine.setspecial(InLine.cast x, TSES);
		       y
		    end
	       else recSub (InLine.cast x, 0)
	   end
       in
       open Susp
       end

       (* equality primitives *)

    fun stringequal (a : string, b : string) =
	  if peql(a,b)
	    then true
            else let
	      val len = vecLen a
              in
	        if ieql(len, vecLen b)
                  then let
		    fun f 0 = true
		      | f i = let
			  val j = fast_sub(i, 1)
			  in
			    InLine.char_eql(ordof(a,j),ordof(b,j)) andalso f j
			  end
	            in
		      f len
                    end
	          else false
	      end

    fun polyequal (a : 'a, b : 'a) = peql(a,b)
	  orelse (boxed a andalso boxed b
	    andalso let
	    (* NOTE: since GC may strip the header from the pair in question,
	     * we must fetch the length before getting the tag, whenever we
	     * might be dealing with a pair.
	     *)
	      val aLen = getObjLen a
	      val aTag = getObjTag a
	      fun pairEq () = let
		    val bLen = getObjLen b
		    val bTag = getObjTag b
		    in
		      ((ieql(bTag, 0x02) andalso ieql(bLen, 2))
		        orelse ineq(andb(bTag, 0x3),0x2))
		      andalso polyequal(recSub(a, 0), recSub(b, 0))
		      andalso polyequal(recSub(a, 1), recSub(b, 1))
		    end
	      fun eqVecData (len, a, b) = let
		    fun f i = ieql(i, len)
			  orelse (polyequal(recSub(a, i), recSub(b, i))
			    andalso f(i+1))
		    in
		      f 0
		    end
	      in
		case aTag
		 of 0x02 (* tag_record *) =>
		      (ieql(aLen, 2) andalso pairEq())
		      orelse (
			ieql(getObjTag b, 0x02) andalso ieql(getObjLen b, aLen)
			andalso eqVecData(aLen, a, b))
		  | 0x06 (* tag_vec_hdr *) => (
		    (* length encodes element type *)
		      case (getObjLen a)
		       of 0 (* seq_poly *) => let
			    val aLen = vecLen a
			    val bLen = vecLen b
			    in
			      ieql(aLen, bLen)
				andalso eqVecData(aLen, getData a, getData b)
			    end
			| 1 (* seq_word8 *) => stringequal(cast a, cast b)
			| _ => raise Match (* shut up compiler *)
		      (* end case *))
		  | 0x0a (* tag_arr_hdr *) => peql(getData a, getData b)
		  | 0x0e (* tag_arr_data and tag_ref *) => false
		  | 0x12 (* tag_raw *) => (
		    (* should either be a boxed 32-bit or boxed 64-bit number. We use
		     * the cast to int32 or int64 to force the loading of the value
		     * to compare from memory.
		     *)
		      case getObjLen a
		       of 1 => i32eq(cast a, cast b)
			| 2 => i64eq(cast a, cast b)
			| _ => raise Match
		      (* end case *))
		  | _ (* tagless pair *) => pairEq()
		(* end case *)
	      end)

        (* trace/debug/profile generation hooks *)
        type tdp_plugin =
	     { name     : string,	(* name identifying plugin *)
	       save     : unit -> unit -> unit,
	       push     : int * int -> unit -> unit,
	       nopush   : int * int -> unit,
	       enter    : int * int -> unit,
	       register : int * int * int * string -> unit }

        local
	    val next = ref 0
	    val hook = ref [] : tdp_plugin list ref

	    val ! = InLine.!
	    infix :=
	    val op := = InLine.:=

	    fun runwith a f = f a

	    fun map f = let
		fun loop [] = []
		  | loop (h :: t) = f h :: loop t
	    in
		loop
	    end

	    fun app f = let
		fun loop [] = ()
		  | loop (h :: t) = (f h; loop t)
	    in
		loop
	    end

	    fun revmap f l = let
		fun loop ([], a) = a
		  | loop (h :: t, a) = loop (t, f h :: a)
	    in
		loop (l, [])
	    end

	    fun onestage sel () =
		let val fns = map sel (!hook)
		in
		    fn arg => app (runwith arg) fns
		end

	    fun twostage sel () =
		let val stage1_fns = map sel (!hook)
		in
		    fn arg =>
		       let val stage2_fns = revmap (runwith arg) stage1_fns
		       in
			   fn () => app (runwith ()) stage2_fns
		       end
		end
	in
	    fun tdp_reserve n = let val r = !next in next := r + n; r end
	    fun tdp_reset () = next := 0

	    (* pre-defined kinds of IDs (to be passed to "register") *)
	    val tdp_idk_entry_point = 0
	    val tdp_idk_non_tail_call = 1
	    val tdp_idk_tail_call = 2

	    val tdp_save = twostage #save
	    val tdp_push = twostage #push
	    val tdp_nopush = onestage #nopush
	    val tdp_enter = onestage #enter
	    val tdp_register = onestage #register

	    val tdp_active_plugins = hook
	end

      (* functions for making intinf literals *)
	val makeNegInf = CoreIntInf.makeNegInf
	val makePosInf = CoreIntInf.makePosInf
	val makeSmallNegInf = CoreIntInf.makeSmallNegInf
	val makeSmallPosInf = CoreIntInf.makeSmallPosInf
	val infLowValue = CoreIntInf.lowValue

      (* these functions below are used in FLINT/trans/transprim.sml as extra
       * arguments to primops.  The primops eventually get replaced with calls
       * to the functions after CPS optimization.
       *)

      (* boxed 32-bit numbers <-> intinf *)
	val truncInf32 = CoreIntInf.truncInf32		(* for `P.TRUNC_INF 32` *)
	val testInf32 = CoreIntInf.testInf32		(* for `P.TEST_INF 32` *)
	val copy32Inf = CoreIntInf.copy32Inf		(* for `P.COPY_INF 32` *)
	val extend32Inf = CoreIntInf.extend32Inf	(* for `P.EXTEND_INF 32` *)

      (* word64-rep (pairs of 32-bit words) <-> intinf *)
	val truncInf64 = CoreIntInf.truncInf64		(* for `P.TRUNC_INF 64` *)
	val testInf64 = CoreIntInf.testInf64		(* for `P.TEST_INF 64` *)
	val copy64Inf = CoreIntInf.copy64Inf		(* for `P.COPY_INF 64` *)
	val extend64Inf = CoreIntInf.extend64Inf	(* for `P.EXTEND_INF 64` *)

(* DEPRECATED -- for backward compatibility with 110.91 *)
	val truncInfLarge = CoreIntInf.truncInf32	(* for `P.TRUNC_INF 32` *)
	val testInfLarge = CoreIntInf.testInf32		(* for `P.TEST_INF 32` *)
	val copyLargeInf = CoreIntInf.copy32Inf		(* for `P.COPY_INF 32` *)
	val extendLargeInf = CoreIntInf.extend32Inf	(* for `P.EXTEND_INF 32` *)

      (* word64-rep (pairs of 32-bit words) -> int *)
(* do we need these functions? *)
	val w64ToInt = CoreWord64.toInt
	val w64ToIntX = CoreWord64.toIntX
	val i64ToInt = CoreInt64.toInt

      (* word64-rep (pairs of 32-bit words) -> large boxed int  *)
	val w64ToInt32 = CoreWord64.toInt32
	val w64ToInt32X = CoreWord64.toInt32X
	val i64ToInt32 = CoreInt64.toInt32

      (* 64-bit arithmetic operations that do not have direct implementations
       * on 32-bit targets.
       *)
	val i64Mul = CoreInt64.*
	val i64Div = CoreInt64.div
	val i64Mod = CoreInt64.mod
	val i64Quot = CoreInt64.quot
	val i64Rem = CoreInt64.rem
	val w64Mul = CoreWord64.*
	val w64Div = CoreWord64.div
	val w64Mod = CoreWord64.mod

      (* the compilation of profiling (compiler/DebugProf/profile/tprof.sml) and
       * the lazy features (compiler/Elaborator/elaborate/elabcore.sm)
       * requires access to these operations
       *)
	val assign = ( InLine.:= )
        val deref = ( InLine.! )
	val unboxedupdate = InLine.arr_unboxed_update
	val subscript = InLine.arr_unsafe_sub
	val iadd = InLine.int_add

    end (* local *)

    val profile_sregister = ref(fn (x:Assembly.object,s:string)=>x)

  end
