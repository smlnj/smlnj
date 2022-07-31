(* target32-inline.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Interfaces to the compiler built-ins, infixes, etc. for 32-bit targets.
 *
 * [dbm, 6/21/06] This module is compiled in the environment PrimEnv.primEnv.
 * See init.cmi
 *)

structure PrimTypes = struct open PrimTypes end
   (* this silliness is to prevent elabstr.sml from sticking a NO_ACCESS
      in the wrong place [dbm: presumably this NO_ACCESS is derived from
      the dummy access value (NO_ACCESS) in the hand-built PrimTypes module.]
      How and why does this access value get propagated into the code? *)

local
  open PrimTypes
in
(* [dbm, 6/21/06] If this is elaborated in the primEnv environment, there is
already an opened PrimType, so is the above code redundant? By experimentation,
it appears that the "local open PrimTypes in ..." is not necessary. *)

structure Assembly = Core.Assembly

  (* The following code was used to create a type-safe version of the InLine
   * structure while preserving the inline property of the functions.
   * Since everything in InLine is now properly typed already, the code
   * should now be seen as:
   *   - organizing things a bit better
   *   - confirming the type information
   * See compiler/Semant/statenv/prim.sml for the origin of the type info
   * in InLine.    (Blume, 1/2001) *)
structure InlineT =
  struct
    type 'a control_cont = 'a control_cont

    val callcc		: ('a cont -> 'a) -> 'a = InLine.callcc
    val throw	 	: 'a cont -> 'a -> 'b = InLine.throw
    val capture		: ('a control_cont -> 'a) -> 'a = InLine.capture
    val escape		: 'a control_cont -> 'a -> 'b = InLine.cthrow
    val isolate         : ('a -> unit) -> 'a cont = InLine.isolate
    val !	 	: 'a ref -> 'a = InLine.!
    val op := 		: 'a ref * 'a -> unit = InLine.:=
    val makeref 	: 'a -> 'a ref = InLine.makeref
    val op = 		: ''a * ''a -> bool  = InLine.=
    val op <> 		: ''a * ''a -> bool = InLine.<>
    val boxed 		: 'a -> bool = InLine.boxed
    val unboxed 	: 'a -> bool = InLine.unboxed
    val cast 		: 'a -> 'b = InLine.cast
    val identity	: 'a -> 'a = InLine.inl_identity
    val objlength	: 'a -> int = InLine.objlength
    val mkspecial	: int * 'a -> 'b = InLine.mkspecial
    val getspecial	: 'a -> int = InLine.getspecial
    val setspecial	: ('a * int) -> unit = InLine.setspecial
    val gethdlr 	: unit -> 'a cont = InLine.gethdlr
    val sethdlr 	: 'a cont -> unit = InLine.sethdlr
    val getvar		: unit -> 'a = InLine.getvar
    val setvar		: 'a -> unit = InLine.setvar
    val compose 	: ('b -> 'c) * ('a -> 'b) -> ('a -> 'c) = InLine.inl_compose
    val op before	: ('a * 'b) -> 'a = InLine.inl_before
    val ignore          : 'a -> unit = InLine.inl_ignore
    val gettag		: 'a -> int = InLine.gettag
    val inlnot		: bool -> bool = InLine.inl_not
    val recordSub	: ('a * int) -> 'b = InLine.recordSub
    val raw64Sub	: ('a * int) -> real = InLine.raw64Sub

    val ptreql          : 'a * 'a -> bool = InLine.ptr_eql

    structure Real64 =
      struct
        val op +   : real * real -> real = InLine.real64_add
        val op -   : real * real -> real = InLine.real64_sub
        val op /   : real * real -> real = InLine.real64_div
        val op *   : real * real -> real = InLine.real64_mul
        val op ==   : real * real -> bool = InLine.real64_eql
        val op !=  : real * real -> bool = InLine.real64_neq
        val op >=  : real * real -> bool = InLine.real64_ge
        val op >   : real * real -> bool = InLine.real64_gt
        val op <=  : real * real -> bool = InLine.real64_le
        val op <   : real * real -> bool = InLine.real64_lt
        val ~      : real -> real = InLine.real64_neg
        val abs    : real -> real = InLine.real64_abs

        val min    : real * real -> real  = InLine.real64_min
        val max    : real * real -> real  = InLine.real64_max

	val from_int32 : int32 -> real    = InLine.int32_to_real64
	val from_int : int -> real        = InLine.int_to_real64

	fun floor (x : real) =
	      if InLine.real64_le(~1073741824.0, x)
	      andalso InLine.real64_lt(x, 1073741824.0)
		then Assembly.A.floor x
	      else if InLine.real64_eql(x, x)
		then raise Assembly.Overflow
		else raise Core.Domain (* nan *)

	val signBit : real -> bool = InLine.real64_sgn

	val toBits : real -> word64 = InLine.real64_to_bits
      end

    structure Int =
      struct
	val toInt : int -> int		= InLine.inl_identity
	val fromInt : int -> int	= InLine.inl_identity
	val toLarge			= InLine.int_to_intinf
	val fromLarge			= InLine.intinf_to_int

        val op *    : int * int -> int  = InLine.int_mul
        val op quot : int * int -> int  = InLine.int_quot
        val op rem  : int * int -> int  = InLine.int_rem
        val op div  : int * int -> int  = InLine.int_div
        val op mod  : int * int -> int  = InLine.int_mod
        val op +    : int * int -> int  = InLine.int_add
        val op -    : int * int -> int  = InLine.int_sub
        val ~       : int -> int        = InLine.int_neg
        val andb    : int * int -> int  = InLine.int_andb
        val orb     : int * int -> int  = InLine.int_orb
        val xorb    : int * int -> int  = InLine.int_xorb
        val rshift  : int * word -> int = InLine.int_raw_rshift
        val lshift  : int * word -> int = InLine.int_raw_lshift
        val notb    : int -> int        = InLine.int_notb
        val op <    : int * int -> bool = InLine.int_lt
        val op <=   : int * int -> bool = InLine.int_le
        val op >    : int * int -> bool = InLine.int_gt
        val op >=   : int * int -> bool = InLine.int_ge
        val op =    : int * int -> bool = InLine.int_eql
        val op <>   : int * int -> bool = InLine.int_neq
        val ltu     : int * int -> bool = InLine.int_ltu
        val geu     : int * int -> bool = InLine.int_geu

        val min     : int * int -> int  = InLine.int_min
        val max     : int * int -> int  = InLine.int_max
        val abs     : int -> int = InLine.int_abs

      (* fast add/subtract that does not do overflow checking *)
	val fast_add : int * int -> int = InLine.int_unsafe_add
	val fast_sub : int * int -> int = InLine.int_unsafe_sub
      end

    structure Int32 =
      struct
	val toInt = InLine.int32_to_int
	val fromInt = InLine.int_to_int32
	val toLarge = InLine.int32_to_intinf
	val fromLarge = InLine.intinf_to_int32

        val op *    : int32 * int32 -> int32  = InLine.int32_mul
        val op quot : int32 * int32 -> int32  = InLine.int32_quot
        val op rem  : int32 * int32 -> int32  = InLine.int32_rem
        val op div  : int32 * int32 -> int32  = InLine.int32_div
        val op mod  : int32 * int32 -> int32  = InLine.int32_mod
        val op +    : int32 * int32 -> int32  = InLine.int32_add
        val op -    : int32 * int32 -> int32  = InLine.int32_sub
        val ~       : int32 -> int32          = InLine.int32_neg
        val op <    : int32 * int32 -> bool   = InLine.int32_lt
        val op <=   : int32 * int32 -> bool   = InLine.int32_le
        val op >    : int32 * int32 -> bool   = InLine.int32_gt
        val op >=   : int32 * int32 -> bool   = InLine.int32_ge
        val op =    : int32 * int32 -> bool   = InLine.int32_eql
        val op <>   : int32 * int32 -> bool   = InLine.int32_neq

        val min     : int32 * int32 -> int32  = InLine.int32_min
        val max     : int32 * int32 -> int32  = InLine.int32_max
        val abs     : int32 -> int32          = InLine.int32_abs
      end

    structure Int64 =
      struct
	val toInt = InLine.int64_to_int
	val fromInt = InLine.int_to_int64
	val toLarge = InLine.int64_to_intinf
	val fromLarge = InLine.intinf_to_int64

        val op +    : int64 * int64 -> int64  = InLine.int64_add
        val op -    : int64 * int64 -> int64  = InLine.int64_sub
        val op *    : int64 * int64 -> int64  = InLine.int64_mul
        val op quot : int64 * int64 -> int64  = InLine.int64_quot
        val op rem  : int64 * int64 -> int64  = InLine.int64_rem
        val op div  : int64 * int64 -> int64  = InLine.int64_div
        val op mod  : int64 * int64 -> int64  = InLine.int64_mod
        val ~       : int64 -> int64          = InLine.int64_neg
        val op <    : int64 * int64 -> bool   = InLine.int64_lt
        val op <=   : int64 * int64 -> bool   = InLine.int64_le
        val op >    : int64 * int64 -> bool   = InLine.int64_gt
        val op >=   : int64 * int64 -> bool   = InLine.int64_ge
        val op =    : int64 * int64 -> bool   = InLine.int64_eql
        val op <>   : int64 * int64 -> bool   = InLine.int64_neq

        val min     : int64 * int64 -> int64  = InLine.int64_min
        val max     : int64 * int64 -> int64  = InLine.int64_max
        val abs     : int64 -> int64          = InLine.int64_abs

        val extern : int64 -> word32 * word32 = InLine.int64_to_pair
	val intern : word32 * word32 -> int64 = InLine.int64_from_pair
      end

    structure IntInf =
      struct
	val toInt     : intinf -> int    = InLine.intinf_to_int
	val fromInt   : int -> intinf    = InLine.int_to_intinf
	val toLarge   : intinf -> intinf = InLine.inl_identity
	val fromLarge : intinf -> intinf = InLine.inl_identity
      end

    structure Word =
      struct
	val toLarge : word -> word64	  = InLine.unsigned_word_to_word64
	val toLargeX : word -> word64	  = InLine.signed_word_to_word64
	val fromLarge : word64 -> word	  = InLine.word64_to_word
	val toInt : word -> int		  = InLine.unsigned_word_to_int
	val toIntX : word -> int          = InLine.signed_word_to_int
	val fromInt : int -> word         = InLine.int_to_word
	val toLargeInt : word -> intinf	  = InLine.unsigned_word_to_intinf
	val toLargeIntX : word -> intinf  = InLine.signed_word_to_intinf
	val fromLargeInt : intinf -> word = InLine.intinf_to_word

      (* extra conversions *)
	val toInt32 : word -> int32 = InLine.copy_word_to_int32
	val toWord32 : word -> word32 = InLine.word_to_word32
	val toWord64 : word -> word64 = InLine.word_to_word64
	val fromWord32 : word32 -> word = InLine.word32_to_word

        val orb     : word * word -> word = InLine.word_orb
        val xorb    : word * word -> word = InLine.word_xorb
        val andb    : word * word -> word = InLine.word_andb
        val op *    : word * word -> word = InLine.word_mul
        val op +    : word * word -> word = InLine.word_add
        val op -    : word * word -> word = InLine.word_sub
	val ~       : word -> word        = InLine.word_neg
        val op div  : word * word -> word = InLine.word_div
        val op mod  : word * word -> word = InLine.word_mod
        val op >    : word * word -> bool   = InLine.word_gt
        val op >=   : word * word -> bool   = InLine.word_ge
        val op <    : word * word -> bool   = InLine.word_lt
        val op <=   : word * word -> bool   = InLine.word_le
        val rshift  : word * word -> word = InLine.word_raw_rshift
        val rshiftl : word * word -> word = InLine.word_raw_rshiftl
        val lshift  : word * word -> word = InLine.word_raw_lshift
	val chkLshift  : word * word -> word = InLine.word_lshift
	val chkRshift  : word * word -> word = InLine.word_rshift
	val chkRshiftl : word * word -> word = InLine.word_rshiftl
        val notb    : word -> word = InLine.word_notb

        val min     : word * word -> word  = InLine.word_min
        val max     : word * word -> word  = InLine.word_max
      end

    structure Word8 =
      struct
	val toLarge	  = InLine.unsigned_word8_to_word64
	val toLargeX	  = InLine.signed_word8_to_word64
	val fromLarge	  = InLine.word64_to_word8
	val toInt         = InLine.unsigned_word8_to_int
	val toIntX        = InLine.signed_word8_to_int
	val fromInt       = InLine.int_to_word8
	val toLargeInt    = InLine.unsigned_word8_to_intinf
	val toLargeIntX   = InLine.signed_word8_to_intinf
	val fromLargeInt  = InLine.intinf_to_word8

	local
	(* wrapper that clamps the result of an operation to 0..255.  Note that
         * this wrapper breaks the inlining of Word8 arithmetic!
	 *)
	  fun w8adapt oper args = InLine.word8_andb(oper args, 0wxFF)
	in
        val orb : word8 * word8 -> word8	= InLine.word8_orb
        val xorb : word8 * word8 -> word8	= InLine.word8_xorb
        val andb : word8 * word8 -> word8	= InLine.word8_andb
        val op * : word8 * word8 -> word8	= w8adapt InLine.word8_mul
        val op + : word8 * word8 -> word8	= w8adapt InLine.word8_add
        val op - : word8 * word8 -> word8	= w8adapt InLine.word8_sub
	val ~ : word8 -> word8			= w8adapt InLine.word8_neg
        val op div : word8 * word8 -> word8	= InLine.word8_div
        val op mod : word8 * word8 -> word8	= InLine.word8_mod
        val op > : word8 * word8 -> bool	= InLine.word8_gt
        val op >= : word8 * word8 -> bool	= InLine.word8_ge
        val op < : word8 * word8 -> bool	= InLine.word8_lt
        val op <= : word8 * word8 -> bool	= InLine.word8_le
        val rshift : word8 * word -> word8	= InLine.word8_raw_rshift
        val rshiftl : word8 * word -> word8	= InLine.word8_raw_rshiftl
        val lshift : word8 * word -> word8	= w8adapt InLine.word8_raw_lshift
        val notb : word8 -> word8		= InLine.word8_notb
	val chkRshift : word8 * word -> word8	= InLine.word8_rshift
	val chkRshiftl : word8 * word -> word8	= InLine.word8_rshiftl
	val chkLshift : word8 * word -> word8	= w8adapt InLine.word8_lshift
	end (* local *)

	val min     : word8 * word8 -> word8 = InLine.word8_min
	val max     : word8 * word8 -> word8 = InLine.word8_max
      end

    structure Word32 =
      struct
	val toLarge : word32 -> word64	 = InLine.unsigned_word32_to_word64
	val toLargeX : word32 -> word64	 = InLine.signed_word32_to_word64
	val fromLarge : word64 -> word32 = InLine.word64_to_word32
	val toInt			 = InLine.unsigned_word32_to_int
	val toIntX			 = InLine.signed_word32_to_int
	val fromInt			 = InLine.int_to_word32
	val toLargeInt			 = InLine.unsigned_word32_to_intinf
	val toLargeIntX			 = InLine.signed_word32_to_intinf
	val fromLargeInt		 = InLine.intinf_to_word32

      (* extra function to support the Int64/Word64 types *)
	val fromInt32 : int32 -> word32 = InLine.copy_int32_to_word32

        val orb : word32 * word32 -> word32	 = InLine.word32_orb
        val xorb : word32 * word32 -> word32	 = InLine.word32_xorb
        val andb : word32 * word32 -> word32	 = InLine.word32_andb
        val op * : word32 * word32 -> word32	 = InLine.word32_mul
        val op + : word32 * word32 -> word32	 = InLine.word32_add
        val op - : word32 * word32 -> word32	 = InLine.word32_sub
	val ~ : word32 -> word32		 = InLine.word32_neg
        val op div : word32 * word32 -> word32	 = InLine.word32_div
        val op mod : word32 * word32 -> word32	 = InLine.word32_mod
        val op > : word32 * word32 -> bool	 = InLine.word32_gt
        val op >= : word32 * word32 -> bool	 = InLine.word32_ge
        val op < : word32 * word32 -> bool	 = InLine.word32_lt
        val op <= : word32 * word32 -> bool	 = InLine.word32_le
        val rshift : word32 * word -> word32     = InLine.word32_raw_rshift
        val rshiftl : word32 * word -> word32    = InLine.word32_raw_rshiftl
        val lshift : word32 * word -> word32     = InLine.word32_raw_lshift
        val notb : word32 -> word32              = InLine.word32_notb
	val chkRshift  : word32 * word -> word32 = InLine.word32_rshift
	val chkRshiftl : word32 * word -> word32 = InLine.word32_rshiftl
	val chkLshift  : word32 * word -> word32 = InLine.word32_lshift

        val min     : word32 * word32 -> word32  = InLine.word32_min
        val max     : word32 * word32 -> word32  = InLine.word32_max
      end

    structure Word64 =
      struct
        val extern : word64 -> word32 * word32   = InLine.word64_to_pair
	val intern : word32 * word32 -> word64   = InLine.word64_from_pair

	val toLarge : word64 -> word64		 = InLine.inl_identity
	val toLargeX : word64 -> word64		 = InLine.inl_identity
	val fromLarge : word64 -> word64	 = InLine.inl_identity
	val toInt : word64 -> int		 = InLine.unsigned_word64_to_int
	val toIntX : word64 -> int		 = InLine.signed_word64_to_int
	val fromInt : int -> word64		 = InLine.int_to_word64
	val toLargeInt : word64 -> intinf	 = InLine.unsigned_word64_to_intinf
	val toLargeIntX : word64 -> intinf	 = InLine.signed_word64_to_intinf
	val fromLargeInt : intinf -> word64	 = InLine.intinf_to_word64

        val op + : word64 * word64 -> word64	 = InLine.word64_add
        val op - : word64 * word64 -> word64	 = InLine.word64_sub
        val op * : word64 * word64 -> word64	 = InLine.word64_mul
        val op div : word64 * word64 -> word64	 = InLine.word64_div
        val op mod : word64 * word64 -> word64	 = InLine.word64_mod
	val ~ : word64 -> word64		 = InLine.word64_neg
        val orb : word64 * word64 -> word64	 = InLine.word64_orb
        val xorb : word64 * word64 -> word64	 = InLine.word64_xorb
        val andb : word64 * word64 -> word64	 = InLine.word64_andb
	val chkLshift  : word64 * word -> word64 = InLine.word64_lshift
	val chkRshift  : word64 * word -> word64 = InLine.word64_rshift
	val chkRshiftl : word64 * word -> word64 = InLine.word64_rshiftl
        val rshift : word64 * word -> word64     = InLine.word64_raw_rshift
        val rshiftl : word64 * word -> word64    = InLine.word64_raw_rshiftl
        val lshift : word64 * word -> word64     = InLine.word64_raw_lshift
        val notb : word64 -> word64              = InLine.word64_notb

        val op > : word64 * word64 -> bool	 = InLine.word64_gt
        val op >= : word64 * word64 -> bool	 = InLine.word64_ge
        val op < : word64 * word64 -> bool	 = InLine.word64_lt
        val op <= : word64 * word64 -> bool	 = InLine.word64_le

        val min     : word64 * word64 -> word64  = InLine.word64_min
        val max     : word64 * word64 -> word64  = InLine.word64_max

      end

    structure Char =
      struct
        val maxOrd = 255

        exception Chr = Core.Chr

	val chr : int -> char = InLine.inl_chr
        val ord : char -> int = InLine.inl_ord

        val (op <)  : (char * char) -> bool = InLine.char_lt
        val (op <=) : (char * char) -> bool = InLine.char_le
        val (op >)  : (char * char) -> bool = InLine.char_gt
        val (op >=) : (char * char) -> bool = InLine.char_ge
      end

    structure PolyArray =
      struct
 	val newArray0 : unit -> 'a array = InLine.newArray0
        val array     : int * 'a -> 'a array = InLine.mkarray
        val length    : 'a array -> int = InLine.seq_length
        val sub       : 'a array * int -> 'a = InLine.arr_unsafe_sub
        val chkSub    : 'a array * int -> 'a = InLine.arr_sub
        val update    : 'a array * int * 'a -> unit = InLine.arr_unsafe_update
        val chkUpdate : 'a array * int * 'a -> unit = InLine.arr_update
	val getData   : 'a array -> 'b = InLine.seq_data
      end

    structure PolyVector =
      struct
        val length    : 'a vector -> int = InLine.seq_length
        val sub       : 'a vector * int -> 'a = InLine.vec_unsafe_sub
        val chkSub    : 'a vector * int -> 'a = InLine.vec_sub
	val getData   : 'a vector -> 'b = InLine.seq_data
      end

    structure Real64Array =
      struct
	type array = Assembly.A.real64array
(* FIXME: this produces an incorrect header object (i.e., it is tagged as
 * a polymorphic array, instead of an array of 64-bit objects.
 *)
 	val newArray0 : unit -> array = InLine.newArray0
        val length    : array -> int = InLine.seq_length
        val sub       : array * int -> real = InLine.real64_arr_unsafe_sub
        val chkSub    : array * int -> real = InLine.real64_arr_sub
        val update    : array * int * real -> unit = InLine.real64_arr_unsafe_update
        val chkUpdate : array * int * real -> unit = InLine.real64_arr_update
	val getData   : array -> 'b = InLine.seq_data
      end

  (** NOTE: we are currently using polymorphic vectors to implement the Real64Vector
   ** structure.
   **)
    structure Real64Vector =
      struct
        val length    : real vector -> int = InLine.seq_length
        val sub       : real vector * int -> real = InLine.vec_unsafe_sub
        val chkSub    : real vector * int -> real = InLine.vec_sub
	val getData   : real vector -> 'b = InLine.seq_data
      end

    structure Word8Array =
      struct
	type array = word8array
(* FIXME: this produces an incorrect header object (i.e., it is tagged as
 * a polymorphic array, instead of an array of bytes
 *)
 	val newArray0 : unit -> array = InLine.newArray0
        val length    : array -> int = InLine.seq_length
        val sub       : array * int -> word8 = InLine.word8_arr_unsafe_sub
        val chkSub    : array * int -> word8 = InLine.word8_arr_sub
        val update    : array * int * word8 -> unit = InLine.word8_arr_unsafe_update
        val chkUpdate : array * int * word8 -> unit = InLine.word8_arr_update
	val getData   : array -> 'a = InLine.seq_data
      end

    (* now the real version with all values *)
    structure Word8Vector =
      struct
        type vector = word8vector
	val create : int -> vector = InLine.cast Assembly.A.create_s
	val length    : vector -> int = InLine.seq_length
        val sub       : vector * int -> word8 = InLine.word8_vec_unsafe_sub
        val chkSub    : vector * int -> word8 = InLine.word8_vec_sub
        val update    : vector * int * word8 -> unit = InLine.word8_vec_unsafe_update
	val getData   : vector -> 'a = InLine.seq_data
      end

    structure CharArray =		(* full *)
      struct
        type array = chararray
(* FIXME: this produces an incorrect header object (i.e., it is tagged as
 * a polymorphic array, instead of an array of bytes
 *)
	val newArray0 : unit -> array = InLine.newArray0
	val create    : int -> array = InLine.cast Assembly.A.create_b
	val length    : array -> int = InLine.seq_length
	val chkSub    : (array * int) -> char = InLine.char_arr_sub
	val chkUpdate : (array * int * char) -> unit = InLine.char_arr_update
	val sub       : (array * int) -> char = InLine.char_arr_unsafe_sub
	val update    : (array * int * char) -> unit = InLine.char_arr_unsafe_update
	val getData   : array -> 'a = InLine.seq_data
      end

    structure CharVector =
      struct
	val length    : string -> int			= InLine.seq_length
	val chkSub    : (string * int) -> char		= InLine.char_vec_sub
	val sub       : (string * int) -> char		= InLine.char_vec_unsafe_sub
	val update    : (string * int * char) -> unit	= InLine.char_vec_unsafe_update
	val getData   : string -> 'a = InLine.seq_data
      end

    structure Pointer =
      struct
	type t = c_pointer
	val toWord32 = InLine.cptr_to_word32
	val fromWord32 = InLine.word32_to_cptr
      end

   end  (* structure InlineT *)

end (* local *)
