(* primop.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Representation of primitive operations in the front-end.  These are
 * bound to visible identifiers in Semant/prim/primop-bindings.sml.
 *)

structure Primop : PRIMOP =
  struct

  (* numkind includes kind and number of bits *)
    datatype numkind
      = INT of int
      | UINT of int
      | FLOAT of int
(* QUESTION: what about IntInf.int? *)

    datatype arithop = datatype ArithOps.arithop

    datatype pureop = datatype ArithOps.pureop

    datatype cmpop = datatype ArithOps.cmpop

(* TODO: define a separate type of "inline primops" and have a single
 * "INLINE of inlineop" constructor in primop.
 *)

  (* datatype primop:
   * Various primitive operations. Those that are designated "inline" (L:) in
   * the comments are expanded into lambda code in terms of other operators,
   * as are the "checked=true" versions of NUMSUBSCRIPT and NUMUPDATE (L?:).
   * "Environmental" primops (occurring in the Inline structure) are indicated
   * by "E:" in the comment.
   *
   * See dev-notes/conversions.md for an explanation of the conversion operators.
   *)
    datatype primop
      = IARITH of {				(* E: integer arithmetic ops *)
	    oper : arithop, sz : int		(* kind = INT sz *)
	  }
      | PURE_ARITH of {				(* E: arithmetic ops *)
	    oper : pureop, kind : numkind
	  }
      | INLDIV of numkind			(* E: integer div *)
      | INLMOD of numkind			(* E: integer mod *)
      | INLQUOT of numkind			(* E: integer/word quot *)
      | INLREM of numkind			(* E: integer/word rem *)
      | INLLSHIFT of numkind			(* E: left shift *)
      | INLRSHIFT of numkind			(* E: right shift *)
      | INLRSHIFTL of numkind			(* E: right shift logical *)
      | CMP of {oper: cmpop, kind: numkind}	(* E: generic compare *)
      | FSGN of int				(* E: floating point sign test *)
      | INLCHR					(* E: inline int to char conversion *)
      | TESTU of int * int         		(* E: word to int conversions, e.g. testu_31_31 *)
      | TEST of int * int          		(* E: int to smaller int conversions, e.g. test_32_31 *)
      | TRUNC of int * int        		(* E: int to smaller int/word truncation, e.g. trunc_32_31 *)
      | EXTEND of int * int        		(* E: sign-extend word/int to larger int *)
      | COPY of int * int          		(* E: conversions, e.g. copy_32_32_ii *)
      | TEST_INF of int            		(* E: intinf conversions; e.g., test_inf_31 *)
      | TRUNC_INF of int           		(* E: intinf truncations; e.g., trunc_inf_31 *)
      | EXTEND_INF of int          		(* E: sign-extend word/int to intinf *)
      | COPY_INF of int            		(* E: conversions to intinf; e.g., copy_8_inf *)
      | REAL_TO_INT of {			(* E: floor, round *)
	    floor: bool, from: int, to: int
	  }
      | INT_TO_REAL of {			(* E: real, real32 *)
	    from: int, to: int
	  }
      | NUMSUBSCRIPT of numkind			(* E: ordof, etc. *)
      | NUMSUBSCRIPTV of numkind		(* E: ordof, etc. *)
      | NUMUPDATE of numkind			(* E: store, etc. *)
      | INLNUMSUBSCRIPT of numkind		(* E: L: ordof, etc. *)
      | INLNUMSUBSCRIPTV of numkind		(* E: L: ordof, etc. *)
      | INLNUMUPDATE of numkind			(* E: L: store, etc. *)
      | SUBSCRIPT                  		(* E: polymorphic array subscript *)
      | SUBSCRIPTV				(* E: poly vector subscript *)
      | INLSUBSCRIPT				(* E: L: poly array subscript *)
      | INLSUBSCRIPTV				(* E: L: poly vector subscript *)
      | INLMKARRAY				(* E: L: poly array creation *)
      | PTREQL | PTRNEQ				(* E: pointer equality *)
      | POLYEQL | POLYNEQ			(* E: polymorphic equality *)
      | BOXED | UNBOXED				(* E: boxity tests *)
      | LENGTH					(* E: vector, string, array, ... length *)
      | OBJLENGTH				(* E: length of arbitrary heap object *)
      | CAST					(* E: cast *)
      | GETHDLR | SETHDLR			(* E: get/set exn handler pointer *)
      | GETVAR | SETVAR				(* E: get/set var register *)
      | MAKEREF					(* E: allocate a ref cell *)
      | CALLCC | CAPTURE | THROW		(* E: continuation operations *)
      | ISOLATE					(* E: isolating a function *)
      | DEREF					(* E: dereferencing *)
      | ASSIGN					(* E: assignment *)
      | UPDATE					(* E: array or reference update (maybe boxed) *)
      | INLUPDATE				(* E: L: array update (maybe boxed) *)
      | UNBOXEDUPDATE				(* E: update array of integers WITH tags
						 * removed by Zhong, put back by Matthias
						 * (see FLINT/trans/primopmap.sml) *)
      | GETTAG					(* E: extract the tag portion of an
						 * object's descriptor as an ML int *)
      | MKSPECIAL				(* E: make a special object *)
      | SETSPECIAL				(* E: set the state of a special object *)
      | GETSPECIAL				(* E: get the state of a special object *)
      | INLMIN of numkind			(* E: L: min *)
      | INLMAX of numkind			(* E: L: max *)
      | INLABS of numkind			(* E: L: abs *)
      | INLNOT					(* E: L: bool not operator *)
      | INLCOMPOSE				(* E: L: compose "op o"  operator *)
      | INLBEFORE				(* E: L: "before" operator *)
      | INLIGNORE				(* E: L: "ignore" function *)
      | INLIDENTITY				(* E: polymorphic identity *)
    (* primops to support new array representations *)
      | NEW_ARRAY0				(* E: allocate zero-length array header *)
      | GET_SEQ_DATA				(* E: get data pointer from arr/vec header *)
      | SUBSCRIPT_REC				(* E: record subscript operation *)
      | SUBSCRIPT_RAW64				(* E: raw64 subscript operation *)
      | INTERN64				(* E: convert a pair of word32 values to
						 * a 64-bit number.
						 *)
      | EXTERN64				(* E: convert a 64-bit number to a pair
						 * of word32 values.
						 *)
    (* Primops to support C FFI. *)
      | RAW_LOAD of numkind			(* E: load from arbitrary memory location *)
      | RAW_STORE of numkind			(* E: store to arbitrary memory location *)
    (* E: make a call to a C-function;
     * The primop carries C function prototype information and specifies
     * which of its (ML-) arguments are floating point. C prototype
     * information is for use by the backend, ML information is for
     * use by the CPS converter. *)
      | RAW_CCALL of {
	    c_proto: CTypes.c_proto,
	    ml_args: ccall_type list,
	    ml_res_opt: ccall_type option,
	    reentrant: bool
	  } option
   (* Allocate uninitialized storage on the heap.
    * The record is meant to hold short-lived C objects, i.e., they
    * are not ML pointers.  The representation is
    * the same as RECORD with tag tag_raw or tag_raw64.
    *)
      | RAW_RECORD of { align64 : bool }  (* E: *)

    (* non-environmental primops (not found in Inline) *)
      | UNBOXEDASSIGN			(* assignment to integer reference *)

      | WCAST				(* ? *)
      | MARKEXN				(* mark an exception value with a string *)

      | INL_ARRAY			(* L: polymorphic array allocation *)
      | INL_VECTOR			(* L: polymorphic vector allocation *)
      | INL_MONOARRAY of numkind	(* L: monomorphic array allocation *)
      | INL_MONOVECTOR of numkind	(* L: monomorphic vector allocation *)

      | MKETAG				(* make a new exception tag *)
      | WRAP				(* box a value by wrapping it *)
      | UNWRAP				(* unbox a value by unwrapping it *)
      | PTR_TO_WORD			(* cast c_pointer to address-sized word type *)
      | WORD_TO_PTR			(* case address-sized word type to c_pointer *)
      | REAL_TO_BITS of int		(* cast real of the given size to a word of
					 * the same size
					 *)

    and ccall_type =
	CCI32 |				(* passed as int32 *)
	CCI64 |				(* int64, currently unused *)
	CCR64 |				(* passed as real64 *)
	CCML				(* passed as Unsafe.Object.object *)

  end  (* structure PrimOp *)
