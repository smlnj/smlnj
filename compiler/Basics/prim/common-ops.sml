(* common-ops.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * Primitive operators that are common between Absyn and FLINT.  Note that the
 * primitive operators defined in ArithOps are also common, but since they are
 * also used in the CPS IR, they are omitted from this module.
 *)

structure CommonOps =
  struct

    datatype t
      = FSGN of int                     (* E: floating point sign test *)
      | TESTU of int * int              (* E: word to int conversions, e.g. testu_31_31 *)
      | TEST of int * int               (* E: int to smaller int conversions, e.g. test_32_31 *)
      | TRUNC of int * int              (* E: int to smaller int/word truncation, e.g. trunc_32_31 *)
      | EXTEND of int * int             (* E: sign-extend word/int to larger int *)
      | COPY of int * int               (* E: conversions, e.g. copy_32_32_ii *)
      | TEST_INF of int            	(* intinf conversions; e.g., test_inf_31 *)
      | TRUNC_INF of int           	(* intinf truncations; e.g., trunc_inf_31 *)
      | EXTEND_INF of int          	(* sign-extend word/int to intinf *)
      | COPY_INF of int            	(* zero-extend word/int to intinf *)
      | REAL_TO_INT of {                (* E: floor, round *)
            floor : bool,
            from : int, to : int
          }
      | INT_TO_REAL of {                (* E: real, real32 *)
            from : int, to : int
          }
      | NUMSUBSCRIPT of NumKind.t       (* E: unchecked numeric array subscript *)
      | NUMSUBSCRIPTV of NumKind.t      (* E: unchecked numeric vector subscript *)
      | NUMUPDATE of NumKind.t          (* E: unchecked numeric array update *)
      | SUBSCRIPT                       (* E: unchecked polymorphic array subscript *)
      | SUBSCRIPTV                      (* E: unchecked polymorphic vector subscript *)
      | UPDATE                          (* E: unchecked polymorphic-array update (maybe boxed) *)
      | UNBOXEDUPDATE                   (* unchecked polymorphic-array update (unboxed) *)
      | LENGTH                          (* E: vector, string, array, ... length *)
      | PTREQL | PTRNEQ                 (* E: pointer equality *)
      | POLYEQL | POLYNEQ               (* E: polymorphic equality *)
      | BOXED | UNBOXED                 (* E: boxity tests *)
      | CAST                            (* E: cast *)
      | REAL_TO_BITS of int             (* E: bitcast real of the given size to
                                         * a word of the same size
                                         *)
      | BITS_TO_REAL of int             (* E: bitcast word of the given size to
                                         * a real of the same size
                                         *)
      | GETHDLR | SETHDLR               (* E: get/set exn handler pointer *)
      | GETVAR | SETVAR                 (* E: get/set var register *)
      | CALLCC | CAPTURE | THROW        (* E: continuation operations *)
      | ISOLATE                         (* E: isolating a function *)
      | MAKEREF                         (* E: allocate a ref cell *)
      | DEREF                           (* E: dereferencing *)
      | ASSIGN                          (* E: ref cell assignment (maybe boxed) *)
      | UNBOXEDASSIGN                   (* ref cell assignment (unboxed) *)
      | OBJLENGTH                       (* E: length of arbitrary heap object *)
      | GETTAG                          (* E: extract the tag portion of an
                                         * object's descriptor as an ML int
                                         *)
      | MKSPECIAL                       (* E: make a special object *)
      | SETSPECIAL                      (* E: set the state of a special object *)
      | GETSPECIAL                      (* E: get the state of a special object *)
      | NEW_ARRAY0                      (* E: allocate zero-length array *)
      | GET_SEQ_DATA                    (* E: get data pointer from arr/vec header *)
      | SUBSCRIPT_REC                   (* E: record subscript operation *)
      | SUBSCRIPT_RAW64                 (* E: raw64 subscript operation *)
    (* Primops to support C FFI. *)
      | CPTR_TO_WORD			(* E: cast c_pointer to address-sized word type *)
      | WORD_TO_CPTR			(* E: case address-sized word type to c_pointer *)
      | RAW_LOAD of NumKind.t           (* E: load from arbitrary memory location *)
      | RAW_STORE of NumKind.t          (* E: store to arbitrary memory location *)
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
      (* Allocate uninitialized raw storage on the heap *)
      | RAW_RECORD of { align : int }  (* E: *)

    and ccall_type
      = CCI32           (* passed as int32 *)
      | CCI64           (* passed as int64 *)
      | CCR64           (* passed as real64 *)
      | CCML            (* passed as Unsafe.Object.object *)

    fun toString p = let
          fun cvtFromTo (prefix, from, to) = concat[
                  prefix, "_", Int.toString from, "_", Int.toString to
                ]
          in
            case p
             of FSGN sz => "fsgn_" ^ Int.toString sz
              | TESTU(from, to) => cvtFromTo ("testu", from, to)
              | TEST(from, to) => cvtFromTo ("test", from, to)
              | TRUNC(from, to) => cvtFromTo ("trunc", from, to)
              | EXTEND(from, to) => cvtFromTo ("extend", from, to)
              | COPY(from, to) => cvtFromTo ("copy", from, to)
              | TEST_INF sz => "test_inf_" ^ Int.toString sz
              | TRUNC_INF sz => "trunc_inf_" ^ Int.toString sz
              | EXTEND_INF sz => concat ["extend_", Int.toString sz, "_inf"]
              | COPY_INF sz =>  concat ["copy_", Int.toString sz, "_inf"]
              | REAL_TO_INT{floor, from, to} => concat [
                    if floor then "floor_real" else "round_real",
                    Int.toString from, "_to_int", Int.toString to
                  ]
              | INT_TO_REAL{from, to} => concat [
                    "int", Int.toString from, "_to_real", Int.toString to
                  ]
              | NUMSUBSCRIPT kind => "numsubscript_" ^ NumKind.toString kind
              | NUMSUBSCRIPTV kind => "numsubscriptv_" ^ NumKind.toString kind
              | NUMUPDATE kind => "numupdate_" ^ NumKind.toString kind
              | SUBSCRIPT => "subscript"
              | SUBSCRIPTV => "subscriptv"
              | UPDATE => "update"
              | UNBOXEDUPDATE => "unboxedupdate"
              | LENGTH => "length"
              | PTREQL => "ptreql"
              | PTRNEQ => "ptrneq"
              | POLYEQL => "polyeql"
              | POLYNEQ => "polyneq"
              | BOXED => "boxed"
              | UNBOXED => "unboxed"
              | CAST => "cast"
              | REAL_TO_BITS sz => "real_to_bits_" ^ Int.toString sz
              | BITS_TO_REAL sz => "bits_to_real_" ^ Int.toString sz
              | GETHDLR => "gethdlr"
              | SETHDLR => "sethdlr"
              | GETVAR => "getvar"
              | SETVAR => "setvar"
              | CALLCC => "callcc"
              | CAPTURE => "capture"
              | THROW => "throw"
              | ISOLATE => "isolate"
              | MAKEREF => "makeref"
              | DEREF => "deref"
              | ASSIGN => "assign"
              | UNBOXEDASSIGN => "unboxedAssign"
              | OBJLENGTH => "objlength"
              | GETTAG => "gettag"
              | MKSPECIAL => "mkspecial"
              | SETSPECIAL => "setspecial"
              | GETSPECIAL => "getspecial"
              | NEW_ARRAY0 => "newarray0"
              | GET_SEQ_DATA => "getseqdata"
              | SUBSCRIPT_REC => "subscriptrec"
              | SUBSCRIPT_RAW64 => "subscriptraw64"
              | CPTR_TO_WORD => "cptr_to_word"
              | WORD_TO_CPTR => "word_to_cptr"
              | RAW_LOAD nk => concat ["raw_load(", NumKind.toString nk, ")"]
              | RAW_STORE nk => concat ["raw_store(", NumKind.toString nk, ")"]
              | RAW_CCALL _ => "raw_ccall"
              | RAW_RECORD{align} => concat ["raw", Int.toString align, "_record"]
            (* end case *)
          end

  end
