(* inline-ops.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * These are primitive operations that are expanded into lambda code in
 * terms of other operators (see `FLINT/trans/transprim.sml`).
 *)

structure InlineOps =
  struct

    datatype numkind = datatype NumKind.t

    datatype t
      (* integer/word operations *)
      = DIV of numkind			(* integer div (round toward -∞) *)
      | MOD of numkind			(* integer mod *)
      | QUOT of numkind			(* integer/word quot (round toward 0) *)
      | REM of numkind			(* integer/word rem *)
      | LSHIFT of numkind		(* left shift *)
      | RSHIFT of numkind		(* right shift *)
      | RSHIFTL of numkind		(* right shift logical *)
      | CNTZ of numkind                 (* count zeros *)
      | CNTO of numkind                 (* count ones (aka population count) *)
      | CNTLZ of numkind                (* count leading zeros *)
      | CNTLO of numkind                (* count leading ones *)
      | CNTTZ of numkind                (* count trailing zeros *)
      | CNTTO of numkind                (* count trailing ones *)
      | IS_POW2 of numkind              (* is power of two? *)
      | CEIL_LOG2 of numkind            (* log2 rounded up *)
      | MIN of numkind			(* integer/word min *)
      | MAX of numkind			(* integer/word max *)
      | ABS of numkind			(* integer abs *)
      | TEST_INF of int            	(* intinf conversions; e.g., test_inf_31 *)
      | TRUNC_INF of int           	(* intinf truncations; e.g., trunc_inf_31 *)
      | EXTEND_INF of int          	(* sign-extend word/int to intinf *)
      | COPY_INF of int            	(* zero-extend word/int to intinf *)
      | CHR				(* checked int to char conversion *)
      (* polymorphic array/vector operations *)
      | MKARRAY				(* polymorphic array creation *)
      | SUBSCRIPT			(* checked polymorphic array subscript *)
      | SUBSCRIPTV			(* checked polymorphic vector subscript *)
      | UPDATE				(* polymorphic array update (maybe boxed) *)
      | UNBOXEDUPDATE                   (* polymorphic array update where the value is *)
                                        (* known to be unboxed *)
      (* monomorphic (numeric) array/vector operations *)
      | NUMSUBSCRIPT of numkind		(* checked numeric array subscript *)
      | NUMSUBSCRIPTV of numkind        (* checked numeric vector subscript *)
      | NUMUPDATE of numkind		(* checked numeric array update *)
      (* miscellaneous Basis functions *)
      | NOT				(* Bool.not *)
      | COMPOSE				(* compose "op o"  operator *)
      | BEFORE				(* "before" operator *)
      | IGNORE				(* "ignore" function *)
      | IDENTITY			(* Fn.id *)
(* possible extensions
      | LIST_NULL                       (* List.null *)
      | LIST_HD
      | LIST_TL
      | LIST_GETITEM
      | OPTION_VALOF
      | OPTION_ISSOME
      | OPTION_ISONNE
      | OPTION_GETOPT                   (* `Option.getOpt` *)

 *)
      (* host-machine properties *)
      | HOST_WORD_SIZE
      | HOST_BIG_ENDIAN

    (* string representation of the primop *)
    local
      fun withKind (p, nk) = concat[p, "_", NumKind.toString nk]
    in
    fun toString p = (case p
           of DIV nk => withKind ("div", nk)
            | MOD nk => withKind ("mod", nk)
            | QUOT nk => withKind ("quot", nk)
            | REM nk => withKind ("rem", nk)
            | LSHIFT nk => withKind ("shiftLeft", nk)
            | RSHIFT nk => withKind ("shiftRight", nk)
            | RSHIFTL nk => withKind ("lShiftRight", nk)
            | CNTZ nk => withKind ("cntZeros", nk)
            | CNTO nk => withKind ("cntOnes", nk)
            | CNTLZ nk => withKind ("cntLeadingZeros", nk)
            | CNTLO nk => withKind ("cntLeadingOnes", nk)
            | CNTTZ nk => withKind ("cntTrailingZeros", nk)
            | CNTTO nk => withKind ("cntTrailingOnes", nk)
            | IS_POW2 nk => withKind ("isPowerOf2", nk)
            | CEIL_LOG2 nk => withKind ("ceilLog2", nk)
            | MIN nk => withKind ("min", nk)
            | MAX nk => withKind ("max", nk)
            | ABS nk => withKind ("abs", nk)
            | CHR => "chr"
            | MKARRAY => "mkArray"
            | SUBSCRIPT => "subscript"
            | SUBSCRIPTV => "vecSubscript"
            | UPDATE => "update"
            | UNBOXEDUPDATE => "unboxedUpdate"
            | NUMSUBSCRIPT nk => withKind ("numSubscript", nk)
            | NUMSUBSCRIPTV nk => withKind ("numSubscriptv", nk)
            | NUMUPDATE nk => withKind ("numUpdate", nk)
            | NOT => "not"
            | COMPOSE => "compose"
            | BEFORE => "before"
            | IGNORE => "ignore"
            | IDENTITY => "identity"
            | HOST_WORD_SIZE => "hostWordSize"
            | HOST_BIG_ENDIAN => "hostBigEndian"
          (* end case *))
    end (* local *)

    (* does the primop have an effect; i.e., can it raise an exception
     * or does it modify mutable memory.  Primops that do not have effects
     * can be dead-code eliminated, but they still might not be referentially
     * transparent.
     *)
    fun hasEffect p = (case p
           of DIV _ => true
            | MOD _ => true
            | QUOT _ => true
            | REM _ => true
            | CHR => true
            | SUBSCRIPT => true
            | SUBSCRIPTV => true
            | UPDATE => true
            | UNBOXEDUPDATE => true
            | NUMSUBSCRIPT _ => true
            | NUMSUBSCRIPTV _ => true
            | NUMUPDATE _ => true
            | _ => false
          (* end case *))

  end
