(* c-type.sml
 *
 * COPYRIGHT (c) 1999 Bell Labs, Lucent Technologies
 *
 * A representation of C Types for specifying the arguments and results
 * of C function calls.
 *)


structure CType =
  struct

    datatype c_type
      = C_void
      | C_float
      | C_double
      | C_long_double
      | C_unsigned of c_int
      | C_signed of c_int
      | C_PTR
      | C_ARRAY of (c_type * int)
      | C_STRUCT of c_type list
      | C_UNION of c_type list

    and c_int
      = I_char
      | I_short
      | I_int
      | I_long
      | I_long_long


  (* multiple calling conventions on a single architecture *)
    type calling_convention = string

  (* prototype describing C function *)
    type c_proto = {
	conv : calling_convention,
	retTy : c_type,
	paramTys : c_type list
      }

    (* eliminate aggregates in a C type *)
    fun flattenCTy cTy = (case cTy
        of (C_STRUCT cTys |
	    C_UNION cTys ) => List.concat (List.map flattenCTy cTys)
	 | C_ARRAY (cTy, n) => List.tabulate (n, fn _ => cTy)
	 | cTy => [cTy])


  end
