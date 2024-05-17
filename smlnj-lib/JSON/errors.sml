(* errors.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * The error exceptions used in JSONDecode and JSONUtil.
 *)

structure Errors =
  struct

    (* exceptions used as errors *)
    exception Failure of string * JSON.value
    exception NotNull of JSON.value
    exception NotBool of JSON.value
    exception NotInt of JSON.value
    exception NotNumber of JSON.value
    exception NotString of JSON.value
    exception NotObject of JSON.value
    exception FieldNotFound of JSON.value * string
    exception NotArray of JSON.value
    exception ArrayBounds of JSON.value * int
    exception ElemNotFound of JSON.value

    (* map the above exceptions to a message string; we use `General.exnMessage`
     * for other exceptions.
     *)
    fun exnMessage exn = let
	  fun v2s (JSON.ARRAY _) = "array"
	    | v2s (JSON.BOOL false) = "'false'"
	    | v2s (JSON.BOOL true) = "'true'"
	    | v2s (JSON.FLOAT _) = "number"
	    | v2s (JSON.INT _) = "number"
	    | v2s JSON.NULL = "'null'"
	    | v2s (JSON.OBJECT _) = "object"
	    | v2s (JSON.STRING _) = "string"
	  in
	    case exn
	     of Failure(msg, v) => String.concat["Failure: ", msg]
              | NotNull v => String.concat[
		    "expected 'null', but found ", v2s v
		  ]
              | NotBool v => String.concat[
		    "expected boolean, but found ", v2s v
		  ]
	      | NotInt(JSON.FLOAT _) => "expected integer, but found floating-point number"
	      | NotInt v => String.concat[
		    "expected integer, but found ", v2s v
		  ]
	      | NotNumber v => String.concat[
		    "expected number, but found ", v2s v
		  ]
	      | NotString v => String.concat[
		    "expected string, but found ", v2s v
		  ]
	      | NotObject v => String.concat[
		    "expected object, but found ", v2s v
		  ]
	      | FieldNotFound(v, fld) => String.concat[
		    "no definition for field \"", fld, "\" in object"
		  ]
	      | NotArray v => String.concat[
		    "expected array, but found ", v2s v
		  ]
              | ArrayBounds(_, i) => String.concat[
                    "index ", Int.toString i, " out of bounds for array"
                  ]
	      | ElemNotFound v => String.concat[
		    "no matching element found in ", v2s v
		  ]
	      | _ => General.exnMessage exn
	    (* end case *)
	  end

  end
