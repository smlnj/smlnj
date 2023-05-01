(* int-const.sml
 *
 * A common representation of typed integer literals to use throughout the
 * different intermediate representations (from Absyn to CPS).
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure IntConst : sig

    type 'ty t = {
	ival : IntInf.int,	(* the value *)
	ty : 'ty		(* the "type" of the value *)
      }
    (* In the front end, the ty component is a primitive type (ty' = Types.ty),
     * while in FLINT, the ty component is a size (ty' = int).
     *)

    val toString : 'ty t -> string

    val fmt : ('ty -> string) -> 'ty t -> string

  (* do two constants have equal values? *)
    val same : 'ty t * 'ty t -> bool

  (* compare the values of two constants *)
    val compare : 'ty t * 'ty t -> order

  end = struct

    type 'ty t = {ival : IntInf.int, ty : 'ty}

    fun toString {ival, ty} = IntInf.toString ival

    fun fmt tyToString {ival, ty} = concat[IntInf.toString ival, ":", tyToString ty]

    fun same (a : 'ty t, b : 'ty t) = (#ival a = #ival b)

    fun compare (a : 'ty t, b : 'ty t) = IntInf.compare (#ival a, #ival b)

  end  (* structure IntConst *)
