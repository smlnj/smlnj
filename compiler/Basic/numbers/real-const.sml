(* real-const.sml
 *
 * A common representation of typed real literals to use throughout the
 * different intermediate representations (from Absyn to CPS).
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure RealConst : sig

    type 'ty t = {
	rval : RealLit.t,	(* the value *)
	ty : 'ty		(* the "type" of the value *)
      }

    val toString : 'ty t -> string

    val fmt : ('ty -> string) -> 'ty t -> string

  (* do two constants have equal values? Note that this test ignores the types! *)
    val same : 'ty t * 'ty t -> bool

  end = struct

    type 'ty t = {rval : RealLit.t, ty : 'ty}

    fun toString {rval, ty} = RealLit.toString rval

    fun fmt tyToString {rval, ty} = concat[RealLit.toString rval, ":", tyToString ty]

    fun same (a : 'ty t, b : 'ty t) = RealLit.same(#rval a, #rval b)

  end


