(* lambdavar.sig
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature LAMBDA_VAR =
  sig

    eqtype lvar

  (* returns true if there is a name assigned to the lvar *)
    val lvarIsNamed : lvar -> bool

    val toString: lvar-> string
    val prLvar : lvar -> string  (* obsolete alias of toString *)

  (* `sameName (lv1, lv2)` sets the name of the first variable to the
   * second's name, if defined.  If `lv2` does not have a name, then
   * it sets the second's name to the first, if defined.  Otherwise,
   * it has no effect if neither variable has a name.
   *)
    val sameName : lvar * lvar -> unit

  (* create a fresh, unamed, lvar *)
    val mkLvar : unit -> lvar
    val nextLvar : unit -> lvar
    val diff : lvar * lvar -> int

    val dupLvar : lvar -> lvar
    val namedLvar : Symbol.symbol -> lvar
    val lvarSym : lvar -> Symbol.symbol option
    val lvarName : lvar -> string

  (* reset the unique id generator and clear the name table *)
    val clear : unit -> unit

  (* conversion between the abstract lvar type and the unique integer
   * IDs used to represent them.  These functions are meant to be used
   * by the pickler and nothing else, since they break the abstraction!!
   *)
    val toId : lvar -> int
    val fromId : int -> lvar

  (* comparisons of lvars *)
    val same : lvar * lvar -> bool
    val < : lvar * lvar -> bool
    val > : lvar * lvar -> bool
    val compare : lvar * lvar -> order

    structure Map : ORD_MAP where type Key.ord_key = lvar
    structure Set : ORD_SET where type Key.ord_key = lvar
    structure Tbl : MONO_HASH_TABLE where type Key.hash_key = lvar

  (* sorted lists of LVars *)
    structure SortedList : sig
	type t = lvar list
	val enter : lvar * t -> t
	val merge : t * t -> t
	val foldmerge : t list -> t
	val uniq : lvar list -> t	(* make a sorted list *)
	val remove : t * t -> t
	val rmv : lvar * t -> t
	val member : t -> lvar -> bool
	val intersect : t * t -> t
	val difference : t * t -> t
      end

  end (* signature LAMBDA_VAR *)
