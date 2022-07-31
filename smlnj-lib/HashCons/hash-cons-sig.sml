(* hash-cons-sig.sml
 *
 * COPYRIGHT (c) 2011 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * TODO: better support for nodes that mix lists and non-lists as args.
 *       (perhaps a hashed-cons list rep)?
 *)

signature HASH_CONS =
  sig

  (* hash table for consing *)
    type 'a tbl

  (* create a new hash-cons table using the given equality function *)
    val new : {eq : 'a * 'a -> bool} -> 'a tbl

  (* clear a table of all elements *)
    val clear : 'a tbl -> unit

  (* a hashed-cons object *)
    type 'a obj = {
	nd : 'a,	(* the underlying representation *)
	tag : word,	(* a tag that is unique for the object (for the object's table) *)
	hash : word	(* a hash of the object (used to index the table) *)
      }

  (* projections *)
    val node : 'a obj -> 'a
    val tag  : 'a obj -> word

  (* comparisons *)
    val same : ('a obj * 'a obj) -> bool
    val compare : ('a obj * 'a obj) -> order

  (* constructors for nodes formed from tuples of children *)
    val cons0 : 'a tbl -> (word * 'a) -> 'a obj
    val cons1 : 'a tbl -> (word * ('b obj -> 'a))
	  -> 'b obj -> 'a obj
    val cons2 : 'a tbl -> (word * ('b obj * 'c obj -> 'a))
	  -> 'b obj * 'c obj -> 'a obj
    val cons3 : 'a tbl -> (word * ('b obj * 'c obj * 'd obj -> 'a))
	  -> 'b obj * 'c obj * 'd obj -> 'a obj
    val cons4 : 'a tbl -> (word * ('b obj * 'c obj * 'd obj * 'e obj -> 'a))
	  -> 'b obj * 'c obj * 'd obj * 'e obj -> 'a obj
    val cons5 : 'a tbl -> (word * ('b obj * 'c obj * 'd obj * 'e obj * 'f obj -> 'a))
	  -> 'b obj * 'c obj * 'd obj * 'e obj * 'f obj -> 'a obj

  (* constructor for nodes formed from a list of children *)
    val consList : 'a tbl -> (word * ('b obj list -> 'a)) -> 'b obj list -> 'a obj

  (* constructors for nodes formed from records of children; the arguments include
   * a node constructor from a tuple of children and a projection from the record
   * type to a tuple type.
   *)
    val consR1 : 'a tbl -> (word * ('b obj -> 'a) * ('r -> 'b obj))
	  -> 'r -> 'a obj
    val consR2 : 'a tbl
	  -> (word * ('b obj * 'c obj -> 'a) * ('r -> 'b obj * 'c obj))
	    -> 'r -> 'a obj
    val consR3 : 'a tbl
	  -> (word * ('b obj * 'c obj * 'd obj -> 'a)
	    * ('r -> 'b obj * 'c obj * 'd obj))
	    -> 'r -> 'a obj
    val consR4 : 'a tbl
	  -> (word * ('b obj * 'c obj * 'd obj * 'e obj -> 'a)
	    * ('r -> 'b obj * 'c obj * 'd obj * 'e obj))
	    -> 'r -> 'a obj
    val consR5 : 'a tbl
	  -> (word * ('b obj * 'c obj * 'd obj * 'e obj * 'f obj -> 'a)
	    * ('r -> 'b obj * 'c obj * 'd obj * 'e obj * 'f obj))
	    -> 'r -> 'a obj

  end

