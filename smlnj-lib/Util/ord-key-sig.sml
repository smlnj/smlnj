(* ord-key-sig.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Abstract linearly ordered keys.
 *)

signature ORD_KEY =
  sig

  (* the type of keys *)
    type ord_key

  (* defines a total ordering on the ord_key type *)
    val compare : ord_key * ord_key -> order

  end (* ORD_KEY *)
