(* lambda-var.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Dummy implementation of LambdaVar structure (and pickler) for testing.
 *)

structure LambdaVar : sig

    eqtype lvar

    val toId : lvar -> int
    val fromId : int -> lvar

    val toString : lvar -> string

    structure Tbl : MONO_HASH_TABLE where type Key.hash_key = lvar

  end = struct

    datatype lvar = LV of int

    fun toId (LV id) = id
    val fromId = LV

    fun toString (LV id) = "v" ^ Int.toString id

    fun same (LV id1, LV id2) = (id1 = id2)

    structure Tbl = HashTableFn(
      struct
        type hash_key = lvar
        fun hashVal (LV id) = Word.fromInt id
        val sameKey = same
      end)

  end
