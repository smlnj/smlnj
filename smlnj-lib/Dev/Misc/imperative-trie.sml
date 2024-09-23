(* trie.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

structure Trie :> sig

    type 'a t

    (* make a trie from a list of string-value pairs.  This function raises
     * the `Fail` exception if there are duplicate keys in the argument list.
     *)
    val make : (string * 'a) list -> 'a t

    (* extend the trie *)
    val extend : 'a t * string * 'a -> unit

    val find : 'a t * string -> 'a option

  end = struct

    type 'a entry = {
        out : (char * int) list,        (* out edges from this state *)
        act : 'a option                 (* accepting result *)
      }

    datatype 'a t = TRIE of {
        nextState : int ref,
        states : 'a entry array ref
      }

    (* the initial array size when creating an empty trie.  We assume that the
     * trie is going to be defined incrementally.
     *)
    val initialArraySz = 256

    val emptyEntry : 'a entry = {out=[], act=NONE}

    fun make [] = TRIE{
            nextState = ref 0,
            states = ref(Array.tabulate(initialArraySz, emptyEntry))
          }
      | make items = let
          (* first sort the items by the strings *)
          val items = ListMergeSort.sort
                (fn ((k1, _), (k2, _)) => String.compare (k1, k2))
                  items
          (* compute the maximum string length, while checking for duplicates *)
          val maxLen = let
                fun chk ((k1, _)::(r as (k2, _)::_), n) = if (k1 = k2)
                      then raise Fail (concat[
                          "duplicate key \"", String.toString k1, "\""
                        ])
                      else chk (r, Int.max(n, size k1))
                  | chk ([(k, _)], n) = Int.max(n, size k)
                  | chk ([], n) = n
                in
                  chk (items, 0)
                end
          (* compute the initial number of states *)
          let nStates = let
                fun lp (i, n) = if (i < maxLen)

          in
          end (* make *)

  end
