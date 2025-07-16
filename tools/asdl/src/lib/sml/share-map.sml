(* share-map.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *
 * Share maps are used to implement the pickling of back references (the `!`
 * type operator.
 *)

signature ASDL_SHARE_MAP =
  sig

    type 'a t

    (* abstract interface to a stateful partial map (e.g., a hash table) *)
    type 'a table = {
        (* lookup an item *)
        find : 'a -> int option,
        (* insert an item with its ID *)
        insert : 'a * int -> unit
      }

    (* `mkShareMap mkTable ()` creates a fresh, empty, share map using the given
     * table implementation.
     *)
    val mkShareMap : (unit -> 'a table) -> unit -> 'a t

    (* `resolve (sMap, item)` returns a pair `(fresh, id)`, where `id` is the
     * unique ID for the item and `fresh` is `true` when this is the first
     * time the item has been encountered.
     *)
    val resolve : 'a t * 'a -> bool * int

  end

structure InternalShareMap =
  struct

    datatype 'a t = SMap of 'a -> bool * int

    type 'a table = {
        find : 'a -> int option,
        insert : 'a * int -> unit
      }

    fun mkShareMap mkTable () = let
          val cnt = ref 0
          val {find, insert} = mkTable()
          in
            SMap(fn item => (case find item
                 of NONE => let
                      val id = !cnt
                      in
                        cnt := id + 1;
                        insert (item, id);
                        (true, id)
                      end
                  | SOME id => (false, id)
                (* end case *)))
          end

    fun resolve (SMap sMap, item) = sMap item

  end

structure ASDLShareMap :> ASDL_SHARE_MAP where type 'a t = 'a InternalShareMap.t
  = InternalShareMap
