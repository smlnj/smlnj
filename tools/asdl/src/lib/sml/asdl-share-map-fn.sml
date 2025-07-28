(* asdl-share-map-fn.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *
 * A functor for defining share maps for objects to be pickled.
 *)

functor ASDLShareMapFn (T : HASH_KEY) : sig

    type item = T.hash_key

    val mkShareMap : int -> item ASDLShareMap.t

  end = struct

    structure Tbl = HashTableFn (T)

    type item = T.hash_key

    fun mkShareMap n = let
          val n = Int.min(2048, Int.max(n, 32))
          val tbl = Tbl.mkTable (n, Fail "share map")
          val find = Tbl.find tbl
          val insert = Tbl.insert tbl
          fun resolveItem item = (case find item
                 of NONE => let
                      val id = Tbl.numItems tbl
                      in
                        insert (item, id);
                        (true, id)
                      end
                  | SOME id => (false, id)
                (* end case *))
          in
            InternalShareMap.SMap resolveItem
          end

  end (* functor ASDLShareMapFn *)
