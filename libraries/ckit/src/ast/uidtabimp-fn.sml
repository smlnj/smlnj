(* Copyright (c) 1998 by Lucent Technologies *)

(* imperative uid tables based on hash table library *)
(* polymorphic table operations *)

functor UidtabImpFn (structure Uid:UID) :> UIDTABIMP where type uid = Uid.uid
=
struct

  structure M = HashTableFn
      (struct (* must match HASH_KEY *)
	 type hash_key = Uid.uid
	 val hashVal = Uid.toWord
	 val sameKey = Uid.equal
       end)

  exception NotFound

  type uid = Uid.uid
  type 'a uidtab = 'a M.hash_table

  fun insert (uidtab,uid,v) : unit = M.insert uidtab (uid,v)

  fun find (uidtab,uid) = M.find uidtab uid

  fun listItems uidtab = M.listItems uidtab

  fun listItemsi uidtab = M.listItemsi uidtab

  fun uidtab () = M.mkTable(50, NotFound)

end (* functor UidtabImperFn *)
