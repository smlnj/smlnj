(* Copyright 1989 by AT&T Bell Laboratories *)
signature INTMAP =
  sig
    type 'a intmap

         (* return a named intmap *)
    val namednew : string * int * exn -> 'a intmap

         (* return an unnamed intmap *)
    val new      : int * exn -> 'a intmap

         (* return the number of elements *)
    val elems    : 'a intmap -> int

         (* insert a new binding *)
    val add      : 'a intmap -> int * 'a -> unit

         (* remove a key; no effect if the key does not exists *)
    val rmv : 'a intmap -> int -> unit

         (* lookup a key; raises exception if the key does not exists *)
    val map : 'a intmap -> int -> 'a

         (* lookup a key; return the key if no associated binding exists *)
    val mapInt : int intmap -> int -> int

         (* lookup a key; return the default value if the key is missing *)
    val mapWithDefault : 'a intmap * 'a -> int -> 'a

         (* iterate over an intmap *)
    val app : (int * 'a -> unit) -> 'a intmap -> unit

         (* convert an intmap to a list *)
    val intMapToList: 'a intmap -> (int * 'a) list

         (* return the keys in an intmap *)
    val keys: 'a intmap -> int list

         (* return the values in an intmap *)
    val values: 'a intmap -> 'a list

         (* clear an intmap *)
    val clear : 'a intmap -> unit

         (* copy an intmap *)
    val copy : 'a intmap -> 'a intmap 
  end

