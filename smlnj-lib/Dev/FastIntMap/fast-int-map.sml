(* integer maps based on
 *
 *      Fast Mergeable Integer Maps
 *      by Chris Okasaki and Andrew Gill
 *      ACM SIGPLAN Workshop on ML, pp. 77–86, September 1998
 *)

signature INT_MAP =
  sig
    type 'a t

    val empty : 'a t
    val find : 'a t * word -> 'a option
    val insert : 'a t * word * 'a -> 'a t
    val unionWith : ('a * 'a -> 'a) -> 'a t * 'a t -> 'a t

   end

(* little-endian implementation *)
structure IntMapLE :> INT_MAP =
  struct

    structure W = Word

    datatype 'a tr
      = Empty
      | Lf of word * 'a
      | Br of word * 'a tr * 'a tr

    type 'a t = (int * 'a tr)

    val empty = (0, Empty)

    (* smart constructor for Br *)
    fun br (p, Empty, r) = r
      | br (p, l, Empty) = l
      | br (p, l, r) = Br(p, l, r)

    fun find ((_, tr), key) = let
          fun find' (_, Empty, _) = NONE
            | find' (_, Lf(k, v), key) = if (k = key) then SOME v else NONE
            | find' (bit, Br(prefix, l, r), key) =
                if W.andb(key, bit-0w1) <> prefix
                  then NONE
                else if (W.andb(bit, key) = 0w0)
                  then find' (W.<<(bit, 0w1), l)
                  else find' (W.<<(bit, 0w1), r)
          in
            find' (tr, key)
          end

    fun insert ((n, tr), key, v) = let
          fun insert' (bit, Empty) = (n+1, Lf(key, v))
            | insert' (bit, t as Lf(k, _)) =
            | insert' (bit, t as Br(prefix, l, r)) =
          in
            insert' (0w1, tr)
          end

  end

(* big-endian implementation *)
structure IntMapBE :> INT_MAP =
  struct

    structure W = Word

    datatype 'a tr
      = Empty
      | Lf of word * 'a
      | Br of {prefix : word, bit : word, l : 'a tr, r : 'a tr}

    type 'a t = (int * 'a tr)

    val empty = (0, Empty)

    fun find ((_, tr),
  end
