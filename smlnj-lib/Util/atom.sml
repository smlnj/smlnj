(* atom.sml
 *
 * COPYRIGHT (c) 1996 by AT&T Research
 *
 * AUTHOR:	John Reppy
 *		AT&T Bell Laboratories
 *		Murray Hill, NJ 07974
 *		jhr@research.att.com
 *
 * TODO: add a gensym operation?
 *)

structure Atom :> ATOM =
  struct

  (* Atoms are hashed strings that support fast equality testing. *)
    datatype atom = ATOM of {
	hash : word,
	id : string
      }

  (* return the string representation of the atom *)
    fun toString (ATOM{id, ...}) = id

  (* return a hash key for the atom *)
    fun hash (ATOM{hash, ...}) = hash

  (* return true if the atoms are the same *)
    fun same (ATOM{hash=h1, id=id1}, ATOM{hash=h2, id=id2}) =
	  (h1 = h2) andalso (id1 = id2)

  (* for backward compatibility *)
    val sameAtom = same

  (* compare two names for their relative order; note that this is
   * not lexical order!
   *)
    fun compare (ATOM{hash=h1, id=id1}, ATOM{hash=h2, id=id2}) =
	if h1 = h2 then String.compare (id1, id2)
	else if h1 < h2 then LESS
	else GREATER

  (* compare two atoms for their lexical order *)
    fun lexCompare (ATOM{id=id1, ...}, ATOM{id=id2, ...}) = String.compare(id1, id2)

  (* the unique name hash table *)
    val tableSz = 64
    val table = ref(Array.array(tableSz, [] : atom list))
    val numItems = ref 0

    infix %
    fun h % m = Word.toIntX (Word.andb (h, m))

  (* Map a string or substring s to the corresponding unique atom. *)
    fun atom0 (toString, hashString, sameString) s = let
	  val h = hashString s
	  val tbl = !table
	  val sz = Array.length tbl
	  val indx = h % (Word.fromInt sz - 0w1)
	  fun look ((a as ATOM{hash, id}) :: rest) =
		if (hash = h) andalso sameString(s, id)
		  then a
		  else look rest
	    | look [] = let
		fun new (tbl, indx) = let
		      val a = ATOM {hash = h, id = toString s}
		      in
			Array.update (tbl, indx, a :: Array.sub (tbl, indx));
			a
		      end
		in
		  if !numItems < sz
		    then new (tbl, indx)
		    else let
		      val newSz = sz + sz
		      val newMask = Word.fromInt newSz - 0w1
		      val newTbl = Array.array (newSz, [])
		      fun ins (item as ATOM{hash, ...}) = let
			    val indx = hash % newMask
			    in
			      Array.update (newTbl, indx, item :: Array.sub (newTbl, indx))
			    end
		      in
			Array.app (app ins) tbl;
			table := newTbl;
			new (newTbl, h % newMask)
		      end
	      end
	  in
	    look (Array.sub (tbl, indx))
	  end

  (* instantiate atom0 for the string case *)
    val atom = atom0 (fn s => s, FNVHash.hashString, op = )

  (* instantiate atom0 for the substring case *)
    val atom' = atom0 (
	  Substring.string,
	  FNVHash.hashSubstring,
	  fn (ss, s) => (Substring.compare(ss, Substring.full s) = EQUAL))

  end (* structure Atom *)
