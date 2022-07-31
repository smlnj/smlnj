(* hash-cons.sml
 *
 * COPYRIGHT (c) 2011 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure HashCons :> HASH_CONS =
  struct

    type 'a obj = {nd : 'a, tag : word, hash : word}

    datatype 'a tbl = Tbl of {
	eq : 'a * 'a -> bool,
	nextTag : word ref,
	tbl : 'a obj list Array.array ref
      }

    val tblSz = PrimeSizes.pick 64

    fun new {eq} = Tbl{
	    eq = eq,
	    nextTag = ref 0w0,
	    tbl = ref(Array.array(tblSz, []))
	  }

    fun clear (Tbl{nextTag, tbl, ...}) = (
	  nextTag := 0w0;
	  Array.modify (fn _ => []) (!tbl))

    fun insert (Tbl{eq, nextTag, tbl}, h, term) = let
	  val tbl' = !tbl
	  val i = Word.toIntX(Word.mod(h, Word.fromInt(Array.length tbl')))
	  val bucket = Array.sub(tbl', i)
	  fun find [] = let
		val id = !nextTag
		val obj = {nd = term, hash = h, tag = id}
		in
		  nextTag := id + 0w1;
		  Array.update(tbl', i, obj::bucket);
(* check for table resize *)
		  obj
		end
	    | find ((obj as {nd, hash, ...})::r) =
		if (h = hash) andalso eq(term, nd)
		  then obj
		  else find r
	  in
	    find bucket
	  end

    fun node {nd, tag, hash} = nd
    fun tag  {nd, tag, hash} = tag

    fun same (a : 'a obj, b : 'a obj) = (#tag a = #tag b)
    fun compare (a : 'a obj, b : 'a obj) = Word.compare(#tag a, #tag b)

    fun <+ (a, b) = Word.<<(a, 0w1) + b
    infix <+

    fun cons0 tbl (id, c) = insert (tbl, id, c)

    fun cons1 tbl (id, cf) (b : 'b obj) =
	  insert (tbl, id <+ (#tag b), cf b)

    fun cons2 tbl (id, cf) (b : 'b obj, c : 'c obj) =
	  insert (tbl, id <+ (#tag b) <+ (#tag c), cf(b, c))

    fun cons3 tbl (id, cf) (b : 'b obj, c : 'c obj, d : 'd obj) =
	  insert (tbl, id <+ (#tag b) <+ (#tag c) <+ (#tag d), cf(b, c, d))

    fun cons4 tbl (id, cf) (b : 'b obj, c : 'c obj, d : 'd obj, e : 'e obj) =
	  insert (tbl, id <+ (#tag b) <+ (#tag c) <+ (#tag d) <+ (#tag e),
	    cf(b, c, d, e))

    fun cons5 tbl (id, cf)
	(b : 'b obj, c : 'c obj, d : 'd obj, e : 'e obj, f : 'f obj) =
	  insert (tbl,
	    id <+ (#tag b) <+ (#tag c) <+ (#tag d) <+ (#tag e) <+ (#tag f),
	    cf(b, c, d, e, f))

    fun consList tbl (id, cf) (l : 'b obj list) =
	  insert (tbl, List.foldl (fn ({tag, ...}, sum) => sum <+ tag) id l, cf l)

  (* consing for records *)
    fun consR1 tbl (id, inj, prj) r = cons1 tbl (id, inj) (prj r)
    fun consR2 tbl (id, inj, prj) r = cons2 tbl (id, inj) (prj r)
    fun consR3 tbl (id, inj, prj) r = cons3 tbl (id, inj) (prj r)
    fun consR4 tbl (id, inj, prj) r = cons4 tbl (id, inj) (prj r)
    fun consR5 tbl (id, inj, prj) r = cons5 tbl (id, inj) (prj r)

  end
