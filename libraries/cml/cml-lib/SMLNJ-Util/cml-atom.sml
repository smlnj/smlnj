(* cml-atom.sml
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories
 * COPYRIGHT (c) 1996 by AT&T Research
 *
 * This is a CML version of the Atom module from the SML/NJ library.
 * It protects the global hash table in a server thread.
 *
 * AUTHOR:	John Reppy
 *		AT&T Bell Laboratories
 *		Murray Hill, NJ 07974
 *		jhr@research.att.com
 *)

structure Atom : ATOM =
  struct

    structure V = SyncVar

  (* local definition of app *)
    fun app f = let
	  fun appF [] = ()
	    | appF (x::r) = (f x; appF r)
	  in
	    appF
	  end

  (* unique names *)
    datatype atom = ATOM of {
	hash : word,
	id : string
      }

    fun toString (ATOM{id, ...}) = id

    fun hash (ATOM{hash=h, ...}) = h

    fun sameAtom (ATOM{hash=h1, id=id1}, ATOM{hash=h2, id=id2}) =
	  (h1 = h2) andalso (id1 = id2)

  (* compare two names for their relative order; note that this is
   * not lexical order!
   *)
    fun compare (ATOM{hash=h1, id=id1}, ATOM{hash=h2, id=id2}) =
	  if (h1 = h2)
	    then if (id1 = id2)
	      then EQUAL
	    else if (id1 < id2)
	      then LESS
	      else GREATER
	  else if (h1 < h2)
	    then LESS
	    else GREATER


  (** the unique name hash table; this is protected in a server thread. **)
    val tableSz = 64 (* initial table size *)

  (* a request to the server *)
    type req = {key : word, str : string, reply : atom V.ivar}

  (* the server's request channel *)
    val reqCh : req CML.chan = CML.channel()

  (* the name server *)
    fun nameServer () = let
	  fun server (tblSize, tbl, numItems) = let
		val {key, str, reply} = CML.recv reqCh
		fun isName (ATOM{hash, id}) = (hash = key) andalso (id = str)
		fun insert (tblSz, tbl, numItems) =
		      if (numItems > tblSz)
			then grow (tblSz, tbl, numItems)
			else let
			  val indx = Word.toIntX(Word.andb(key, Word.fromInt tblSz - 0w1))
			  fun look [] = let
				val newName = ATOM{hash = key, id = str}
				in
				  Array.update (
				    tbl, indx, newName :: Array.sub(tbl, indx));
				  V.iPut(reply, newName);
				  (tblSz, tbl, numItems+1)
				end
			    | look (name::r) = (
				if (isName name)
				  then (
				    V.iPut(reply, name);
    				    (tblSz, tbl, numItems))
				  else look r)
			  in
			    look (Array.sub(tbl, indx))
			  end
	      (* double the table size *)
		and grow (tblSz, tbl, numItems) = let
		      val newSz = tblSz+tblSz
		      val newMask = Word.fromInt newSz - 0w1
		      val newTbl = Array.array(newSz, [])
		      fun ins (item as ATOM{hash, ...}) = let
			    val indx = Word.toIntX(Word.andb(hash, newMask))
			    in
			      Array.update (newTbl, indx,
				item :: Array.sub(newTbl, indx))
			    end
		      val appins = app ins
		      fun copy i = (appins (Array.sub(tbl, i)); copy(i+1))
		      in
			(copy 0) handle _ => ();
			insert (newSz, newTbl, numItems)
		      end
		in
		  server (insert (tblSize, tbl, numItems))
		end (* server *)
	  in
	    server (tableSz, Array.array(tableSz, [] : atom list), 0)
	  end

  (* make an atom from a string; this operation is split into a client
   * part (compute the hash key), and a server part (map to unique
   * representation).
   *)
    fun atom s = let
	  val replyV = V.iVar()
	  in
	    CML.send (reqCh, {key=HashString.hashString s, str=s, reply=replyV});
	    V.iGet replyV
	  end

  (* eventually, we should hash the substring and check for prior definition
   * before creating the string.
   *)
    fun atom' ss = atom(Substring.string ss)

  (** Initialization code **)
    fun startup () = (CML.spawn nameServer; ())
    fun shutdown () = ()

    val _ = RunCML.logServer("Name", startup, shutdown)
    val _ = RunCML.logChannel("Name:reqCh", reqCh)

  end (* Atom *)
