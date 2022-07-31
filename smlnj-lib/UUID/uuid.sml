(* uuid.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * An implementation of Universally Unique IDs (UUIDs).  To generate
 * UUIDs, use the GenUUID structure.
 *)

structure UUID :> sig

    type t

  (* the all-zeros UUID *)
    val null : t

  (* compare two UUIDs *)
    val compare : t * t -> order

  (* are two UUIDs the same *)
    val same : t * t -> bool

  (* hash a UUID; we use the 64-bit FNV-1a hash function for this purpose *)
    val hash : t -> word

  (* format the UUID as "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx" where each "x" is
   * a lower-case hexadecimal digit.
   *)
    val toString : t -> string

  (* convert a string of the form produced by `toString` to a UUID value; leading whitespace
   * is ignored.
   *)
    val fromString : string -> t option

  (* return the UUID as a big-endian sequence of bytes *)
    val toBytes : t -> Word8Vector.vector

  (* convert a 16-element byte vector to a UUID.  The Size exception is raised
   * if the length of the vector is not exactly 16 bytes.  Otherwise, there is
   * no validity chechking of the UUID (i.e., the variant and type are not checked).
   *)
    val fromBytes : Word8Vector.vector -> t

  end = struct

    structure W8V = Word8Vector
    structure W8VS = Word8VectorSlice
    structure SS = Substring

    type t = W8V.vector

    val null : t = W8V.tabulate(16, fn _ => 0w0)

  (* compare two UUIDs *)
    val compare = W8V.collate Word8.compare

    fun same (uuid1 : t, uuid2 : t) = (uuid1 = uuid2)

(* NOTE: For 110.98, we can switch to using the new FNVHash module in the Util library *)
  (* hash a UUID; we use the 64-bit FNV-1a hash function for this purpose *)
    fun hash uuid = let
	  val offsetBasis : Word64.word = 0wxcbf29ce484222325
	  val prime : Word64.word = 0wx00000100000001B3
	  fun hashByte (b, h) =
		Word64.xorb (Word64.fromLargeWord(Word8.toLargeWord b), h) * prime
	  in
	     Word.fromLarge(W8V.foldl hashByte offsetBasis uuid)
	  end

    fun toString (uuid : t) = let
	  fun n2c b = String.sub("0123456789abcdef", Word8.toInt b)
	  fun b2list (b, l) = n2c(Word8.>>(b, 0w4)) :: n2c(Word8.andb(b, 0wxf)) :: l
	  fun slice2list (start, len, l) =
		W8VS.foldr b2list l (W8VS.slice(uuid, start, SOME len))
	  val chrs = slice2list (10, 6, [])
	  val chrs = slice2list (8, 2, #"-" :: chrs)
	  val chrs = slice2list (6, 2, #"-" :: chrs)
	  val chrs = slice2list (4, 2, #"-" :: chrs)
	  val chrs = slice2list (0, 4, #"-" :: chrs)
	  in
	    String.implode chrs
	  end

    local
    (* the lengths of the fields *)
      val fieldLens = [8, 4, 4, 4, 12]
      val scan8 = Word8.scan StringCvt.HEX SS.getc
    (* converts a list of fields to a list of bytes. If there is the wrong number of
     * fields, or an incorrect length field, or a invalid digit, then `NONE` is
     * returned.
     *)
      fun fields2bytes flds = let
	    fun toBytes ([], [], bytes) = SOME(List.rev bytes)
	      | toBytes (fld::flds, len::lens, bytes) = if SS.size fld <> len
		  then NONE
		  else let
		    fun lp (ss, bytes) = if SS.isEmpty ss
			  then toBytes (flds, lens, bytes)
			  else let
			    val (b, rest) = SS.splitAt (ss, 2)
			    in
			      case scan8 b
			       of SOME(b, _) => lp (rest, b::bytes)
				| NONE => NONE
			    end
		    in
		      lp (fld, bytes)
		    end
	      | toBytes _ = NONE
	    in
	      toBytes (flds, fieldLens, [])
	    end
    val splitFields = SS.fields (fn #"-" => true | _ => false)
    in
    fun fromString s = let
	  val ss = StringCvt.skipWS SS.getc (SS.full s)
	  in
	  (* the length of the UUID should be 36 characters (32 digits plus four "-"s) *)
	    if (SS.size ss >= 36)
	      then (case fields2bytes(splitFields (SS.slice(ss, 0, SOME 36)))
		 of SOME bytes => SOME(W8V.fromList bytes)
		  | _ => NONE
		(* end case *))
	      else NONE
	  end
    end (* local *)

    fun toBytes uuid = uuid
    fun fromBytes v = if (W8V.length v <> 16) then raise Size else v

  end
