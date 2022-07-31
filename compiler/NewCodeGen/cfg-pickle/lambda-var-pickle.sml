(* lambda-var-pickle.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Pickler for LambdaVar.lvar type
 *)

structure LambdaVarPickle : sig

    val write_lvar : ('a * Word8.word -> unit) -> ('a * LambdaVar.lvar) -> unit
    val read_lvar : ('a -> Word8.word) -> 'a -> LambdaVar.lvar

  end = struct

  (* LVars are represented by positive integers (the Int63.int type on 64-bit
   * machines).  We use the top 3 bits of the first byte to specify the number
   * of additional bytes in the representation, and the other 5 bits are the
   * most-significant bits in the value.
   *)

    fun toByte w = Word8.fromLarge(Word.toLarge w)
    fun fromByte w = Word.fromLarge(Word8.toLarge w)

  (* the mask for IDs depends on the native word size *)
    val mask = Word.<<(0w1, Word.fromInt Word.wordSize - 0w2) - 0w1

    fun write_lvar output1 (outS, lv) = let
	(* mask out low bits of ID *)
	  val w = Word.andb (Word.fromInt(LambdaVar.toId lv), mask)
	(* convert to a list of bytes *)
	  fun cvt (0w0, bytes) = bytes
	    | cvt (w, bytes) = cvt (Word.>>(w, 0w8), toByte w :: bytes)
	  val bytes = (case cvt (w, [])
		 of [] => [0w0]
		  | (bytes as b0::rest) => let
		      val (b0, rest) = if (b0 <= 0wx1f)
			    then (b0, rest)
			    else (0w0, b0::rest)
		      val n = Word.<<(Word.fromInt(length rest), 0w5)
		      in
			Word8.orb(toByte n, b0) :: rest
		      end
		(* end case *))
	  in
	    List.app (fn b => output1 (outS, b)) bytes
	  end

    fun read_lvar input1 inS = let
	  val b0 = fromByte (input1 inS)
	  val n = Word.>>(b0, 0w5)
	  fun decode (0w0, w) = LambdaVar.fromId (Word.toIntX w)
	    | decode (n, w) = decode (n-0w1, Word.orb(Word.<<(w, 0w8), fromByte(input1 inS)))
	  in
	    decode (n, Word.andb(b0, 0wx1f))
	  end

  end
