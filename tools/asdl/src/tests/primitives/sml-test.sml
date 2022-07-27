(* sml-test.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Test primitive memory pickling operations from the ASDL library.
 *)

structure Test =
  struct

    local
      structure Pkl = ASDLMemoryPickle
    (* pickle/unpickle identity *)
      fun ident (pickle, unpickle) x = let
	    val buf = Word8Buffer.new 100
	    val _ = pickle (buf, x)
            val inS = ASDLMemoryPickle.openVector(Word8Buffer.contents buf)
	    val y = unpickle inS
	    in
	      if not(Pkl.endOfStream inS)
		then raise Fail "excess bytes after unpickling"
		else y
	    end
    (* check that the pickle/unpickle cycle preserves values *)
      fun check name (toStr, same, pick, unpick) x = let
	    val _ = print(concat["check ", name, ": unpickle(pickle ", toStr x, ")"])
	    val y = ident (pick, unpick) x
	    in
	      if same(x, y)
		then print " ok\n"
		else print(concat[" fail (", toStr y, ")\n"])
	    end
	      handle exn => print(concat[" fail(", exnMessage exn, ")\n"])
    in
  (* booleans *)
    fun chkBool () = let
	  val chk = check "boolean" (Bool.toString, op =, Pkl.writeBool, Pkl.readBool)
	  in
	    chk true;
	    chk false
	  end
  (* int *)
    fun chkInt () = let
	  val chk = check "int" (Int.toString, op =, Pkl.writeInt, Pkl.readInt)
	  in
	    chk 0;
	    chk ~1;
	    chk 1;
	    chk ~32;
	    chk ~31;
	    chk 31;
	    chk 32;
	    chk ~8192;
	    chk ~8191;
	    chk 8191;
	    chk 8192;
	    chk ~2097152;
	    chk ~2097151;
	    chk 2097151;
	    chk 2097152;
	    chk ~536870912;	(* lower bound *)
	    chk 536870911	(* upper bound *)
	  end
  (* uint *)
    fun chkUInt () = let
	  fun toS w = "0x" ^ Word.toString w
	  val chk = check "uint" (toS, op =, Pkl.writeUInt, Pkl.readUInt)
	  in
	    chk 0w0;
	    chk 0w1;
	    chk 0wx3f;
	    chk 0wx100;
	    chk 0wx3fff;
	    chk 0wx10000;
	    chk 0wx3fffff;
	    chk 0wx1000000;
	    chk 0wx3fffffff	(* upper bound *)
	  end
  (* integer *)
    fun chkInteger () = let
	  val chk = check "integer"
		(IntInf.toString, op =, Pkl.writeInteger, Pkl.readInteger)
	  in
	    chk 0;
	    chk ~1;
	    chk 1;
	    chk ~64;
	    chk ~63;
	    chk 63;
	    chk 64;
	    chk ~8192;
	    chk ~8191;
	    chk 8191;
	    chk 8192;
	    chk ~2097152;
	    chk ~2097151;
	    chk 2097151;
	    chk 2097152;
	    chk ~36893488147419103232;	(* -2^65 *)
	    chk 36893488147419103232;	(* 2^65 *)
	    chk 73786976294838206463	(* 2^66-1 *)
	  end
  (* string *)
    fun chkString () = let
	  fun toS s = String.concat["\"", String.toString s, "\""]
	  val chk = check "string" (toS, op =, Pkl.writeString, Pkl.readString)
	  in
	    chk "";
	    chk " ";
	    chk "hello world\n"
	  end
  (* identifier *)
    fun chkIdentifier () = let
	  fun toS s = String.concat["\"", String.toString(Atom.toString s), "\""]
	  val chk = check "identifier"
		(toS, Atom.same, Pkl.writeIdentifier, Pkl.readIdentifier)
	  in
	    chk (Atom.atom "");
	    chk (Atom.atom "x");
	    chk (Atom.atom "x1");
	    chk (Atom.atom "hello world\n")
	  end
  (* tag8 *)
    fun chkTag8 () = let
	  fun toS w = "0x" ^ Word.toString w
	  val chk = check "tag8" (toS, op =, Pkl.writeTag8, Pkl.readTag8)
	  in
	    chk 0w0;
	    chk 0w1;
	    chk 0w17;
	    chk 0w255		(* upper bound *)
	  end
  (* tag16 *)
    fun chkTag16 ()= let
	  fun toS w = "0x" ^ Word.toString w
	  val chk = check "tag8" (toS, op =, Pkl.writeTag16, Pkl.readTag16)
	  in
	    chk 0w0;
	    chk 0w1;
	    chk 0w17;
	    chk 0w255;
	    chk 0w256;
	    chk 0w65535		(* upper bound *)
	  end
    end (* local *)

  (* check all primitive types *)
    fun chkAll () = (
	  chkBool ();
	  chkInt ();
	  chkUInt ();
	  chkInteger ();
	  chkString ();
	  chkIdentifier ();
	  chkTag8 ();
	  chkTag16 ())

  (* functions to support interactive debugging *)
    local
      structure Pkl = ASDLMemoryPickle
      fun toBytes pickle x = let
	    val buf = Word8Buffer.new 100
	    in
	      pickle (buf, x);
	      Word8Vector.toList(Word8Buffer.contents buf)
	    end
    in
    val intToBytes = toBytes Pkl.writeInt
    val uintToBytes = toBytes Pkl.writeUInt
    end (* local *)

  end
