(* asdl-sexp-pickle.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure ASDLSExpPickle : sig

    type instream = TextIO.instream
    type outstream

    val writeBool : outstream * ASDL.bool -> unit
    val readBool : instream -> ASDL.bool

    val writeBoolOption : outstream * ASDL.bool option -> unit
    val readBoolOption : instream -> ASDL.bool option

    val writeInt : outstream * ASDL.int -> unit
    val readInt : instream -> ASDL.int

    val writeUInt : outstream * ASDL.uint -> unit
    val readUInt : instream -> ASDL.uint

    val writeInteger : outstream * ASDL.integer -> unit
    val readInteger : instream -> ASDL.integer

    val writeString : outstream * ASDL.string -> unit
    val readString : instream -> ASDL.string

    val writeIdentifier : outstream * ASDL.identifier -> unit
    val readIdentifier : instream -> ASDL.identifier

  (* output utility functions *)
    val writeSExp : outstream * string * (unit -> unit) -> unit
    val writeOption : (outstream * 'a -> unit) -> (outstream * 'a option) -> unit
    val writeSeq : (outstream * 'a -> unit) -> (outstream * 'a list) -> unit
    val writeEnum : outstream * string -> unit
    val space : outstream -> unit

  (* input utility functions *)
    val readOption : (instream -> 'a) -> instream -> 'a option
    val readSeq : (instream -> 'a) -> instream -> 'a list

  (* pickle to/from files *)
    val toFile : (outstream * 'a -> unit) -> (string * 'a) -> unit
    val fromFile : (instream -> 'a) -> string -> 'a

  end  = struct

(* for post-110.86
    structure PP = TextPP
*)
    structure PP = TextIOPP

    type instream = TextIO.instream
    type outstream = PP.stream

    val lineWid = 120

    fun space ppS = PP.space ppS 1;

    fun writeBool (ppS, true) = PP.string ppS "#t"
      | writeBool (ppS, false) = PP.string ppS "#f"

    fun readBool (inS : instream) : bool = raise Fail "unimplemented"

    fun writeInt (ppS, n) = if n < 0
	  then PP.string ppS ("-" ^ Int.toString(~n))
	  else PP.string ppS (Int.toString n)

    fun readInt inS = raise Fail "unimplemented"

    fun writeUInt (ppS, n) = PP.string ppS (Word.fmt StringCvt.DEC n)

    fun readUInt inS = raise Fail "unimplemented"

    fun writeInteger (ppS, n) = if n < 0
	  then PP.string ppS ("-" ^ IntInf.toString(~n))
	  else PP.string ppS (IntInf.toString n)

    fun readInteger inS = raise Fail "unimplemented"

(* FIXME: what is the correct escape encoding? *)
    fun writeString (ppS, s) =
	  PP.string ppS (String.concat["\"", String.toCString s, "\""])

    fun readString inS = raise Fail "unimplemented"

    fun writeIdentifier (ppS, id) = PP.string ppS (Atom.toString id)

    fun readIdentifier inS = raise Fail "unimplemented"

  (* write an S-Expression *)
    fun writeSExp (ppS, f, wrContents) = (
	  PP.openHBox ppS;
	    PP.string ppS "(";
	    PP.openHVBox ppS (PP.Rel 1);
	      PP.string ppS f;
	      wrContents();
	      PP.string ppS ")";
	    PP.closeBox ppS;
	  PP.closeBox ppS)

  (* write an option *)
    fun writeOption wrFn (ppS, NONE) =
          writeSExp (ppS, "?", fn () => ())
      | writeOption wrFn (ppS, SOME obj) =
          writeSExp (ppS, "?", fn () => (space ppS; wrFn(ppS, obj)))

  (* read an option *)
    fun readOption rdFn inS = raise Fail "readOption"

    val writeBoolOption = writeOption writeBool
    val readBoolOption : instream -> bool option = readOption readBool

  (* write a list of values as a sequence *)
    fun writeSeq wrFn (ppS, []) = PP.string ppS "(*)"
      | writeSeq wrFn (ppS, [x]) = (
	  PP.openHBox ppS;
	    PP.string ppS "(*";
	    space ppS; wrFn(ppS, x);
	    PP.string ppS ")";
	  PP.closeBox ppS)
      | writeSeq wrFn (ppS, xs as x1::xr) = (
	  PP.openHBox ppS;
	    PP.string ppS "(*"; space ppS;
	    PP.openHOVBox ppS (PP.Rel 0);
	      wrFn(ppS, x1);
	      List.app (fn x => (space ppS; wrFn(ppS, x))) xr;
	      PP.string ppS ")";
	    PP.closeBox ppS;
	  PP.closeBox ppS)

  (* write an enumeration constructor as a quoted symbol *)
    fun writeEnum (ppS, id) = PP.string ppS ("'" ^ id)

  (* read a sequence into a sequence of values *)
    fun readSeq rdFn inS = raise Fail "readSeq"

    fun toFile write (file, x) = let
	  val outS = TextIO.openOut file
(* for post-110.86
	  val ppS = PP.openOutstream {dst=outS, wid=lineWid}
*)
	  val ppS = PP.openOut {dst=outS, wid=lineWid}
	  fun finish () = (PP.flushStream ppS; TextIO.closeOut outS)
	  in
	    write (ppS, x) handle exn => (finish(); raise exn);
	    finish()
	  end

    fun fromFile read file = let
	  val inS = TextIO.openIn file
	  in
	    (read inS handle exn => (TextIO.closeIn inS; raise exn))
	    before TextIO.closeIn inS
	  end

  end (* structure ASDLSExpPickle *)
