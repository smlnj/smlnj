(* heap2asm.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Generating an assembly code file corresponding to a heap image.
 *
 * NOTE: there is an argument that this program should be part of the
 * runtime system, because it would make getting the correct assembly
 * directives right easier.
 *)

structure Main : sig

    val main: string * string list -> OS.Process.status

  end = struct

  (* number of bytes per line *)
    val bytesPerLine = 20

(* FIXME: getHostSize is deprecated and should be replaced with getArchSize *)
    val size64 = (SMLofNJ.SysInfo.getHostSize() = 64)

  (* assembly directives *)
    val textSection = "\t.text\n"
    val alignCode = if size64 then "\t.p2align 4\n" else "\t.p2align 3\n"
    val alignData = "\t.p2align 2\n"
    fun long n = concat ["\t.long ", Int.toString n, "\n"]
    fun global lab = concat["\t.globl ", lab, "\n"]
    fun label s = s ^ ":\n"

    fun doFile (inf, outf) = let
	  val inS = BinIO.openIn inf
	  val outS = TextIO.openOut outf
	  fun out s = TextIO.output (outS, s)
	  fun finish nb = (
		out textSection;
		out alignData;
		out (label "_smlnj_heap_image_len");
		out (long nb))
	  fun lineOut bytes = let
		fun b2s b = Word8.fmt StringCvt.DEC b
		in
		  out (concat[
		      "\t.byte ",
		      String.concatWithMap "," b2s (Word8Vector.toList bytes),
		      "\n"
		    ])
		end
	  fun lines nb = let
		val bytes = BinIO.inputN (inS, bytesPerLine)
		in
		  case Word8Vector.length bytes
		   of 0 => finish nb
		    | n => (
			lineOut bytes;
			if (n < bytesPerLine)
			  then finish (nb + n)
			  else lines (nb + n))
		  (* end case *)
		end
	  in
	    out (global "_smlnj_heap_image");
	    out (global "_smlnj_heap_image_len");
	    out textSection;
	    out alignCode;
	    out (label "_smlnj_heap_image");
	    lines 0;
	    BinIO.closeIn inS;
	    TextIO.closeOut outS
	  end

    fun complain (p, s) = (
	  TextIO.output (TextIO.stdErr, concat [p, ": ", s, "\n"]);
	  OS.Process.failure)

    fun main (p, [inf, outf]) = (
	(doFile (inf, outf); OS.Process.success)
	  handle e => complain (p, "exception: " ^ General.exnMessage e))
      | main (p, _) = complain (p, "usage: " ^ p ^ " heapfile asmfile")

  end
