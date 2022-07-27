(* err.sml
 *
 * COPYRIGHT (c) 2006 
 * John Reppy (http://www.cs.uchicago.edu/~jhr)
 * Aaron Turon (http://www.cs.uchicago.edu/~adrassi)
 * All rights reserved.
 *
 * Global error handling, including printing of error messages
 * and a flag to trigger halting of the tool.
 *)

structure Err = 
  struct

  (* signal that the program should be aborted with no further messages printed *)
    exception Abort

    type pos = string * int * int
(*    type pos = StreamPos.pos *)
    type span = pos * pos

(*
    val emptySpan = let val npos = 0 in (npos, npos) end
*)
    val emptySpan = let val npos = ("",0,0) in (npos, npos) end

  (* global flag to record the existance of errors *)
    val anyErrors = ref false
    val leftRecurs : string list ref = ref []

    fun abortIfErr() = if !anyErrors then raise Abort else ()

    fun errMsg l = (
	  anyErrors := true;
	  TextIO.output(TextIO.stdErr, String.concat l ^ "\n"))

    fun warning l = 
	  TextIO.output(TextIO.stdErr, String.concat l ^ "\n")

    local
      fun lc2str (l, c) = Int.toString l ^ "." ^ Int.toString c
    in

    fun pos2str  (fname, l, c) = "[" ^ fname ^ ":" ^ lc2str (l, c) ^ "]"
    fun span2str ((fname, l1, c1), (_, l2, c2)) = 
	  if l1 = l2 andalso c1 = c2 
	  then pos2str (fname, l1, c1)
	  else
	    "[" ^ fname ^ ":" ^ lc2str (l1, c1) ^ "-" ^ lc2str (l2, c2) ^ "]"
(*
    fun pos2str _ = ""
    fun span2str _ = ""
*)

  (* error function at a single position *)
    fun posErr (pos, msg) = errMsg (["Error ", pos2str pos, ": "]@msg)
  (* error function over a position span *)
    fun spanErr (span, msg) = errMsg (["Error ", span2str span, ": "]@msg)

    end

  (* left recursion detected *)
    fun leftRecur name = 
	if List.exists (fn n => (n = name)) (!leftRecurs)
	then ()
	else (
	  leftRecurs := name::(!leftRecurs);
	  warning ["Left recursion detected: ", name, " -> ", name, " ..."])

    val printDebug = ref true

  (* print a debugging message *)
    fun debug s = if !printDebug
          then (TextIO.output(TextIO.stdErr, s); TextIO.output(TextIO.stdErr, "\n"))
          else ()
    fun debugs ss = debug (concat ss)

  (* print a status message *)
    fun status s = TextIO.output(TextIO.stdErr, concat ["[ml-antlr: ", s, "]\n"])
    fun statuss ss = status (concat ss)

  end
