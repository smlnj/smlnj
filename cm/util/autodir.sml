(*
 * Opening files for output while automagically creating any
 * necessary directories.
 *
 * Copyright (c) 1999 by Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@cs.princeton.edu)
 *)
structure AutoDir :> sig
    val openBinOut : string -> BinIO.outstream
    val openTextOut : string -> TextIO.outstream
    val makeDirs : string -> unit
end = struct

    structure P = OS.Path
    structure F = OS.FileSys

    fun fileExists n = F.access (n, []) handle _ => false

    fun openEither fileopener p = let
	fun mkDir d =
	    F.mkDir d handle exn => (if fileExists d then () else raise exn)
	fun generic (maker, pmaker, p) =
	    maker p
	    handle exn => let
		val dir = P.dir p
	    in
		(* If the parent dir exists, then we must consider
		 * these cases:
		 *   - non-parallel: we should signal an error
		 *   - parallel: somebody else may have made this dir
		 *      in the meantime, so we should try again
		 * Both cases can be handled by simply calling maker
		 * again.  (It will fail in the non-parallel case, but
		 * that's actually what we want.) *)
		if dir = "" orelse fileExists dir then maker p
		else (pmaker dir; maker p)
	    end
	fun makedirs dir = generic (mkDir, makedirs, dir)
	fun advertisemakedirs dir =
	    (Say.vsay ["[creating directory ", dir, "]\n"];
	     makedirs dir)
    in
	generic (fileopener, advertisemakedirs, p)
    end

    (* In the open-for-output case we first get rid of the file if it
     * already existed... *)
    fun openOut fileopener =
	openEither (fn n => (if fileExists n then (F.remove n handle _ => ())
			     else ();
			     fileopener n))

    val openTextOut = openOut TextIO.openOut
    val openBinOut = openOut BinIO.openOut

    (* makeDirs is supposed to make all directories leading up to a
     * given file.  The file itself is supposed to be left alone if
     * it already existed.  The trick here is to (ab)use our openOut
     * function with a "maker" parameter set to "BinIO.openIn".  This
     * is pretty hack-ish, but it allows us to reuse the existing logic. *)
    local
	exception NonexistentFile
	fun boi f = BinIO.openIn f handle _ => raise NonexistentFile
    in
        fun makeDirs f = BinIO.closeIn (openEither boi f)
	    handle NonexistentFile => ()
    end
end
