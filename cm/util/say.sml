(* say.sml
 *
 * COPYRIGHT (c) 2022 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Output of feedback and diagnostics.
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)

signature SAY = sig

    (* output *)
    val say: string list -> unit

    (* output in verbose mode only *)
    val vsay: string list -> unit

    (* output in debug mode only *)
    val dsay: string list -> unit

  end

structure Say :> SAY = struct

    structure Print = PrintControl

    fun say l = (Print.say (concat l); Print.flush ())

    fun csay { get, set } l = if get () then say l else ()
    val vsay = csay StdConfig.verbose
    val dsay = csay StdConfig.debug

  end
