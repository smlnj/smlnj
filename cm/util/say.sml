(*
 * Output of feedback and diagnostics.
 *
 *   Copyright (c) 1999 by Lucent Technologies, Bell Laboratories.
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
signature SAY = sig
    val say: string list -> unit
    val vsay: string list -> unit
    val dsay: string list -> unit
end

structure Say :> SAY = struct

    structure Print = Control.Print

    fun say l = (Print.say (concat l); Print.flush ())

    fun csay { get, set } l = if get () then say l else ()
    val vsay = csay StdConfig.verbose
    val dsay = csay StdConfig.debug
end
