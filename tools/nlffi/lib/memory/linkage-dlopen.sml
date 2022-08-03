(*  linkage-dlopen.sml
 *
 * This module implements a high-level interface for dlopen.
 *   While addresses (those obtained by applying function "addr" below
 *   or addresses derived from those) will not remain valid across
 *   export{ML,Fn}/restart, handles *will* stay valid.
 *
 * Copyright (c) 2004 by The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (blume@tti-c.org)
 *)
structure DynLinkage :> DYN_LINKAGE = struct

    exception DynLinkError of string

    local
	type era = unit ref
	type addr = Word32.word

	(* a handle remembers an address and the era of its creation as
	 * well as a function to re-create the address when necessary *)
	type h = (addr * era) ref * (unit -> addr)
    in
        type lib_handle = h
	type addr_handle = h
    end

    local
	structure CI = Unsafe.CInterface

	(* low-level linkage via dlopen/dlsym *)
	val dlopen : string option * bool * bool -> Word32.word =
	    CI.c_function "UNIX-Dynload" "dlopen"
	val dlsym : Word32.word * string -> Word32.word =
	    CI.c_function "UNIX-Dynload" "dlsym"
	val dlerror : unit -> string option =
	    CI.c_function "UNIX-Dynload" "dlerror"
	val dlclose : Word32.word -> unit =
	    CI.c_function "UNIX-Dynload" "dlclose"

	(* label used for CleanUp *)
	val label = "DynLinkNewEra"

	(* generate a new "era" indicator *)
	fun newEra () = ref ()

	(* the current era *)
	val now = ref (newEra ())

	(* make a handle, remember era of creation of its current value *)
	fun mkHandle f = (ref (f (), !now), f)

	(* fetch from a handle; use the stored address if it was created
	 * in the current era, otherwise regenerate the address *)
	fun get (r as ref (a, e), f) =
	    if e = !now then a
	    else let val a = f ()
		 in r := (a, !now); a
		 end

	(* call a dl-function and check for errors *)
	fun checked dlf x = let
	    val r = dlf x
	in
	    case dlerror () of
		NONE => r
	      | SOME s => raise DynLinkError s
	end

	(* add a cleanup handler that causes a new era to start
	 * every time the runtime system is started anew *)
	open SMLofNJ.Internals.CleanUp
	val _ = addCleaner (label, [AtInit, AtInitFn],
			    fn _ => now := newEra ())
    in
        val main_lib = mkHandle (fn () => checked dlopen (MainLib.name, true, true))

	fun open_lib' { name, lazy, global, dependencies } =
	    mkHandle (fn () => (app (ignore o get) dependencies;
				checked dlopen (SOME name, lazy, global)))
	fun open_lib { name, lazy, global } =
	    open_lib' { name = name, lazy = lazy, global = global,
			dependencies = [] }

	fun lib_symbol (lh, s) = mkHandle (fn () => checked dlsym (get lh, s))

	val addr = get

	fun close_lib lh = dlclose (get lh)
    end
end
