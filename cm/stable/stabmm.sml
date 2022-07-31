(* stabmm.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *
 * author: Matthias Blume (blume@research.bell-lab.com)
 *)
local
    structure SE = StaticEnv
    structure MI = ModuleId
in
    (* This module implements a "central" modmap for stable libraries.
     * By having only one such map, sharing should be maximized. *)
    signature STAB_MODMAP = sig
	val get : unit -> MI.tmap
	val reset : unit -> unit
	val addEnv : SE.staticEnv -> MI.tmap
    end

    functor StabModmapFn () :> STAB_MODMAP = struct

        val mm = ref MI.emptyTmap

	fun reset () = mm := MI.emptyTmap
	fun get () = !mm

	fun addEnv se = let
	    val m = GenModIdMap.mkMap' (se, !mm)
	in
	    mm := m; m
	end
    end
end
