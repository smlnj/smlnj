(*
 * Bundling information pertaining to the member of a stable group.
 *   - includes information that does not require running
 *     the machine-dependent part of the compiler only
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
signature BININFO = sig

    type info
    type ord_key = info
    type complainer = ErrorMsg.complainer
    type region = SourceMap.region
    type pid = PersStamps.persstamp

    val new : { group: SrcPath.file,
	        mkStablename: unit -> string,
	        error: complainer,
		spec: string,
		offset: int,
		rts_pid: pid option,
		sh_mode: Sharing.mode } -> info

    val compare : info * info -> order
    val describe : info -> string
    val offset : info -> int
    val group : info -> SrcPath.file
    val stablename : info -> string
    val rts_pid : info -> pid option
    val sh_mode : info -> Sharing.mode
    val error : info -> complainer
end

structure BinInfo :> BININFO = struct

    type complainer = ErrorMsg.complainer
    type region = SourceMap.region
    type pid = PersStamps.persstamp

    datatype info =
	INFO of { group: SrcPath.file,
		  mkStablename: unit -> string,
		  spec: string,
		  offset: int,
		  rts_pid: pid option,
		  sh_mode: Sharing.mode,
		  error: complainer }

    type ord_key = info

    val new = INFO

    fun compare (INFO i, INFO i') =
	case Int.compare (#offset i, #offset i') of
	    EQUAL => SrcPath.compare (#group i, #group i')
	  | unequal => unequal

    fun describe (INFO { group, spec, offset, ... }) =
	concat [SrcPath.descr group, "@", Int.toString offset, "(", spec, ")"]

    fun group (INFO { group = g, ... }) = g
    fun offset (INFO { offset = os, ... }) = os
    fun rts_pid (INFO { rts_pid = p, ... }) = p
    fun sh_mode (INFO { sh_mode = s, ... }) = s
    fun stablename (INFO { mkStablename = msn, ... }) = msn ()

    fun error (INFO { error = e, ... }) = e
end
