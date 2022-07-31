(*
 * The "group registry".  CM uses this to remember which groups it is
 * currently working on and what the corresponding input sources are.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
signature GROUPREG = sig

    type groupreg

    val new : unit -> groupreg
    val register :
	groupreg -> SrcPath.file * Source.inputSource -> unit
    val lookup : groupreg -> SrcPath.file -> Source.inputSource
    val registered : groupreg -> SrcPath.file -> bool
    val error : groupreg ->
		SrcPath.file * SourceMap.region -> ErrorMsg.complainer
    val anyErrors : groupreg -> SrcPath.file -> bool
end

structure GroupReg :> GROUPREG = struct

    type groupreg = Source.inputSource SrcPathMap.map ref

    fun new () = ref SrcPathMap.empty : groupreg

    fun register gr (p, s) = gr := SrcPathMap.insert (!gr, p, s)
    fun lookup gr p =
	case SrcPathMap.find (!gr, p) of
	    SOME s => s
	  | NONE => raise Fail ("GroupReg.lookup " ^ SrcPath.descr p)
    fun registered gr g = isSome (SrcPathMap.find (!gr, g))
    fun error gr (g, r) = ErrorMsg.error (lookup gr g) r
    fun anyErrors gr g = !(#anyErrors (lookup gr g : Source.inputSource))
end
