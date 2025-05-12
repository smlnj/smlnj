(*
 * Implementation of environments for CM "preprocessor" variables.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
signature SYMVAL = sig

    type env

    val look : env -> string -> int option
    val empty : env
    val define : env * string * int option -> env

    val default : { arch: string,
		    big: bool,
		    size: int,
		    os: SMLofNJ.SysInfo.os_kind,
		    version: int list,
		    extra_syms: string list }
	-> env
end

structure SymVal :> SYMVAL =
  struct

    type env = int StringMap.map

    fun look e s = StringMap.find (e, s)

    val empty = StringMap.empty

    fun define (e, s, NONE) =
	(#1 (StringMap.remove (e, s))
	 handle LibBase.NotFound => e)
      | define (e, s, SOME v) = StringMap.insert (e, s, v)

    fun default { arch, big, size, os, version, extra_syms } = let
	val arch_sym = "ARCH_" ^ arch
	val endian_sym = if big then "BIG_ENDIAN" else "LITTLE_ENDIAN"
	val size_sym = "SIZE_" ^ Int.toString size
	val os_syms = (case os
	       of SMLofNJ.SysInfo.UNIX => [("OPSYS_UNIX", 1)]
                  (* for backwards compatibility, we allow two symbols for windows *)
		| SMLofNJ.SysInfo.WINDOWS => [("OPSYS_WIN32", 1), ("OPSYS_WINDOWS", 1)]
	      (* end case *))
	val (major, minor) = (case version
               of [] => (0, 0)
	        | [major] => (major, 0)
                | major :: minor :: _ => (major, minor)
              (* end case *))
	val major_sym = "SMLNJ_VERSION"
	val minor_sym = "SMLNJ_MINOR_VERSION"

	val alldefs = List.foldr (op ::) (
              (arch_sym, 1) :: (endian_sym, 1) :: (size_sym, 1)
                :: (major_sym, major) :: (minor_sym, minor)
                :: ("NEW_CM", 1) :: os_syms)
              (List.map (fn s => (s, 1)) extra_syms)
        in
          List.foldl StringMap.insert' empty alldefs
        end

  end
