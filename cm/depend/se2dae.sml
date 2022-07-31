(*
 * Convert a given static env to a "dependency-analysis env".
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
signature STATENV2DAENV = sig
    val cvt : StaticEnv.staticEnv -> DAEnv.env * (unit -> SymbolSet.set)

    (* The thunk passed to cvtMemo will not be called until the first
     * attempt to query the resulting DAEnv.env.
     * If the symbols for which queries succeed are known, then one
     * should further guard the resulting env with an appropriate filter
     * to avoid queries that are known in advance to be unsuccessful
     * because they would needlessly cause the thunk to be called. *)
    val cvtMemo : (unit -> StaticEnv.staticEnv) -> DAEnv.env
end

structure Statenv2DAEnv :> STATENV2DAENV = struct

    structure BSE = BrowseStatEnv

    fun cvt_fctenv look = DAEnv.FCTENV (cvt_result o look)

    and cvt_result (BSE.Env { look, ... }) = SOME (cvt_fctenv look)
      | cvt_result BSE.NoEnv = NONE

    fun cvt sb = let
	fun l2s l = let
	    fun addModule ((sy, _), set) =
		case Symbol.nameSpace sy of
		    (Symbol.STRspace | Symbol.SIGspace |
		     Symbol.FCTspace | Symbol.FSIGspace) =>
		    SymbolSet.add (set, sy)
		   | _ => set
	in
	    foldl addModule SymbolSet.empty l
	end
	val dae = cvt_fctenv (BSE.browse sb)
	fun mkDomain () = l2s (StaticEnv.sort sb)
    in
	(dae, mkDomain)
    end

    fun cvtMemo getSB = let
	val l = ref (fn s => raise Fail "se2dae: uninitialized")
	fun looker s = let
	    fun getCME () = BSE.browse (getSB ())
	    val lk = cvt_result o (getCME ())
	in
	    l := lk;
	    lk s
	end
    in
	l := looker;
	DAEnv.FCTENV (fn s => !l s)
    end
end
