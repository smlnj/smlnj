(* cm/depend/dbm/se2dae.sml
 *
 * Convert a static env to a "dependency-analysis env" (DAEnv.env).
 *
 * (C) 2023 The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (matthias.blume@gmail.com)
 *)

signature STATENV2DAENV =
sig

  val cvt : StaticEnv.staticEnv -> DAEnv.env * SymbolSet.set

  (* [DBM] The staticEnv passed to cvtMemo in stabilize.sml (lazy_statenv) is doubly
   * thunked when it is created in stabilize.sml, but then it is forced once to define the
   * local variable "ge" in impexp, and can easily be forced again as it is passed
   * to cvtMemo: "cvtMemo (ge ())".  Thus cvtMemo can just take a staticEnv argument.
   *
   * [Blume] If the symbols for which queries succeed are known, then one
   * should further guard the resulting env with an appropriate filter
   * to avoid queries that are known in advance to be unsuccessful
   * because they would needlessly cause the thunk to be called. (???) *)
  val cvtMemo : StaticEnv.staticEnv -> DAEnv.env

end (* signature STATENV2DAENV *)

structure Statenv2DAEnv :> STATENV2DAENV =
struct

local (* top *)

  structure S = Symbol          (* compiler/Basics/symbol *)
  structure SE = StaticEnv      (* compiler/ElabData/statenv *)
  structure BSE = BrowseStatEnv (* compiler/ElabData/statenv *)

  structure E = DAEnv
  structure SS = SymbolSet

in (* top local *)

    (* cvt_look : (S.symbol -> BSE.bind_info) -> E.env *)
    fun cvt_look (look: S.symbol -> BSE.bind_info) : E.env = 
	E.FCTENV (cvt_result o look)

    (* cvt_bind_info : BSE.bind_info -> E.env option *)
    and cvt_bind_info (BSE.Env { look, ... }) = SOME (cvt_look look)
      | cvt_bind_info BSE.NoEnv = NONE

    (* cvt : SE.staticEnv -> E.env * SS.set *)
    (* there does not seem to be any reason to thunkify the returned domain symbol set,
     * so we do not thunkify it. *)
    fun cvt (statenv: SE.staticEnv) =
	let fun moduleSymbols (symbols : S.symbol list)  =
		let (* addModule : S.symbol * SS.set -> SS.set *)
		    fun addModule (sym, set) =
		        (case S.nameSpace sym
			   of (S.STRspace | S.SIGspace | S.FCTspace | S.FSIGspace) =>
			      SS.add (set, sy)
			    | _ => set)  (* core symbols and fixity not included *)
		 in foldl addModule SS.empty symbols
		end
	    val dae = cvt_look (BSE.browse statenv)
	    val domain = moduleSymbols (StaticEnv.symbols statenv)

	 in (dae, domain)
	end

(*
    (* cvtMemo : (unit -> SE.staticEnv) -> E.env *)
    (* called  once in stabilize.sml on a statenv thunk^2 (lazy_statenv) produced by
     * UU.r_lazy applied to a "session" and a (staticEnv UnpickleUtil.reader) (l 389).
     * lazy_statenv is doubly thunked, since it is "forced" to produce
     * "ge" before cvtMemo is called with ge in impexp.
     *
     * Whether the suspension is doing us any good is unclear -- depends on machinery
     * in stabilize.sml and UnpickleUtil.
     *
     * Anyway, the getSB think is evaluated _every_ time the looker function is called!
     * That may mean that a staticEnv pickle is being unpickled every time the looker
     * function is called. That does not seem right! *)
    fun cvtMemo (getSB: unit -> SE.staticEnv) : E.env =
	let fun looker (s: S.symbol) = cvt_bind_info ((BSE.browse (getSB ())) s)
	 in E.FCTENV looker
	end

    (* alternate version of cvtMemo : (unit -> SE.staticEnv) -> E.env *)
    (* This version forces the staticEnv thunk argument just once, when cvtMemo
     * is called. cvtMemo is called inside the impexp function, so won't be called
     * until impexp is called. *)
    fun cvtMemo (senv_thunk: unit -> SE.staticEnv) : E.env =
	let val browse : S.symbol -> BSE.bind_info = BSE.browse (senv_thunk ())
            val look = cvt_bind_info o browse
	 in E.FCTENV look
	end
*)

    (* Here is a 3rd, simpler version of cvtMemo taking a staticEnv instead of a staticEnv
     * thunk. This could be called inside the impexp function in stabilize.sml as
     * "cvtMemo (ge ())". *)
    (* cvtMemo : SE.staticEnv -> E.env *)
    fun cvtMemo (senv: SE.staticEnv) : E.env = E.FCTENV (cvt_bind_info o (BSE.browse senv))

end (* top local *)
end (* structure Statenv2DAEnv *)
