(* compinfo.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure CompInfo =
  struct

  (* only used in the form Absyn.dec compInfo, so the
   *  'absyn parameter not needed:
   *
   * type compInfo = { ... tranform: Absyn.dec -> Absyn.dec, ...}
   *)
    type 'absyn compInfo = {
	mkStamp: unit -> Stamps.stamp,
	mkLvar: Symbol.symbol option -> LambdaVar.lvar,
	anyErrors: bool ref,
	error: ErrorMsg.errorFn,
	errorMatch: SourceMap.region -> string,
	transform: 'absyn -> 'absyn, (* normally the identity *)
	sourceName : string
      }

    fun mkCompInfo { source, transform : 'a -> 'a, mkStampGenerator } : 'a compInfo = let
          val { error, errorMatch, anyErrors } = ErrorMsg.errors source
          val _ = LambdaVar.clear () (* reset base lambda var to 0 *)
          val gen = mkStampGenerator ()
          fun mkLvar NONE = LambdaVar.mkLvar ()
            | mkLvar (SOME sym) = LambdaVar.namedLvar sym
	  in {
	    mkStamp = fn () => Stamps.fresh gen,
	    mkLvar = mkLvar,
	    anyErrors = anyErrors,
	    error = error,
	    errorMatch = errorMatch,
	    transform = transform,
	    sourceName = #fileOpened source
	  } end

    fun anyErrors (ci : 'a compInfo) = ! (#anyErrors ci)

  end (* structure CompInfo *)
