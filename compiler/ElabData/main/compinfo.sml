(* compinfo.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure CompInfo =
  struct

   (* compInfo: elements passed to many compilation phases
    *   formerly contained a "transfor" element that is no longer used.
    *   The idea is/was that mkStamp and mkLvar should not be "global",
    *   but in fact, the same mkStamp and mlLvar function are used everywhere (?)
    *   so having them as part of a compInfo argument does nothing. *)
   type compInfo =
        {mkStamp: unit -> Stamps.stamp,
         mkLvar: Symbol.symbol option -> LambdaVar.lvar,
         source : Source.inputSource}

    fun mkCompInfo {source : Source.inputSource, mkStampGenerator : unit -> Stamps.generator}
	           : compInfo =
	let val _ = LambdaVar.clear () (* reset base lambda var to 0 *)
            val gen = mkStampGenerator ()
            fun mkLvar NONE = LambdaVar.mkLvar ()
              | mkLvar (SOME sym) = LambdaVar.namedLvar sym
	  in {mkStamp = fn () => Stamps.fresh gen,
	      mkLvar = mkLvar,
	      source = source}
	end

    fun anyErrors ({source, ...} : 'a compInfo) = ! (#anyErrors source)

  end (* structure CompInfo *)
