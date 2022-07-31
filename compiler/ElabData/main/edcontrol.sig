(* edcontrol.sig
 *
 * (C) 2021 The Fellowship of SML/NJ
 *)

signature ELABDATA_CONTROL =
sig

  val saveLvarNames : bool ref

  val envdebugging : bool ref
      (* Env debugging *)

  val eedebugging : bool ref
      (* EntityEnv debugging *)

  val mudebugging : bool ref
      (* ModuleUtil debugging *)

  val tudebugging : bool ref
      (* TypesUtil debugging *)

  val tpdebugging : bool ref
      (* PPType debugging *)

  val ppabsyndebugging : bool ref
      (* PPAbsyn debugging *)

  val typesInternals : bool ref
      (* show types internal reps *)

  val typeUnalias : bool ref
      (* "unalias" types in PPType *)

  val mpdebugging : bool ref
      (* PPMod debugging *)

  val modulesInternals : bool ref
      (* show modules internal reps *)

  val absynLineprint : bool ref
      (* PPAbsyn: print positions as line number + column *)

  val absynInternals : bool ref
      (* PPAbsyn: show absyn internal reps (e.g. MARKs) *)

  val varconInternals : bool ref
      (* show value/constructor internal reps *)

  val internals : bool ref
      (* (general) show internal reps *)

  val setInternals : unit -> bool * bool * bool * bool * bool
      (* set all internals controls to true, returning "former" values *)

  val resetInternals : bool * bool * bool * bool * bool -> unit
      (* set all internals controls to new values given by arguments *)

  val boxedconstconreps : bool ref
      (* constructor representation (used in ConRep) *)

end (* signature ELABDATA_CONTROL *)
