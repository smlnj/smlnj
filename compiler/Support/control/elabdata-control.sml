(* Admin/control/elabdata-control.sml
 *
 * (C) 2023 The Fellowship of SML/NJ (www.smlnj.org)
 *)

structure ElabDataControl : ELABDATA_CONTROL =
struct

  val saveLvarNames = ref false (* "save lambdavar names" *)
  val envdebugging = ref false (* "Env debugging" *)
  val eedebugging = ref false (* "EntityEnv debugging" *)
  val mudebugging = ref false (* "ModuleUtil debugging" *)
  val ppabsyndebugging = ref false (* "PPAbsyn debugging" *)

  val tudebugging = ref false (* "TypesUtil debugging" *)
  val tpdebugging = ref false (* "PPType debugging" *)
  val typeUnalias = ref true (* "PPType type unaliasing" *)
  val mpdebugging = ref false (* "PPMod debugging" *)

  (* print internal reps *)
  val typesInternals = ref false (* "show internal types reps" *)
  val modulesInternals = ref false (* "show internal module reps" *)
  val absynLineprint = ref false (* "PPAbsyn: print positions as line,column" *)
  val absynInternals = ref false (* "PPAbsyn: show internal absyn info" *)
  val varconInternals = ref false (* "show internal var/con reps" *)
  val internals = ref false (* "show internal reps" *)

  (* conreps *)
  val boxedconstconreps = ref false (* "boxed const constructors" *)

  fun setInternals () =
      let val oldInternals = (!varconInternals, !absynInternals, !typesInternals,
			      !modulesInternals, !internals)
       in varconInternals := true;
	  absynInternals := true;
	  typesInternals := true;
	  modulesInternals := true;
	  internals := true;
	  oldInternals
      end

  fun resetInternals (oldvar, oldabsyn, oldtypes, oldmodules, oldinternals) =
      (varconInternals := oldvar;
       absynInternals := oldabsyn;
       typesInternals := oldtypes;
       modulesInternals := oldmodules;
       internals := oldinternals)


end (* EladDataControl *)
