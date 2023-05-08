(* edcontrol.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *)

structure ElabDataControl : ELABDATA_CONTROL =
struct

  val {newBool = new, ...} = MakeControls.make {name = "CodeGen", priority = [1]}

  val saveLvarNames = new ("save-lvar-names", "save Lvar names", false)
  val envdebugging = new ("env-debugging", "Env debugging", false)
  val eedebugging = new ("ee-debugging", "EntityEnv debugging", false)
  val mudebugging = new ("mu-debugging", "ModuleUtil debugging", false)
  val ppabsyndebugging = new ("ppabsyn-debugging", "PPAbsyn debugging", false)

  val tudebugging = new ("tu-debugging", "TypesUtil debugging", false)
      (* TypesUtil *)
  val tpdebugging = new ("tp-debugging", "PPType debugging", false)
      (* PPType -- debugging type pretty-printing *)
  val typeUnalias = new ("typeUnalias", "PPType type unaliasing", true)
      (* PPType -- unalias tycons in paths when computing effective path*)
  val mpdebugging = new ("mp-debugging", "PPMod debugging", false)
      (* PPMod *)
  val boxedconstconreps = new ("boxedconstconreps", "boxed const constructors", false)
      (* ConRep *)

  val typesInternals = new ("types-internals", "show internal types reps", false)
  val modulesInternals = new ("modules-internals", "show internal module reps", false)
  val absynLineprint = new ("absyn-lineprint", "PPAbsyn: print positions as line,column", false)
  val absynInternals = new ("absyn-internals", "PPAbsyn: show internal absyn info", false)
  val varconInternals = new ("varcon-internals", "show internal var/con reps", false)
  val internals = new ("general-internals", "show internal reps", false)

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
