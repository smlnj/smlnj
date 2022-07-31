(* profile-fn.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * User interface for controling profiling.
 *
 *)

functor ProfileFn (structure ProfEnv: PROF_ENV
		   val pervasive : { get: unit -> ProfEnv.env,
				     set: ProfEnv.env -> unit }) : PROFILE =
  struct

    structure PC = SMLofNJ.Internals.ProfControl

    val report		= Profile.report
    val reportAll	= Profile.reportAll
    val reportData	= Profile.reportData

    val profMode : bool ref = SMLofNJ.Internals.ProfControl.profMode

    local
      val pervDone = ref false
    in
    fun doPerv() =
	  if !pervDone then ()
          else (
	    pervDone := true;
	    Control_Print.say
		"Creating profiled version of standard library\n";
	    ProfEnv.replace pervasive)
    end

    fun setProfMode true = (doPerv(); profMode := true)
      | setProfMode false = (profMode := false)
    fun getProfMode () = !profMode

    fun setTimingMode true = PC.profileOn()
      | setTimingMode false = PC.profileOff()
    val getTimingMode = PC.getTimingMode

    val reset = Profile.reset

  end;
