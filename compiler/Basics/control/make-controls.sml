(* Basics/control/make-controls.sml *)

signature MAKE_CONTROLS =
sig

  val topregistry : ControlRegistry.registry

  val make : {name: string, priority: int list} ->
	     {newBool : string * string * bool -> bool ref,
	      newInt : string * string * int -> int ref,
	      newString : string * string * string -> string ref,
	      newStrings : string * string * string list -> string list ref}

  val registerControl : string * string * bool ref -> unit

end (* signature CONTROLMAKERS *)		 

structure MakeControls :> MAKE_CONTROLS =
struct

   val topregistry = ControlRegistry.new { help = "SML/NJ controls" }

   val bool_cvt = ControlUtil.Cvt.bool
   val int_cvt = ControlUtil.Cvt.int
   val string_cvt = ControlUtil.Cvt.string
   val stringlist_cvt = ControlUtil.Cvt.stringList

   val priority = [1]

   fun make {name: string, priority: int list} = 
       let val newRegistry = ControlRegistry.new {help = name}

           val _ = ControlRegistry.nest topregistry
		     {prefix = SOME name,
		      pri = priority,
		      obscurity = 1,
		      reg = newRegistry}

	   fun 'a new (convert: 'a -> string)
		      (ctlName: string, ctlHelp: string, default: 'a) =
	       let val ctlRef = ref default
		   val ctl = Controls.control
			       {pri = [0], obscurity = 0,
				name = ctlName, help = ctlHelp,
				ctl = ctlRef}
	        in ControlRegistry.register newRegistry
	            {ctl = Controls.stringControl convert ctl,
		     envName = SOME (ControlUtil.EnvName.toUpper name)};
	           ctlRef
	       end

       in {newBool = new bool_cvt,
	   newInt = new int_cvt,
	   newString = new string_cvt,
	   newStrings = new stringlist_cvt}
       end

   (* registerControl only used in TDPInstrument (DebugProf/profile/tdp-instrument) *)
   fun registerControl (name: string, help: string, ctlRef : bool ref) : unit =
       	let val registry = ControlRegistry.new {help = name}
	    val ctl = Controls.control {name = name, pri = [0], obscurity = 1,
					help = help, ctl = ctlRef}
	 in ControlRegistry.nest topregistry
	      {prefix = SOME name, pri = priority, obscurity = 1, reg = newRegistry};
	    ControlRegistry.register registry
	      {ctl = Controls.stringControl ControlUtil.Cvt.bool ctl,
	       envName = SOME (name ^ "_env")}
	end

end (* structure Controls *)
