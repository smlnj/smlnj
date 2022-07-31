(* controls.sml
 *
 * COPYRIGHT (c) 2015 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Controls : CONTROLS =
  struct

    open ControlReps

    fun control {name, pri, obscurity, help, ctl} = Ctl{
	    name = Atom.atom name,
	    get = fn () => !ctl,
	    set = fn SOME v => (fn () => ctl := v)
		   | NONE => let val v = !ctl in fn () => ctl := v end,
	    priority = pri,
	    obscurity = obscurity,
	    help = help
	  }

    fun genControl {name, pri, obscurity, help, default} = control {
	    name = name, pri = pri, obscurity = obscurity, help = help,
	    ctl = ref default
	  }

  (* this exception is raised to communicate that there is a syntax error
   * in a string representation of a control value.
   *)
    exception ValueSyntax of {tyName : string, ctlName : string, value : string}

    fun stringControl {tyName, fromString, toString} (Ctl c) = let
	  val {name, get, set, priority, obscurity, help} = c
	  fun fromString' s = (case fromString s
		 of NONE => raise ValueSyntax{
			tyName = tyName, ctlName = Atom.toString name, value = s
		      }
		  | SOME v => v
		(* end case *))
	  in
	    Ctl{
		name = name,
		get = toString o get,
		set = set o Option.map fromString',
		priority = priority,
		obscurity = obscurity,
		help = help
	      }
	  end

    fun name (Ctl{name, ...}) = Atom.toString name
    fun get (Ctl{get, ...}) = get()
    fun set (Ctl{set, ...}, v) = set (SOME v) ()
    fun set' (Ctl{set, ...}, v) = set (SOME v)
    fun help (Ctl{help, ...}) = help
    fun info (Ctl{priority, obscurity, help, ...}) =
	  { priority = priority, obscurity = obscurity, help = help }

  (* package a boolean control as a GetOpt option descriptor (NoArg).  If the control
   * is initialized to false command-line option will set it to true, whereas if the control
   * is initialized to true, the the command-line option will set it to false.
   *)
    fun mkOptionFlag {ctl=Ctl{get, set, help, ...}, short, long} = {
	    short = short,
	    long = (case long of NONE => [] | SOME opt => [opt]),
	    desc = GetOpt.NoArg(set (SOME(not (get ())))),
	    help = help
	  }

  (* package a string control as a GetOpt option descriptor with required argument (ReqArg)  *)
    fun mkOptionReqArg {ctl=Ctl{set, help, ...}, arg, short, long} = {
	    short = short,
	    long = (case long of NONE => [] | SOME opt => [opt]),
	    desc = GetOpt.ReqArg(fn s => set (SOME s) (), arg),
	    help = help
	  }

  (* package a string control as a GetOpt option descriptor with an optional argument (OptArg) *)
    fun mkOption {ctl=Ctl{set, help, ...}, arg, default, short, long} = {
	    short = short,
	    long = (case long of NONE => [] | SOME opt => [opt]),
	    desc = let
	      fun setFn NONE = set (SOME default) ()
		| setFn someVal = set someVal ()
	      in
		GetOpt.OptArg(setFn, arg)
	      end,
	    help = help
	  }

    fun save'restore (Ctl{set,...}) = set NONE

    fun compare (Ctl{priority=p1, ...}, Ctl{priority=p2, ...}) =
	List.collate Int.compare (p1, p2)

  end
