(* control-registry.sml
 *
 * COPYRIGHT (c) 2015 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure ControlRegistry : CONTROL_REGISTRY =
  struct

    structure Rep = ControlReps
    structure CSet = ControlSet
    structure ATbl = AtomTable

    type control_info = {
	envName : string option
      }

    type ctl_set = (string, control_info) Rep.control_set

    datatype registry = Reg of {
	help : string,			(* registry's description *)
	ctls : ctl_set,			(* control's in this registry *)
	qRegs : subregistry ATbl.hash_table, (* qualified sub-registries *)
	uRegs : subregistry list ref	(* unqualified sub-registries *)
      }

    and subregistry = SubReg of {
	prefix : string option,		(* the key for qualified registries *)
	priority : Controls.priority,	(* control's priority *)
	obscurity : int,		(* registry's detail level; higher means *)
					(* more obscure *)
	reg : registry
      }

    fun new {help} = Reg{
	    help = help,
	    ctls = CSet.new(),
	    qRegs = ATbl.mkTable (8, Fail "qualified registries"),
	    uRegs = ref[]
	  }

  (* register a control *)
    fun register (Reg{ctls, ...}) {ctl, envName} =
	  CSet.insert (ctls, ctl, {envName=envName})

  (* register a set of controls *)
    fun registerSet (Reg{ctls, ...}) {ctls=cs, mkEnvName} = let
	  fun insert {ctl, info} =
		CSet.insert (ctls, ctl, {envName=mkEnvName(Controls.name ctl)})
	  in
	    CSet.app insert cs
	  end

  (* nest a registry inside another registry *)
    fun nest (Reg{uRegs, qRegs, ...}) {prefix, pri, obscurity, reg} = let
	  val subReg = SubReg{
		  prefix = prefix,
		  priority = pri,
		  obscurity = obscurity,
		  reg = reg
		}
	  in
	    case prefix
	     of NONE => uRegs := subReg :: !uRegs
	      | SOME qual => ATbl.insert qRegs (Atom.atom qual, subReg)
	    (* end case *)
	  end

    fun control reg (path : string list) = let
	  fun find (_, []) = NONE
	    | find (Reg{ctls, uRegs, ...}, [name]) = (
		case CSet.find(ctls, name)
		 of SOME{ctl, ...} => SOME ctl
		  | NONE => findInList (!uRegs, [name])
		(* end case *))
	    | find (Reg{qRegs, uRegs,...}, prefix::r) = (
		case ATbl.find qRegs prefix
		 of NONE => findInList(!uRegs, prefix::r)
		  | SOME(SubReg{reg, ...}) => (case find(reg, r)
		       of NONE => findInList(!uRegs, prefix::r)
			| someCtl => someCtl
		      (* end case *))
		(* end case *))
	  and findInList ([], _) = NONE
	    | findInList (SubReg{reg, ...}::r, path) = (case find (reg, path)
		 of NONE => findInList(r, path)
		  | someCtl => someCtl
		(* end case *))
	  in
	    find (reg, List.map Atom.atom path)
	  end

  (* initialize the controls in the registry from the environment *)
    fun init (Reg{ctls, qRegs, uRegs, ...}) = let
	  fun initCtl {ctl, info={envName=SOME var}} = (
		case OS.Process.getEnv var
		 of SOME value => Controls.set(ctl, value)
		  | NONE => ()
		(* end case *))
	    | initCtl _ = ()
	  fun initSubreg (SubReg{reg, ...}) = init reg
	  in
	    CSet.app initCtl ctls;
	    ATbl.app initSubreg qRegs;
	    List.app initSubreg (!uRegs)
	  end

    datatype registry_tree = RTree of {
	path : string list,
	help : string,
	ctls : { ctl: string Controls.control, info: control_info } list,
	subregs : registry_tree list
      }

    val sortSubregs =
	  ListMergeSort.sort
	    (fn (SubReg{priority=p1, ...}, SubReg{priority=p2, ...}) =>
	      Rep.priorityGT(p1, p2))

    fun controls (root, obs) = let
	(* a function to build a list of subregistries, filtering by obscurity *)
	  val gather = (case obs
		 of NONE => op ::
		  | SOME obs => (fn (x as SubReg{obscurity, ...}, l) =>
		      if (obscurity < obs) then x::l else l)
		(* end case *))
	  fun getTree (path, root as Reg{help, ctls, qRegs, uRegs, ...}) = let
		val subregs =
		      List.foldl gather (ATbl.fold gather [] qRegs) (!uRegs)
		val subregs = sortSubregs subregs
		fun getReg (SubReg{prefix=SOME prefix, reg, ...}) =
		      getTree(prefix::path, reg)
		  | getReg (SubReg{reg, ...}) = getTree (path, reg)
		in
		  RTree{
		      path = List.rev path,
		      help = help,
		      ctls = case obs
			      of NONE => ControlSet.listControls ctls
			       | SOME obs =>
				   ControlSet.listControls' (ctls, obs)
		             (* end case *),
		      subregs = List.map getReg subregs
		    }
		end
	  in
	    getTree ([], root)
	  end

  end
