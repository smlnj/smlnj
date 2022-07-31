(* control-registry-sig.sml
 *
 * COPYRIGHT (c) 2015 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * A registry collects together string controls; it supports generation
 * of help messages and initialization from the environment.
 *)

signature CONTROL_REGISTRY =
  sig

    type registry

    type control_info = { envName : string option }

    val new : {
	    help : string	(* registry's description *)
	  } -> registry

  (* register a control *)
    val register : registry -> {
	    ctl : string Controls.control,
	    envName : string option
	  } -> unit

  (* register a set of controls *)
    val registerSet : registry -> {
	    ctls : (string, 'a) ControlSet.control_set,
	    mkEnvName : string -> string option
	  } -> unit

  (* nest a registry inside another registry *)
    val nest : registry -> {
	    prefix : string option,
	    pri : Controls.priority,	(* registry's priority *)
            obscurity : int,		(* registry's detail level; higher means *)
					(* more obscure *)
	    reg : registry
	  } -> unit

  (* find a control *)
    val control : registry -> string list -> string Controls.control option

  (* initialize the controls in the registry from the environment *)
    val init : registry -> unit

    datatype registry_tree = RTree of {
	path : string list,
	help : string,
	ctls : { ctl : string Controls.control, info : control_info } list,
	subregs : registry_tree list
      }

  (* get the registry-tree representation of a registry; an optional obscurity
   * argument may be supplied to filter out obscure options.
   *)
    val controls : (registry * int option) -> registry_tree

  end
