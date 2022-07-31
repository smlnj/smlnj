(* controls-sig.sml
 *
 * COPYRIGHT (c) 2015 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * The Controls structure provides a uniform way to manage controls that can
 * be set from the command line or from environment variables.
 *)

signature CONTROLS =
  sig

    type priority = int list
    type 'a control

  (* a converter for control values *)
    type 'a value_cvt = {
	tyName : string,
	fromString : string -> 'a option,
	toString : 'a -> string
      }

  (* create a new control *)
    val control : {
	    name : string,	(* name of the control *)
	    pri : priority,	(* control's priority *)
            obscurity : int,	(* control's detail level; higher means *)
				(* more obscure *)
	    help : string,	(* control's description *)
	    ctl : 'a ref	(* ref cell holding control's state *)
	  } -> 'a control

  (* generate a control *)
    val genControl : {
	    name : string,
	    pri : priority,
            obscurity : int,
	    help : string,
	    default : 'a
	  } -> 'a control

  (* this exception is raised to communicate that there is a syntax error
   * in a string representation of a control value.
   *)
    exception ValueSyntax of {tyName : string, ctlName : string, value : string}

  (* create a string control from a typed control *)
    val stringControl : 'a value_cvt -> 'a control -> string control

  (* control operations *)
    val name : 'a control -> string
    val get : 'a control -> 'a
    val set : 'a control * 'a -> unit
    val set' : 'a control * 'a -> unit -> unit (* delayed; error checking in 1st stage *)
    val help : 'a control -> string
    val info : 'a control -> {priority : priority, obscurity : int, help : string}

  (* package a boolean control as a GetOpt option descriptor (NoArg) *)
    val mkOptionFlag : {
	    ctl : bool control,		(* the control that will be set by the command-line option *)
	    short : string,		(* the short name for the option; either zero or one chars *)
	    long : string option	(* an optional long-name for the option *)
	  } -> unit GetOpt.opt_descr

  (* package a string control as a GetOpt option descriptor with required argument (ReqArg)  *)
    val mkOptionReqArg : {
	    ctl : string control,	(* the control that will be set by the command-line option *)
	    arg : string,		(* the name for the argument, which is used in the usage message *)
	    short : string,		(* the short name for the option; either zero or one chars *)
	    long : string option	(* an optional long-name for the option *)
	  } -> unit GetOpt.opt_descr

  (* package a string control as a GetOpt option descriptor with an optional argument (OptArg) *)
    val mkOption : {
	    ctl : string control,	(* the control that will be set by the command-line option *)
	    arg : string,		(* the name for the argument, which is used in the usage message *)
	    default : string,		(* the default value for when no argument is given *)
	    short : string,		(* the short name for the option; either zero or one chars *)
	    long : string option	(* an optional long-name for the option *)
	  } -> unit GetOpt.opt_descr

  (* capture current value (1st stage) and restore it (2nd stage) *)
    val save'restore : 'a control -> unit -> unit

  (* compare the priority of two controls *)
    val compare : ('a control * 'a control) -> order

  end
