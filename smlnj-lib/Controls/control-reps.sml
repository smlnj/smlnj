(* control-reps.sml
 *
 * COPYRIGHT (c) 2015 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure ControlReps =
  struct

  (* priorities are used for ordering help messages (lexical order) *)
    type priority = int list

    datatype 'a control = Ctl of {
	name : Atom.atom,		(* name of the control *)
	set : 'a option -> unit -> unit,(* function to set the control's value;
					 * it is delayed (error checking in 1st
					 * stage, actual assignment in 2nd);
					 * if the argument is NONE, then
					 * the 2nd stage will restore the
					 * value that was present during the
					 * first stage *)
	get : unit -> 'a,		(* return the control's value *)
	priority : priority,		(* control's priority *)
	obscurity : int,		(* control's detail level; higher means *)
					(* more obscure *)
	help : string			(* control's description *)
      }

    withtype ('a, 'b) control_set =
	  {ctl : 'a control, info : 'b} AtomTable.hash_table

  (* conversion functions for control values *)
    type 'a value_cvt = {
	tyName : string,
	fromString : string -> 'a option,
	toString : 'a -> string
      }

    fun priorityGT priorities = List.collate Int.compare priorities = GREATER

  end
