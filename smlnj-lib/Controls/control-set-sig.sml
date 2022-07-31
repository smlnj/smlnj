(* control-set-sig.sml
 *
 * COPYRIGHT (c) 2015 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature CONTROL_SET =
  sig

    type 'a control = 'a Controls.control
    type ('a, 'b) control_set

    val new : unit -> ('a, 'b) control_set

    val member : (('a, 'b) control_set * Atom.atom) -> bool
    val find   : (('a, 'b) control_set * Atom.atom)
	  -> {ctl : 'a control, info : 'b} option
    val insert : (('a, 'b) control_set * 'a control * 'b) -> unit
    val remove : (('a, 'b) control_set * Atom.atom) -> unit
    val infoOf : ('a, 'b) control_set -> 'a control -> 'b option

  (* list the members; the list is ordered by priority.  The listControls'
   * function allows one to specify an obscurity level; controls with equal
   * or higher obscurity are omitted from the list.
   *)
    val listControls : ('a, 'b) control_set -> {ctl : 'a control, info : 'b} list
    val listControls' : (('a, 'b) control_set * int)
	  -> {ctl : 'a control, info : 'b} list

  (* apply a function to the controls in a set *)
    val app : ({ctl : 'a control, info : 'b} -> unit)
	  -> ('a, 'b) control_set -> unit

  (* convert the controls in a set to string controls and create a new set
   * for them.
   *)
    val stringControls : 'a Controls.value_cvt -> ('a, 'b) control_set
	  -> (string, 'b) control_set

  end
