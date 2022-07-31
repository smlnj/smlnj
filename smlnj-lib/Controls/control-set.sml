(* control-set.sml
 *
 * COPYRIGHT (c) 2015 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure ControlSet : CONTROL_SET =
  struct

    structure Rep = ControlReps
    structure ATbl = AtomTable

    type 'a control = 'a Controls.control
    type ('a, 'b) control_set = ('a, 'b) ControlReps.control_set

    fun new () = ATbl.mkTable (16, Fail "control set")

    fun member (cset, name) = (case ATbl.find cset name
	   of NONE => false
	    | _ => true
	  (* end case *))

    fun find (cset, name) = ATbl.find cset name

    fun insert (cset, ctl as Rep.Ctl{name, ...}, info) =
	  ATbl.insert cset (name, {ctl=ctl, info=info})

    fun remove (cset, name) = (case ATbl.find cset name
	   of NONE => ()
	    | _ => ignore (ATbl.remove cset name)
	  (* end case *))

    fun infoOf (cset : ('a, 'b) control_set) (Rep.Ctl{name, ...}) =
	  Option.map #info (ATbl.find cset name)

  (* list the members; the list is ordered by descreasing priority.  The
   * listControls' function allows one to specify an obscurity level; controls
   * with equal or higher obscurioty are omitted from the list.
   *)
    local
      fun priorityOf {ctl=Rep.Ctl{priority, ...}, info} = priority
      fun gt (a, b) = Rep.priorityGT(priorityOf a, priorityOf b)
    in
    fun listControls cset = ListMergeSort.sort gt (ATbl.listItems cset)

    fun listControls' (cset, obs) = let
	  fun add (item as {ctl=Rep.Ctl{obscurity, ...}, info}, l) =
		if (obs > obscurity)
		  then item::l
		  else l
	  in
	    ListMergeSort.sort gt (ATbl.fold add [] cset)
	  end
    end (* local *)

    fun app f cset = ATbl.app f cset

  (* convert the controls in a set to string controls and create a new set
   * for them.
   *)
    fun stringControls cvt cset = let
	  val stringCtl = Controls.stringControl cvt
	  fun cvtCtl {ctl, info} = {ctl = stringCtl ctl, info = info}
	  in
	    ATbl.map cvtCtl cset
	  end

  end

