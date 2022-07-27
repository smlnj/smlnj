(* common-view.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * View properties that are common to all views
 *)

structure CommonView : sig

    val template : View.template

  (* `prop (name, accum)` constructs a property description *)
    val prop : Atom.atom * bool -> View.Prop.desc

  end = struct

    structure PN = PropNames

    fun prop (name, accum) = View.Prop.Desc{name = name, accumulator = accum}

    val template = {
	    fileProps = List.map prop [
		(PN.header, false),
		(PN.name, false),
		(PN.implementation_epilogue, true),
		(PN.implementation_prologue, true),
		(PN.interface_epilogue, true),
		(PN.interface_prologue, true),
		(PN.suppress, false)
	      ],
	    moduleProps = List.map prop [
		(PN.name, false),
		(PN.interface_prologue, true),
		(PN.interface_epilogue, true),
		(PN.implementation_prologue, true),
		(PN.implementation_epilogue, true),
		(PN.suppress, false)
	      ],
	    typeProps = List.map prop [
		(PN.name, false),
		(PN.natural_type, false),
		(PN.writer, false),
		(PN.reader, false),
		(PN.wrapper, false),
		(PN.unwrapper, false)
	      ],
	    consProps = List.map prop [
		(PN.name, false)
	      ]
	  }

  end
