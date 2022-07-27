(* parse-tree.sml
 *
 * Parse tree representation of an ASDL specification
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure ParseTree =
  struct

    type 'a mark = 'a Error.mark

    type id = Atom.atom mark

  (* possibly qualified identifier *)
    type qual_id = id option * id

    val topId = Atom.atom "<top>"

    datatype file = File of {
	  includes : string mark list,
	  decls : decl list
	}

    and decl
      = D_Mark of decl mark
      | D_Module of {		(* `module` <id> <imports> `{` <decls> `}` *)
	    name : id,
	    imports : import list,
	    decls : type_decl list
	  }
      | D_Primitive of {	(* `primitive` <id> `{` <exports> `}` *)
	    name : id,
	    exports : id list
	  }
      | D_View of {		(* `view` <id> `{` <entries> `}` *)
	    name : id,
	    entries : view_entry list
	  }

    and import
      = Import_Mark of import mark
      | Import of {
	    module : id,
            alias : id option
	  }

    and type_decl
      = TD_Mark of type_decl mark
      | TD_Alias of {
	    name : id,
	    def : qual_id * tycon option
	  }
      | TD_Sum of {
	    name : id,
	    attribs : field list,
	    cons : cons list
	  }
      | TD_Product of {
	    name : id,
	    fields : field list
	  }

    and field
      = Field_Mark of field mark
      | Field of {
	    typ : qual_id,		(* type of field *)
	    tycon : tycon option,	(* type operator *)
	    label : id option		(* optional field label *)
	  }

    and cons
      = Cons_Mark of cons mark
      | Cons of id * field list

    and tycon = Optional | Sequence | Shared

    and view_entry
      = VEntry_Mark of view_entry mark
      | VEntry of view_entity list * view_property list
      | VEntry_Multiple of id * (view_entity * string) list

    and view_entity
      = VEntity_Mark of view_entity mark
      | VEntity_File			(* '<top>' *)
      | VEntity_Module of id		(* 'module' <module> *)
      | VEntity_Type of id * id		(* <module> '.' <type> *)
      | VEntity_AllCons of id * id	(* <module> '.' <type> '.*' *)
      | VEntity_Cons of id * id * id	(* <module> '.' <type> '.' <cons> *)

    and view_property
      = VProp_Mark of view_property mark
      | VProp of id * string

  end
