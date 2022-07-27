(* ast.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

local

  structure ModuleId = IdentFn()
  structure TypeId = IdentFn()
  structure ConstrId = IdentFn()
  structure ViewId = IdentFn()

(* functor to add a definition property to an IDENTIFIER structure *)
  functor AddDefPropFn (
      type def
      structure Id : IDENTIFIER
    ) = struct

	type def = def

	open Id

	local
	  val {setFn, peekFn : t -> def option, ...} =
		newProp (fn id => raise Fail(concat[
		    "no definition for '", nameOf id, "'"
		  ]))
	in
	  val bind = setFn
	  val bindingOf = peekFn
	end (* local *)

    end

in

structure AST =
  struct

    datatype module = Module of {
	  isPrim : bool,			(* true for primitive modules *)
	  id : ModuleId.t,
	  decls : type_decl list ref
	}

    and type_decl = TyDcl of {
	  id : TypeId.t,
	  def : ty_def ref,
	  owner : module
	}

    and named_ty
      = BaseTy of TypeId.t
      | ImportTy of ModuleId.t * TypeId.t
      | LocalTy of type_decl

    and ty_def
      = EnumTy of constructor list
      | SumTy of {
	    attribs : field list,
	    cons : constructor list
	  }
      | ProdTy of {
	    fields : field list
	  }
      | AliasTy of ty_exp
      | PrimTy

    and constructor = Constr of {
	    id : ConstrId.t,
	    owner : TypeId.t,
	    fields : field list		(* fields of the constructor (includes attribs) *)
	  }

    and ty_exp
      = Typ of named_ty * tyc

    and tyc = NoTyc | OptTyc | SeqTyc | SharedTyc

  (* field labels are either positional or labeled *)
    and label = Pos of int | Lab of string

    withtype field = {
	  label : label,
	  ty : ty_exp
	}

    structure ModuleId = AddDefPropFn(
	type def = module
	structure Id = ModuleId)

    structure TypeId = struct
	local
	  structure Id = AddDefPropFn(
	      type def = type_decl
	      structure Id = TypeId)
	in
	open Id

      (* is a type ID type bound to a primitive type? *)
	fun isPrim id = let
	      fun isPrimDcl (TyDcl{def, ...}) = (case !def
		     of AliasTy(Typ(nty, NoTyc)) => (case nty
			   of BaseTy id => isPrimId id
			    | ImportTy(_, id) => isPrimId id
			    | LocalTy tyDcl => isPrimDcl tyDcl
			  (* end case *))
		      | PrimTy => true
		      | _ => false
		    (* end case *))
	      and isPrimId id = (case bindingOf id
	             of SOME tyDcl => isPrimDcl tyDcl
		      | NONE => raise Fail(concat[
			    "AST.TypeId.isPrim: no binding for '", nameOf id, "'"
			  ])
		    (* end case *))
	      in
		isPrimId id
	      end
	end (* local *)

      (* property flags to track if a type is used in an optional, sequence, or
       * shared context.
       *)
	local
	  fun mkFlag () = let
		val {getFn, setFn} = newFlag()
		in
		  (getFn, fn x => setFn(x, true))
		end
	in
	val (isOptionArg, markOptionArg) = mkFlag()
	val (isSeqArg, markSeqArg) = mkFlag()
	val (isSharedArg, markSharedArg) = mkFlag()
	end (* local *)
      end (* structure TypeId *)

    structure ConstrId = AddDefPropFn(
	type def = constructor
	structure Id = ConstrId)

  (* get the type ID for a named_ty *)
    fun idOfNamedTy (BaseTy id) = id
      | idOfNamedTy (ImportTy(_, id)) = id
      | idOfNamedTy (LocalTy(TyDcl{id, ...})) = id

  (* debugging support *)
    fun tyToString (Typ(nty, tyc)) = let
	  val tyc = (case tyc
		 of NoTyc => ""
		  | OptTyc => "?"
		  | SeqTyc => "*"
		  | SharedTyc => "!"
		(* end case *))
	  in
	    case nty
	     of BaseTy id => TypeId.nameOf id ^ tyc
	      | ImportTy(modId, id) => concat[
		    ModuleId.nameOf modId, ".", TypeId.nameOf id, tyc
		  ]
	      | LocalTy(TyDcl{owner=Module{id=modId, ...}, id, ...}) => concat[
		    "(", ModuleId.nameOf modId, ")", TypeId.nameOf id, tyc
		  ]
	    (* end case *)
	  end

  end (* structure AST *)

end (* local *)
