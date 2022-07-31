(* variable.sml
 *
 * COPYRIGHT (c) 2021 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Variable : VARIABLE =
  struct

    structure A  = Access
    structure LV = LambdaVar
    structure T  = Types
    structure S  = Symbol
    structure SP = SymPath

    fun bug msg = ErrorMsg.impossible ("Variable: " ^ msg)

    datatype var
      = VALvar of			(* ordinary variables *)
	  {path : SP.path,
	   typ : T.ty ref,
	   btvs : T.tyvar list ref,     (* polymorphic "parameters" of the variable? *)
	   access : A.access,
	   prim : PrimopId.prim_id}
      | OVLDvar of       	      	(* overloaded identifier *)
	  {name : S.symbol,             (* name of the overloaded operator *)
	   variants : var list}         (* variant variables (VALvars) *)
      | ERRORvar			(* error variables *)

    fun varPath (VALvar{path,...}) = path
      | varPath _ = bug "varPath"

    fun varName (VALvar{path,...}) = SymPath.last path
      | varName _ = bug "varName"

    fun varType (VALvar{typ,...}) = !typ
      | varType _ = bug "varType"

    fun varAccess (VALvar{access,...}) = access
      | varAccess _ = bug "varAccess"

    fun varBtvs (VALvar{btvs, ...}) = !btvs
      | varBtvs _ = bug "varBtvs"

    fun hasLvarAccess (VALvar{access = A.LVAR _, ...}) = true
      | hasLvarAccess _ = false

    fun varToLvar (VALvar{access = A.LVAR lv, ...}) = lv
      | varToLvar var = bug ("varToLvar: " ^ S.name(varName var))

    fun lvarToVar (lvar: LV.lvar) : var =
	let val path = SP.SPATH [Symbol.varSymbol ("v" ^ LV.toString lvar)]
	in VALvar {path = path,
		   typ = ref T.UNDEFty,
		   access = A.LVAR lvar,
		   btvs = ref [],
		   prim = PrimopId.NonPrim}
	end

    fun mkVALvar (id, acc) =
	  VALvar{path = SP.SPATH [id],
		 typ = ref T.UNDEFty,
		 access = acc,
		 btvs = ref [],
		 prim = PrimopId.NonPrim}

    fun newVALvar (id, ty) =
	  VALvar{path = SP.SPATH [id],
		 typ = ref ty,
		 access = A.LVAR (LV.mkLvar()),
		 btvs = ref [],
		 prim = PrimopId.NonPrim}

    (* replaceLvar : var -> var * LV.lvar *)
    fun replaceLvar (VALvar{path, typ, access, btvs, prim}) =
	(case access
	  of A.LVAR _ =>
	       let val newlvar = LV.mkLvar()
	       in (VALvar{path=path, typ=typ, btvs=btvs, prim=prim,
			  access=A.LVAR(newlvar)},
		   newlvar)
	       end
 	     | _ => bug "replaceLvar: VALvar{access,...}")
      | replaceLvar _ = bug "replaceLvar: not VALvar"

    (* eqVar : var * var -> bool *)
    (* eqVar should only be applied to local bound vars, which are VALvars
     * with LVAR accesses. Equality is based on equal lvars. *)
    fun eqVar (var1 as VALvar{access=a1,...}, var2 as VALvar{access=a2,...}) =
	(case a1
	  of A.LVAR lv1 =>
	     (case a2
	       of A.LVAR lv2 => LV.same (lv1, lv2)
		| _ => bug ("eqVar: second arg access not LVAR: "^
			   Symbol.name(varName var2)))
	   | _ => bug ("eqVar: first arg access not LVAR: "^
		       Symbol.name(varName var1)))
      | eqVar _ = bug "eqVar: an arg is not a VALvar"

(* Wildcard special "variable" *)

    val wildSymbol = S.varSymbol "%WILD%"

    (* FIX: wildVar and isWildVar probably obsolete. *)
    (* a pseudo-variable used to represent wildcard patterns in the match compiler *)
    val wildVar = VALvar{path = SP.SPATH [wildSymbol],
			 typ = ref(Types.UNDEFty),
			 btvs = ref nil,
			 access = A.NO_ACCESS,
			 prim = PrimopId.NonPrim}

    fun isWildVar (VALvar{path,...}) = S.eq (SymPath.last path, wildSymbol)

    (* toString : var -> string *)
    fun toString (VALvar{path, access, ...}) =
	  concat [S.name(SymPath.last path), "[", A.prAcc access, "]"]
      | toString (OVLDvar _) = "OVLD"
      | toString ERRORvar = "ERROR"

  end (* structure Variable *)
