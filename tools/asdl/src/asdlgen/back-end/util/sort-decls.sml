(* sort-decls.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure SortDecls : sig

 (* topologically sort a list of ASDL type declarations into sublists of
  * mutually recursive declarations.  The sublists are ordered with definitions
  * before uses.
  *)
    val sort : AST.type_decl list -> AST.type_decl list list

  end = struct

    structure DclOrd : ORD_KEY =
      struct
	type ord_key = AST.type_decl
	fun compare (AST.TyDcl{id=a, ...}, AST.TyDcl{id=b, ...}) = AST.TypeId.compare(a, b)
      end

    structure DS = RedBlackSetFn (DclOrd)

    structure TopSort = GraphSCCFn (DclOrd)

  (* the follow function for the type declarations in the dependency graph *)
    fun follow (AST.TyDcl{def, ...}) = let
	  fun doType (AST.Typ(AST.LocalTy td, _), deps) = DS.add(deps, td)
	    | doType (_, deps) = deps
	  fun doField ({label, ty}, deps) = doType(ty, deps)
	  fun doCons (AST.Constr{fields, ...}, deps) = List.foldl doField deps fields
	  in
	    case !def
	     of AST.EnumTy _ => []
	      | AST.SumTy{cons, ...} => DS.toList (List.foldl doCons DS.empty cons)
	      | AST.ProdTy{fields} => DS.toList (List.foldl doField DS.empty fields)
	      | PrimTy => []
	    (* end case *)
	  end

    fun sort dcls = let
	  val grps = TopSort.topOrder'{ roots = dcls, follow = follow }
	  fun post (TopSort.SIMPLE nd) = [nd]
	    | post (TopSort.RECURSIVE nds) = nds
	  in
	    List.revMap post grps
	  end

  end
