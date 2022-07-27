(* env.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Environment for type checking ASDL.
 *)

structure Env : sig

    type t

    val new : unit -> t

  (* find a module in the environment; if we are inside a call to `withModule`,
   * then only the imports will be visible.
   *)
    val findModule : t * Atom.atom -> AST.ModuleId.t option

    val withModule : t * AST.ModuleId.t * (t -> 'a) -> 'a

    val addImport : t * Atom.atom * AST.ModuleId.t -> unit

    val currentModule : t -> AST.ModuleId.t

  (* lookup a type.  The second argument is an optional module *)
    val findType : t * AST.ModuleId.t option * Atom.atom -> AST.named_ty option

  (* insert a local type into the current module's type environment *)
    val insertType : t * Atom.atom * AST.type_decl -> unit

  (* find a constructor in the current module's environment *)
    val findConstr : t * Atom.atom -> AST.ConstrId.t option

  (* insert a constructor into the current module's environment *)
    val insertConstr : t * AST.ConstrId.t -> unit

  (* find a view by name *)
    val findView : t * Atom.atom -> View.t option

  end = struct

    structure MId = AST.ModuleId
    structure ATbl = AtomTable

    datatype module_env = ModEnv of {
	id : MId.t,
	tyEnv : AST.named_ty ATbl.hash_table,
	consEnv : AST.ConstrId.t ATbl.hash_table
      }

    datatype t
      = GEnv of {
	    modEnv : module_env ATbl.hash_table,	(* the global module environment *)
	    views : View.t list				(* known views *)
	  }
      | LEnv of {
	    curMod : module_env,			(* the current module *)
	    imports : module_env ATbl.hash_table	(* imports in the current module *)
	  }

  (* a property to map a module ID to its corresponding module environment *)
    val {getFn = (getEnv : MId.t -> module_env), setFn = setEnv, ...} =
	  MId.newProp (fn id => raise Fail(concat[
	      "no environment for '", MId.nameOf id, "'"
	    ]))

    fun new () = GEnv{
	    modEnv = ATbl.mkTable(8, Fail "modEnv"),
	    views = [CxxView.view, SMLView.view]
	  }

    fun findModule (env, m) = let
	  fun find tbl = (case ATbl.find tbl m
		 of SOME(ModEnv{id, ...}) => SOME id
		  | NONE => NONE
		(* end case *))
	  in
	    case env
	     of (GEnv{modEnv, ...}) => find modEnv
	      | (LEnv{imports, ...}) => find imports
	    (* end case *)
	  end

    fun withModule (env as GEnv{modEnv, ...}, modId, chkFn) = let
	  val menv = ModEnv{
		  id = modId,
		  tyEnv = ATbl.mkTable(8, Fail "tyEnv"),
		  consEnv = ATbl.mkTable(16, Fail "consEnv")
		}
	  val res = chkFn (LEnv{curMod = menv, imports = ATbl.mkTable(8, Fail "imports")})
	  in
	  (* bind the module Id to its module environment *)
	    setEnv (modId, menv);
	    ATbl.insert modEnv (MId.atomOf modId, menv);
	    res
	  end
      | withModule _ = raise Fail "withModule in local environment"

    fun currentModule (LEnv{curMod=ModEnv{id, ...}, ...}) = id
      | currentModule _ = raise Fail "currentModule in global environment"

    fun addImport (LEnv{imports, ...}, name, modId) = let
	  val ModEnv{tyEnv, ...} = getEnv modId
	(* import a type from its defining module *)
	  fun importTy (_, AST.LocalTy(AST.TyDcl{id, ...})) = AST.ImportTy(modId, id)
	    | importTy (name, _) = raise Fail (concat[
		  "bogus type '", Atom.toString name, "' in import module '", MId.nameOf modId
		])
	(* construct the imported module environment for the module, which only contains
	 * type bindings.
	 *)
	  val importModEnv = ModEnv{
		  id = modId,
		  tyEnv = ATbl.mapi importTy tyEnv,
		  consEnv = ATbl.mkTable(0, Fail "consEnv")
		}
	  in
	    ATbl.insert imports (name, importModEnv)
	  end
      | addImport _ = raise Fail "addImport applied to global environment"

  (* find a type in a module's environment *)
    fun findTy (ModEnv{tyEnv, ...}, name) = ATbl.find tyEnv name

    fun findType (LEnv{curMod, ...}, NONE, name) = (case findTy (curMod, name)
	   of NONE => PrimTypes.find name
	    | someTy => someTy
	  (* end case *))
      | findType (LEnv{imports, ...}, SOME module, name) = (
	  case ATbl.find imports (MId.atomOf module)
	   of SOME modEnv => findTy (modEnv, name)
	    | NONE => NONE
	  (* end case *))
      | findType (GEnv{modEnv, ...}, SOME module, name) = (
	  case ATbl.find modEnv (MId.atomOf module)
	   of SOME modEnv => findTy (modEnv, name)
	    | NONE => NONE
	  (* end case *))
      | findType _ = raise Fail "findType applied to global environment"

    fun insertType (LEnv{curMod=ModEnv{tyEnv, ...}, ...}, name, dcl) =
	  ATbl.insert tyEnv (name, AST.LocalTy dcl)

  (* find a constructor in the current module's environment *)
    fun findConstr (LEnv{curMod=ModEnv{consEnv, ...}, ...}, name) = ATbl.find consEnv name
      | findConstr _ = raise Fail "findConstr applied to global environment"

  (* insert a constructor into the current module's environment *)
    fun insertConstr (LEnv{curMod=ModEnv{consEnv, ...}, ...}, consId) =
	  ATbl.insert consEnv (AST.ConstrId.atomOf consId, consId)
      | insertConstr _ = raise Fail "insertConstr applied to global environment"

  (* find a view by name *)
    fun findView (GEnv{views, ...}, name) = List.find (View.isView name) views
      | findView _ = raise Fail "findView applied to local environment"

  end (* structure Env *)
