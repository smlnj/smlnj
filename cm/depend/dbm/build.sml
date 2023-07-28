(* cm/depend/dbm/build.sml
 *
 * COPYRIGHT (c) 2023 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Build the dependency graph for one group/library.
 *
 * Author: Matthias Blume (matthias.blume@gmail.com)
 * Edited: DBM, 2023.6
 *)

signature BUILDDEPEND =
sig

    val build :
	(* imports: *) DependencyGraph.impexp SymbolMap.map *
	(* smlfiles: *) (SmlInfo.info * SymbolSet.set) list *
	(* localdefs: *) SmlInfo.info SymbolMap.map *
	(* filter: *) SymbolSet.set *
	GeneralParams.info *
	DependencyGraph.farsbnode	(* a node representing the pervasive env *)
	->
	DependencyGraph.impexp SymbolMap.map *	(* exports *)
	SymbolSet.set			(* imported symbols *)

    (* for the autoloader *)
    type looker = Symbol.symbol -> DAEnv.env
    val look : looker -> DAEnv.env -> looker
    val processOneSkeleton : looker -> Skeleton.decl -> unit

end (* signature BUILDDEPEND *)

structure BuildDepend :> BUILDDEPEND =
struct

local

  structure S  = Symbol
  structure SS = SymbolSet
  structure SM = SymbolMap
  structure SP = SymPath
  structure SK = Skeleton
  structure DE = DAEnv
  structure DG = DependencyGraph
  structure EM = ErrorMsg
  structure PP = Formatting

  structure GP = GeneralParams
  structure SI = SmlInfo       (* cm/smlfile/smlinfo.sml *)
  structure SIM = SmlInfoMap
  structure CS = CheckSharing  (* CS.check called once near the end *)

  type path = string list

in

    type looker = S.symbol -> DE.env

    (* look : looker -> DE.env -> S.symbol -> DE.env *)
    (* DE.env lookup with a default looker, "otherwise" *)
    fun look (otherwise: looker) DE.EMPTY (sym: S.symbol) = otherwise sym
      | look otherwise (DE.BINDING (sym', env')) sym =
	  if S.eq (sym, sym') then env' else otherwise sym
      | look otherwise (DE.LAYER (env1, env2)) sym = look (look otherwise env2) env1 sym
      | look otherwise (DE.FUNENV envfn) sym =
	  (case envfn sym
	     of NONE => otherwise sym
	      | SOME env => env)
      | look otherwise (DE.FILTER (symset, env)) sym =
	  if SS.member (symset, sym)
	  then look otherwise env sym
	  else otherwise sym

    (* skeletonExports : looker -> SK.decl -> DE.env *)		 
    fun skeletonExports (lookimport: looker) =
	let 
	    (* lookup : DE.env -> S.symbol -> DE.env *)
	    (* build the lookup function for DG.env *)
	    val lookup = look lookimport

	    (* lookPath : DE.env -> path -> DE.env *)
	    fun lookPath env nil = DE.EMPTY (* is this possible? *)
	      | lookPath env (h :: t) =
		    (* again, if we don't find it here we just ignore
		     * the problem and let the compiler catch it later *)
		  let val lookup' = look (fn _ => DE.EMPTY) (* fails without complaint *)
		      (* loop : DE.env * path -> DE.env *)
		      fun loop (env, nil) = env
			| loop (env, sym :: syms) = loop (lookup' env sym, syms)
		   in loop (lookup env h, t)
		  end

            (* layered : DE.env list -> DE.env *)
	    (* combine a list of envs into an env using LAYER *)
	    fun layered nil = DE.EMPTY
	      | layered [env] = env
	      | layered decls = foldr1 DE.LAYER envs  (* UNDEFINED: foldr1 *)

	    (* "declExports" -- compute the export environment of a skeleton *)
	    (* declExports [eval]: SK.decl -> DE.env *)
            (* renaming: eval -> declExports *)
	    fun declExports (decl: SK.decl) =
		let
		    (* edecl : DE.env -> SK.decl -> DE.env *)
		    (* re-elaborate an SK.decl *)
		    fun edecl env (SK.Bind (name, exp)) =
			  DE.BINDING (name, eexp env exp)
		      | edecl env (SK.Local (decls1, decls2)) =
			  edecl (DE.LAYER (edecl env decls1, e)) decls2
		      | edecl env (SK.Seq decls) = edecls env decls
		      | edecl env (SK.Par decls)) = layered (map (edecl env) decls)
		      | edecl env (SK.Open exp) =  (* exp will normally be a Var module expression? *)
			  eexp env exp
		      | edecl env (SK.Ref ss) =  (* ss is "free (thus imported?) module identifiers" *)
  			  (SS.app (ignore o lookup env) ss; DE.EMPTY)
			  (* lookup (look lookimport) is assumed to add symbols to import list ref(s)
			   * as a side-effect, but only when lookup resorts to calling lookimport
			   * (i.e. the symbol is not bound in env) *)

		    (* edecls : DE.env -> SK.decl list -> DE.env *)
		    and edecls _ [] = DE.EMPTY
		      | edecls env (head :: rest) =
			  let fun folder (decl, env') =
				  DE.LAYER (edecl (DE.LAYER (env', env)) decl, env')
			      val base_env = edecl env head
			   in foldl folder base_env rest
			  end

		    (* eexp [evalModExp]: DE.env -> SK.exp -> DE.env *)
		    (* re-elaborate an SK.exp *)
		    and eexp env (SK.Var path) = lookPath env path
		      | eexp env (SK.Decl decls) = edecls env decls
		      | eexp env (SK.Let (decls, mexp)) =
			  eexp (DE.LAYER (edecls env decls, env)) mexp
		      | eexp env (SK.Pair (mexp1, mexp2)) =
			  (eexp env mexp1; eexp env mexp2)
			  (* "eexp env mexp1" has an effect via lookimport? *)

	     in edecl DE.EMPTY decl
	    end (* declExports *)

	 in declExports
	end (* fun skeletonExports *)

    (* processOneSkeleton : looker -> SK.decl -> unit
     * called in build function below, and in main/autoload.sml
     * Called for effects (updating "globalimports") that happen when lookimport is called *)
    fun processOneSkeleton lookimport sk =
	ignore (skeletonExports lookimport sk)

    (* formatSymbol : S.symbol -> PP.format *)
    (* get the description for a symbol *)
    fun formatSymbol (s: S.symbol) =
	PP.hblock [PP.text (S.nameSpaceToString (S.nameSpace s)), PP.text (S.name s)]

    (* blackboard (in build) is an snode_env option SIM.map; entries are created by
     * analyze, which returns snode_env *)
    type snode_env = DG.snode * DAEnv.env
    (* history is an alist mapping symbols to SI.info (can there be multiple entries for a symbol?) *)
    type historyTy = (S.symbol * SI.info) list

    (* build : -- see BUILDDEPEND.build *)
    fun build (imports: DG.impexp SM.map,
	       smlfiles: (SI.info * SS.set) list,
	       localdefs: SI.info SM.map,
	       filter: SS.set,  (* option? *)
	       gp: GP.info,   (* used 3 times, once for gp.error *)
	       perv_fsbnode: DG.farsbnode) (* pervasive env *)
      = let
	    (* per_file_exports : SS.set SIM.map *)
	    val per_file_exports =
		foldl (fn ((info, symbolset), simap) => SIM.insert (simap, info, symbolset))
		      SIM.empty smlfiles


	    (* bb: snode_env option SIM.map ref -- the "blackboard" *)
	    (* the "blackboard" where analysis results are announced *)
	    (* (also used for cycle detection) *)
	    val bb : snode_env option SIM.map ref = ref SIM.empty

	    (* lock : SI.info -> unit *)
	    (* the entry for a SI.info key is _locked_ if it is mapped to NONE *)
	    fun lock (info: SI.info) = bb := SIM.insert (!bb, info, NONE)

	    (* release : SI.info * snode_env -> snode_env *)
	    (* release an SI.info key by setting it to SOME r (thus adding a binding to the
             * blackboard) *)
	    fun release (info: SI.info, r: snode_env) =
		  (bb := SIM.insert (!bb, info, SOME r); r)

	    (* fetch : SI.info -> snode_env option option *)
	    fun fetch smlinfo = SIM.find (!bb, smlinfo)


	    (* We collect all imported symbols so that we can then: 
	     * "narrow the list of libraries." *)
	    (* imported_syms : SS.set ref *)
	    val imported_syms : SS.set ref = ref SS.empty
        
	    (* add_imported_sym : S.symbol -> unit *)
	    fun add_imported_sym s = imported_syms := SS.add (!imported_syms, s)

	    (* getResult : [smlinfo:]SI.info * historyTy -> snode_env *)
	    (* get the result from the blackboard if it is there, otherwise trigger analysis
	     * -- detect cycles using locking
	     * -- maintain root set *)
	    fun getResult (smlinfo: SI.info, history: historyTy) =
		case fetch smlinfo
		  of NONE => (lock info; release (smlinfo, analyze (smlinfo, history)))
		   | SOME (SOME snode_env) => snode_env
		   | SOME NONE =>	(* file is locked => cycle found => error message *)
		       let val file = SI.file smlinfo
			   val filepath = Path.pathToFpath (File.fileToPath file)
			   val errorBody : PP.format =
			       let fun lines (_: string, nil: (S.symbol * SI.info) list) = nil
					 (* shouldn't happen because i should = i'
					  * for some (s, i') in history *)
				     | lines (prevpath, (sym, smlinfo') :: rest) =
					 let val file' = SI.file smlinfo'
					     val filepath' = Path.pathToFpath (File.fileToPath file')
					     val line : PP.format = 
						 PP.hblock
						   [PP.text filepath',  (* library name *)
						    PP.text "refers", PP.text "to",
						    formatSymbol sym,
						    PP.text "defined", PP.text "in",
						    PP.text prevpath]   (* library name *)
					  in if SI.eq (smlinfo, smlinfo')
					     then [line]
					     else line :: lines (n', rest)
					 end
			        in PP.vblock (rev (lines (filepath, history)))
			       end

			in SI.error gp smlinfo
			      EM.COMPLAIN "cyclic ML dependencies" errorBody;
			   (* carry on with bogus release (for type checking?) *)
			   release (smlinfo,
				    (DG.SNODE {smlinfo = smlinfo, localimports = nil, globalimports = nil},
				     DE.EMPTY))
		       end

	    (* analyze : (SI.info * historyTy) -> snode_env *)
	    (* do the actual analysis of an ML source and generate the
	     * corresponding snode and DE.env *)
	    and analyze (smlinfo: SI.info, history: historyTy) =
		let
		    val localImports  : DG.snode list ref = ref nil (* collect local imports *)
		    val globalImports : DG.farsbnode list ref = ref [perv_fsbnode]

		    (* addLocalImport : DG.snode -> unit *)
		    (* register a local import, adding it to li if it is not present *)
		    fun addLocalImport (snode: DG.snode) =
			if List.exists (fn snode' => DG.seq (snode, snode')) (!li)
			then ()  (* snode already registered -- exists in !localImports -- do nothing *)
			else localImports := snode :: !localImports  (* new snode, add it to !localImports *)

		    (* addGlobalImport : S.symbol * DG.farsbnode -> unit *)
		    (* register a global import, maintain filter sets *)
		    fun addGlobalImport (sym: S.symbol, ((filterOp, sbnode): DG.farsbnode) =
			let fun sameNode ((_, sbnode'): DG.farsbnode) : bool =
				DG.sbeq (sbnode, sbnode')
			 in add_imported_sym sym;
			    case List.find sameNode (!globalImports)
			      of NONE => globalImports := (f, n) :: !globalImports (* new farsbnode *)
			       | SOME (NONE, n') => () (* no filter -> no change *)
			       | SOME (SOME filter', sbnode') =>
				   (* sbnode exists in !globalImports but with a possibly different filter,
				    *  namely filter' =>
				    *  see if SOME filter' is the same as filterOp, and if not replace it
				    *  with the "union" of the filters (if both are SOME), or with NONE if
				    *  filterOp is NONE, thus discarding filter'. *)
				   let fun replace (filterOp: DG.filter option) : unit =
					    globalImports :=
					      (filterOp, sbnode)
					      :: List.filter (not o sameNode) (!globalImports)
				    in case filterOp
					 of NONE => replace NONE  (* discarding the old filter'! *)
					  | SOME filter =>
					      if SS.equal (filter, filter')
					      then ()
					      else replace (SOME (SS.union (filter, filter')))
				   end
			end (* fun addGlobalImport *)

		    val thisFile: File.file = SI.file smlinfo
		    fun isSelf (smlinfo': SI.info) : bool = SI.eq (smlinfo, smlinfo')

		    (* lookimport : S.symbol -> DE.env *)
		    (* lookup function for things not defined in the same ML file.
		     * As a side effect, this function registers local and global imports. *)
		    fun lookimport (sym: S.symbol) =
			let fun dontcomplain _ = DE.EMPTY
			    fun lookfar () =
				case SM.find (imports, sym)
				  of SOME (farsbnode, env: DE.env, _) =>
				       (addGlobalImport (sym, farsbnode);
					look dontcomplain env sym)
				   | NONE => DE.EMPTY
					(* We could complain here about an undefined
					 * name.  However, since CM doesn't have the
					 * proper source locations available, it is
					 * better to handle this case silently and
					 * have the compiler catch the problem later. *)
			 in case SM.find (localdefs, sym)
			      of SOME smlinfo' =>
				   if isSelf smlinfo' then lookfar ()
				   else let val (snode, env) = getResult (smlinfo', (sym, smlinfo) :: history)
				         in addLocalImport snode;
					    look dontcomplain env sym
					end
			       | NONE => lookfar ()
			end

		    val env: DE.env =
			case SI.skeleton gp thisFile
			  of SOME sk => skeletonExports lookimport sk
			   | NONE => DE.EMPTY

		    val snode = DG.SNODE {smlinfo = thisFile,
				          localimports = !localImports,
					  globalimports = !globalImports}

		 in (snode, env)
		end (* fun analyze *)

	(* run the analysis on a single SML source file -- causing the blackboard
	 * to be updated accordingly *)
	fun doSmlFile (smlinfo: SI.info, _) = ignore (getResult (smlinfo, nil))

	(* run the analysis *)
	val _ = app doSmlFile smlfiles

	(* Invert the "localdefs" map so that each smlinfo is mapped to the
	 * corresponding _set_ of symbols: *)
	val ilocaldefs : SS.set SIM.map = 
  	    let fun folder (symbol, info, m) =
		    (case SIM.find (m, smlinfo)
		       of NONE => SIM.insert (m, smlinfo, SS.singleton symbol)
		        | SOME ss => SIM.insert (m, smlinfo, SS.add (ss, symbol)))
	     in SM.foldli folder SIM.empty localdefs
	    end

	fun addDummyFilter i =
	    let val (sn, e) = valOf (valOf (fetch i))
		val sbn = DG.SB_SNODE sn
		val fsbn = (SIM.find (per_file_exports, i), sbn)
	     in (* fsbn unthunkified! *)
		(fsbn, e, valOf (SIM.find (ilocaldefs, i)))
	    end

	(* First we make a map of all locally defined symbols to
	 * the local "far sb node"
	 * but with only a dummy filter attached.
	 * This makes it consistent with the current state
	 * of "imports" where there can be filters, but
	 * where those filters are not yet strengthened according to
	 * filter *)
	val localmap = SM.map addDummyFilter localdefs

	val exports : DG.impexp SM.map =
	    let (* Strengthening a local export is directly described by filter. *)
		val local_filter = filter

		(* In contrast, strengthening a re-export must take into
		 * account local definitions: anything defined locally
		 * must be removed from re-exports. *)
		val reexport_filter =
		    SS.subtractList (filter, SM.listKeys localdefs)

		(* We now always have a filter.
		 * We export only the things in the filter.
		 * They can be taken from either localmap or else from
		 * imports.  In either case, it is necessary to strengthen
		 * the filter attached to each node. *)
		fun strengthen ss (nth, e, allsyms) =
		    let val (fopt', sbn) = nth ()
			val new_fopt =
			    case fopt' of
				NONE => SOME ss
			      | SOME ss' => SOME (SS.intersection (ss, ss'))
			fun nth' () = (new_fopt, sbn)
		     in (nth', DE.FILTER (ss, e), SS.intersection (allsyms,ss))
		    end

		fun addNodeFor (s, m) =
		    (case SM.find (localmap, s)
		       of SOME n => SM.insert (m, s, strengthen local_filter n)
		        | NONE => (case SM.find (imports, s)
				     of SOME n =>
					  (add_imported_sym s;
					   SM.insert (m, s, strengthen reexport_filter n))
				      | NONE =>
					  (* This should never happen since we
					   * checked beforehand during
					   * parsing/semantic analysis *)
					EM.impossible "build: undefined export"
				  (* end case *))
		    (* end case *))

	     in SS.foldl addNodeFor SM.empty filter
	    end (* val exports *)

     in CS.check (exports, gp);
	(exports, !imported_syms)
    end (* end build ? *)

end (* top local *)
end (* structure BuildDepend *)

(* NOTES

1. "flattened" the arguments to the build function -- one big tuple.

*)
