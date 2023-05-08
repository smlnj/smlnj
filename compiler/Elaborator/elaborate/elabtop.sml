(* elabtop.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature ELABTOP =
  sig

    val elabTop : Ast.dec * StaticEnv.staticEnv * ElabUtil.compInfo
	  -> Absyn.dec * StaticEnv.staticEnv

    val debugging : bool ref

  end (* signature ELABTOP *)

structure ElabTop : ELABTOP =
  struct

    structure EM = ErrorMsg

    structure PP = Formatting
    structure PSP = PPSymPaths
    structure PPA = PPAbsyn

    structure S = Symbol
    structure SP = SymPath
    structure IP = InvPath
    structure DA = Access
    structure AS = Absyn
    structure T = Types
    structure V = Variable
    structure M = Modules
    structure MU = ModuleUtil
    structure B = Bindings
    structure SE = StaticEnv
    structure L = Lookup
    structure EU = ElabUtil
    structure ED = ElabDebug
    structure EE = EntityEnv
    structure EP = EntPath
    structure EPC = EntPathContext
    open Ast

  (* debugging *)
    val say = PrintControl.say
    val debugging = ElabControl.etopdebugging (* default false *)
    fun debugmsg (msg: string) =
          if !debugging then (say msg; say "\n") else ()

    val debugPrint = ED.debugPrint debugging

  (* localStrName: used in makeOpenDec to build redeclaration of components *)
    val localStrName = S.strSymbol "<a funny structure>"

    fun bug msg = EM.impossible("ElabTop: "^msg)

  (*
   * makeOpenDecls is a hack; it is written to make sure that the backend
   * will generate the right dynamic code for all structure components.
   * Once the static environment and the dynamic environment are merged,
   * these code should become obsolete. (ZHONG)
   *)
    fun makeOpenDecls (str, spath) =
	let fun build (name, dl) =
	    (case S.nameSpace name
	      of S.VALspace =>
		  let val v = MU.getValPath(str, SP.SPATH[name],
					    SP.SPATH(spath@[name]))
		   in case v
		       of AS.VAR (V.VALvar _) =>
			    ValDec([Vb{pat=VarPat[name],
				       exp=VarExp([localStrName,name]),
				       lazyp=false}],
				   nil)
			     :: dl
			  (* here is the source of bug 788.  If name is bound
			     to a constructor in the top level environment,
			     then this will not have the desired effect of
			     rebinding name, but will probably result in a
			     type error. Possible fix would be to narrow down
			     the static environment. *)

			| AS.CON (T.DATACON{rep=DA.EXN _, ...}) =>
			    ExceptionDec [EbDef{exn=name,
						edef=([localStrName,name])}] :: dl

			| _ => dl
		  end
	       | S.STRspace =>
		  StrDec [Strb{name=name,
			       def=VarStr([localStrName,name]),
			       constraint=NoSig}] :: dl
	       | S.FCTspace =>
		  FctDec [Fctb{name=name,
			       def=VarFct([localStrName,name],NoSig)}] :: dl

	       | _ => dl)

	    val nds = foldr build [] (MU.getStrSymbols str)

	 in LocalDec(StrDec[Strb{name=localStrName, def=VarStr(spath),
				 constraint=NoSig}],
		     SeqDec nds)
	end

  (*
   * The main purpose of having a separate layer of elabTop above elabDecl
   * is to deal with the top-level OPEN declarations; once statenv and dynenv
   * are merged, there should be no special treatment for OPEN declarations,
   * and elabTop probably can be dramatically simplied. (ZHONG)
   *)
    (* elabTop : Ast.dec * SE.staticEnv * EU.compInfo
	         -> AS.dec * SE.staticEnv *)
    fun elabTop(dec, env, compInfo as {source,...}: EU.compInfo) =
	let val _ = debugmsg ">>elabTop";
	    val error = EM.error source

	    val _ = EU.initializeMatchBind env
                (* initialize the definitions of the Match and Bind exceptions *)

	 (* elab : Ast.dec * SE.staticEnv * bool * region -> AS.dec * SE.staticEnv *)
	  fun elab (SeqDec decs, env0, top, region) =
		let fun elabOne (dec, (abdecls, env)) =
		      let val (abdecl, env') = elab (dec, SE.atop(env,env0), top, region)
		       in (abdecl::abdecls, SE.atop(env', env))
		      end
		    val (abdecls,env') = foldl elabOne ([], SE.empty) decs
		 in (AS.SEQdec(rev abdecls), env')
		end

	    | elab (LocalDec (decl_in, decl_out), env0, top, region) =
		let val top_in = EU.hasModules decl_in orelse EU.hasModules decl_out
		    val (adec_in, env1) = elab (decl_in, env0, top_in, region)
		    val (adec_out, env2) =
		        elab(decl_out, SE.atop(env1, env0), top, region)
		 in (AS.LOCALdec(adec_in, adec_out), env2)
		end

	    | elab (MarkDec (dec,region'), env, top, region) =
		let val (dec', env') = elab (dec, env, top, region')
		in (if !ElabControl.markabsyn then AS.MARKdec (dec', region') else dec',
		    env')
		end

	    | elab (OpenDec paths, env, top, region) =
		let val _ = debugPrint ("top level open: ",
				  PP.psequence PP.comma (map PSP.fmtSymPath (map SP.SPATH paths)))

		    val err = error region

		    (* look up the structure variables *)
		    val strs = map (fn path => L.lookStr (env, SP.SPATH path, err)) paths

		    (* open their environments to add datatypes, etc. *)

		    (* openStr : M.str * SE.staticEnv -> SE.staticEnv *)
		    fun openStr (M.ERRORstr, env) = env
		      | openStr (str, env) = MU.openStructure (env, str)

		    val openEnv = foldl openStr SE.empty strs

		    fun openDecs ((M.ERRORstr, spath), decs) = decs
		      | openDecs ((str, spath), decs) =
			  let val ndec = makeOpenDecls (str, spath)
			   in ndec::decs
			  end

		    val newDecs = foldr openDecs nil (ListPair.zip (strs, paths))

		    (* hack to fix bugs 788, 847.
		     * narrow the static environment used to elaborate newDecs
		     * to one only binding the initial symbols of the paths.
		     * Doesn't hurt if more than one path has same head symbol. *)

		    val minEnv =
			  let fun headBind (head::_ : S.symbol list, env': SE.staticEnv) =
				    let fun err' _ _ _ = () (* to suppress duplicate error messages *)
					val str = L.lookStr (env, SP.SPATH [head], err') (* could fail? *)
				     in SE.bind(head, Bindings.STRbind str, env')
				    end
				| headBind (nil, _) = bug "unexpected case OpenDec"
			  in foldl headBind SE.empty paths
			  end

		    val {absyn=ds, statenv=env'} =
			  ElabMod.elabDecl{ast=(SeqDec newDecs), statenv=minEnv,
					   entEnv=EE.empty, context=EU.TOP,
					   level=top, tdepth=0, (* top *)
					   epContext=EPC.initContext,
					   path=IP.IPATH[], region=region,
					   compInfo=compInfo}

		    val nenv = SE.consolidate (SE.atop (env',openEnv))

		    val strs' = ListPair.zip (map SP.SPATH paths, strs)

		 in (AS.SEQdec [AS.OPENdec strs', ds], nenv)
		end

	    | elab (dec, env, top, region) =
		let val _ = debugmsg "--elabTop.elab[dec]: calling ElabMod.elabDecl"
		    val {absyn=d, statenv=env'} =
		      ElabMod.elabDecl{ast=dec, statenv=env, entEnv=EE.empty,
				       context=EU.TOP, level=top, tdepth=0,  (* top *)
				       epContext=EPC.initContext, path=IP.IPATH[],
				       region=region, compInfo=compInfo}
		 in (d, env')
		end

	  val (dec, env) = elab(dec,env,true,SourceMap.nullRegion)
    in
        debugmsg "<<elabTop";
	ED.debugPrint ElabControl.printAbsyn ("ABSYN::", PPA.fmtDec (env, SOME source) (dec,100));
	CheckUnused.check error dec;
	(dec, env)
    end (* fun elabTop *)

  end (* structure ElabTop *)
