(* Copyright 1996 by AT&T Bell Laboratories *)
(* elabsig.sml *)

signature ELABSIG =
sig

  val elabSig :
         {sigexp    : Ast.sigexp,
          nameOp    : Symbol.symbol option,
          env       : StaticEnv.staticEnv,
          entEnv    : Modules.entityEnv,
          epContext : EntPathContext.context,
          region    : SourceMap.region,
          compInfo  : ElabUtil.compInfo} -> Modules.Signature

  val elabFctSig :
         {fsigexp   : Ast.fsigexp,
          nameOp    : Symbol.symbol option,
          env       : StaticEnv.staticEnv,
          entEnv    : Modules.entityEnv,
          epContext : EntPathContext.context,
          region    : SourceMap.region,
          compInfo  : ElabUtil.compInfo} -> Modules.fctSig

  val debugging : bool ref

end (* signature ELABSIG *)


structure ElabSig : ELABSIG =
struct

local structure S  = Symbol
      structure SS = SpecialSymbols
      structure EM = ErrorMsg
      structure A = Access
      structure EP = EntPath
      structure EPC = EntPathContext
      structure EE = EntityEnv
      structure M =  Modules
      structure MU = ModuleUtil
      structure B  = Bindings
      structure SP = SymPath
      structure IP = InvPath
      structure LU  = Lookup
      structure SE = StaticEnv
      structure T  = Types
      structure BT = BasicTypes
      structure TU = TypesUtil
      structure EU = ElabUtil
      structure ET = ElabType
      structure EX = ExpandTycon
      structure ST = Stamps
      open Ast Modules
in

(* debugging *)
fun bug msg = ErrorMsg.impossible ("ElabSig: " ^ msg)

val say = Control_Print.say
val debugging = ElabControl.esdebugging (* ref false *)
fun debugmsg (msg: string) = if (!debugging) then (say msg; say "\n") else ()
fun debugPrint x = ElabDebug.debugPrint debugging x

open ElabDebug
val debugPrint = (fn x => debugPrint debugging x)

(* utility stuff *)
fun stripMarkSig(MarkSig(sigexp,region'),_) = stripMarkSig(sigexp,region')
  | stripMarkSig x = x


fun lookStrDef(env,spath,epContext,err) =
    let val strDef = LU.lookStrDef(env,spath,err)
     in case strDef
	  of VARstrDef _ => strDef
	   | CONSTstrDef str =>
	     (case str
		of M.ERRORstr => strDef
	         | M.STR { sign, ... } =>
		    (case EPC.lookStrPath(epContext,MU.strId str)
		       of NONE => strDef
			| SOME entPath => VARstrDef(sign,entPath))
		 | M.STRSIG _ => bug "lookStrDef")
    end

(* code for processing where defs *)

fun closedDefs defs =
    not(List.exists
          (fn ((_,TYCdef{relative=true,...}) | (_,STRdef(_,VARstrDef _))) => true
	    | _ => false)
	  defs)

(* defs = prepare whereDefs  (* sorted by initial path symbol *) *)

fun sortdefs(defs) =
    let fun gt ([],_) = false
	  | gt (_,[]) = true
          | gt (s1::_,s2::_) = Symbol.symbolGt(s1,s2)
     in ListMergeSort.sort (fn ((p1,d1),(p2,d2)) => gt(p1,p2)) defs
    end

fun prepareDefs whereDefs =
    sortdefs(map (fn (def as STRdef(SP.SPATH p,_)) => (p,def)
		   | (def as TYCdef{path=SP.SPATH p,...}) => (p,def))
		 whereDefs)

fun pushDefs(elements,defs,error,mkStamp) =
    let fun findDefs(sym,defs) =
	    let fun loop((item as (s::rest,def))::defs,this,others) =
		    if S.eq(s,sym) then loop(defs,(rest,def)::this,others)
		    else if S.symbolGt(s,sym) then
		      (sortdefs this,(rev others@(item::defs)))
		    else loop(defs,this,item::others)
                  | loop(nil,this,others) = (sortdefs this,rev others)
		  | loop _ = bug "pushDefs:findDefs:loop"
             in loop(defs,nil,nil)
	    end
	fun applyTycDef(tspec as TYCspec{entVar,info=RegTycSpec{spec,...}},
			TYCdef{path=spath,tyc,...}) =
	    (case spec
	      of T.GENtyc {kind,arity,eq=eqp,path=tpath,...} =>
		 (case kind
		   of T.FORMAL =>
		      if TU.tyconArity tyc = arity
		      then TYCspec{entVar=entVar,
                                   info=RegTycSpec{spec=tyc, repl=false,
				                   scope=SP.length spath}}
		      (* DBM: we should check at this point that the
		       * definition represented by TYCdef#tyc has the
		       * appropriate equality property to match the
		       * spec, but this does not seem to be feasible
		       * without excessive work.  The problem is computing
		       * whether tyc is an equality tycon, when it contains
		       * PATHtycs, as in bug1433.2.sml. *)
		      else (error ("where type definition has wrong arity: " ^
				   SP.toString spath);
			    tspec)
		    | T.DATATYPE _ =>
		  (* We allow a where type defn to constrain a datatype spec,
		   * if rhs datatype is "compatible" with spec.  We use
		   * an extremely weak notion of compatibility -- same arity.
		   * The definition should be a compatible datatype
		   * (not checked here!), making this an indirect
		   * datatype replication spec.
		   *)
                  (* tyc is DEFtyc! This will have to be unwrapped when the
                   * signature is instantiated (bugs 1364, 1432).
		   *)
		      if arity = TU.tyconArity tyc
		      then TYCspec{entVar=entVar,
                                   info=RegTycSpec{spec=tyc, repl=true,
				                   scope=SP.length spath (* ??? *)}}
		      else (error ("where type definition has wrong arity: " ^
				   SP.toString spath);
			    tspec)
		    | _ => bug "elabsig: GENtyc is neither FORMAL nor DATA")
	       | T.DEFtyc _ =>
		  (error ("where type defn applied to definitional spec: " ^
			  SP.toString spath);
 		   tspec)
	       | _ => bug "applyTycDef (1)")
	  | applyTycDef _ = bug "applyTycDef (2)"
	fun applyStrDefs(spec as STRspec{entVar,sign,def,slot},defs) =
	    (* in the case where the where def has a different signature,
	     * could propagate defs in to the components, as is done currently
             * during instantiation.  If a VARstrDef applies to a spec
	     * with a different signature, this propagation of VAR defs
	     * into the components means that the spec signature is
	     * open (i.e. the "closed" field should become false).
	     * This is currently being handled within instantiate. *)
	    (case def
	       of SOME _ =>
		   (error "where defn applied to definitional spec";
		    spec)
		| NONE =>
		   (case defs
		      of (nil,STRdef(spath,strDef))::rest =>
                          (* applies directly *)
			  (case rest
			     of nil =>
				 STRspec{entVar=entVar,sign=sign,
					 def=SOME(strDef,SP.length spath),
					 slot=slot}
			      | _ => (error "redundant where definitions";
				      spec))
		       | _ => STRspec{entVar=entVar,def=NONE,slot=slot,
				      sign=addWhereDefs(sign,defs,NONE,
							error,mkStamp)}))
	  | applyStrDefs _ = bug "applyStrDefs"
	fun loop(nil,defs,elems) =  (* all elements processed *)
	      (case defs
		 of nil => rev elems  (* all defs consumed *)
		  | _ => (* left-over defs *)
		    (app (fn (_,TYCdef{path=p,...}) =>
			      (error (concat
				      ["unbound left hand side in where type: ",
				       SP.toString p]))
			   | (_,STRdef(p,_)) =>
			      (error (concat
			      ["unbound left hand side in where (structure): ",
			       SP.toString p])))
		         defs;
		     rev elems))
	  | loop(elems0,nil,elems) =
              rev elems @ elems0 (* all defs processed *)
          | loop((elem as (sym,tspec as TYCspec _))::elems,defs,elems') =
	      let val (localdefs,otherdefs) = findDefs(sym,defs)
	       in case localdefs
		    of [(nil,tycDef)] =>
			 loop(elems,otherdefs,
			      (sym,applyTycDef(tspec,tycDef))::elems')
                     | nil => loop(elems,defs,elem::elems')
		     | _ => (error ("multiple where defs for "^S.name sym);
			     loop(elems,otherdefs,elem::elems'))
	      end
          | loop((elem as (sym,sspec as STRspec _))::elems,defs,elems') =
	      let val (localdefs,otherdefs) = findDefs(sym,defs)
	       in case localdefs
		    of nil => (* no defs apply to this element *)
			loop(elems,otherdefs,elem::elems')
		     | _ =>
			loop(elems,otherdefs,
			     (sym,applyStrDefs(sspec,localdefs))::elems')
	      end
          | loop(elem::elems,defs,elems') = loop(elems,defs,elem::elems')
     in loop(elements,defs,nil)
    end

(* does this belong in ModuleUtil or ElabUtil? DBM *)
and addWhereDefs(sign,nil,nameOp,error,mkStamp) = bug "addWhereDefs"
  | addWhereDefs(sign as SIG {stamp,name,closed,fctflag,stub,
			      elements, properties,
			      typsharing,strsharing},
		 whereDefs,nameOp,error,mkStamp) =
    SIG{stamp = mkStamp(),
	(* give modified sig a new stamp
	 * -- could stack stamps *)
	name=case nameOp
	      of SOME _ => nameOp (* new name provided *)
	       | NONE => name, (* retain old name (?) *)
	closed=closed andalso closedDefs whereDefs,
        fctflag=fctflag,
	elements=pushDefs(elements,whereDefs,error,mkStamp),
	properties = PropList.newHolder (),
	typsharing=typsharing,
	strsharing=strsharing,
	stub = NONE}
  | addWhereDefs _ = bug "addWhereDefs"

fun localPath(p,elements) =
      (MU.getSpec(elements,SP.first p); true) handle MU.Unbound _ => false

val paramId = S.strSymbol "<param>"
val functorId = S.fctSymbol "<functor>"

(*
 * Elements are added in reverse order, so at the end, the elements
 * lists must be reversed. In the long run, this could be changed
 * if we move to a env-based representation of the elements.
 *)
fun addElement(x,elements) = x::elements

(* add: symbol * spec * elements * error fn -> elements
 * Adds a symbol, spec pair to the elements unless the symbol already
 * occurs (duplicate spec), in which case it generates an error message
 * and returns the original elements list.
 * Checking for duplicates is now primarily the responsibility of the
 * checkDups function below, but we retain the checking in add for now
 * for redundancy *)
fun add(symbol,spec,elements,err) =
  (* check to see whether symbol is already bound in the given env *)
  (debugmsg (">>add: "^S.name symbol);
   if List.exists (fn (n,_) => S.eq(symbol,n)) elements
   then (* if so, this indicates a duplicate specification error *)
     (err EM.COMPLAIN ("duplicate specifications for "
		       ^S.nameSpaceToString(S.nameSpace symbol)
		       ^" "^S.name symbol^" in signature")
	  EM.nullErrorBody;
      elements)
   (* otherwise, add the symbol *)
   else addElement((symbol,spec),elements))

(* check for duplicate specs
 * return true if none detected. If duplicate detected, print error message
 * and return false *)
fun checkDups(name: S.symbol, elements, err) : bool =
   if List.exists (fn (n,_) => S.eq(name,n)) elements
   then (* if so, this indicates a duplicate specification error *)
     (err EM.COMPLAIN ("duplicate specifications for "
		       ^S.nameSpaceToString(S.nameSpace name)
		       ^" "^S.name name^" in signature")
	  EM.nullErrorBody;
      false)
   else true

(* elaborating where type clauses around signatures *)
fun elabWhere (sigexp,env,epContext,mkStamp,error,region) =
    let fun loop(AugSig(sigexp,whspecs),defs,region) =
	    let fun loop1(nil,defs) = loop(sigexp,defs,region)
		  | loop1(WhType(path,tyvars,ty)::rest,defs) =
		      let val spath = SP.SPATH path
			  val _ = debugmsg("elabWhere:WhType: " ^
                                           SP.toString spath)
			  val tvs = ET.elabTyvList(tyvars,error,region)
			  val arity = length tvs
			  val (ty,tvs') = ET.elabType(ty,env,error,region)
			  val _ = EU.checkBoundTyvars(tvs',tvs,error region)
			  val _ = TU.bindTyvars tvs
			  val _ = TU.compressTy ty
			  val stamp = mkStamp ()
			  val path = IP.IPATH [List.last path]
			  val strict = TU.calcStrictness(arity,ty)
			  val (nty,relative) = MU.relativizeType epContext ty
			  val tycon =
                            T.DEFtyc{stamp=stamp,
				     path=path,
				     strict=strict,
				     tyfun=T.TYFUN{arity=arity,body=nty}}
		       in loop1(rest,TYCdef{path=spath,tyc=tycon,
					    relative=relative}::defs)
		      end
		  | loop1(WhStruct(lhs,rhs)::rest,defs) =
		     (let val lhspath = SP.SPATH lhs
			  val strDef =
		           lookStrDef(env,SP.SPATH rhs,epContext,error region)
			  val strDef =
                              (* remove access & inline info (bug 1201) *)
			      case strDef
				of CONSTstrDef(STR {sign,rlzn,...}) =>
				   CONSTstrDef(STR{sign=sign,rlzn=rlzn,
						   access=Access.nullAcc,
						   prim=[]})
				 | _ => strDef
		       in loop1(rest,STRdef(lhspath,strDef)::defs)
		      end
		      handle SE.Unbound =>
		       (error region EM.COMPLAIN
			  "unbound rhs in where clause"
			  EM.nullErrorBody;
		        loop1(rest,defs)))
	     in loop1(whspecs,defs)
	    end
	  | loop(MarkSig(sigexp,region),defs,_) =
	      loop(sigexp,defs,region)
	  | loop(sigexp,defs,region) = (sigexp,defs,region)
     in loop(sigexp,nil,region)
    end

(*
 * elabBody is the  main function for elaborating signature bodies.
 *
 * Its return type is
 *
 *    elements * tycShareSpec list * strShareSpec list * bool
 *
 * It does not need to return an updated statenv.
 *)
fun elabBody(specs, env, entEnv, sctxt, epContext, region,
             compInfo as {mkStamp,error,...} : EU.compInfo) =
let

(*** elaborating type specification --- returning "env * elements" ***)
fun elabTYPEspec(tspecs, env, elements, eqspec, region) =
  let val _ = debugmsg ">>elabTYPEspec"
      val err = error region
      val eqprop = if eqspec then T.YES else T.IND

      fun loop([], env, elems) = (env, elems)
        | loop((name,tyvars,abbrev)::rest, env, elems) =
          if checkDups(name, elems, err) then
            let val tvs = ET.elabTyvList(tyvars,error,region)
                val arity = length tvs
                val tycon =
                  case abbrev
                   of SOME def =>
		       if eqspec
		       then (error region EM.COMPLAIN
			      ("eqtype spec with a definition: " ^
			       S.name name)
			      EM.nullErrorBody;
			     T.ERRORtyc)
		       else
                        let val (ty,tvs') = ET.elabType(def,env,error,region)
                            val _ = EU.checkBoundTyvars(tvs',tvs,err)
                            val _ = TU.bindTyvars tvs
                            val _ = TU.compressTy ty
                            val (nty,_) = MU.relativizeType epContext ty
                         in T.DEFtyc{stamp = mkStamp(),
                                     path=IP.IPATH [name],
                                     strict=TU.calcStrictness(arity,ty),
                                     tyfun=T.TYFUN{arity=arity,body=nty}}
                        end
                    | NONE => T.GENtyc {stamp = mkStamp(),
					path = IP.IPATH [name],
					arity = arity, eq = ref eqprop,
					kind = T.FORMAL,
					stub = NONE}

                val ev = mkStamp()
                val etyc = T.PATHtyc{arity=arity,entPath=[ev],
                                     path=IP.IPATH[name]}
                val env' = SE.bind(name, B.TYCbind etyc, env)

                val ts = TYCspec{entVar=ev,
                                 info=RegTycSpec{spec=tycon, repl=false, scope=0}}
                val elems' = add(name, ts, elems, err)
             in loop(rest, env', elems')
            end
          else loop(rest, env, elems)
   in loop(tspecs, env, elements)
  end

fun allButLast l = List.take(l,List.length l - 1)

(* elaborate datatype replication specs.
 *  Uses DEFtyc wrappings of the rhs datatype in the resulting specs.
 *  Need to check that this will do the "right thing" in instantiate. *)
fun elabDATAreplSpec(name,path,env,elements,region) =
    if checkDups(name,elements,error region) then
    let val tyc = Lookup.lookTyc(env, SP.SPATH path, error region)
	(* rhs is not local to current (outermost) signature *)
	fun no_datatype () =
	    (error region EM.COMPLAIN
	           "rhs of datatype replication spec not a datatype"
		   EM.nullErrorBody;
		   (env,elements))
     in case tyc
          of T.PATHtyc{entPath,arity,...} =>
	      (* local to current outermost signature *)
	      (* get the spec, using expandTycon. check it is a datatype (not stripped) *)
	      let val sigContext = elements::sctxt
		  val tyc' = EX.expandTycon(tyc,sigContext,entEnv)
	      in case tyc'
                   of T.GENtyc { kind, ... } =>
		      (case kind of
			   T.DATATYPE{index, family as {members,...},
                                      stamps, freetycs, stripped = false, ...} =>
		           let val stamp = Vector.sub(stamps,index)
                               val {tycname, arity, dcons, sign, lazyp, ...} =
			           Vector.sub(members,index)
			       (* add the type *)
			       val ev = mkStamp()
			       (* spec uses wrapped version of the PATHtyc!! *)
			       val tspec = TYCspec{entVar=ev,
                                                   info=RegTycSpec
                                                     {spec=TU.wrapDef(tyc, mkStamp()),
						      repl=true,scope=0}}
			       val elements' =
				   add(name,tspec,elements,error region)
			       val etyc = T.PATHtyc{arity=arity,entPath=[ev],
						    path=IP.IPATH[name]}
			       val env' = SE.bind(name, B.TYCbind etyc, env)
			    (* unlike normal case (rhs=Constrs), won't bother
			       to re-register the tyc in epContext *)

			       val prefix = allButLast entPath
			       fun expandTyc(tyc as T.PATHtyc{entPath=ep,
                                                              arity,path}) =
				   (* see if the path ep is defined externally
				    * in the entEnv *)
				   ((EE.look(entEnv,hd ep);
				     tyc) (* external tyc *)
				    handle EE.Unbound =>
					   (* tyc is local to sig *)
					   T.PATHtyc{entPath=prefix @ ep,
						     arity=arity,
						     path=path})
				 | expandTyc(T.FREEtyc n) =
				    (* [GK 5/4/07] Bug fix: entPath must be
				       extended here too. If entPath is not
				       extended then bug 1603.1 will fail.
				       In general, entityenv lookups of
                                       freetycs would fail. *)
				     (expandTyc (List.nth(freetycs,n))
				      handle _ =>
					     bug "unexpected freetycs in expandTyc")
				 | expandTyc(T.RECtyc n) =
				   if n = index then etyc
				   (* could equivalently be tyc? *)
				   else let val stamp = Vector.sub(stamps,n)
                                            val {tycname,arity,...} =
  						Vector.sub(members,n)
					in T.PATHtyc{arity=arity,
						     entPath=prefix@[stamp],
						     path=IP.IPATH[tycname]}
					end
				 (* reconstructing the entPath for sibling
				  * datatypes using the fact that the entVar
				  * for a datatype spec is the same as the
				  * stamp of the datatype.
				  * See elabDATATYPEspec *)
				 | expandTyc tyc = tyc

			       val expand = TU.mapTypeFull expandTyc

			       fun addDcons([], elems) = elems
				 | addDcons((d as {name,rep,domain})::dds,
					    elems) =
				   let val typ =
				      TU.dconType(tyc,Option.map expand domain)
				       val const = case domain
						    of NONE => true
						     | _ => false
				       val nd = T.DATACON {name=name,rep=rep,
							   const=const,
							   lazyp=lazyp,
                                                           sign=sign,
							   typ=typ}
 			               val dspec = CONspec{spec=nd, slot=NONE}
			               val elems' = add(name, dspec, elems,
							error region)
				   in addDcons(dds, elems')
				   end
			       val elements'' =
				   addDcons(dcons, elements')

			   in (env', elements'')
			   end
			 | _ => no_datatype ())
		    | _ => no_datatype ()
	      end
	   | T.GENtyc {arity,kind,...} =>
	     (case kind
	       of T.DATATYPE _ =>
		  (* rhs is not local to current outermost signature *)
		  let val (tyc',_) = MU.relativizeTyc epContext tyc
		  in case tyc'
		      of T.PATHtyc{entPath,arity,...} =>
		(* outside current sig but local to enclosing functor *)
			 let (* add the type *)
			     val ev = mkStamp()
			     (* spec uses wrapped version of the PATHtyc!! *)
			     val tspec =
				 TYCspec{entVar=ev,
                                         info=RegTycSpec
                                           {spec=TU.wrapDef(tyc',mkStamp()),
					    repl=true,scope=0}}

			     val elements' =
				 add(name,tspec,elements,error region)

			     val etyc = T.PATHtyc{arity=arity,entPath=[ev],
						  path=IP.IPATH[name]}
			     val env' = SE.bind(name, B.TYCbind etyc, env)

			   (* get the dcons -- quick and dirty (buggy?) hack *)
			     val dcons = TU.extractDcons tyc
			     fun addDcons([], elems) = elems
			       | addDcons((d as T.DATACON{name,rep,const,
							  lazyp,sign,
							  typ})::ds,
					  elems) =
				 let val nd =
				         T.DATACON {name=name,rep=rep,
						    lazyp=lazyp,
						    const=const,sign=sign,
						    typ= #1(MU.relativizeType
								epContext typ)}
				     val dspec = CONspec{spec=nd, slot=NONE}
				     val elems' =
					 add(name, dspec, elems, error region)
				 in addDcons(ds, elems')
				 end

			     val elements'' =
				 addDcons(dcons, elements')
			 in (env', elements'')
			 end
		       | _ => (* fixed global *)
			 let (* add the type *)
			     val ev = mkStamp()
			     val wrappedTyc =
				 case TU.wrapDef(tyc,mkStamp())
				  of T.DEFtyc{stamp, tyfun, strict, path} =>
				      T.DEFtyc{stamp=stamp, tyfun=tyfun, strict=strict,
					       path=IP.IPATH[name]}
				    | _ => bug "--elabSig[elabDATATYPErepl] TU.wrapDef failed."
			     (* [GK 5/4/07] This TU.wrapDef operation seems to replace the
			        name (path) in this signature with the replicated datatype's
				path. The printing of such datatype replications is wrong.

			        signature S = sig datatype u = datatype t end
			        should not print datatype t = datatype t
				[BUGFIX BLOCK]
			      *)
			     val tspec =
				 M.TYCspec{entVar=ev,
                                           info=RegTycSpec{spec=wrappedTyc,
					                   repl=true,scope=0}}
			     (* put in the constant tyc
					   how to treat this in instantiate?*)
			     val elements' =
				 add(name,tspec,elements,error region)

			     val etyc = T.PATHtyc{arity=arity,entPath=[ev],
						  path=IP.IPATH[name]}
			     val env' = SE.bind(name, B.TYCbind etyc, env)

			     val dcons = TU.extractDcons tyc
			     fun addDcons([], elems) = elems
			       | addDcons((dc as T.DATACON{name,...})::dcs,
					  elems) =
				 let val dspec = CONspec{spec=dc, slot=NONE}
				     val elems' =
					 add(name, dspec, elems, error region)
				 in addDcons(dcs, elems')
				 end
			     val elements'' =
				 addDcons(dcons, elements')
		         in (env', elements'')
			 end
		  end
		| _ => no_datatype ())
	   | _ => no_datatype ()
    end
    else (env,elements)

(* exception raised with ElabDATATYPEspec0 in case of duplicated type specs *)
exception TypeDups

(*** elaborating datatype specification ***)
fun elabDATATYPEspec(dtycspec, env, elements, region) =
  let val _ = debugmsg ">>elabDATATYPEspec"
      val err = error region

      (* push a local epContext environment to be used to relativize the
         datacon types and bodies of withtype defns within this declaration *)
      val epContext = EPC.enterClosed(epContext)

      fun isFree (T.PATHtyc _) = true
        | isFree tc =
            (case EPC.lookTycPath(epContext, MU.tycId tc)
              of SOME _ => true
               | _ => false)

      val (dtycs,wtycs,dcons,_) =
            ET.elabDATATYPEdec(dtycspec, env, elements::sctxt, entEnv,
			       isFree, IP.IPATH[], region, compInfo)
      val _ = debugmsg "--elabDATATYPEspec: elabDATATYPEdec done"

      (* the following code readjusts the definitions of datatypes
         and withtycs without changing their stamps; this is ok,
         because all references to the datatypes with same tycs
         will be relativized, so there won't be two datatycs with
         same type stamps.  The ones returned from elabDATATYPEdec,i.e.,
         dtycs, are destroyed. (ZHONG)
       *)
      val vizty = (fn ty => #1(MU.relativizeType epContext ty))
      val viztc = (fn tc => #1(MU.relativizeTyc epContext tc))
      val ndtycs =
        (case dtycs
          of (T.GENtyc { stamp, kind, ... } :: _) =>
	     (case kind of
		  T.DATATYPE{index=0,family,freetycs, stamps, root, ...} =>
		  let (* MAJOR GROSS HACK: use the stamp of the type as its
                       * entVar. This makes possible to reconstruct the
		       * entPath associated with a RECty when translating the
		       * types of domains in elabDATAreplSpec.  See >>HACK<< signs.
                       *)
                      val rtev = stamp (* mkStamp() >>HACK<< *)
                      val nfreetycs = map viztc freetycs
                      fun newdt (dt as T.GENtyc {kind,arity,eq,path,...}) =
			  (case kind of
			       T.DATATYPE{index=i,...} =>
                               let val s = Vector.sub(stamps, i)
				   val (ev, rt) =
				       if i=0 then (rtev, NONE)
				       else (s (* mkStamp() >>HACK<< *),
					     SOME rtev)
				   val nkind =
				       T.DATATYPE{index=i, stamps=stamps,
						  freetycs=nfreetycs,root=rt,
						  family=family,stripped=false}
				   val ndt =
				       T.GENtyc{arity=arity, eq=eq,
						kind=nkind,
						stub=NONE,
						path=path, stamp=s}

				   val _ =
				       EPC.bindTycPath(epContext,
						       MU.tycId ndt, ev)
                               in (ev, arity, ndt)
                               end
			     | _ => bug "unexpected case in newdtyc (1)")
			| newdt _ = bug "unexpected case in newdtyc (2)"
		  in map newdt dtycs
		  end
		| _ => bug "unexpected tycs in bindNewTycs (1)")
           | _ => bug "unexpected tycs in bindNewTycs (2)")

      val nwtycs =
        let fun newwt (T.DEFtyc{stamp, tyfun=T.TYFUN{arity,body},
                                strict, path}) =
		let val ev = stamp (* mkStamp()   >>HACK<< *)
                    val nwt =
			T.DEFtyc{stamp=stamp,strict=strict,path=path,
				 tyfun=T.TYFUN{arity=arity, body=vizty body}}
                    val _ = EPC.bindTycPath(epContext, MU.tycId nwt, ev)
		in (ev, arity, nwt)
		end
	      | newwt _ = bug "newwt"
         in map newwt wtycs
        end

      fun addTycs([], env, elems) = (env, elems)
        | addTycs((ev,arity,tyc)::tycs, env, elems) =
            let val tspec = TYCspec{entVar=ev,
                                    info=RegTycSpec{spec=tyc, repl=false, scope=0}}
                val name = TU.tycName tyc
		val _ = debugmsg ("--elabDATATYPEspec - name: "^ S.name name)
                val _ = if checkDups(name,elems,err) then () else raise TypeDups
                val elems' = add(name, tspec, elems, err)
                val etyc = T.PATHtyc{arity=arity,entPath=[ev],
                                     path=IP.IPATH[name]}
                val env' = SE.bind(name, B.TYCbind etyc, env)
             in addTycs(tycs, env', elems')
            end

      val (env', elements') =
            addTycs(ndtycs@nwtycs, env, elements)
      val _ = debugmsg "--elabDATATYPEspec: tycs added"

      fun addDcons([], elems) = elems
        | addDcons((T.DATACON{name,rep,const,sign,typ,lazyp})::ds, elems) =
            let val _ = debugPrint("addDcons - typ: ",
		   (fn pps => fn ty => PPType.ppType env pps ty), typ)
		val nd = T.DATACON {name=name, rep=rep, const=const, lazyp=lazyp,
                                    sign=sign, typ=vizty typ}
                (** NOTICE that the call to vizty will kill all the
                    references to old datatycs, dtycs, because the
                    same stamp has been mapped to PATHtyc in epContext
                    already. Is it tricky ?! (ZHONG) *)

                val dspec = CONspec{spec=nd, slot=NONE}
                val elems' = add(name, dspec, elems, err)
             in addDcons(ds, elems')
            end

      val elements'' = addDcons(dcons, elements')

      val _ = debugmsg "--elabDATATYPEspec: dcons added"
      val _ = debugmsg "<<elabDATATYPEspec"

   in (env', elements'')
  end
  handle TypeDups => (env,elements)
    (* in case of duplicate type specs introduced by the datatype specs,
     * ignore the datatype specs and return original env and elements *)
(*
fun elabDATATYPEspec(db as {datatycs,withtycs}, env, elements, region) =
    case datatycs
      of ([spec as Db{rhs=Repl path,tyc=name,tyvars=[],lazyp=false}]) =>
	  (* LAZY: not allowing datatype replication with lazy keyword *)
	  elabDATAreplSpec(name,path,env,elements,region)
       | (Db{rhs=Constrs _,...}::_) =>
	  (elabDATATYPEspec0(db,env,elements,region)
           handle TypeDups => (env,elements))
       | _ => (error region EM.COMPLAIN "ill-formed datatype spec"
	         EM.nullErrorBody;
	       (env,elements))
*)

(*** elaborating structure specification ***)
fun elabSTRspec((name,sigexp,defOp), env, elements, slots, region) =
  if checkDups(name,elements,error region) then
  let val _ = debugmsg ("--elabSTRspec: "^S.name name)
      val region0 = region
      val err = error region

      val ev = mkStamp()     (* the entVar for this structure element *)

      val (sign,defStrOp) =
	   let val (sigexp,whereDefs,region) =
		   elabWhere(sigexp,env,epContext,mkStamp,error,region)
	       val sign =
		   case sigexp
		     of VarSig name' => LU.lookSig(env,name',err)
		      | BaseSig specs =>
			  let val (elements', tycShare', strShare',
				   fflag') =
			          elabBody(specs, env, entEnv, elements::sctxt,
					   epContext, region, compInfo)

			      val sign' =
				SIG{stamp = mkStamp(),
				    name=NONE, closed=false,fctflag=fflag',
				    elements=elements',
				    properties = PropList.newHolder (),
				    typsharing=tycShare',
				    strsharing=strShare',
				    stub = NONE}

			   in sign'
			  end
		      | _ => bug "elabSTRspec.strspecs"

	       val sign =
		   case sign
		     of ERRORsig => ERRORsig
		      | _ =>
		       (case whereDefs
			  of nil => sign  (* no where defs *)
			   | _ => addWhereDefs(sign,prepareDefs whereDefs,
					       NONE,
					       (fn msg =>
						  error region
						    EM.COMPLAIN msg
						    EM.nullErrorBody),
					       mkStamp))
	       val defStrOp =
		   case defOp
		    of NONE => NONE
		     | SOME path =>
		       (SOME(lookStrDef(env,SP.SPATH path,epContext,
					error region),
			     length path)
			handle SE.Unbound =>
			 (error region EM.COMPLAIN
			    "unbound rhs in structure definition spec"
			    EM.nullErrorBody;
			  NONE))
	    in  (sign, defStrOp)
	   end

      val _ = debugmsg "--elabSTRspec: signature elaborated"

      val env' = SE.bind(name, B.STRbind(STRSIG{sign=sign,entPath=[ev]}), env)
      val strspec = STRspec{sign=sign,entVar=ev,def=defStrOp,slot=slots}

      val elements' = add(name, strspec, elements, err)

      val _ = debugmsg "<<elabSTRspec"

      val fflag = case sign of SIG {fctflag,...} => fctflag
                             | _ => false

   in (env', elements', fflag)
  end
  else (env, elements, false)
  (* fun elabSTRspec *)

(*** elaborating structure specifications ***)
fun elabSTRspecs([], env, elements, slots, region, fflag) =
      (env, elements, [], [], slots, fflag)

  | elabSTRspecs(spec::rest, env, elements, slots, region, fflag) =
      let val (env', elements', fctflag') =
            elabSTRspec(spec, env, elements, slots, region)
       in elabSTRspecs(rest, env', elements',
                       slots+1, region, fflag orelse fctflag')
      end (* function elabSTRspecs *)

(*
 * Current signature's elements are passed in so that add can check for
 * respecifications of the same name.  The result accumulates new specs
 * in the new values of elements that are returned in the result, along
 * with the new value of slots.
 *
 * The env argument includes all previous specs (i.e. argument elements)
 * at this signature level, as well as outer signature levels.
 *
 * The elements are in order of specification.
 *
 * The return type of elabSpec is
 *
 *    SE.staticEnv * elements * tycShareSpec list * strShareSpec list
 *    * int (slot #)
 *
 * Only the IncludeSpec, ShareTycSpec, and ShareStrSpec cases can produce
 * non-nil tycShareSpec and strShareSpec result components.
 *)

fun elabSpec (spec, env, elements, slots, region) =
  case spec
   of StrSpec specs =>
        elabSTRspecs(specs, env, elements, slots, region, false)

    | FctSpec specs =>
        let val _ = debugmsg "--elabSpec[FctSpec]"
            val err = error region
            fun fctspecs(nil,elems,slots) =
                  (env, elems, [], [], slots, true)
              | fctspecs((name,fsig)::rest,elems,slots) =
		  if checkDups(name,elems,err) then
                      let val fctsig =
                            elabFctSig0 {fsigexp=fsig, nameOp=NONE, env=env,
                                         entEnv=entEnv, sigContext=sctxt,
                                         epContext=epContext, region=region,
                                         compInfo=compInfo, curried=false}
                          val ev = mkStamp()
                          val spec = FCTspec{sign=fctsig, slot=slots, entVar=ev}
                          val elems' = add(name,spec,elems, err)
                       in fctspecs(rest, elems', slots+1)
                      end
                  else fctspecs(rest, elems, slots)
         in fctspecs(specs,elements,slots)
        end

    | TycSpec (specs,eqspec) =>
        let val _ = debugmsg "--elabSpec[TycSpec]"
            val (env', elems') =
              elabTYPEspec(specs, env, elements, eqspec, region)
         in (env', elems', [], [], slots, false)
        end

    | DataSpec spec =>
        let val _ = debugmsg "--elabSpec[DataSpec]"
            val (env', elems') =
                elabDATATYPEspec(spec, env, elements, region)
	        handle TypeDups => (env,elements)
         in (env', elems', [], [], slots, false)
        end

    | DataReplSpec(name,path) =>
        let val _ = debugmsg "--elabSpec[DataReplSpec]"
            val (env', elems') =
                elabDATAreplSpec(name,path,env,elements,region)
         in (env', elems', [], [], slots, false)
        end

    | ValSpec specs =>
        let val err = error region
            fun valspecs(nil,elems,slots) =
                  (env,elems,[],[],slots,false)
              | valspecs((name,ty)::rest,elems,slots) =
                if checkDups(name,elems,err) then
                let val _ = debugmsg ("--elabSpec[ValSpec]: " ^ S.name name)
                      val (ty,tv) = ET.elabType(ty,env,error,region)
                      val typ =
                        case TyvarSet.elements tv
                         of [] => ty
                          | tvs =>
                              let val sign = TU.bindTyvars1 tvs
                               in T.POLYty{sign=sign,
                                           tyfun=T.TYFUN{arity=length tvs,
                                                         body=ty}}
                              end
                      val _ = TU.compressTy typ
                      val (typ,_) = MU.relativizeType epContext typ
                      val vspec = VALspec{spec=typ, slot=slots}
                      val elems' = add(name,vspec,elems,err)
                   in valspecs(rest, elems', slots+1)
                  end
                else valspecs(rest,elems,slots)
         in valspecs(specs,elements,slots)
        end

    | ExceSpec (specs) =>
        let val err = error region
            fun exnspecs(nil,elems,slots) =
                  (env,elems,[],[],slots, false)
              | exnspecs((name,tyOp)::rest,elems,slots) =
                if checkDups(name,elems,err) then
                  let val (typ, const) =
                        (case tyOp
                          of SOME ty =>
                               let val (body,tv) =
                                     ET.elabType(ty,env,error,region)
                                   val nty =
                                     case TyvarSet.elements tv
                                      of nil => BT.-->(body,BT.exnTy)
                                       | _ =>
                                          (err EM.COMPLAIN
                                           ("type variable in exception spec: "
                                           ^ S.name name)
                                           EM.nullErrorBody;
                                           T.WILDCARDty)
                                   val _ = TU.compressTy nty
                                in (#1(MU.relativizeType epContext nty), false)
                               end
                           | NONE => (BT.exnTy, true))

                      val rep = A.EXN(A.nullAcc)
                      val dcon = T.DATACON{name=name, const=const, lazyp=false,
                                           typ=typ, sign=A.CNIL, rep=rep}
                      val cspec = CONspec{spec=dcon, slot=SOME slots}
                      val elems' = add(name,cspec,elems,err)
                   in exnspecs(rest, elems', slots+1)
                  end
                else exnspecs(rest, elems, slots)
         in exnspecs(specs,elements,slots)
        end

    | MarkSpec (spec,region') =>
        elabSpec(spec,env,elements,slots,region')

    | ShareStrSpec pl =>
        let fun loop(nil,internal) = internal
              | loop(p::rest,paths) =
		 if localPath(SP.SPATH p,elements)
		 then (case lookStrDef(env,SP.SPATH p,epContext,error region)
			 of VARstrDef z => loop(rest,(SP.SPATH p)::paths)
			  | CONSTstrDef(ERRORstr) => loop(rest,paths)
			     (* lookStrDef has already complained *)
			  | _ => bug "elabSpec[ShareStrSpec]")
(*		      handle SE.Unbound =>
			(error region EM.COMPLAIN
			   ("unbound path in structure sharing: " ^
			    SP.toString(SP.SPATH p))
			   EM.nullErrorBody;
			 loop(rest,paths))
*)
		 else (error region EM.COMPLAIN
		         ("nonlocal path in structure sharing: " ^
			  SP.toString(SP.SPATH p))
		         EM.nullErrorBody;
		       loop(rest,paths))

            val sharespec = loop(pl,nil)

         in (env,elements,[],[sharespec],slots,false)
        end

    | ShareTycSpec pl =>
        let fun loop(nil,paths) = paths
              | loop(p::rest,paths) =
                  if localPath(SP.SPATH p,elements)
		  then (LU.lookTyc(env,SP.SPATH p,error region);
			loop(rest,(SP.SPATH p)::paths))
		  else (error region EM.COMPLAIN
		         ("nonlocal path in type sharing: " ^
			  SP.toString(SP.SPATH p))
		         EM.nullErrorBody;
		       loop(rest,paths))

            val sharespec = loop(pl,nil)
         in (env,elements,[sharespec],[],slots,false)
        end

    | IncludeSpec sigexp =>  (* param was "name" *)
        let val nsig = elabSig{sigexp=sigexp, nameOp=NONE, env=env,
			       entEnv=entEnv, epContext=epContext,
			       region=region, compInfo=compInfo}
          (* LU.lookSig(env,name,error region) *)
	    (* BUG: this may not work with open sigexps *)
            val (env',elems',tycShare',strShare',slots',fctflag') =
                 Include.elabInclude(nsig, env, elements,
                                     slots, region, compInfo)
         in (env',elems',tycShare',strShare',slots',fctflag')
        end


and elabSpecs ([], env, elements, tycShare, strShare,
	       slots, region, fflag) =
      (env, elements, tycShare, strShare, slots, fflag)

  | elabSpecs (spec::rest, env, elements, tycShare, strShare,
               slots, region, fflag) =
      let val (env',elems',tycShare',strShare',slots', fflag') =
              elabSpec(spec,env,elements,slots,region)

       in elabSpecs(rest, env', elems',
                    tycShare'@tycShare, strShare'@strShare,
                    slots', region, fflag' orelse fflag)
      end

val (_,elements,tycShare,strShare,slots,fflag) =
       elabSpecs(specs,env,nil,nil,nil,0,region,false)

in (rev elements,tycShare,strShare,fflag)

end (* function elabBody *)


and elabFctSig0 {fsigexp, curried, nameOp, env, entEnv, sigContext, epContext,
                 region, compInfo as {mkStamp,error,...}: EU.compInfo} =
let val sname = case nameOp
		  of SOME name => S.name name
		   | _ => "<anonfsig>"
    val _ = debugmsg (">>elabFctSig: " ^ sname)
in

case fsigexp
 of BaseFsig{param=[(paramNameOp,paramSpec)],result} =>
      let val paramSig =
            elabSig0 {sigexp=paramSpec, nameOp=NONE, env=env, entEnv=entEnv,
                     sigContext=sigContext, epContext=epContext,
                     region=region, compInfo=compInfo}
          val paramName = case paramNameOp of NONE => paramId
                                            | SOME sym => sym
          val paramEntVar = mkStamp()
          val paramStr = STRSIG {sign=paramSig,entPath=[paramEntVar]}

          local val paramSpec = STRspec {entVar=paramEntVar, sign=paramSig,
                                         def=NONE, slot=0}
                val paramElmt = [(paramName, paramSpec)]
          in val nsctxt = paramElmt :: sigContext
          end (* a temporary work-around for the sigContext hack *)
	  (* val _ = print "elabFctSig0\n" *)
          val env' =
            case paramNameOp
             of SOME id => (* expose binding of paramName *)
                  SE.bind(id,B.STRbind paramStr,env)
              | NONE => MU.openStructure(env,paramStr)

          val (result,region) = stripMarkSig(result,region)

          val result = if curried then result
		       else BaseSig[StrSpec[(SS.resultId,result,NONE)]]

          val bodySig =
            elabSig0 {sigexp=result, nameOp=NONE, env=env', entEnv=entEnv,
                     sigContext=nsctxt, epContext=epContext,
                     region=region, compInfo=compInfo}

       in FSIG{kind = nameOp,
               paramsig = paramSig,
               paramvar = paramEntVar,
               paramsym = paramNameOp,
               bodysig = bodySig}
      end

  (*** currying fctSig arguments automatically inserts structure wrapping ***)
  | BaseFsig{param = a::r,result} =>
      let val nfsig = BaseSig[FctSpec[(functorId,BaseFsig{param=r,result=result})]]
       in elabFctSig0 {fsigexp=BaseFsig{param=[a],result=nfsig}, nameOp=nameOp,
                       env=env, entEnv=entEnv, sigContext=sigContext,
                       epContext=epContext, region=region, compInfo=compInfo,
		       curried=true}
      end

  | VarFsig name' =>
      LU.lookFsig(env, name', error region)

  | BaseFsig{param = [],result} => bug "elabFctSig"

  | MarkFsig(fsigexp',region') =>
      elabFctSig0 {fsigexp=fsigexp', nameOp=nameOp, env=env, entEnv=entEnv,
                  epContext=epContext, sigContext=sigContext,
                  region=region', compInfo=compInfo, curried=curried}

end (* function elabFctSig0 *)

and elabSig0 {sigexp, nameOp, env, entEnv, sigContext, epContext, region,
             compInfo as {mkStamp,error,...}: EU.compInfo} =
let val region0 = region
    val sname = case nameOp
		  of SOME name => S.name name
		   | _ => "<anonfsig>"
    val _ = debugmsg (">>elabSig: " ^ sname)

    val (sigexp,whereDefs,region) =
	elabWhere(sigexp,env,epContext,mkStamp,error,region)
    val sign =
      case sigexp
	of VarSig name' => 
	     let val SIG{stamp,name,closed,fctflag,elements,properties,typsharing,strsharing,stub}
			 = LU.lookSig(env,name',error region)
	     in SIG{stamp = stamp,
		    name = (case nameOp of NONE => name 
					|  SOME _ => nameOp),  (* update the name field *)
		    closed = closed,
		    fctflag = fctflag,
		    elements = elements,
		    properties = properties,
		    typsharing = typsharing,
		    strsharing = strsharing,
		    stub = stub}
	     end

	 | BaseSig specs =>
	     let val _ = debugmsg "--elabSig >> BaseSig"

		 val (elements, tycShare, strShare, fflag) =
		     elabBody(specs, env, entEnv, sigContext, epContext,
                              region, compInfo)
		 val _ = debugmsg "--elabSig: after elabBody"

		 val sign=SIG{stamp = mkStamp(),
			      name = nameOp,
			      closed = case nameOp
					of SOME _ => true
					 | NONE => false,
			      fctflag=fflag,  (* FLINT *)
			      elements = elements,
			      properties = PropList.newHolder (),
			      typsharing = tycShare,
			      strsharing = strShare,
			      stub = NONE}

	      in debugPrint("--elabSig: returned signature:",
		   (fn pps => fn s => PPModules.ppSignature pps env (s, 6)),sign);
		 debugmsg "--elabSig: << BaseSig";
		 sign
	     end

	 | MarkSig(sigexp',region') => bug "elabSig0"
	     (* elabWhere should have stripped this *)
	 | _ => bug "elabSig0:sigexp"

    val sign =
	case sign
	  of ERRORsig => ERRORsig
	   | _ =>
	    (case whereDefs
	       of nil => sign  (* no where defs *)
		| _ => addWhereDefs(sign,prepareDefs whereDefs,nameOp,
				    (fn msg =>
				       error region0
					 EM.COMPLAIN msg
					 EM.nullErrorBody),
				    mkStamp))

 in sign
end (* function elabSig0 *)

and elabFctSig {fsigexp, nameOp, env, entEnv, epContext, region, compInfo} =
      elabFctSig0 {fsigexp=fsigexp, nameOp=nameOp, env=env, entEnv=entEnv,
                   sigContext=[], epContext=epContext, region=region,
                   compInfo=compInfo, curried=false}

and elabSig {sigexp, nameOp, env, entEnv, epContext, region, compInfo} =
      elabSig0 {sigexp=sigexp, nameOp=nameOp, env=env, entEnv=entEnv,
                sigContext=[], epContext=epContext, region=region,
                compInfo=compInfo}

(*
val elabSigPhase = Stats.makePhase "Compiler 032 5-elabSig"
val elabSig = fn x => Stats.doPhase elabSigPhase elabSig x
*)

end (* local *)
end (* structure ElabSig *)
