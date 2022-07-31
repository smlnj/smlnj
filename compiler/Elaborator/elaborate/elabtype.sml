(* COPYRIGHT (c) 1998 Bell Laboratories *)
(* elabtype.sml *)

structure ElabType : ELABTYPE =
struct

local structure EM = ErrorMsg
      structure S  = Symbol
      structure SP = SymPath
      structure IP = InvPath
      structure SE = StaticEnv
      structure L  = Lookup
      structure B  = Bindings
      structure T  = Types
      structure TU = TypesUtil
      structure BT = BasicTypes
      structure EU = ElabUtil
      structure TS = TyvarSet
      open Symbol Absyn Ast PrintUtil Types TypesUtil Variable
in

val debugging = ElabControl.etdebugging (* ref false *)
val say = Control_Print.say
fun debugmsg (msg: string) =
    if !debugging then (say msg; say "\n") else ()

fun bug msg = ErrorMsg.impossible("ElabType: " ^ msg)

(**** TYPES ****)

val --> = BT.-->
infix -->

fun elabTyv(tyv:Ast.tyvar,error,region:region)  =
    case tyv
     of Tyv vt => mkTyvar(mkUBOUND(vt))
      | MarkTyv(tyv,region) => elabTyv(tyv,error,region)  (* ignore MarkTy *)

fun elabTyvList (tyvars,error,region) =
  let val tvs = map (fn tyv => elabTyv(tyv,error,region)) tyvars
      val names = map (fn (ref(UBOUND{name,...})) => name
                        | _ => bug "elabTyvList") tvs
   in EU.checkUniq((error region),"duplicate type variable name",names);
      tvs
  end

fun elabType(ast:Ast.ty,env:SE.staticEnv,error,region:region)
            : (Types.ty * TS.tyvarset) =
     case ast
      of VarTy vt =>
	   let val tyv = elabTyv(vt,error,region)
	    in (VARty tyv, TS.singleton tyv)
	   end
       | ConTy (co,ts) =>
	   let val co1 =
		   if (S.name (hd co)) = "->"
		   then BT.arrowTycon
		   else L.lookArTyc(env,SP.SPATH co,length ts,error region)
	       val (lts1,lvt1) = elabTypeList(ts,env,error,region)
	    in (CONty (co1,lts1),lvt1)
	   end
       | RecordTy lbs =>
	   let val (lbs1,lvt1) = elabTLabel(lbs,env,error,region)
	    in (BT.recordTy(EU.sortRecord(lbs1,error region)),lvt1)
	   end
       | TupleTy ts =>
	   let val (lts1,lvt1) = elabTypeList(ts,env,error,region)
	    in (BT.tupleTy lts1,lvt1)
	   end
       | MarkTy (ty,region) => elabType(ty,env,error,region)  (* ignore MarkTy *)
	 (*  let val (tyc,lvt) = elabType(ty,env,error,region)
	      in (MARKty(tyc,region),lvt)
	     end
          *)

and elabTLabel(labs,env,error,region:region) =
    foldr
      (fn ((lb2,t2),(lts2,lvt2)) =>
	  let val (t3,lvt3) = elabType(t2,env,error,region)
	   in ((lb2,t3) :: lts2, TS.union(lvt3,lvt2,error region))
	  end)
      ([],TS.empty) labs

and elabTypeList(ts,env,error,region:region) =
    foldr
      (fn (t2,(lts2,lvt2)) =>
	  let val (t3,lvt3) = elabType(t2,env,error,region)
	   in (t3 :: lts2, TS.union(lvt3,lvt2,error region))
	  end)
      ([],TS.empty) ts


(**** DATACON DECLARATIONS ****)
exception ISREC

fun elabDB((tyc,args,name,def,region,lazyp),env,rpath:IP.path,error) =
   let val rhs = CONty(tyc, map VARty args)

       fun checkrec(_,NONE) = ()
         | checkrec(_,SOME typ) =
	     let fun findname(VarTy _) = ()
		   | findname(ConTy([co],ts)) =
                       if Symbol.eq (co,name) then (raise ISREC)
		       else app findname ts
		   | findname(ConTy(_,ts)) = app findname ts
		   | findname(RecordTy lbs) = app (fn (_,t) => findname t) lbs
		   | findname(TupleTy ts) = app findname ts
		   | findname(MarkTy(t,_)) = findname t

	      in findname(typ)
	     end

	fun elabConstr (cname, tyOp) =
	    (if EU.checkForbiddenCons cname
	     then error region EM.COMPLAIN
			(concat["datatype \"", S.name name, "\" has forbidden constructor name: \"",
				S.name cname, "\""])
			EM.nullErrorBody
	     else ();
	     case tyOp
	      of SOME ty => let val (t,tv) = elabType(ty,env,error,region)
			    in ((cname,false,(t --> rhs)),tv)
			    end
	       | NONE => ((cname,true,rhs),TS.empty))

	val arity = length args
	val isrec = (app checkrec def; false) handle ISREC => true
	val (dcl,tvs) =
	      foldr
		(fn (d,(dcl1,tvs1)) =>
		   let val (dc2,tv2) = elabConstr d
		    in (dc2::dcl1,TS.union(tv2,tvs1,error region))
		   end)
		([],TS.empty) def
	val _ = EU.checkBoundTyvars(tvs,args,error region)
	val _ = TU.bindTyvars args
	val sdcl = EU.sort3 dcl
	val (reps, sign) = ConRep.infer isrec sdcl
	fun makeDcon ((sym,const,typ), rep) =
	      let val _ = TU.compressTy typ
		  val typ =
		      if arity > 0
		      then POLYty {sign=mkPolySign arity,
				   tyfun=TYFUN{arity=arity,body=typ}}
		      else typ
	       in DATACON{name=sym, const=const, rep=rep,
                          sign=sign, typ=typ, lazyp=lazyp}
	      end

     in if length sdcl < length dcl  (* check for duplicate constructor names *)
	then let fun member(x:string,[]) = false
		   | member(x,y::r) = (x = y) orelse member(x,r)
		 fun dups([],l) = l
		   | dups(x::r,l) =
		       if member(x,r) andalso not(member(x,l))
		       then dups(r,x::l)
		       else dups(r,l)
		 fun add_commas [] = []
		   | add_commas (y as [_]) = y
		   | add_commas (s::r) = s :: "," :: add_commas(r)
		 val duplicates = dups(map (fn (n,_,_) => S.name n) dcl,[])
	      in error region EM.COMPLAIN
		   (concat["datatype ", S.name name,
			    " has duplicate constructor name(s): ",
			    concat(add_commas(duplicates))])
		   EM.nullErrorBody
	     end
	else ();
	ListPair.mapEq makeDcon (sdcl, reps)
    end


(**** TYPE DECLARATIONS ****)

fun elabTBlist(tbl:Ast.tb list,notwith:bool,env0,rpath,region,
	       {mkStamp,error,...}: EU.compInfo)
      : T.tycon list * S.symbol list * SE.staticEnv =
    let fun elabTB(tb: Ast.tb, env, region): (T.tycon * symbol) =
	    case tb
	      of Tb{tyc=name,def,tyvars} =>
		   let val tvs = elabTyvList(tyvars,error,region)
		       val (ty,tv) = elabType(def,env,error,region)
		       val arity = length tvs
		       val _ = EU.checkBoundTyvars(tv,tvs,error region)
		       val _ = TU.bindTyvars tvs
		       val _ = TU.compressTy ty
		       val tycon =
			   DEFtyc{stamp=mkStamp(),
				  path=InvPath.extend(rpath,name),
				  strict=TU.calcStrictness(arity,ty),
				  tyfun=TYFUN{arity=arity, body=ty}}
		    in (tycon,name)
		   end
	      | MarkTb(tb',region') => elabTB(tb',env,region')
	fun loop(nil,tycons,names,env) = (rev tycons,rev names,env)
	  | loop(tb::rest,tycons,names,env) =
	      let val env' = if notwith then env0 else SE.atop(env,env0)
		  val (tycon,name) = elabTB(tb,env',region)
	       in loop(rest,tycon::tycons,name::names,
		       SE.bind(name,B.TYCbind tycon,env))
	      end
     in loop(tbl,nil,nil,SE.empty)
    end

fun elabTYPEdec(tbl: Ast.tb list,env,rpath,region,
		compInfo as {error,mkStamp,...}: EU.compInfo)
      : Absyn.dec * SE.staticEnv =
    let	val _ = debugmsg ">>elabTYPEdec"
	val (tycs,names,env') =
            elabTBlist(tbl,true,env,rpath,region,compInfo)
	val _ = debugmsg "--elabTYPEdec: elabTBlist done"
     in EU.checkUniq(error region, "duplicate type definition", names);
	debugmsg "<<elabTYPEdec";
        (TYPEdec tycs, env')
    end

fun elabDATATYPEdec({datatycs,withtycs}, env0, sigContext,
                     sigEntEnv, isFree, rpath, region,
                     compInfo as {mkStamp,error,...}: EU.compInfo) =
    let (* predefine datatypes *)
	val _ = debugmsg ">>elabDATATYPEdec"
	fun preprocess region (Db{tyc=name,rhs=def,tyvars,lazyp}) =
	    let val tvs = elabTyvList(tyvars,error,region)
		val strictName =
		    if lazyp
		    then S.tycSymbol(S.name name ^ "!")
		    else name
		val tyc = GENtyc{path=IP.extend(rpath,strictName),
				 arity=length tyvars,
				 stamp=mkStamp(),
				 eq=ref DATA,
				 kind = TEMP,
				 stub = NONE}
		val binddef =
		    if lazyp then
			   DEFtyc{stamp=mkStamp(),
				  tyfun=TYFUN{arity=length tyvars,
					      body=CONty(BT.suspTycon,
						    [CONty(tyc,map VARty tvs)])},
			          strict=map (fn _ => true) tyvars,
				  path=IP.extend(rpath,name)}

		    else tyc
	     in {tvs=tvs, name=name,def=def,region=region,
		 tyc=tyc, binddef=binddef,lazyp=lazyp,
		 strictName=strictName}
	    end
	  | preprocess _ (MarkDb(db',region')) = preprocess region' db'

        val dbs = map (preprocess region) datatycs
        val _ = debugmsg "--elabDATATYPEdec: preprocessing done"

        val envDTycs = (* staticEnv containing preliminary datatycs *)
	      foldl (fn ({name,binddef,...},env) =>
			   SE.bind(name, B.TYCbind binddef, env))
		       SE.empty dbs
        val _ = debugmsg "--elabDATATYPEdec: envDTycs defined"

	(* elaborate associated withtycs *)
	val (withtycs,withtycNames,envWTycs) =
	    elabTBlist(withtycs,false,SE.atop(envDTycs,env0),
		       rpath,region,compInfo)
        val _ = debugmsg "--elabDATATYPEdec: withtycs elaborated"

	(* check for duplicate tycon names *)
        val _ = EU.checkUniq(error region,
			     "duplicate type names in type declaration",
			     map #name dbs @ withtycNames);
        val _ = debugmsg "--elabDATATYPEdec: uniqueness checked"

        (* add lazy auxiliary withtycs if any *)
        val withtycs = map #binddef (List.filter #lazyp dbs) @ withtycs

	(* staticEnv containing only new datatycs and withtycs *)
	val envTycs = SE.atop(envWTycs, envDTycs)
	(* staticEnv for evaluating the datacon types *)
	val fullEnv = SE.atop(envTycs,env0)
        val _ = debugmsg "--elabDATATYPEdec: envTycs, fullEnv defined"

        val prelimDtycs = map #tyc dbs

        (* the following functions pull out all the flexible components
           inside the domains of the datatypes, and put them into the
           freetycs field in the DATATYPE kind; this way, future
           re-instantiations of the datatypes only need to modify the
           freetycs list, rather than all the domains (ZHONG)
         *)
        val freeTycsRef = ref ([] : tycon list, 0)
        fun regFree tyc =
          let val (ss, n) = !freeTycsRef
              fun h (x::rest, i) =
                   if eqTycon(tyc, x) then FREEtyc (i-1)
                   else h(rest, i-1)
                | h ([], _) =
                   let val _ = (freeTycsRef := (tyc::ss, n+1))
                    in FREEtyc n
                   end
           in h (ss, n)
          end

	fun transTyc (tyc as GENtyc { kind = TEMP, ... }) =
	    let fun g(tyc,i,x::rest) =
		    if eqTycon(tyc,x) then RECtyc i
                    else g(tyc,i+1,rest)
		  | g(tyc,_,nil) = tyc
	    in g(tyc,0,prelimDtycs)
	    end
	  | transTyc (tyc as GENtyc _) =
	    if isFree tyc then regFree tyc else tyc
	  | transTyc (tyc as (DEFtyc _ | PATHtyc _)) =
              if isFree tyc then regFree tyc else tyc
          | transTyc tyc = tyc

	fun transType t =
	    case TU.headReduceType t
	      of CONty(tyc, args) =>
		   CONty(transTyc tyc,map transType args)
	       | POLYty{sign,tyfun=TYFUN{arity,body}} =>
		   POLYty{sign=sign,
			  tyfun=TYFUN{arity=arity,body=transType body}}
	       | MARKty(tyc, region) => transType tyc
	       | t => t

	(* elaborate the definition of a datatype *)
	fun elabRHS ({tvs,name,def,region,tyc,lazyp,binddef,strictName},
		     (i,done)) =
	    let val datacons =
                      elabDB((tyc,tvs,name,def,region,lazyp),fullEnv,rpath,error)
		fun mkDconDesc (DATACON{name,const,rep,sign,typ,lazyp}) =
		    {name=name, rep=rep,
		     domain=
		       if const then NONE
		       else case transType typ
			      of CONty(_,[dom,_]) => SOME dom
                               | POLYty{tyfun=TYFUN{body=CONty(_,[dom,_]),...},
					...} => SOME dom
			       | _ => bug "elabRHS"}
	     in (i+1,
		 {name=name,
		  dconNames=map (fn DATACON{name,...} => name) datacons,
		    (* duplicate names removed *)
		  dcons=datacons,
		  dconDescs=map mkDconDesc datacons,
		  tyc=tyc,
		  index=i,
		  lazyp=lazyp,
		  strictName=strictName} :: done)
	    end

        val (_,dbs') = foldl elabRHS (0,nil) dbs
	val dbs' = rev dbs'
        val _ = debugmsg "--elabDATATYPEdec: RHS elaborated"

        fun mkMember{name,dcons=DATACON { sign, ... } :: _,
		     dconDescs, tyc=GENtyc { stamp, arity, eq, ... },
		     dconNames,index,lazyp,strictName} =
    (* extract common sign from first datacon *)
	    (stamp, {tycname=strictName,dcons=dconDescs,arity=arity,
                     eq=eq,lazyp=lazyp,sign=sign})
	  | mkMember _ = bug "mkMember"

        val (mstamps, members) = ListPair.unzip (map mkMember dbs')

        val nstamps = Vector.fromList mstamps
        val nfamily = {members=Vector.fromList members,
		       properties = PropList.newHolder (),
                       (* lambdatyc=ref NONE, *)
                       mkey=mkStamp()}
        val nfreetycs =
          let val (x, n) = !freeTycsRef
              val _ = if length x = n then ()  (* sanity check *)
                      else bug "unexpected nfreetycs in elabDATATYPEdec"
           in rev x
          end
        val _ = debugmsg "--elabDATATYPEdec: members defined"

        fun fixDtyc{name,index,
		    tyc as GENtyc {path,arity,stamp,eq,kind,stub},
		    dconNames,dcons,dconDescs,lazyp,strictName} =
	    {old=tyc,
	     name=strictName,
	     new=GENtyc{path=path,arity=arity,stamp=stamp,eq=eq,
			stub=NONE,
			kind=DATATYPE{index=index,
				      stamps=nstamps,
				      family=nfamily,
				      freetycs=nfreetycs,
				      root=NONE,
				      stripped=false}}}
	  | fixDtyc _ = bug "fixDtyc"

        val dtycmap = map fixDtyc dbs'  (* maps prelim to final datatycs *)
        val _ = debugmsg "--elabDATATYPEdec: fixDtycs done"

	val finalDtycs = map #new dtycmap
        val _ = debugmsg "--elabDATATYPEdec: finalDtycs defined"

        val _ = EqTypes.defineEqProps(finalDtycs,sigContext,sigEntEnv)
        val _ = debugmsg "--elabDATATYPEdec: defineEqProps done"

        fun applyMap m =
            let fun sameTyc(GENtyc g1, GENtyc g2) =
		    Stamps.eq(#stamp g1, #stamp g2)
                  | sameTyc(tyc1 as DEFtyc _, tyc2 as DEFtyc _) =
		      equalTycon(tyc1, tyc2)
                  | sameTyc _ = false

                fun f(CONty(tyc, args)) =
	              let fun look({old,new,name}::rest) =
			      if sameTyc(old,tyc) then new else look rest
			    | look nil = tyc
		       in CONty(look m, map (applyMap m) args)
		      end
		  | f (POLYty{sign,tyfun=TYFUN{arity,body}}) =
		      POLYty{sign=sign,tyfun=TYFUN{arity=arity,body=f body}}
		  | f (MARKty(t,_)) = f t
		  | f t = t
             in f
            end

        fun augTycmap (tyc as DEFtyc{tyfun=TYFUN{arity,body},stamp,
                                     strict,path}, tycmap) =
            {old=tyc,name=IP.last path,
	     new=DEFtyc{tyfun=TYFUN{arity=arity,body=applyMap tycmap body},
			strict=strict,stamp=stamp,path=path}}
	    :: tycmap
	  | augTycmap _ = bug "augTycMap"

        (* use foldl to process the withtycs in their original order *)
        val alltycmap = foldl augTycmap dtycmap withtycs
        val _ = debugmsg "--elabDATATYPEdec: alltycmap defined"

        fun header(_, 0, z) = z
          | header(a::r, n, z) = header(r, n-1, a::z)
          | header([], _, _) = bug "header2 in elabDATATYPEdec"

	val finalWithtycs = map #new (header(alltycmap,length withtycs,[]))
        val _ = debugmsg "--elabDATATYPEdec: finalWithtycs defined"

        fun fixDcon (DATACON{name,const,rep,sign,typ,lazyp}) =
	    DATACON{name=name,const=const,rep=rep,sign=sign,lazyp=lazyp,
		    typ=applyMap alltycmap typ}

        val finalDcons = List.concat(map (map fixDcon) (map #dcons dbs'))
        val _ = debugmsg "--elabDATATYPEdec: finalDcons defined"

        val envDcons = foldl (fn (d as DATACON{name,...},e)=>
			         SE.bind(name,B.CONbind d, e))
	                     SE.empty
	                     finalDcons

        val finalEnv = foldl (fn ({old,name,new},e) =>
			         SE.bind(name,B.TYCbind new,e))
	                     envDcons alltycmap
        val _ = debugmsg "--elabDATATYPEdec: envDcons, finalEnv defined"

     in EU.checkUniq
          (error region, "duplicate datacon names in datatype declaration",
	   List.concat(map #dconNames dbs'));
        debugmsg "<<elabDATATYPEdec";
	(finalDtycs,finalWithtycs,finalDcons,finalEnv)
    end (* fun elabDATATYPEdec *)

end (* local *)
end (* structure ElabType *)
