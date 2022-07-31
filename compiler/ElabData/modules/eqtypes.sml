(* Copyright 1996 by AT&T Bell Laboratories *)
(* eqtypes.sml *)

signature EQTYPES =
sig

  val eqAnalyze : Modules.Structure * (Stamps.stamp -> bool)
                                    * ErrorMsg.complainer -> unit

  val defineEqProps : Types.tycon list * ExpandTycon.sigContext
                      * EntityEnv.entityEnv -> unit

  val checkEqTySig : Types.ty * Types.polysign -> bool
      (* check whether type ty is an equality type, given a polysign
       * indicating which IBOUND elements are equality types.
       * This isn't accurate on (relatized) types containing PATHtycs,
       * which are effectively treated as OBJ *)

  val isEqTycon : Types.tycon -> bool
  val isEqType : Types.ty -> bool

  val debugging : bool ref

end (* signature EQTYPES *)


structure EqTypes : EQTYPES =
struct

(* functions to determine and check equality types *)
local structure EM = ErrorMsg
      structure IP = InvPath
      structure TU = TypesUtil
      structure M = Modules
      structure MU = ModuleUtil
      open Types Stamps TypesUtil

in

(* debugging *)
fun bug msg = EM.impossible("EqTypes: "^msg)
val say = Control_Print.say
val debugging = ref false
fun debugmsg (msg: string) =
    if !debugging then (say msg; say "\n") else ()

fun all (f: 'a -> bool) [] = true
  | all f (x::r) = f x andalso all f r

(* join of eqprops *)
exception INCONSISTENT

fun join(UNDEF,YES) = YES
  | join(YES,UNDEF) = YES
  | join(UNDEF,NO) = NO
  | join(NO,UNDEF) = NO
  | join(UNDEF,IND) = IND
  | join(IND,UNDEF) = IND
  | join(UNDEF,DATA) = DATA
  | join(DATA,UNDEF) = DATA
  | join(UNDEF,UNDEF) = UNDEF
  | join(DATA,YES) = YES
  | join(YES,DATA) = YES
  | join(DATA,NO) = NO
  | join(NO,DATA) = NO
  | join(DATA,IND) = IND
  | join(IND,DATA) = IND
  | join(DATA,DATA) = DATA
  | join(IND,YES) = YES (* ? *)
  | join(YES,IND) = YES (* ? *)
  | join(IND,NO) = NO
  | join(NO,IND) = NO
  | join(IND,IND) = IND
  | join(YES,YES) = YES
  | join(NO,NO) = NO
  | join(OBJ,OBJ) = OBJ
  | join(ABS,e) = join(NO,e)
  | join(e,ABS) = join(e,NO)
  | join(e1,e2) =
     (say(String.concat[TU.eqpropToString e1,",",TU.eqpropToString e2,"\n"]);
      raise INCONSISTENT)

fun objectTyc (GENtyc { eq = ref OBJ, ... }) = true
  | objectTyc _ = false

(* calculating eqtypes in toplevel signatures *)

exception NOT_EQ
exception UnboundStamp

(*
 * eqAnalyze is called in just one place, in Instantiate, to compute the
 * actual eqprops of types in an instantiated signature.  It has to propagate
 * equality properties to respect type equivalences induced by sharing
 * constraints.
 *)

fun eqAnalyze(str,localStamp : Stamps.stamp -> bool,err : EM.complainer) =
let val tycons = ref StampMap.empty
    val depend = ref StampMap.empty
    val dependr = ref StampMap.empty
    val eqprop = ref StampMap.empty
    val dependsInd = ref false
    val tycStampsRef : stamp list ref = ref nil

    fun dflApply dfl (mr, k) =
	case StampMap.find (!mr, k) of
	    NONE => dfl
	  | SOME x => x

    fun applyMap' x = dflApply [] x
    fun applyMap'' x = dflApply UNDEF x

    fun updateMap mr (k, v) = mr := StampMap.insert (!mr, k, v)

    val err = fn s => err EM.COMPLAIN s EM.nullErrorBody

    fun checkdcons(datatycStamp: stamp,
		   evalty: ty -> ty,
		   dcons: dconDesc list,
                   stamps, members, freetycs) : (eqprop * stamp list) =
	let val depend = ref([]: stamp list)
	    val dependsInd = ref false
	    fun member(stamp,[]) = false
	      | member(st,st'::rest) = Stamps.eq(st,st') orelse member(st,rest)
	    fun eqtyc(tyc as GENtyc { stamp, eq, ... }) =
		(case !eq
		  of YES => ()
		   | OBJ => ()
		   | (NO | ABS) => raise NOT_EQ
		   | IND => dependsInd := true
		   | (DATA | UNDEF) =>
		     if member(stamp,!depend)
			orelse Stamps.eq(stamp,datatycStamp) then ()
		     else depend := stamp :: !depend)
	      | eqtyc(RECORDtyc _) = ()
	      | eqtyc _ = bug "eqAnalyze.eqtyc"
	    and eqty(VARty(ref(INSTANTIATED ty)))
                  = eqty ty  (* shouldn't happen *)
	      | eqty(ty as CONty(tyc,args)) =
                  let val ntyc =
                        (case tyc
                          of FREEtyc i =>
                             (List.nth(freetycs, i) handle _ =>
                                  bug "unexpected freetycs in eqty")
                           | _ => tyc)
                   in (case ntyc
  		        of GENtyc _ =>
			   (if objectTyc ntyc then ()
			    else (eqtyc ntyc; app eqty args))
		         | DEFtyc{tyfun,...} => eqty(headReduceType ty)
		         | RECtyc i =>
		            let val stamp = Vector.sub(stamps,i)
                                val {tycname,dcons,...}: dtmember =
                                      Vector.sub(members,i)
		             in if member(stamp,!depend)
			        orelse Stamps.eq(stamp,datatycStamp)
			        then ()
			        else depend := stamp :: !depend
  		            end
  		         | _ => app eqty args)
                  end
	      | eqty (MARKty(tyc, region)) = eqty tyc
	      | eqty _ = ()
	    fun eqdcon({domain=SOME ty',...}: dconDesc) = eqty ty'
	      | eqdcon _ = ()
	 in app eqdcon dcons;
	    case (!depend,!dependsInd)
	      of ([],false) => (YES,[])
	       | (d,false) => (DATA,d)
	       | (_,true) => (IND,[])
	end
	handle NOT_EQ => (NO,[])

    fun addstr(str as M.STR { sign, rlzn = {entities,...}, ... }) =
	let fun addtyc (tyc as (GENtyc { stamp, eq, kind, path, ... })) =
		if localStamp stamp  (* local spec *)
		then ((updateMap tycons
				 (stamp,tyc::applyMap'(tycons,stamp));
		       tycStampsRef := stamp :: !tycStampsRef;
		       case kind
                        of DATATYPE{index,stamps,family={members,...},
                                    root,freetycs,stripped} =>
                              let val dcons = #dcons(Vector.sub(members,index))
				  val eqOrig = !eq
				  val (eqpCalc,deps) =
                                      case eqOrig
                                       of DATA =>
					  checkdcons(stamp,
						     MU.transType entities,
                                                     dcons,stamps,members,
						     freetycs)
					| e => (e,[])
                                  (* ASSERT: e = YES or NO *)
                                  val eq' =
				      join(join(eqOrig,
                                                applyMap''(eqprop,stamp)),
                                           eqpCalc)
                              in
				  eq := eq';
                                  updateMap eqprop (stamp,eq');
                                  app (fn s => updateMap dependr
				      (s, stamp :: applyMap'(dependr,s))) deps;
                                  updateMap depend
					(stamp, deps @ applyMap'(depend,stamp))
                              end
                            | (FLEXTYC _ | ABSTRACT _ | PRIMITIVE) =>
                              let val eq' = join(applyMap''(eqprop,stamp), !eq)
                              in
				  eq := eq';
                                  updateMap eqprop (stamp,eq')
                              end
                            | _ => bug "eqAnalyze.scan.tscan")
			  handle INCONSISTENT =>
                                 err "inconsistent equality properties")
                else () (* external -- assume eqprop already defined *)
              | addtyc _ = ()
	 in
	    if localStamp(MU.getStrStamp str) then
                (List.app (fn s => addstr s) (MU.getStrs str);
                 List.app (fn t => addtyc t) (MU.getTycs str))
            (* BUG? - why can we get away with ignoring functor elements??? *)
            else ()
	end
      | addstr _ = ()   (* must be external or error structure *)

    fun propagate (eqp,depset,earlier) =
	let fun prop stamp' =
	      app (fn s =>
		       let val eqpold = applyMap''(eqprop,s)
                           val eqpnew = join(eqp,eqpold)
		        in if eqpold <> eqpnew
			   then (updateMap eqprop (s,eqp);
			         if earlier s then prop s else ())
			   else ()
		       end handle INCONSISTENT =>
	                     err "inconsistent equality properties B")
                  (depset(stamp'))
	 in prop
	end

    (* propagate the NO eqprop forward and the YES eqprop backward *)
    fun propagate_YES_NO(stamp) =
      let fun earlier s = Stamps.compare(s,stamp) = LESS
       in case applyMap''(eqprop,stamp)
	   of YES =>
               propagate (YES,(fn s => applyMap'(depend,s)),earlier) stamp
	    | NO => propagate (NO,(fn s => applyMap'(dependr,s)),earlier) stamp
            | _ => ()
      end

    (* propagate the IND eqprop *)
    fun propagate_IND(stamp) =
      let fun depset s = applyMap'(dependr,s)
	  fun earlier s = Stamps.compare(s,stamp) = LESS
       in case applyMap''(eqprop,stamp)
	   of UNDEF => (updateMap eqprop (stamp,IND);
		        propagate (IND,depset,earlier) stamp)
	    | IND => propagate (IND,depset,earlier) stamp
	    | _ => ()
      end

    (* phase 0: scan signature strenv, joining eqprops of shared tycons *)
    val _ = addstr str
    val tycStamps =
      ListMergeSort.sort (fn xy => Stamps.compare xy = GREATER) (!tycStampsRef)
 in
    (* phase 1: propagate YES backwards and NO forward *)
    app propagate_YES_NO tycStamps;

    (* phase 2: convert UNDEF to IND and propagate INDs *)
    app propagate_IND tycStamps;  (* convert UNDEFs to INDs and propagate *)

    (* phase 3: convert DATA to YES; reset stored eqprops from eqprop map *)
    app (fn s =>
          let val eqp = case applyMap''(eqprop,s)
			  of DATA => YES
			   | e => e
	      fun set (GENtyc { eq, ... }) = eq := eqp
		| set _ = ()
           in app set (applyMap'(tycons,s))
          end)
    tycStamps
end

exception CHECKEQ


(* WARNING - defineEqTycon uses eq field ref as a tycon identifier.
   Since defineEqTycon is called only within elabDATATYPEdec, this
   should be ok.*)

val unitTy = BasicTypes.unitTy

fun member(_,[]) = false
  | member(i:int, j::rest) = i=j orelse member(i,rest)

fun namesToString ([]: Symbol.symbol list) = "[]"
  | namesToString (x::xs) =
    String.concat("[" :: (Symbol.name x) ::
		  foldl (fn (y,l) => ","::(Symbol.name y)::l) ["]"] xs)

fun defineEqProps (datatycs,sigContext,sigEntEnv) =
let val names = map TU.tycName datatycs
    val _ = debugmsg (">>defineEqProps: "^ namesToString names)
    val n = List.length datatycs
    val {family={members,...}, freetycs,...} =
	case List.hd datatycs of
	    GENtyc { kind = DATATYPE x, ...} => x
	  | _ => bug "defineEqProps (List.hd datatycs)"
    val eqs =
	let fun get (GENtyc { eq, ... }) = eq
	      | get _ = bug "eqs:get"
	in map get datatycs
	end
    fun getEq i =
           !(List.nth(eqs,i))
            handle Subscript =>
                    (say "$getEq "; say(Int.toString i); say " from ";
		     say(Int.toString(length eqs)); say "\n";
		     raise Subscript)
    fun setEq(i,eqp) =
       (debugmsg (String.concat["setEq: ",Int.toString i," ",
			     TU.eqpropToString eqp]);
	(List.nth(eqs,i) := eqp)
	   handle Subscript =>
	   (say (String.concat["$setEq ",(Int.toString i)," from ",
	    (Int.toString(length eqs)),"\n"]);
	raise Subscript))
     val visited = ref([]: int list)

     fun checkTyc (tyc0 as GENtyc { eq, kind, path, ... }) =
	 (case (!eq, kind) of
	      (DATA, DATATYPE { index, ... }) =>
	      let val _ = debugmsg (">>checkTyc: "^
				    Symbol.name(IP.last path)^" "^
				    Int.toString index)
		  fun eqtyc (GENtyc { eq = e', kind = k', path, ... }) =
		      (case (!e', k')
			of (DATA,DATATYPE{index,...}) =>
			   (debugmsg ("eqtyc[GENtyc(DATA)]: " ^
				      Symbol.name(IP.last path) ^
				      " " ^ Int.toString index);
			   (* ASSERT: argument tycon is a member of datatycs *)
			    checkDomains index)
			 | (UNDEF,_) =>
			   (debugmsg ("eqtyc[GENtyc(UNDEF)]: " ^
				      Symbol.name(IP.last path));
			    IND)
			 | (eqp,_) =>
			   (debugmsg ("eqtyc[GENtyc(_)]: " ^
				      Symbol.name(IP.last path) ^
				      " " ^ TU.eqpropToString eqp);
			    eqp))
		    | eqtyc(RECtyc i) =
		      (debugmsg ("eqtyc[RECtyc]: " ^ Int.toString i);
		       checkDomains i)
		    | eqtyc(RECORDtyc _) = YES
		    | eqtyc(ERRORtyc) = IND
		    | eqtyc(FREEtyc i) = bug "eqtyc - FREEtyc"
		    | eqtyc(PATHtyc _) = bug "eqtyc - PATHtyc"
		    | eqtyc(DEFtyc _) = bug "eqtyc - DEFtyc"

		  and checkDomains i =
		      if member(i,!visited) then getEq i
		      else let
			  val _ = visited := i :: !visited
			  val {tycname,dcons,...} : dtmember
			    = Vector.sub(members,i)
			      handle Subscript =>
				     (say (String.concat
					       ["$getting member ",
						Int.toString i,
						" from ",
						Int.toString(Vector.length members),"\n"]);
				      raise Subscript)
			  val _ = debugmsg("checkDomains: visiting "
					   ^ Symbol.name tycname ^ " "
					   ^ Int.toString i)
			  val domains =
			      map (fn ({domain=SOME ty,...}: dconDesc) => ty
				    | {domain=NONE,...} => unitTy)
				  dcons
			  val eqp = eqtylist(domains)
			  in
			      setEq(i,eqp);
			      debugmsg ("checkDomains: setting "^
					Int.toString i^
					" to "^TU.eqpropToString eqp);
			      eqp
			  end

		  and eqty(VARty(ref(INSTANTIATED ty))) =
		      (* shouldn't happen *)
		      eqty ty
		    | eqty(CONty(tyc,args)) =
		      (case ExpandTycon.expandTycon(tyc,sigContext,sigEntEnv)
			of FREEtyc i =>
			   let val _ =
				debugmsg ("eqtyc[FREEtyc]: " ^ Int.toString i)
                               val tc = (List.nth(freetycs,i)
					 handle _ =>
						bug "unexpected freetycs 343")
			   in
			       eqty(CONty(tc, args))
			   end
			 | DEFtyc{tyfun,...} =>
			   (* shouldn't happen - type abbrevs in domains
			    * should have been expanded *)
			   eqty(applyTyfun(tyfun,args))
			 | tyc =>
			   (case eqtyc tyc
			     of (NO | ABS) => NO
			      | OBJ => YES
			      | YES => eqtylist(args)
			      | DATA =>
				(case eqtylist(args) of YES => DATA | e => e)
			      | IND => IND
			      | UNDEF =>
				bug ("defineEqTycon.eqty: UNDEF - " ^
				     Symbol.name(TU.tycName tyc))))
		    | eqty (MARKty (tyc, region)) = eqty tyc
		    | eqty _ = YES

		  and eqtylist(tys) =
		      let fun loop([],eqp) = eqp
			    | loop(ty::rest,eqp) =
			      case eqty ty
			       of (NO | ABS) => NO  (* return NO immediately;
			      no further checking *)
				| YES => loop(rest,eqp)
				| IND => loop(rest,IND)
				| DATA =>
				  (case eqp
				    of IND => loop(rest,IND)
				     | _ => loop(rest,DATA))
				| _ => bug "defineEqTycon.eqtylist"
		      in loop(tys,YES)
		      end

	      in
		  case eqtyc tyc0
		   of YES => app (fn i =>
				     case getEq i
				      of DATA => setEq(i,YES)
				       | _ => ()) (!visited)
		    | DATA => app (fn i =>
				      case getEq i
				       of DATA => setEq(i,YES)
					| _ => ()) (!visited)
		    | NO => app (fn i =>
				    if i > index
				    then case getEq i
 		     			  of IND => setEq(i,DATA)
					   | _ => ()
				    else ()) (!visited)
		    (* have to be reanalyzed, throwing away information ??? *)
		    | IND => ()
		    | _ => bug "defineEqTycon";
		  (* ASSERT: eqprop of tyc0 is YES, NO, or IND *)
		  case !eq
		   of (YES | NO | IND) => ()
		    | DATA =>
		      bug ("checkTyc[=>DATA]: "^Symbol.name(IP.last path))
		    | _ =>
		      bug ("checkTyc[=>other]: "^Symbol.name(IP.last path))
	      end
	    | _ => ())
       | checkTyc _ = ()
in
    List.app checkTyc datatycs
end

fun isEqType ty =
    let fun eqty(VARty(ref(INSTANTIATED ty))) = eqty ty
	  | eqty(VARty(ref(OPEN {eq,...}))) =
	      if eq then ()
	      else raise CHECKEQ
	  | eqty(CONty(DEFtyc{tyfun,...}, args)) = eqty(applyTyfun(tyfun,args))
	  | eqty(CONty(GENtyc { eq, ... }, args)) =
	      (case !eq
		 of OBJ => ()
		  | YES => app eqty args
		  | (NO | ABS | IND) => raise CHECKEQ
		  | _ => bug "isEqType")
	  | eqty(CONty(RECORDtyc _, args)) = app eqty args
	  | eqty (MARKty (tyc, region)) = eqty tyc
	  | eqty _ = ()
     in eqty ty; true
    end
    handle CHECKEQ => false

fun checkEqTySig(ty, sign: polysign) =
    let fun eqty(VARty(ref(INSTANTIATED ty))) = eqty ty
	  | eqty(CONty(DEFtyc{tyfun,...}, args)) =
	      eqty(applyTyfun(tyfun,args))
	  | eqty(CONty(GENtyc { eq, ... }, args)) =
	      (case !eq
		 of OBJ => ()
		  | YES => app eqty args
		  | (NO | ABS | IND) => raise CHECKEQ
		  | _ => bug "checkEqTySig")
	  | eqty(IBOUND n) =
	      let val eq = List.nth(sign,n)
	       in if eq then () else raise CHECKEQ
	      end
	  | eqty _ = ()
     in eqty ty;
	true
    end
    handle CHECKEQ => false

fun replicate(0,x) = nil | replicate(i,x) = x::replicate(i-1,x)

fun isEqTycon(GENtyc { eq, ... }) =
      (case !eq
	 of YES => true
	  | OBJ => true
	  | _ => false)
  | isEqTycon(DEFtyc{tyfun as TYFUN{arity,...},...}) =
      isEqType(applyTyfun(tyfun,replicate(arity,BasicTypes.intTy)))
  | isEqTycon _ = bug "isEqTycon"

end (* local *)
end (* structure EqTypes *)


