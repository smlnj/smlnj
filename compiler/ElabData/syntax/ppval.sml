(* ppval.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(* modified to use SML/NJ Lib PP. [dbm, 7/30/03]) *)

signature PPVAL =
sig
  val ppAccess: PrettyPrint.stream -> Access.access -> unit
  val ppRep: PrettyPrint.stream -> Access.conrep -> unit
  val ppDcon: PrettyPrint.stream -> Types.datacon -> unit
  val ppVar: PrettyPrint.stream -> Variable.var -> unit
  val ppDebugDcon : PrettyPrint.stream
		    -> StaticEnv.staticEnv -> Types.datacon -> unit
  val ppDebugVar: (PrimopId.prim_id -> string) ->
		  PrettyPrint.stream
		  -> StaticEnv.staticEnv -> Variable.var -> unit
end (* signature PPVAL *)

structure PPVal : PPVAL =
struct

local
  structure PP = PrettyPrint
  structure PU = PPUtil
  structure TU = TypesUtil
  structure LU = Lookup
  structure A = Access
  structure LV = LambdaVar
  open PrettyPrint PPUtil Variable Types

in

val internals = ElabDataControl.varconInternals

fun C f x y = f y x

val pps = PP.string
val ppType = PPType.ppType
val ppTycon = PPType.ppTycon
val ppTyfun = PPType.ppTyfun

fun ppAccess ppstrm a = pps ppstrm ("["^(A.prAcc a)^"]")

fun ppInfo ii2string ppstrm a = pps ppstrm (" ["^(ii2string a)^"]")

fun ppRep ppstrm rep = PP.string ppstrm (A.prRep rep)

fun ppCsig ppstrm csig = PP.string ppstrm (A.prCsig csig)

fun ppDcon ppstrm =
    let fun ppD(DATACON{name, rep=A.EXN acc, ...}) =
	       (ppSym ppstrm name;
		if !internals then ppAccess ppstrm acc else ())
	  | ppD(DATACON{name,...}) = ppSym ppstrm name
     in ppD
    end

fun ppDebugDcon ppstrm env (DATACON{name,rep,const,typ,sign,lazyp}) =
    let val {openHVBox, openHOVBox, closeBox, pps, break,...} = en_pp ppstrm
	val ppSym = ppSym ppstrm
     in openHVBox 3;
        pps "DATACON";
	break{nsp=0,offset=0};
	pps "{name = "; ppSym name; ppcomma_nl ppstrm;
	pps "const = "; pps (Bool.toString const); ppcomma_nl ppstrm;
	pps "typ = "; ppType env ppstrm typ; ppcomma_nl ppstrm;
	pps "lazyp = "; pps (Bool.toString lazyp); ppcomma_nl ppstrm;
	pps "conrep ="; ppRep ppstrm rep; ppcomma_nl ppstrm;
        pps "sign = ["; ppCsig ppstrm sign; pps "]}";
        closeBox()
    end

fun ppDatacon (env:StaticEnv.staticEnv,DATACON{name,typ,...}) ppstrm =
    let val {openHVBox, openHOVBox,closeBox,pps,...} = en_pp ppstrm
     in openHOVBox 0;
	ppSym ppstrm name; pps " : "; ppType env ppstrm typ;
	closeBox()
    end

fun ppConBinding ppstrm =
    let val {openHVBox, openHOVBox,closeBox,pps,...} = en_pp ppstrm
	fun ppCon (DATACON{name, typ, rep=A.EXN _, ...}, env) =
		(openHVBox 0;
		 pps "exception "; ppSym ppstrm name;
                 if BasicTypes.isArrowType typ then
                   (pps " of ";
   		    ppType env ppstrm (BasicTypes.domain typ))
                 else ();
		 closeBox())
	  | ppCon (con,env) =
	      let exception Hidden
		  val visibleDconTyc =
		      let val tyc = TU.dataconTyc con
		       in
			  (TypesUtil.equalTycon
			      (LU.lookTyc
			         (env,SymPath.SPATH
				       [InvPath.last(valOf(TypesUtil.tycPath tyc))],
				  fn _ => raise Hidden),
			       tyc)
			     handle Hidden => false)
		      end
	       in if !internals orelse not visibleDconTyc
	          then (openHVBox 0;
			pps "con ";
			ppDatacon(env,con) ppstrm;
		        closeBox())
	          else ()
	      end
     in ppCon
    end

fun ppVar ppstrm (VALvar {access,path,...}) =
      (pps ppstrm (SymPath.toString path);
       if !internals
       then (case access
	       of A.LVAR lvar => pps ppstrm ("." ^ LV.toString lvar)
	        | _ => ppAccess ppstrm access)
       else ())
  | ppVar ppstrm (OVLDvar {name,...}) = ppSym ppstrm (name)
  | ppVar ppstrm (ERRORvar) = PP.string ppstrm "<errorvar>"

fun ppDebugVar ii2string ppstrm env  =
    let val {openHVBox, openHOVBox,closeBox,pps,...} = en_pp ppstrm
	val ppAccess = ppAccess ppstrm
        val ppInfo = ppInfo ii2string ppstrm
	fun ppDV(VALvar {access,path, btvs, typ,prim}) =
	     (openHVBox 0;
	      pps "VALvar";
	      openHVBox 3;
	      pps "({access="; ppAccess access; ppcomma_nl ppstrm;
              pps "prim="; ppInfo prim; ppcomma_nl ppstrm;
	      pps "path="; pps (SymPath.toString path); ppcomma_nl ppstrm;
	      pps "typ=ref "; ppType env ppstrm (!typ);
	      pps "})";
	      closeBox(); closeBox())
	  | ppDV (OVLDvar {name,variants}) =
	     (openHVBox 0;
	      pps "OVLDvar";
	      openHVBox 3;
	      pps "({name="; ppSym ppstrm (name); ppcomma_nl ppstrm;
	      ppcomma_nl ppstrm; pps "})";
	      closeBox();
	      closeBox())
	  | ppDV (ERRORvar) = pps "<ERRORvar>"
     in ppDV
    end

fun ppVariable ppstrm  =
    let val {openHVBox, openHOVBox,closeBox,pps,...} = en_pp ppstrm
	fun ppV(env:StaticEnv.staticEnv,VALvar{btvs,path,access,typ,prim}) =
	      (openHVBox 0;
	       pps(SymPath.toString path);
	       if !internals then ppAccess ppstrm access else ();
	       pps " : "; ppType env ppstrm (!typ);
	       closeBox())
	  | ppV (env,OVLDvar {name,variants}) =
	      (openHVBox 0;
	       ppSym ppstrm (name);
	       closeBox())
	  | ppV(_,ERRORvar) = pps "<ERRORvar>"
     in ppV
    end

end (* local *)
end (* structure PPVal *)
