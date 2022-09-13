(* ppval.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(* modified to use SML/NJ Lib PP. [dbm, 7/30/03]) *)

signature PPVAL =
sig
  val fmtAccess: Access.access -> PP.format
  val fmtRep: Access.conrep -> PP.format
  val fmtDcon: Types.datacon -> PP.format
  val fmtVar: Variable.var -> PP.format
  val fmtDebugDcon : StaticEnv.staticEnv -> Types.datacon -> PP.format
  val fmtDebugVar: (PrimopId.prim_id -> string)
		   -> StaticEnv.staticEnv -> Variable.var -> PP.format
end (* signature PPVAL *)

structure PPVal : PPVAL =
struct

local
  structure PP = NewPP
  structure PPU = NewPPUtil
  structure PPT = PPType
  structure TU = TypesUtil
  structure LU = Lookup
  structure A = Access
  structure LV = LambdaVar
  structure V = Variable
  structure T = Types
in

val internals = ElabDataControl.varconInternals

fun C f x y = f y x

fun fmtAccess a = PP.brackets (PP.text (A.accessToString a))

fun fmtRep rep = PP.text (A.repToString rep)

fun fmtConsig csig = PP.text (A.consigToString csig)

fun fmtDcon dcon =
    (case dcon
      of (DATACON{name, rep=A.EXN acc, ...}) =>
	   PP.ccat
	       (PPU.fmtSymbol name;
		if !internals then fmtAccess acc else PP.empty)
	  | DATACON{name,...} => PPU.fmtSymbol name)

fun fmtDebugDcon env (DATACON{name,rep,const,typ,sign,lazyp}) =
    let fun foo (field : string, value : PP.format) = PP.hblock [PP.text field, PP.text "=", value]
     in PP.hcat
          (PP.text "DATACON",
	    (PP.braces
               (PP.vblock
	          [foo ("name", PPU.fmtSymbol name),
		   foo ("const", PP.bool const),
		   foo ("typ", PPT.fmtType env typ),
		   foo ("lazyp", PP.bool lazyp),
		   foo ("conrep", fmtConrep rep),
		   foo ("sign", PP.brackets (fmtConsig sign))])))


fun fmtDatacon (env:StaticEnv.staticEnv,DATACON{name,typ,...}) =
    PP.pblock [PPU.fmtSymbol name, PP.colon, fmtType env typ]

fun fmtConBinding (env, dcon) =
    (case dcon
       of DATACON{name, typ, rep=A.EXN _, ...} =>  (* exception constructor *)
	    PP.pblock
	     [PP.text "exception",
	      PPU.fmtSymbol name,
	      if BasicTypes.isArrowType typ then
              then PP.hcat (PP.text "of", PPT.fmtType env (BasicTypes.domain typ))
              else PP.empty]
	| con =>
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
	    end)

fun fmtVar (VALvar {access,path,...}) =
      (PP.text (SymPath.toString path)
       if !internals
       then (case access
	       of A.LVAR lvar => pps ("." ^ LV.toString lvar)
	        | _ => fmtAccess access)
       else ())
  | fmtVar (OVLDvar {name,...}) = PPU.fmtSymbol name
  | fmtVar (ERRORvar) = PP.string "<errorvar>"

fun ppDebugVar ii2string env  =
    let fun ppDV(VALvar {access,path, btvs, typ,prim}) =
	     (openHVBox 0;
	      pps "VALvar";
	      openHVBox 3;
	      pps "({access="; fmtAccess access; ppcomma_nl ppstrm;
              pps "prim="; ppInfo prim; ppcomma_nl ppstrm;
	      pps "path="; pps (SymPath.toString path); ppcomma_nl ppstrm;
	      pps "typ=ref "; ppType env (!typ);
	      pps "})";
	      closeBox(); closeBox())
	  | ppDV (OVLDvar {name,variants}) =
	     (openHVBox 0;
	      pps "OVLDvar";
	      openHVBox 3;
	      pps "({name="; PPU.fmtSymbol (name); ppcomma_nl ppstrm;
	      ppcomma_nl ppstrm; pps "})";
	      closeBox();
	      closeBox())
	  | ppDV (ERRORvar) = pps "<ERRORvar>"
     in ppDV
    end

fun fmtVariable (env : StaticEnv.staticEnv, variable) =
    (case variable
       of VALvar{btvs,path,access,typ,prim} =>
	    PP.pblock
	      [PP.text (SymPath.toString path),
	       if !internals then fmtAccess access else PP.empty,
	       PP.colon, fmtType env (!typ)]
	| OVLDvar {name,variants} =>
	      (openHVBox 0;
	       PPU.fmtSymbol (name);
	       closeBox())
	  | fmt (_, ERRORvar) = pps "<ERRORvar>"
     in ppV
    end

end (* local *)
end (* structure PPVal *)
