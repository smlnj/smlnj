(* ppval.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(* modified to use SML/NJ Lib PP. [dbm, 7/30/03]) *)

signature PPVAL =
sig
  val fmtAccess :  Access.access -> PP.format
  val fmtRep :     Access.conrep -> PP.format
  val fmtDatacon : Types.datacon -> PP.format
  val fmtDataconTyped : StaticEnv.staticEnv * Types.datacon -> PP.format
  val fmtDataconDebug : StaticEnv.staticEnv * Types.datacon -> PP.format
  val fmtVar :     Variable.var -> PP.format
  val fmtVarTyped :     StaticEnv.staticEnv * Variable.var -> PP.format
  val fmtVarDebug :     StaticEnv.staticEnv * Variable.var -> PP.format
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
  structure SE = StaticEnv
in

val internals = ElabDataControl.varconInternals

fun fmtSymPath path = PP.text (SymPath.toString path)

fun fmtAccess a = PP.brackets (PP.text (A.accessToString a))

fun fmtRep rep = PP.text (A.repToString rep)

fun fmtConsig csig = PP.text (A.consigToString csig)

fun fmtField (field : string, value : PP.format) =
    PP.hblock [PP.text field, PP.text "=", value]

fun fmtPrim prim = PP.text "<prim>"

fun fmtDatacon (T.DATACON {name, rep, ...} : T.datacon) =
    PP.ccat (PPU.fmtSymbol name,
	     case rep
	       of A.EXN acc => if !internals then fmtAccess acc else PP.empty)
		| _ => PP.empty)

fun fmtDataconTyped (env: SE.staticEnv, DATACON{name,typ,...}) =
    PP.pblock [PPU.fmtSymbol name, PP.colon, fmtType env typ]

fun fmtDataconDebug (env: SE.staticEnv, T.DATACON {name, rep, const, typ, sign, lazyp}) =
    PP.hcat
      (PP.text "DATACON",
	(PP.braces
	   (PP.vblock
	      [fmtField ("name", PPU.fmtSymbol name),
	       fmtField ("const", PP.bool const),
	       fmtField ("typ", PPT.fmtType env typ),
	       fmtField ("lazyp", PP.bool lazyp),
	       fmtField ("conrep", fmtConrep rep),
	       fmtField ("sign", PP.brackets (fmtConsig sign))])))

(* BUG: not exported, not used *)
fun fmtConBinding (env : SE.staticEnv, dcon: T.datacon) =
    (case dcon
       of T.DATACON{name, typ, rep=A.EXN _, ...} =>  (* exception constructor case *)
	    PP.pblock
	     [PP.text "exception",
	      PPU.fmtSymbol name,
	      if BasicTypes.isArrowType typ then
              then PP.hcat (PP.text "of", PPT.fmtType env (BasicTypes.domain typ))
              else PP.empty]
	| con =>      (* ordinary datacon case *)
	    let exception Hidden
		val visible =
		      let val tyc = TU.dataconTyc con
		       in TypesUtil.equalTycon
			    (LU.lookTyc
			       (env, SymPath.SPATH
				       [InvPath.last(valOf(TypesUtil.tycPath tyc))],
				fn _ => raise Hidden),
			     tyc)
			  handle Hidden => false
		      end
	     in if !internals orelse not visible
	        then PP.hcat (PP.text "con ", fmtDatacon (env,con))
	        else PP.empty
	    end)

fun fmtVar (VALvar {access,path,...}) =
       PP.ccat (fmtSymPath path,
		if !internals
		then (case access
		       of A.LVAR lvar => PP.ccat (period, PP.text (LV.toString lvar))
			| _ => fmtAccess access)
		else PP.empty)
  | fmtVar (OVLDvar {name,...}) = PPU.fmtSymbol name
  | fmtVar (ERRORvar) = PP.text "<errorvar>"

fun fmtVarTyped (env : SE.staticEnv, variable : V.variable) =
    (case variable
       of VALvar {btvs, path, access, typ, prim} =>
	    PP.pblock
	      [fmtPath path,
	       if !internals then fmtAccess access else PP.empty,
	       PP.colon,
	       fmtType env (!typ)]
	| OVLDvar {name, ...} => PPU.fmtSymbol name
	| ERRORvar = PP.text "<ERRORvar>")

fun fmtVarDebug (env: SE.staticEnv, var: V.variable) =
    (case var
      of VALvar {access,path, btvs, typ,prim} =>
	 PP.hcat
	   (PP.text "VALvar",
	      (PP.braces
		 (PP.vblock
	            [fmtField ("access", fmtAccess access),
                     fmtField ("prim", fmtPrim prim),
		     fmtField ("path", fmtSymPath path),
		     fmtField ("typ", fmtType env (!typ))])))
       | OVLDvar {name,variants} =>
	 PP.hcat   
	   (PP.text "OVLDvar",
	    (PP.braces (fmtField ("name", PPU.fmtSymbol (name)))))
       | ERRORvar => PP.text "<ERRORvar>")

end (* local *)
end (* structure PPVal *)
