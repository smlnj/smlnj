(* ppval.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(* modified to use SML/NJ Lib PP. [dbm, 7/30/03]) *)

signature PPVAL =
sig
  val fmtAccess :       Access.access -> NewPP.format
  val fmtConrep :       Access.conrep -> NewPP.format
  val fmtDatacon :      Types.datacon -> NewPP.format
  val fmtDataconTyped : StaticEnv.staticEnv * Types.datacon -> NewPP.format
  val fmtDataconDebug : StaticEnv.staticEnv * Types.datacon -> NewPP.format
  val fmtConBinding :   StaticEnv.staticEnv * Types.datacon -> NewPP.format
  val fmtVar :          Variable.var -> NewPP.format
  val fmtVarTyped :     StaticEnv.staticEnv * Variable.var -> NewPP.format
  val fmtVarDebug :     StaticEnv.staticEnv * Variable.var -> NewPP.format
end (* signature PPVAL *)

structure PPVal : PPVAL =
struct

local
  structure PP = NewPP
  structure PPU = NewPPUtil
  structure PPS = PPSymbols
  structure PPP = PPSymPaths
  structure PPT = PPType
  structure IP = InvPath
  structure TU = TypesUtil
  structure LU = Lookup
  structure A = Access
  structure LV = LambdaVar
  structure V = Variable
  structure T = Types
  structure SE = StaticEnv
in

val internals = ElabDataControl.varconInternals

exception PPVAL of string

fun fmtAccess a = PP.brackets (PP.text (A.accessToString a))

fun fmtConrep rep = PP.text (A.conrepToString rep)

fun fmtConsig csig = PP.text (A.consigToString csig)

fun fmtField (field : string, value : PP.format) =
    PP.hblock [PP.text field, PP.text "=", value]

fun fmtPrim prim = PP.text "<prim>"

fun fmtDatacon (T.DATACON {name, rep, ...} : T.datacon) =
    PP.ccat (PPS.fmtSym name,
	     case rep
	       of A.EXN acc => if !internals then fmtAccess acc else PP.empty
		| _ => PP.empty)

fun fmtDataconTyped (env: SE.staticEnv, T.DATACON{name, typ, ...}) =
    PP.pblock [PPS.fmtSym name, PP.colon, PPT.fmtType env typ]

fun fmtDataconDebug (env: SE.staticEnv, T.DATACON {name, rep, const, typ, sign, lazyp}) =
    PP.hcat
      (PP.text "DATACON",
	(PP.braces
	   (PP.vblock
	      [fmtField ("name", PPS.fmtSym name),
	       fmtField ("const", PP.bool const),
	       fmtField ("typ", PPT.fmtType env typ),
	       fmtField ("lazyp", PP.bool lazyp),
	       fmtField ("conrep", fmtConrep rep),
	       fmtField ("sign", PP.brackets (fmtConsig sign))])))

(* fmtConBinding : SE.staticEnv * T.datacon -> PP.format
 * used in PPModule *)
fun fmtConBinding (env : SE.staticEnv, dcon: T.datacon) =
    (case dcon
       of T.DATACON{name, typ, rep=A.EXN _, ...} =>  (* exception constructor case *)
	    PP.pblock
	     [PP.text "exception",
	      PPS.fmtSym name,
	      if BasicTypes.isArrowType typ
              then PP.hcat (PP.text "of", PPT.fmtType env (BasicTypes.domain typ))
              else PP.empty]
	| con =>      (* ordinary datacon case *)
	    let exception Hidden
		val visible =
		      let val tyc = TU.dataconTyc con
		       in (case TU.tycPath tyc
		             of NONE => raise (PPVAL "fmtConBinding: is hidden?")
			      | SOME (IP.IPATH syms) =>
				  (case syms
				     of nil => raise (PPVAL "fmtConBinding: null rpath")
				      | tycName :: _ => 
					  (TypesUtil.equalTycon
					     (LU.lookTyc (env, SymPath.SPATH [tycName],
							  fn _ => raise Hidden),
					      tyc))))
			  handle Hidden => false
		      end
	     in if visible orelse !internals
	        then PP.hcat (PP.text "con", fmtDatacon con)
	        else PP.empty
	    end)

fun fmtVar (V.VALvar {access,path,...}) =
       PP.ccat (PPP.fmtSymPath path,
		if !internals
		then (case access
		       of A.LVAR lvar => PP.ccat (PP.period, PP.text (LV.toString lvar))
			| _ => fmtAccess access)
		else PP.empty)
  | fmtVar (V.OVLDvar {name,...}) = PPS.fmtSym name
  | fmtVar (V.ERRORvar) = PP.text "<errorvar>"

fun fmtVarTyped (env : SE.staticEnv, variable : V.var) =
    (case variable
       of V.VALvar {btvs, path, access, typ, prim} =>
	    PP.pblock
	      [PPP.fmtSymPath path,
	       if !internals then fmtAccess access else PP.empty,
	       PP.colon,
	       PPT.fmtType env (!typ)]
	| V.OVLDvar {name, ...} => PPS.fmtSym name
	| V.ERRORvar => PP.text "<ERRORvar>")

fun fmtVarDebug (env: SE.staticEnv, var: V.var) =
    (case var
      of V.VALvar {access,path, btvs, typ,prim} =>
	 PP.hcat
	   (PP.text "VALvar",
	      (PP.braces
		 (PP.vblock
	            [fmtField ("access", fmtAccess access),
                     fmtField ("prim", fmtPrim prim),
		     fmtField ("path", PPP.fmtSymPath path),
		     fmtField ("typ", PPT.fmtType env (!typ))])))
       | V.OVLDvar {name,variants} =>
	 PP.hcat   
	   (PP.text "OVLDvar",
	    (PP.braces (fmtField ("name", PPS.fmtSym (name)))))
       | V.ERRORvar => PP.text "<ERRORvar>")

end (* local *)
end (* structure PPVal *)
