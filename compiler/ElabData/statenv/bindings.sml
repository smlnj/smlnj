(* bindings.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Bindings : BINDINGS =
  struct

    structure S  = Symbol
    structure T  = Types
    structure V  = Variable
    structure M =  Modules

    datatype binding
      = VALbind of V.var
      | CONbind of T.datacon
      | TYCbind of T.tycon
      | SIGbind of M.Signature
      | STRbind of M.Structure
      | FSGbind of M.fctSig
      | FCTbind of M.Functor
      | FIXbind of Fixity.fixity

  (* used for statenv sorting in env/statenv.sml *)
    fun binderGt ((s1, rb1), (s2, rb2)) = let
	(* hopefully the following gets optimized into an identity function
	 * on tags... *)
	  fun bnum (VALbind _) = 0
	    | bnum (CONbind _) = 1
	    | bnum (TYCbind _) = 2
	    | bnum (SIGbind _) = 3
	    | bnum (STRbind _) = 4
	    | bnum (FSGbind _) = 5
	    | bnum (FCTbind _) = 6
	    | bnum (FIXbind _) = 7
	  in
	    case Int.compare (bnum rb1, bnum rb2)
	     of EQUAL => S.symbolGt (s1, s2)
	      | GREATER => true
	      | LESS => false
	  end

    (* bindingSymbol : binding -> Symbol.symbol
     * tries to determine the bound name associated with a binding from the
     * binding itself. This name is not always available. So here we are
     * returning "suggestive" pseudo-names, e.g. <ERRORvar>.
     * It would probably be better to returns a symbol option. *)
    fun bindingSymbol (VALbind v) =
	(case v
	  of V.VALvar{path,...} => SymPath.last path
	   | V.OVLDvar{name,...} => name
	   | ERRORvar => S.varSymbol "<ERRORvar")
      | bindingSymbol (CONbind(T.DATACON{name,...})) = name
      | bindingSymbol (TYCbind(tyc)) =
	(case tyc
	  of T.GENtyc{path,...} => InvPath.last path
	   | T.DEFtyc{path,...} => InvPath.last path
	   | T.PATHtyc{path,...} => InvPath.last path
	   | T.ERRORtyc => S.tycSymbol "<ERRORtyc>"
	   | _ => S.tycSymbol "anonTyc")
      | bindingSymbol (SIGbind sg) =
	(case sg
	  of M.SIG{name,...} =>
	     (case name
	       of SOME s => s
		| NONE => S.sigSymbol "<anonSig>")
	   | M.ERRORsig => S.sigSymbol "<ERRORsig>")
      | bindingSymbol (STRbind str) =
	(case str
	  of M.STR{rlzn={rpath,...},...} => InvPath.last rpath
	   | M.STRSIG _ => S.strSymbol "<STRSIG>"
	   | M.ERRORstr => S.strSymbol "<ERRORstr>")
      | bindingSymbol (FSGbind fsig) =
        (* the name bound is not recoverable from the binding *)
	(case fsig
	  of M.FSIG _ => S.fsigSymbol "<FSIG>"
	   | M.ERRORfsig => S.fsigSymbol "<ERRORfsig>")
      | bindingSymbol (FCTbind fct) =
	(case fct
	  of M.FCT{rlzn={rpath,...},...} => InvPath.last rpath
	   | M.ERRORfct => S.fctSymbol "<ERRORfct>")
      | bindingSymbol (FIXbind _) = S.fixSymbol "<FIXITY>"
        (* the name bound is not recoverable from the binding *)

  end (* structure Bindings *)
