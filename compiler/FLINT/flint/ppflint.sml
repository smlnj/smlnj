(* FLINT/flint/ppflint-new.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Pretty Printer (formatter) for Flint IL, using new PrettyPrint library.
 *)

structure PPFlint :> PPFLINT =
struct

local

  structure S = Symbol
  structure LV = LambdaVar

  structure PL = PLambda
  structure F = FLINT
  structure FU = FlintUtil
  structure LT = Lty
  structure LD = LtyDef
  structure FR = FunRecMeta
  structure PO = Primop

  structure PP = Formatting
  structure PF = PrintFormat
  structure SF = StringFormats
  structure PPT = PPLty

in

    (* inlineToString : FR.inline -> string *)
    fun inlineToString FR.IH_SAFE = "s"
      | inlineToString FR.IH_ALWAYS = "a"
      | inlineToString FR.IH_UNROLL = "u"
      | inlineToString (FR.IH_MAYBE _) = "m"

    (* fflagToString : F.fflag -> string *)
    fun fflagToString ff =
        let fun h b = if b then "r" else "c"
         in LD.ffw_var (ff, fn (b1,b2) => (h b1)^(h b2), fn _ => "f")
        end

    (* fkindToString : FR.fkind -> string *)
    fun fkindToString ({isrec,cconv,inline,...}: FR.fkind) =
	(case inline
	   of FR.IH_ALWAYS => "(i)"
	    | FR.IH_UNROLL => "(u)"
	    | FR.IH_MAYBE(s,ws) => "(i:" ^ Int.toString s ^ ")"
	    | FR.IH_SAFE => "")  ^
	(case isrec
	   of SOME(_,FR.LK_UNKNOWN) => "R"
	    | SOME(_,FR.LK_LOOP) => "LR"
	    | SOME(_,FR.LK_TAIL) => "TR"
	    | NONE => "")  ^
	(case cconv
	   of FR.CC_FCT => "FCT"
	    | FR.CC_FUN fixed => "FUN " ^ fflagToString fixed)

    fun fmtFKind fkind = PP.text (fkindToString fkind)

    (* csf = "comma separated formats" (packed alignmnet) *)
    val csf = PP.psequence PP.comma

    (* fmtRKind : FR.rkind -> format *)
    (* format record kinds (FR.rkind) *)
    fun fmtRKind rkind =
        (case rkind
	  of (FR.RK_VECTOR tyc) => 
	     PP.cblock [PP.text "VECTOR", PP.brackets (PPT.fmtTyc 100 tyc)]
	  | FR.RK_STRUCT => PP.text "STRUCT"
	  | FR.RK_TUPLE => PP.text "RECORD")

    (** con: used to specify all possible switching statements. *)
    fun conToString (PL.DATAcon((symbol,_,_),_,_))   = S.name symbol
      | conToString (PL.INTcon{ival, ty}) =
	  String.concat ["(I", Int.toString ty, ")", IntInf.toString ival]
      | conToString (PL.WORDcon{ival, ty}) =
	  String.concat ["(W", Int.toString ty, ")", IntInf.toString ival]
      | conToString (PL.STRINGcon s) = SF.formatString s

    (* fmtCon : PL.con -> format *)
    fun fmtCon con = PP.text (conToString con)

    (** simple values, including variables and static constants. *)
    fun valueToString (F.VAR v) = LV.lvarName v
      | valueToString (F.INT{ival, ty}) =
	  String.concat ["(I", Int.toString ty, ")", IntInf.toString ival]
      | valueToString (F.WORD{ival, ty}) =
	  String.concat ["(W", Int.toString ty, ")", IntInf.toString ival]
      | valueToString (F.REAL{rval, ty}) =
	  String.concat ["(R", Int.toString ty, ")", RealLit.toString rval]
      | valueToString (F.STRING s) = SF.formatString s

    fun fmtValue value = PP.text (valueToString value)

    fun fmtLvar lvar = PP.text (LV.lvarName lvar)

    fun fmtTvTk (tv:LT.tvar, tk) =
	PP.cblock [PP.text (LV.lvarName tv), PP.text ":", PPT.fmtTKind 100 tk]

    fun fmtValList xs = PP.list (map fmtValue xs)
    fun fmtLvarList xs = PP.list (map fmtLvar xs)
    fun fmtTvTkList xs = PP.list (map fmtTvTk xs)

    (* fmtTagged : string * PP.format -> PP.format *)
    fun fmtTagged (tag: string, fmt: PP.format) =
	PP.cblock [PP.text tag, PP.parens fmt]
    (* fmtDecon : PL.con -> format *)
    fun fmtDecon (PL.DATAcon((_,Access.CONSTANT _,_),_,_)) = PP.empty
        (* WARNING: a hack, but then what about constant exceptions ? *)
      | fmtDecon (PL.DATAcon((symbol,conrep,lty),tycs,lvar)) =
	(* <lvar> = DECON(<symbol>,<conrep>,<lty>,[<tycs>]) *)
	PP.pblock
	 [fmtLvar lvar,
	  PP.text "=",
	  PP.cblock
	    [PP.text "DECON",
	     PP.parens
	       (PP.pblock
		  [PP.text (S.name symbol), PP.comma,
		   PP.text (Access.conrepToString conrep), PP.comma,
		   PPT.fmtLty 100 lty, PP.comma,
		   PP.list (map (PPT.fmtTyc 100) tycs)])]]
      | fmtDecon _ = PP.empty

    (** the definitions of the lambda expressions *)

    fun complex (F.LET _ | F.FIX _ | F.TFN _ | F.SWITCH _ | F.CON _ | F.HANDLE _) = true
      | complex _ = false

    (* fmtLexp : int -> F.lexp -> format *)
    fun fmtLexp (pd: int) (lexp: F.lexp) =
	let val pd' = pd - 1
	    val fmtLexp' = fmtLexp pd'
	    val fmtTyc' = PPLty.fmtTyc pd'
	    val fmtLty' = PPLty.fmtLty pd'
            val fmtFundec' = fmtFundec pd'

	    (* fmtE : F.lexp * int -> format *)
	    fun fmtE (F.RET values) =
		(* RETURN [values] *)
		PP.hblock [PP.text "RETURN", fmtValList values]

	      | fmtE (F.APP (f, args)) =
		(* APP(f, [args]) *)
		fmtTagged ("APP", PP.pblock [PP.cblock [fmtValue f, PP.comma], fmtValList args])

	      | fmtE (F.TAPP (tf, tycs)) =
		(* TAPP(tf, [tycs]) *)
		fmtTagged ("TAPP", 
			   PP.pblock [fmtValue tf, PP.comma, PP.list (map fmtTyc' tycs)])

	      | fmtE (F.LET (vars, def, body)) =
		(* [vars] = lexp   OR   [vars] =
		 *   body                   lexp
		 *                        body
		 *)
		let val header = PP.hblock [PP.text "LET", fmtLvarList vars, PP.text "="]
		    val defFmt = fmtLexp' def
		    val bodyFmt = fmtLexp' body
		 in PP.vblock [PP.pblock [header, PP.indent 4 defFmt], PP.indent 2 bodyFmt]
		end

	      | fmtE (F.FIX (fundecs, body)) =
		(* FIX <fundec1>,
		 *     <fundec2>,
		 *     <fundec3>
		 *  IN <body>
		 *)
		PP.vblock
		  [PP.hblock [PP.text "FIX", PP.vblock (map fmtFundec' fundecs)],
		   PP.hblock [PP.text " IN", fmtLexp' body]]

	      | fmtE (F.TFN ((tfk as {inline}, lvar, tv_tk_list, tfnbody), body)) =
		(* v =
		 *   TFN([tk],lty,
		 *     <tfnbody>)
		 * <body>
		 *)
		let val header = PP.hblock [PP.text "LET!", fmtLvar lvar, PP.text "="]
		    val inlineFmt = PP.text (inlineToString inline)
		    val tfnFmt = PP.pblock [PP.cblock [PP.text "TFN", inlineFmt],
					 fmtTvTkList tv_tk_list, PP.text ".",
					 PP.indent 2 (fmtLexp' tfnbody)]
		 in PP.vblock
		      [header,
		       PP.indent 4 tfnFmt,
		       PP.indent 2 (fmtLexp' body)]
		end

	      (** NOTE: ignoring consig when formatting SWITCH **)
	      | fmtE (F.SWITCH (value, consig, cases, lexpOp)) =
		(* SWITCH <value>
		 *   <con> =>
		 *       <lexp>
		 *   <con> =>
		 *       <lexp>
		 *)
		let fun fmtCase (con, lexp) =
			PP.vblock
			  [PP.hblock [fmtCon con, PP.text "=>"],
			   PP.vblock [fmtDecon con, fmtLexp' lexp]]

		in PP.vblock
		    ([PP.hblock [PP.text "SWITCH", fmtValue value],
		      PP.indent 2 (PP.vblock (map fmtCase cases))]
		     @ (case  lexpOp
			  of NONE => nil
			   | SOME default => (* default case *)
			      [PP.pblock
				[PP.text "_ =>",
				 PP.indent 4 (fmtLexp' default)]]))
		end

	      | fmtE (F.CON ((symbol,_,_), tycs, value, lvar, body)) =
		 (* <lvar> = CON(<symbol>, <tycs>, <value>)
		  * <body>
		  *)
		 PP.vblock
		   [PP.hblock [fmtLvar lvar, PP.text "=",
			    fmtTagged ("CON",
				       csf [PP.text (S.name symbol), 
					    PP.tuple (map fmtTyc' tycs),
					    fmtValue value])],
		    fmtLexp' body]

	      | fmtE (F.RECORD (rkind, values, lvar, body)) =
		 (* <lvar> = RECORD(<rkind>, <values>)
		  * <body>
		  *)
		PP.vblock
		  [PP.hblock [fmtLvar lvar, PP.text "=",
			   fmtTagged ("RECORD",
				      (csf [fmtRKind rkind,
					    PP.list (map fmtValue values)]))],
		   fmtLexp' body]

	      | fmtE (F.SELECT (value, i, lvar, body)) =
		 (* <lvar> = SELECT(<value>, <int>)
		  * <body>
		  *)
		PP.vblock
		  [PP.hblock
		     [fmtLvar lvar, PP.text "=",
		      fmtTagged ("SELECT",
				 PP.hblock [PP.cblock [fmtValue value, PP.comma], PP.integer i])],
		   fmtLexp' body]

	      | fmtE (F.RAISE (value, ltys)) =
		(* RAISE(<value> : <ltys>) *)
		PP.hblock [fmtTagged ("RAISE", fmtValue value), PP.colon, PP.tuple (map fmtLty' ltys)]

	      | fmtE (F.HANDLE (body, value)) =
		 (* <body>
		  * HANDLE(<value>)
		  *)
		PP.vblock [fmtLexp' body, fmtTagged ("HANDLE", fmtValue value)]

	      | fmtE (F.BRANCH ((d, primop, lty, tycs), values, body1, body2)) =
		 (* IF PRIMOP/GENOP (<primop>, <lty>, [<tycs>]) [<values>]
		  * THEN
		  *   <body1>
		  * ELSE
		  *   <body2>
		  *)
		let val tag = case d of NONE => "PRIMOP" | _ => "GENOP"
		 in PP.vblock
		      [PP.hblock
			 [PP.text "IF",
			  fmtTagged (tag,
				     PP.psequence PP.comma
				       [PP.text (PrimopUtil.toString primop),
					fmtLty' lty,
					PP.list (map fmtTyc' tycs)]),
			  PP.list (map fmtValue values)],
		       PP.pblock [PP.text "THEN", PP.indent 2 (fmtLexp' body1)],
		       PP.pblock [PP.text "ELSE", PP.indent 2 (fmtLexp' body2)]]
		end

	      | fmtE (F.PRIMOP (p as (_, PO.MKETAG, _, _), [value], lvar, body)) =
		 (* <lvar> = ETAG(<value>[<tyc>])
		  * <body>
		  *)
		PP.vblock
		  [PP.hblock [fmtLvar lvar, PP.text "=",
			 fmtTagged ("ETAG",
				    PP.hblock [fmtValue value,
					  PP.brackets (fmtTyc' (FU.getEtagTyc p))])],
		   fmtLexp' body]

	      | fmtE (F.PRIMOP (p as (_, PO.WRAP, _, _), [value], lvar, body)) =
		 (* <lvar> = WRAP(<tyc>, <value>)
		  * <body>
		  *)
		PP.vblock
		  [PP.hblock [fmtLvar lvar, PP.text "=",
			   fmtTagged ("WRAP",
				      csf [fmtTyc' (FU.getWrapTyc p),
					   fmtValue value])],
		   fmtLexp' body]

	      | fmtE (F.PRIMOP (p as (_, PO.UNWRAP, _, []), [value], lvar, body)) =
		 (* <lvar> = UNWRAP(<tyc>, <value>)
		  * <body>
		  *)
		PP.vblock
		  [PP.hblock [fmtLvar lvar, PP.text "=",
			   fmtTagged ("UNWRAP",
				      csf [fmtTyc' (FU.getUnWrapTyc p),
					   fmtValue value])],
		   fmtLexp' body]

	      | fmtE (F.PRIMOP ((d, primop, lty, tycs), values, lvar, body)) =
		 (* <lvar> = PRIM(<primop>, <lty>, [<tycs>]) [<values>]
		  * <body>
		  *)
		let val tag = (case d of NONE => "PRIMOP" | _ => "GENOP" )
		 in PP.vblock
		      [PP.hblock [fmtLvar lvar, PP.text "=",
			     fmtTagged (tag,
					csf [PP.text (PrimopUtil.toString primop),
					     fmtLty' lty,
					     PP.list (map (PPT.fmtTyc (pd - 1)) tycs)]),
			     fmtValList values],
		       fmtLexp' body]
		end
	 in fmtE lexp
	end (* end fmtLexp *)

    (* fmtFundec :  int -> F.fundec -> format *)
    and fmtFundec (pd: int) ((fkind as {cconv,...}, lvar, lvar_lty_list, body): F.fundec) =
	(*  <lvar> : (<fkind>) =
	 *    FN([v1 : lty1,
	 *        v2 : lty2],
	 *      <body>)
	 *)
	let fun fmtarg (lvar, lty) =
		PP.hblock [PP.cblock [fmtLvar lvar, PP.colon],
		      if !Control.FLINT.printFctTypes orelse cconv <> FR.CC_FCT
		      then PPT.fmtLty (pd - 1) lty
		      else PP.text "<lty>"]
         in PP.vblock
	      [PP.hblock [PP.cblock [fmtLvar lvar, PP.colon],
	               PP.parens (fmtFKind fkind),
		       PP.text "="],
	       (*** the result lty no longer available ---- fmtLty lty; ***)
	       fmtTagged ("FN",
			  PP.pblock [PP.brackets (PP.vblock (map fmtarg lvar_lty_list)),
			        PP.indent 4 (fmtLexp (pd - 1) body)])]
	end

    (* ppLexp : lexp -> unit *)
    fun ppLexp (lexp: F.lexp) =
	PF.render (fmtLexp (!Control.Print.printDepth) lexp, !Control.Print.lineWidth)

    (* ppLexpLimited : int -> lexp -> unit *)
    fun ppLexpLimited (printDepth: int) (lexp: F.lexp) =
	PF.render (fmtLexp printDepth lexp, !Control.Print.lineWidth)

    (* ppProg : prog -> unit *)
    fun ppProg (prog: F.prog) =
	PF.render (fmtFundec (!Control.Print.printDepth) prog, !Control.Print.lineWidth)

    (* ppProgLimited : int -> prog -> unit *)
    fun ppProgLimited (printDepth: int) (prog: F.prog) =
	PF.render (fmtFundec printDepth prog, !Control.Print.lineWidth)

end (* top local *)
end (* structure PPFlint *)
