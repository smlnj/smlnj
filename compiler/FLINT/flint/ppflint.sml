(* FLINT/flint/ppflint-new.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Pretty Printer (formatter) for Flint IL, using NewPP prettyprint library.
 *)

structure PPFlint :> PPFLINT =
struct

local
  (** frequently used structures *)
  structure S = Symbol
  structure LV = LambdaVar
  structure LT = Lty
  structure LK = LtyKernel
  structure LD = LtyDef
  structure LB = LtyBasic
  structure LE = LtyExtern
  structure FR = FunRecMeta
  structure PL = PLambda
  structure F = FLINT
  structure FU = FlintUtil
  structure PO = Primop
  structure PU = PrintUtil
  structure PP = NewPP
  structure PPT = PPLty

  open NewPP

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

    fun fmtFKind fkind = text (fkindToString fkind)

    (* csf = "comma separated formats *)
    val csf = seqFormats {alignment=P, sep=comma}

    (* fmtRKind : FR.rkind -> format *)
    (* format record kinds (FR.rkind) *)
    fun fmtRKind rkind =
        (case rkind
	  of (FR.RK_VECTOR tyc) => 
	     ccat (text "VECTOR", brackets (PPT.fmtTyc 100 tyc))
	  | FR.RK_STRUCT => text "STRUCT"
	  | FR.RK_TUPLE => text "RECORD")

    (** con: used to specify all possible switching statements. *)
    fun conToString (PL.DATAcon((symbol,_,_),_,_))   = S.name symbol
      | conToString (PL.INTcon{ival, ty}) =
	  String.concat ["(I", Int.toString ty, ")", IntInf.toString ival]
      | conToString (PL.WORDcon{ival, ty}) =
	  String.concat ["(W", Int.toString ty, ")", IntInf.toString ival]
      | conToString (PL.STRINGcon s) = PrintUtil.formatString s

    (* fmtCon : PL.con -> format *)
    fun fmtCon con = text (conToString con)

    (** simple values, including variables and static constants. *)
    fun valueToString (F.VAR v) = LV.lvarName v
      | valueToString (F.INT{ival, ty}) =
	  String.concat ["(I", Int.toString ty, ")", IntInf.toString ival]
      | valueToString (F.WORD{ival, ty}) =
	  String.concat ["(W", Int.toString ty, ")", IntInf.toString ival]
      | valueToString (F.REAL{rval, ty}) =
	  String.concat ["(R", Int.toString ty, ")", RealLit.toString rval]
      | valueToString (F.STRING s) = PrintUtil.formatString s

    fun fmtValue value = text (valueToString value)

    fun fmtLvar lvar = text (LV.lvarName lvar)

    fun fmtTvTk (tv:LT.tvar, tk) =
	PP.concat [text (LV.lvarName tv), text ":", PPT.fmtTKind 100 tk]

    val fmtValList = formatList fmtValue
    val fmtLvarList = formatList fmtLvar
    val fmtTvTkList = formatList fmtTvTk

    (* fmtTagged : string * PP.format -> PP.format *)
    fun fmtTagged (tag: string, fmt: format) =
	ccat (text tag, parens fmt)

    (* fmtDecon : PL.con -> format *)
    fun fmtDecon (PL.DATAcon((_,Access.CONSTANT _,_),_,_)) = empty
        (* WARNING: a hack, but then what about constant exceptions ? *)
      | fmtDecon (PL.DATAcon((symbol,conrep,lty),tycs,lvar)) =
	(* <lvar> = DECON(<symbol>,<conrep>,<lty>,[<tycs>]) *)
	pblock
	 [fmtLvar lvar, text "=",
	  enclose {front = text "DECON(", back = rparen}
	   (pblock
	     [text (S.name symbol), comma,
	      text (Access.prRep conrep), comma,
	      PPT.fmtLty 100 lty, comma,
	      PP.formatList (PPT.fmtTyc 100) tycs])]
      | fmtDecon _ = empty

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
		hcat (text "RETURN", fmtValList values)

	      | fmtE (F.APP (f, args)) =
		(* APP(f, [args]) *)
		fmtTagged ("APP", 
			   pblock [ccat (fmtValue f, comma), fmtValList args])

	      | fmtE (F.TAPP (tf, tycs)) =
		(* TAPP(tf, [tycs]) *)
		fmtTagged ("TAPP", 
			   pblock [fmtValue tf, comma, PP.formatList fmtTyc' tycs])

	      | fmtE (F.LET (vars, def, body)) =
		(* [vars] = lexp   OR   [vars] =
		 *   body                   lexp
		 *                        body
		 *)
		let val header = hblock [text "LET", fmtLvarList vars, text "="]
		    val defFmt = fmtLexp' def
		    val bodyFmt = fmtLexp' body
		 in vcat
		      (pcat (header,
			     if complex def
			     then hardIndent (4, defFmt)
			     else softIndent (4, defFmt)),
		       hardIndent (2, bodyFmt))
		end

	      | fmtE (F.FIX (fundecs, body)) =
		(* FIX <fundec1>,
		 *     <fundec2>,
		 *     <fundec3>
		 *  IN <body>
		 *)
		vcat
		  (hcat (text "FIX",
			 (vblock (map fmtFundec' fundecs))),
		   hcat (text " IN", fmtLexp' body))

	      | fmtE (F.TFN ((tfk as {inline}, lvar, tv_tk_list, tfnbody), body)) =
		(* v =
		 *   TFN([tk],lty,
		 *     <tfnbody>)
		 * <body>
		 *)
		let val header = hblock [text "LET!", fmtLvar lvar, text "="]
		    val inlineFmt = text (inlineToString inline)
		    val tfnFmt = pblock [ccat (text "TFN", inlineFmt),
					 fmtTvTkList tv_tk_list, text ".",
					 softIndent (2, fmtLexp' tfnbody)]
		 in vblock
		      [header,
		       hardIndent (4, tfnFmt),
		       hardIndent (2, fmtLexp' body)]
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
			vblock
			  [hcat (fmtCon con, text "=>"),
			   vcat (fmtDecon con, fmtLexp' lexp)]

		in vblock
		    ([hcat (text "SWITCH", fmtValue value),
		      hardIndent (2, vblock (map fmtCase cases))]
		     @ (case  lexpOp
			  of NONE => nil
			   | SOME default => (* default case *)
			      [pblock
				[text "_ =>",
				 softIndent (4, fmtLexp' default)]]))
		end

	      | fmtE (F.CON ((symbol,_,_), tycs, value, lvar, body)) =
		 (* <lvar> = CON(<symbol>, <tycs>, <value>)
		  * <body>
		  *)
		 vblock
		   [hblock [fmtLvar lvar, text "=",
			    fmtTagged ("CON",
				       csf [text (S.name symbol), 
					    formatTuple fmtTyc' tycs,
					    fmtValue value])],
		    fmtLexp' body]

	      | fmtE (F.RECORD (rkind, values, lvar, body)) =
		 (* <lvar> = RECORD(<rkind>, <values>)
		  * <body>
		  *)
		vblock
		  [hblock [fmtLvar lvar, text "=",
			   fmtTagged ("RECORD",
				      (csf [fmtRKind rkind,
					    formatList fmtValue values]))],
		   fmtLexp' body]

	      | fmtE (F.SELECT (value, i, lvar, body)) =
		 (* <lvar> = SELECT(<value>, <int>)
		  * <body>
		  *)
		vblock
		  [hblock [fmtLvar lvar, text "=",
			   fmtTagged ("SELECT",
				      (hcat (ccat (fmtValue value, comma), integer i)))],
		   fmtLexp' body]

	      | fmtE (F.RAISE (value, ltys)) =
		 (* RAISE(<value> : <ltys>) *)
		 hblock [fmtTagged ("RAISE", fmtValue value),
			 colon,
			 PP.formatTuple fmtLty' ltys]

	      | fmtE (F.HANDLE (body, value)) =
		 (* <body>
		  * HANDLE(<value>)
		  *)
		vcat
		  (fmtLexp' body,
		   fmtTagged ("HANDLE", fmtValue value))

	      | fmtE (F.BRANCH ((d, primop, lty, tycs), values, body1, body2)) =
		 (* IF PRIMOP/GENOP (<primop>, <lty>, [<tycs>]) [<values>]
		  * THEN
		  *   <body1>
		  * ELSE
		  *   <body2>
		  *)
		let val tag = case d of NONE => "PRIMOP" | _ => "GENOP"
		 in vblock
		     [hblock [text "IF",
			      fmtTagged (tag,
					 seqFormats {alignment=P, sep=comma}
					   [text (PrimopUtil.toString primop),
					    fmtLty' lty,
					    PP.formatList fmtTyc' tycs]),
			      formatList fmtValue values],
		      hcat (text "THEN", softIndent (2, fmtLexp' body1)),
		      hcat (text "ELSE", softIndent (2, fmtLexp' body2))]
		end

	      | fmtE (F.PRIMOP (p as (_, PO.MKETAG, _, _), [value], lvar, body)) =
		 (* <lvar> = ETAG(<value>[<tyc>])
		  * <body>
		  *)
		vblock
		  [hblock [fmtLvar lvar, text "=",
			   fmtTagged ("ETAG",
				      (hcat (fmtValue value,
					     brackets (fmtTyc' (FU.getEtagTyc p)))))],
		   fmtLexp' body]

	      | fmtE (F.PRIMOP (p as (_, PO.WRAP, _, _), [value], lvar, body)) =
		 (* <lvar> = WRAP(<tyc>, <value>)
		  * <body>
		  *)
		vblock
		  [hblock [fmtLvar lvar, text "=",
			   fmtTagged ("WRAP",
				      csf [fmtTyc' (FU.getWrapTyc p),
					   fmtValue value])],
		   fmtLexp' body]

	      | fmtE (F.PRIMOP (p as (_, PO.UNWRAP, _, []), [value], lvar, body)) =
		 (* <lvar> = UNWRAP(<tyc>, <value>)
		  * <body>
		  *)
		vblock
		  [hblock [fmtLvar lvar, text "=",
			   fmtTagged ("UNWRAP",
				      csf [fmtTyc' (FU.getUnWrapTyc p),
					   fmtValue value])],
		   fmtLexp' body]

	      | fmtE (F.PRIMOP ((d, primop, lty, tycs), values, lvar, body)) =
		 (* <lvar> = PRIM(<primop>, <lty>, [<tycs>]) [<values>]
		  * <body>
		  *)
		let val tag = (case d of NONE => "PRIMOP" | _ => "GENOP" )
		 in vblock
		      [hblock [fmtLvar lvar, text "=",
			       fmtTagged (tag,
					  csf [text (PrimopUtil.toString primop),
					       fmtLty' lty,
					       PP.formatList (PPT.fmtTyc (pd - 1)) tycs]),
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
		hcat (ccat (fmtLvar lvar, colon),
		      if !Control.FLINT.printFctTypes orelse cconv <> FR.CC_FCT
		      then PPT.fmtLty (pd - 1) lty
		      else text "<lty>")
         in vcat
	     (hblock [ccat (fmtLvar lvar, colon),
	              parens (fmtFKind fkind),
		      text "="],
	     (*** the result lty no longer available ---- fmtLty lty; **)
	      fmtTagged ("FN",
			 (hcat (brackets (vblock (map fmtarg lvar_lty_list)),
				hardIndent (4, fmtLexp (pd - 1) body)))))
	end

    (* ppLexp : lexp -> unit *)
    fun ppLexp (lexp: F.lexp) =
	render (fmtLexp (!Control.Print.printDepth) lexp,
		Control.Print.say, !Control.Print.lineWidth)

    (* ppLexpLimited : int -> lexp -> unit *)
    fun ppLexpLimited (printDepth: int) (lexp: F.lexp) =
	render (fmtLexp printDepth lexp,
		Control.Print.say, !Control.Print.lineWidth)

    (* ppProg : prog -> unit *)
    fun ppProg (prog: F.prog) =
	render (fmtFundec (!Control.Print.printDepth) prog,
		Control.Print.say, !Control.Print.lineWidth)

    (* ppProgLimited : int -> prog -> unit *)
    fun ppProgLimited (printDepth: int) (prog: F.prog) =
	render (fmtFundec printDepth prog, Control.Print.say, !Control.Print.lineWidth)

end (* top local *)
end (* structure PPFlint *)
