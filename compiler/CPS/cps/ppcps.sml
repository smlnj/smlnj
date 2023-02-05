(* ppcps.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature PPCPS =
sig
    val valueToString : CPS.value -> string
    val vpathToString : CPS.value * CPS.accesspath -> string
    val rkToString : CPS.record_kind -> string

    (* string conversions for various CPS.P types *)
    val numkindToString : CPS.P.numkind -> string
    val arithopToString : CPS.P.arithop -> string
    val pureopToString : CPS.P.pureop -> string
    val cmpopToString : CPS.P.cmpop -> string
    val fcmpopToString : CPS.P.fcmpop -> string
    val branchToString : CPS.P.branch -> string
    val setterToString : CPS.P.setter -> string
    val lookerToString : CPS.P.looker -> string
    val arithToString : CPS.P.arith -> string
    val pureToString : CPS.P.pure -> string

    val ppFunction : CPS.function -> unit
    val ppCps : CPS.cexp -> unit

end (* signature PPCPS *)

structure PPCps : PPCPS =
  struct

    structure LV = LambdaVar
    structure U = CPSUtil
    structure LT = Lty
    structure PP = PrettyPrint

    open CPS

    val say = Control.Print.say

(* NOT USED!
    type lvarLtyTable = LT.lty LV.Tbl.hash_table

    (* printLvarLtyTable : lvarLtyTable -> unit *)
    fun printLvarLtyTable (table: lvarLtyTable) =
        let fun ppLvarLty (lvar, lty) =
		  PP.printFormatNL (PP.hblock [fmtLvar lvar, colon, PPLty.fmtLty 20 lty])
	 in (say "### lvar-lty table\n";
	    say "************************************************\n";
	    LV.Tbl.appi ppLvarLty m;
	    say "************************************************\n")
	end
*)

    fun valueToString (VAR v) = LV.lvarName v
      | valueToString (LABEL v) = "(L)" ^ LV.lvarName v
      | valueToString (NUM{ival, ty={sz=0, ...}}) = "(II)" ^ IntInf.toString ival
      | valueToString (NUM{ival, ty={sz, tag=true}}) =
	  concat["(I", Int.toString sz, "t)", IntInf.toString ival]
      | valueToString (NUM{ival, ty={sz, ...}}) =
	  concat["(I", Int.toString sz, ")", IntInf.toString ival]
      | valueToString (REAL{rval, ty}) =
	  concat["(R", Int.toString ty, ")", RealLit.toString rval]
      | valueToString (STRING s) = concat["\"", String.toString s, "\""]
      | valueToString (VOID) = "<<void>>"

    fun numkindToString (P.INT bits) = "i" ^ Int.toString bits
      | numkindToString (P.UINT bits) = "u" ^ Int.toString bits
      | numkindToString (P.FLOAT bits) = "f" ^ Int.toString bits

    val arithopToString = ArithOps.arithopToString

    val pureopToString = ArithOps.pureopToString

    val cmpopToString = ArithOps.cmpopToString

    fun fcmpopToString P.F_EQ   = "="
      | fcmpopToString P.F_ULG = "?<>"
      | fcmpopToString P.F_GT = ">"
      | fcmpopToString P.F_GE = ">="
      | fcmpopToString P.F_LT = "<"
      | fcmpopToString P.F_LE = "<="
      | fcmpopToString P.F_LG = "<>"
      | fcmpopToString P.F_LEG = "<="
      | fcmpopToString P.F_UGT = "?>"
      | fcmpopToString P.F_UGE = "?>="
      | fcmpopToString P.F_ULT = "?<"
      | fcmpopToString P.F_ULE = "?<="
      | fcmpopToString P.F_UE = "?="
      | fcmpopToString P.F_UN = "?"

    fun branchToString (P.CMP{oper, kind}) = numkindToString kind ^ cmpopToString oper
      | branchToString (P.FCMP{oper, size}) = numkindToString (P.FLOAT size) ^ fcmpopToString oper
      | branchToString (P.FSGN sz) = numkindToString (P.FLOAT sz) ^ "sgn"
      | branchToString P.BOXED = "boxed"
      | branchToString P.UNBOXED = "unboxed"
      | branchToString P.PEQL = "peql"
      | branchToString P.PNEQ = "pneq"
      | branchToString (P.STREQL s) = concat["streql(\"", String.toString s, "\")"]

    fun setterToString P.UNBOXEDUPDATE = "unboxedupdate"
      | setterToString P.UPDATE = "update"
      | setterToString (P.NUMUPDATE{kind}) = ("numupdate" ^ numkindToString kind)
      | setterToString P.UNBOXEDASSIGN = "unboxedassign"
      | setterToString P.ASSIGN = "assign"
      | setterToString P.SETHDLR = "sethdlr"
      | setterToString P.SETVAR = "setvar"
      | setterToString P.SETSPECIAL = "setspecial"
      | setterToString (P.RAWSTORE{kind}) = ("rawstore" ^ numkindToString kind)
      | setterToString (P.RAWUPDATE cty) = ("rawupdate" ^ U.ctyToString cty)

    fun lookerToString P.DEREF = "!"
      | lookerToString P.GETHDLR = "gethdlr"
      | lookerToString P.SUBSCRIPT = "subscript"
      | lookerToString (P.NUMSUBSCRIPT{kind}) = ("numsubscript" ^ numkindToString kind)
      | lookerToString P.GETVAR = "getvar"
      | lookerToString P.GETSPECIAL = "getspecial"
      | lookerToString (P.RAWLOAD{kind}) = ("rawload" ^ numkindToString kind)

    val cvtParam = Int.toString
    fun cvtParams (prefix, from, to) = concat [prefix, cvtParam from, "_", cvtParam to]

    fun arithToString (P.IARITH{oper, sz}) = arithopToString oper ^ cvtParam sz
      | arithToString (P.TEST{from, to}) = cvtParams ("test_", from, to)
      | arithToString (P.TESTU{from, to}) = cvtParams ("testu_", from, to)
      | arithToString (P.TEST_INF i) = "test_inf_" ^ cvtParam i
      | arithToString (P.REAL_TO_INT{floor, from, to}) =
	  concat[if floor then "floor_" else "round_", cvtParam from, "to", cvtParam to]

    fun pureToString P.LENGTH = "length"
      | pureToString (P.PURE_ARITH{oper,kind}) = pureopToString oper ^ numkindToString kind
      | pureToString P.OBJLENGTH = "objlength"
      | pureToString P.MAKEREF = "makeref"
      | pureToString (P.EXTEND{from, to}) = cvtParams ("extend_", from, to)
      | pureToString (P.COPY{from, to}) = cvtParams ("copy_", from, to)
      | pureToString (P.TRUNC{from, to}) = cvtParams ("trunc_", from, to)
      | pureToString (P.TRUNC_INF i) = "trunc_inf_" ^ cvtParam i
      | pureToString (P.COPY_INF i) = concat ["copy_", cvtParam i, "_inf"]
      | pureToString (P.EXTEND_INF i) =  concat ["extend_", cvtParam i, "_inf"]
      | pureToString (P.INT_TO_REAL{from, to}) =
	  concat ["real", cvtParam from, "_", cvtParam to]
      | pureToString P.SUBSCRIPTV = "subscriptv"
      | pureToString (P.PURE_NUMSUBSCRIPT{kind}) = "numsubscriptv" ^ numkindToString kind
      | pureToString P.GETTAG = "gettag"
      | pureToString P.MKSPECIAL = "mkspecial"
      | pureToString P.CAST = "cast"
      | pureToString P.GETCON = "getcon"
      | pureToString P.GETEXN = "getexn"
      | pureToString P.BOX = "box"
      | pureToString P.UNBOX = "unbox"
      | pureToString (P.WRAP kind) = "wrap_" ^ numkindToString kind
      | pureToString (P.UNWRAP kind) = "unwrap_" ^ numkindToString kind
      | pureToString P.GETSEQDATA = "getseqdata"
      | pureToString P.RECSUBSCRIPT = "recsubscript"
      | pureToString P.RAW64SUBSCRIPT = "raw64subscript"
      | pureToString P.NEWARRAY0 = "newarray0"
      | pureToString (P.RAWRECORD rk) = "rawrecord_"^getOpt(Option.map rkToString rk, "notag")

    (* rkToString : record_kind -> string *)
    and rkToString rk = (case rk
	   of RK_VECTOR => "RK_VECTOR"
	    | RK_RECORD => "RK_RECORD"
	    | RK_ESCAPE => "RK_ESCAPE"
	    | RK_CONT => "RK_CONT"
	    | RK_FCONT => "RK_FCONT"
	    | RK_KNOWN => "RK_KNOWN"
	    | RK_RAW64BLOCK => "RK_RAW64BLOCK"
	    | RK_RAWBLOCK => "RK_RAWBLOCK"
          (* end case *))

    fun funkindToString (fk: fun_kind) = (case fk
           of CONT => "std_cont"
            | KNOWN => "known"
            | KNOWN_REC => "known_rec"
            | KNOWN_CHECK => "known_check"
            | KNOWN_TAIL => "known_tail"
            | KNOWN_CONT => "known_cont"
            | ESCAPE => "std"
            | NO_INLINE_INTO => "no_inline"
          (* end case *))

    (* vpathToString : value * accesspath -> string *)
    fun vpathToString (v, p) = let
          fun toList (OFFp 0) = []
            | toList (OFFp n) = ["+", Int.toString n] (* assumes n > 0 *)
            | toList (SELp(i, p)) = "." :: toList p
          in
            String.concat (valueToString v :: toList p)
	  end

    (* fmtLvar : LV.lvar -> PP.format *)
    fun fmtLvar (lvar: LV.lvar) = PP.text (LV.lvarName lvar)

    (* fmtCty : cty -> PP.format *)
    fun fmtCty cty = PP.text (U.ctyToString cty)

    (* fmtTypedLvar : lvar * cty -> format *)
    fun fmtTypedLvar (lvar, cty) =
        PP.hcat (PP.ccat (fmtLvar lvar, PP.colon), fmtCty cty)

    (* fmtValue : value -> PP.format *)
    fun fmtValue (v: value) : PP.format = PP.text (valueToString v)

    (* fmtValues : value list -> PP.format *)
    fun fmtValues (vs: value list) =
	  PP.formatSeq {alignment=PP.H, sep=PP.comma, formatter=fmtValue} vs

    (* fmtRecord : record_kind * int -> PP.format *)
    fun fmtRecord (RK_RECORD, _) = PP.empty
      | fmtRecord (RK_VECTOR, _) = PP.empty
      | fmtRecord (k, n) = PP.hcat (PP.text (rkToString k), PP.integer n)

    (* fmtParams : lvar list * cty list -> P.format *)
    fun fmtParams (lvars, ctys) = let
	  fun fmtParam (lvar,cty) =
                PP.hcat (PP.ccat (fmtLvar lvar, PP.colon), fmtCty cty)
          in
            PP.tupleFormats (map fmtParam (ListPair.zipEq (lvars, ctys)))
	  end

    (* fmtVpath : value * accesspath -> PP.format *)
    fun fmtVpath (v, path) = PP.text (vpathToString(v, path))

    (* fmtCexp : cexp -> PP.format *)
    fun fmtCexp (RECORD(rk, vps, lvar, cexp')) = let
	  val front = PP.text (case rk of RK_VECTOR => "#{" | _ => "{")
          val back = PP.text "} -> "
          in
            PP.vcat (
	      PP.hcat (
                PP.enclose {front=front, back=back}
                (PP.hcat (fmtRecord (rk, length vps), PP.list fmtVpath vps)), fmtLvar lvar),
              fmtCexp cexp')
	  end
      | fmtCexp (SELECT(i,v,w,t,e)) =
	  PP.vcat (PP.hblock [PP.cblock [fmtValue v, PP.text ".", PP.integer i],
			      PP.text "->", fmtTypedLvar (w, t)],
		   fmtCexp e)
      | fmtCexp (OFFSET(i,v,w,e)) =
	  PP.vcat (PP.hblock [PP.cblock[fmtValue v, PP.text "+", PP.integer i],
			      PP.text "->", fmtLvar w],
		   fmtCexp e)
      | fmtCexp (APP(rator,argvals)) =
	  PP.hcat (fmtValue rator, PP.tupleFormats (map fmtValue argvals))

      | fmtCexp (FIX (functions, body)) = let
	  fun fmtFunction (_,flvar,arglvars,argctys,body) =
                PP.vcat
                  (PP.hblock [fmtLvar flvar,
                              fmtParams (arglvars, argctys),
                              PP.text "="],
                   PP.indent 3 (fmtCexp body))
	  in
            PP.vblock [
                PP.hcat (PP.text "FIX", PP.vblock (map fmtFunction functions)),
                PP.hcat (PP.text " IN", fmtCexp body)
              ]
	  end
      | fmtCexp (SWITCH (subject, lvar, cases)) = let
	  fun fmtCase (i,rhs) =
                PP.vcat (PP.hcat (PP.integer i, PP.text "=>"), PP.indent 3 rhs)
           fun folder (cexp, (i, fmts)) = (i+1, fmtCase (i, fmtCexp cexp) :: fmts)
           val caseFmts = rev (#2 (foldl folder (0, nil) cases))
	   in
            PP.vcat (
              PP.hcat (PP.text "case", PP.ccat (fmtValue subject, PP.brackets (fmtLvar lvar))),
              PP.hcat (PP.text "of", PP.vblock caseFmts))
           end
      | fmtCexp (LOOKER (i,vl,w,t,e)) =
	  (PP.vcat
	     (PP.hblock
		[PP.text (lookerToString i),
		 PP.tupleFormats (map fmtValue vl),
		 PP.text "->", fmtTypedLvar (w, t)],
	      fmtCexp e))
      | fmtCexp (ARITH (i,vl,w,t,e)) =
	  (PP.vcat
	     (PP.hblock
		[PP.text (arithToString i),
		 PP.tupleFormats (map fmtValue vl),
		 PP.text "->", fmtTypedLvar (w, t)],
	      fmtCexp e))
      | fmtCexp (PURE (i,vl,w,t,e)) =
	  (PP.vcat
	     (PP.hblock
		[PP.ccat (PP.text (pureToString i), PP.tupleFormats (map fmtValue vl)),
		 PP.text "->", fmtTypedLvar (w, t)],
	      fmtCexp e))
      | fmtCexp (SETTER (i,vl,e)) =
	   PP.vcat
	     (PP.ccat (PP.text (setterToString i), PP.tupleFormats (map fmtValue vl)),
	      fmtCexp e)
      | fmtCexp (BRANCH (i,vl,c,e1,e2)) =
	  PP.vblock
	    [PP.hblock
	       [PP.text "if",
		PP.ccat (PP.text (branchToString i), PP.tupleFormats (map fmtValue vl)),
		PP.brackets (fmtLvar c)],
	     PP.text "then",
	     PP.indent 3 (fmtCexp e1),
	     PP.text "else",
	     PP.indent 3 (fmtCexp e2)]
      | fmtCexp (RCC (b, l, p, values, lvars_ctys, e)) =
	  PP.vcat
	    (PP.hblock
	      [PP.ccat (if b then PP.text "reentrant " else PP.empty,
			if l = "" then PP.empty else PP.text l),  (* sp *)
	       PP.ccat (PP.text "rcc", PP.tupleFormats (map fmtValue values)), (* sp *)
	       PP.text "->",  (* sp *)
	       PP.tupleFormats (map fmtTypedLvar lvars_ctys)],
	     fmtCexp e)

    (* fmtFunction : function -> P.format *)
    fun fmtFunction (fk, f, vl, cl, body) =
	  PP.vcat
	    (PP.hblock [PP.text (funkindToString fk), fmtLvar f,
			fmtParams (vl,cl), PP.text "="],
	     PP.indent 3 (fmtCexp body))

    (* ppFunction : function -> unit  -- was printcps0 *)
    fun ppFunction (f : function) = PP.printFormat (fmtFunction f)

    (* ppCps : cexp -> unit *)
    fun ppCps (cexp : cexp) = PP.printFormat (fmtCexp cexp)

  end (* structure PPCps *)
