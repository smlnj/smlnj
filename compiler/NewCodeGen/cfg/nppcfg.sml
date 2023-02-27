(* nppcfg.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 * 
 * Formatting cfg using the new PrettyPrint library.
 *)


signature PP_CFG =
sig

    val fmtCluster : CFG.cluster -> PrettyPrint.format

    val fmtCompUnit : CFG.comp_unit -> PrettyPrint.format

    val fmtExp : CFG.exp -> PrettyPrint.format

    val fmtNumkind : CFG_Prim.numkind * int -> PrettyPrint.format

    val paramToString : CFG.param -> string

    (* formatting for various CFG_Prim types *)
    val fmtAlloc : CFG_Prim.alloc -> PrettyPrint.formt
    val fmtArithop : CFG_Prim.arithop -> PrettyPrint.format
    val fmtPureop : CFG_Prim.pureop -> PrettyPrint.format
    val fmtCmpop : CFG_Prim.cmpop -> PrettyPrint.format
    val fmtFcmpop : CFG_Prim.fcmpop -> PrettyPrint.format
    val fmtBranch : CFG_Prim.branch -> PrettyPrint.format
    val fmtSetter : CFG_Prim.setter -> PrettyPrint.format
    val fmtLooker : CFG_Prim.looker -> PrettyPrint.format
    val fmtArith : CFG_Prim.arith -> PrettyPrint.format
    val fmtPure : CFG_Prim.pure -> PrettyPrint.format

end (* signature PP_CFG *)

structure PPCfg : PP_CFG =
struct

    structure C = CFG
    structure LV = LambdaVar
    structure P = CFG_Prim
    structure PP = PrettyPrint

    fun numkind2s (P.INT, bits) = ["i", i2s bits]
      | numkind2s (P.FLT, bits) = ["f", i2s bits]

    fun fmtNumKind (P.INT, bits) = PP.ccat [PP.text "i", PP.integer bits]
    fun fmtNumKind (P.FLT, bits) = PP.ccat [PP.text "f", PP.integer bits]
  
    val fmtCmpop = PP.text o PPCps.cmpopToString
    val fmtFcmpop = PP.text o PPCps.fcmpopToString

    fun arithopToString oper = (case oper
	   of P.IADD => "IADD"
	    | P.ISUB => "ISUB"
	    | P.IMUL => "IMUL"
	    | P.IDIV => "IDIV"
	    | P.IREM => "IREM"
	  (* end case *))

    fun fmtArithop (oper: P.arithop) = PP.text o arithopToString

    (* fmtBranch : P.branch -> PP.format *)
    fun branchToString oper =
	(case oper
	   of P.CMP{oper, signed, sz} =>
	        PP.ccat [fmtCmpop oper,
			   PP.text (if signed then "_i" else "_u"),
		           PP.integer sz]
	    | P.FCMP{oper, sz} =>
	        PP.ccat [fmtFcmpop oper, PP.text "_f", PP.integer sz]
	    | P.FSGN sz =>
	        PP.ccat [PP.text "f", PP.integer sz, PP.text "sgn"]
	    | P.PEQL => PP.text "peql"
	    | P.PNEQ => PP.text "pneq"
	    | P.LIMIT n =>
	        PP.label "needGC:" (PP.parens (PP.text (Word.fmt StringCvt.DEC n)))
	(* end case *))

    (* fmtIntInf : IntInf.int -> PP.format *)
    fun fmtIntInf (i: IntInf.int) : PP.format =
	PP.ccat [PP.text "0x", PP.text (IntInf.fmt StringCvt.HEX desc)]

    (* fmtAlloc : P.alloc -> PP.format *)
    fun fmtAlloc (P.RECORD{desc, mut=false}) =
	  ccat [PP.text "record", PP.brackets (fmtIntInf desc)]
      | fmAlloc (P.RECORD{desc, mut=true}) =
	  ccat [PP.text "mut_record", PP.brackets (fmtIntInf desc)]
      | fmAlloc (P.RAW_RECORD{desc, ...}) =
	  ccat [PP.text "raw_record", PP.brackets (fmtIntInf desc)]
      | fmAlloc (P.RAW_ALLOC{desc, align, len}) =
	  PP.ccat
	    [PP.ccat [PP.text "raw_", PP.integer align, PP.text "_alloc"],
	     PP.brackets
	       (PP.ccat [case desc
			   of SOME d => PP.ccat (fmtIntInf d, PP.semicolon)
			    | NONE => PP.empty,
			 PP.integer len])]

    (* fmtSetter : P.setter -> PP.format *)
    fun fmtSetter P.UNBOXED_UPDATE = PP.text "unboxedupdate"
      | fmtSetter P.UPDATE = PP.text "update"
      | fmtSetter P.UNBOXED_ASSIGN = PP.text "unboxedassign"
      | fmtSetter P.ASSIGN = PP.text "assign"
      | fmtSetter (P.RAW_UPDATE{kind, sz}) =
	  PP.ccat [PP.text "update_", fmtNumkind (kind, sz)]
      | fmtSetter (P.RAW_STORE{kind, sz}) =
	  PP.ccat [PP.text "store_", fmtNumkind (kind, sz)]
      | fmtSetter P.SET_HDLR = PP.text "sethdlr"
      | fmtSetter P.SET_VAR = PP.text "setvar"

    (* fmtLooker : P.looker -> PP.format *)
    fun fmtLooker P.DEREF = PP.text "!"
      | fmtLooker P.SUBSCRIPT = PP.text "array_sub"
      | fmtLooker (P.RAW_SUBSCRIPT{kind, sz}) =
	  PP.ccat [PP.text "array_sub_", fmtNumkind (kind, sz)]
      | fmtLooker (P.RAW_LOAD{kind, sz}) =
	  PP.ccat [PP.text "load_", fmtNumkind (kind, sz)]
      | fmtLooker P.GET_HDLR = PP.text "gethdlr"
      | fmtLooker P.GET_VAR = PP.text "getvar"

    (* cvtParams : string * int * int -> PP.format *)
    fun cvtParams (prefix, from, to) =
	  PP.ccat [PP.text prefix, PP.text "_", PP.integer from,
		     PP.text "_to_", PP.integer to]

     fun fmtArith (P.ARITH {oper, sz}) = PP.ccat [fmtArithop oper, PP.integer sz]
      | fmtArith (P.FLOAT_TO_INT {mode, from, to}) = 
	  let fun toS (prefix: string) =
		    PP.ccat [PP.text prefix, PP.integer from,
			       PP.text "_i", PP.integer to]
	   in case mode
		of P.TO_NEAREST => toS "round_f"
		 | P.TO_NEGINF => toS "floor_f"
		 | P.TO_POSINF => toS "ceil_f"
		 | P.TO_ZERO => toS "trunc_f"
	      (* end case *)
	  end

    fun pureopToString rator = (case rator
	   of P.ADD => "add"
	    | P.SUB => "sub"
	    | P.SMUL => "smul"
	    | P.SDIV => "sdiv"
	    | P.SREM => "srem"
	    | P.UMUL => "umul"
	    | P.UDIV => "udiv"
	    | P.UREM => "urem"
	    | P.LSHIFT => "lshift"
	    | P.RSHIFT => "rshift"
	    | P.RSHIFTL => "rshiftl"
	    | P.ORB => "orb"
	    | P.XORB => "xorb"
	    | P.ANDB => "andb"
	    | P.FADD => "fadd"
	    | P.FSUB => "fsub"
	    | P.FMUL => "fmul"
	    | P.FDIV => "fdiv"
	    | P.FNEG => "fneg"
	    | P.FABS => "fabs"
	    | P.FSQRT => "fsqrt"
	    | P.FCOPYSIGN => "fcopysign"
	  (* end case *))

    val fmtPureop = PP.text o pureopToString

    (* fmtPure : P.pure -> PP.format *)
    fun fmtPure (P.PURE_ARITH {oper, sz}) =
	  PP.ccat [fmtPureop oper, PP.integer sz]
      | fmtPure (P.EXTEND{signed=true, from, to}) =
	  cvtParams ("sign_extend_", from, to)
      | fmtPure (P.EXTEND{signed=false, from, to}) =
	  cvtParams ("zero_extend_", from, to)
      | fmtPure (P.TRUNC{from, to}) = cvtParams ("trunc", from, to)
      | fmtPure (P.INT_TO_FLOAT{from, to}) =
	  PP.ccat [PP.text "i", PP.integer from, PP.text "_to_f", PP.integer to]
      | fmtPure (P.FLOAT_TO_BITS{sz}) =
	  PP.ccat [PP.text "f", PP.integer sz, PP.text "_to_bits"]
      | fmtPure (P.BITS_TO_FLOAT{sz}) =
	  PP.ccat [PP.text "f", PP.integer sz, PP.text "_from_bits"]
      | fmtPure P.PURE_SUBSCRIPT = PP.text "vector_sub"
      | fmtPure (P.PURE_RAW_SUBSCRIPT{kind, sz}) =
	  PP.ccat [PP.text "vector_sub_", fmtNumkind (kind, sz)]
      | fmtPure (P.RAW_SELECT{kind, sz, offset}) =
	  PP.ccat [PP.text "select_", fmtNumkind (kind, sz), PP.text "@", PP.integer offset]

    (* space : int -> string *)
    fun space n = CharVector.tabulate (n, fn _ => #" ")

    (* fmtLvar : LV.lvar -> PP.format *)
    fun fmtLvar (v: LV.lvar) = PP.text (LV.lvarName name) 

    (* fmtExp C.exp -> PP.format *)
    fun fmtExp e =
	(case e
	   of C.VAR {name} => fmtLvar name
	    | C.LABEL {name} => PP.ccat [PP.text "L_", fmtLvar name]
	    | C.NUM {iv, sz} =>
		PP.ccat [PP.parens (PP.ccat [PP.text "i", PP.integer sz]),
			 PP.text (IntInf.toString iv)]
	    | C.LOOKER {oper, args} => fmtApp (fmtLooker oper, args)
	    | C.PURE {oper, args} => fmtApp (fmtPure oper, args)
	    | C.SELECT {idx, arg} => fmtApp (PP.ccat [PP.text "#", PP.integer idx], [arg])
	    | C.OFFSET {idx, arg} => fmtApp (PP.ccat [PP.text "@", PP.integer idx], [arg])
	  (* end case *))

    (* fmtApp : PP.format * C.exp list -> PP.format *)
    and fmtApp (prefix, es) = 
	PP.ccat [prefix, PP.tuple (map fmtExp exps)]

    fun sayList sayItem [] = say "()"
      | sayList sayItem [item] = (say "("; sayItem item; say ")")
      | sayList sayItem (fst::rest) = (
	  say "("; sayItem fst;
	  List.app (fn item => (say ","; sayItem item)) rest;
	  say ")")

    fun fmtTy cty = PP.text (CFGUtil.tyToString cty)

    fun fmtParam {name: LV.lvar, ty: C.ty} =
        PP.ccat [fmtLvar name, PP.colon, fmtTy ty]

    (* fmtArg : C.exp * C.ty -> PP.format *)
    fun fmtArg (e, ty) = PP.ccat [fmtExp e, PP.colon, fmtTy ty]

    (* fmtArgs : C.exp list * C.ty list -> PP.format *)
    fun fmtArgs (args, tys) = PP.tuple (ListPair.mapEq fmtArg args tys)

    (* fmtStm : C.stm -> PP.format *)
    local fun fmtBr (P.LIMIT 0w0, []) = PP.text "needsGC"
	    | fmtBr (oper as P.LIMIT _, []) = fmtBranch oper
	    | fmtBr (oper, args) = fmtApp (fmtBranch oper, args)
       in fun fmtStm stm =
		case stm
		 of C.LET(e, x, stm) =>
		      PP.pcat [PP.hcat [fmtExp e, PP.text "->" fmtParam x],
			       PP.indent 2 (fmtStm stm)]
		  | C.ALLOC(p as P.RAW_ALLOC _, [], x, stm) =>
		      PP.pcat [PP.hcat [fmtAlloc p, PP.text "->", fmtLvar x],
			       PP.indent 2 (fmtStm stm)]
		  | C.ALLOC(p, args, x, stm) => 
		      PP.pcat [PP.hcat [fmtApp (fmtAlloc p, args), PP.text " -> ", fmtLvar x],
		               PP.indent 2 (fmtStm stm)]
		  | C.APPLY(f, args, tys) => 
		      PP.hcat [PP.text "apply", fmtExp f, fmtArgs (args, tys)]
		  | C.THROW(f, args, tys) =>
		      PP.hcat [PP.text "throw ", fmtExp f, fmtArgs (args, tys)]
		  | C.GOTO(lab, args) =>
		      fmtApp (PP.ccat [PP.text "goto L_", fmtLvar lab), args]
		  | C.SWITCH(arg, cases) =>
		      let fun fmtCase (i, e) =
			      PP.vcat [PP.hcat (PP.text "case", PP.integer i),
				       PP.indent 2 (fmtStm e)]
		      in PP.vcat
			   [PP.hcat [PP.text "switch", PP.parens (fmtExp arg), PP.lbrace],
			    indent 2 (PP.vcat (List.mapi fmtCase cases)),
			    PP.rbrace]
		      end
		  | C.BRANCH(p, args, 0, stm1, stm2) =>
		      PP.vcat
			[PP.hcat [PP.text "if", fmtBr (p, args), PP.lbrace],
		         PP.indent 2 (fmtStm stm1),
			 PP.hcat [PP.lbrace, PP.text "else" PP.rbrace],
			 PP.indent 2 (fmtStm stm2),
			 PP.rbrace]
		  | C.BRANCH(p, args, prob, stm1, stm2) =>
  		      PP.vcat
			[PP.hcat
			   [PP.text "if ", fmtBr (p, args),
			    PP.lbrace, PP.brackets (PP.ccat [PP.integer prob, PP.text "/1000"])],
			 PP.indent 2 (fmtStm stm1),
			 PP.hcat [PP.lbrace, PP.text "else" PP.rbrace,
				    PP.brackets (PP.ccat [PP.integer (100-prob), PP.text "/1000"])],
			 PP.indent 2 (fmtStm stm2),
			 PP.rbrace]
		  | C.ARITH(p, args, x, stm) =>
		      PP.vcat
			[PP.hcat [fmtApp (fmtArith p, args), PP.text "->", fmtParam x],
		         PP.indent 2 (fmtStm stm)]
		  | C.SETTER(p, args, stm) =>
		      PP.vcat [fmtApp (fmtSetter p, args),
			       PP.indent 2 (fmtStm stm)]
		  | C.CALLGC(roots, newRoots, stm) =>
		      PP.vcat
			[PP.hcat [fmtApp ("callgc", roots), PP.text "->", 
				    PP.tuple (map fmtLvar newRoots)],
			 PP.indent 2 (fmtStm stm)]
		  | C.RCC{reentrant, linkage, proto, args, results, live, k} => (
		      PP.vcat
			[PP.hcat
			   [PP.text (if reentrant then "reentrant c_call" else "c_call"),
			    if linkage = "" then PP.empty else PP.text linkage,
			    PP.hsequence PP.comma (map fmtExp args),
			    PP.text "->",
			    PP.tuple (map fmtParam results)],
			 PP.label "live" (PP.listMap fmtParam live),
			 PP.indent 2 fmtStm k]
		  (* end case *))
    end (* local *)

    fun fmtFrag (C.Frag{kind, lab, params, body}) =
	let val kindString =
		case kind
		  of C.STD_FUN => "std_fun"
		   | C.STD_CONT => "std_cont"
		   | C.KNOWN_FUN => "known_fun"
		   | C.INTERNAL => "frag"
	        (* end case *)
	 in PP.vcat
	      [PP.hcat 
	         [PP.text kindString,
		  PP.ccat [PP.text "(L)", fmtLvar lab],
	          PP.psequence PP.comma (map fmtParam params),
	          PP.lbrace],
	       PP.indent 2 (fmtStm body),
	       PP.rbrace]
	end

    fun fmtCluster (C.Cluster{attrs, frags}) =
          PP.vcat
	    [PP.ccat
	       [PP.hcat [PP.text "# CLUSTER; align", PP.integer (#alignHP attrs)],
		if (#needsBasePtr attrs) then PP.hcat [PP.semicolon, PP.text "base-ptr"] else PP.empty,
 	        if (#hasTrapArith attrs) then PP.hcat [PP.semicolon, PP.text "overflow"] else PP.empty,
		if (#hasRCC attrs) then PP.hcat [PP.semicolon, PP.text "raw-cc"] else PP.empty]
	     PP.lbrace,
	     PP.indent 2 (PP.vcat (List.map fmtFrag frags)),
	     PP.rbrace]

    fun fmtCompUnit {srcFile, entry, fns} =
	  PP.vcat
	    [PP.hcat [PP.text "##########", PP.text srcFile],
	     fmtCluster entry;
	     PP.vcat (map (fn f => (PP.vcat [PP.text "#####", fmtCluster f)]) fns);
	     PP.text "##########"]

  end
