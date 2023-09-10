(* main.sml
 *
 * COPYRIGHT (c) 2023 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * To build:
 *	ml-build print-pkl.cm PrintPkl.main print-pkl
 *)

structure Main : sig

    val main : string * string list -> OS.Process.status

  end = struct

    structure C = CFG
    structure LV = LambdaVar
    structure P = CFG_Prim

    val say = print

    val i2s = Int.toString

    fun numkind2s (P.INT, bits) = ["i", i2s bits]
      | numkind2s (P.FLT, bits) = ["f", i2s bits]

    val numkindToString = String.concat o numkind2s

    fun cmpopToString P.GT = ">"
      | cmpopToString P.LT = "<"
      | cmpopToString P.GTE = ">="
      | cmpopToString P.LTE = "<="
      | cmpopToString P.EQL = "="
      | cmpopToString P.NEQ = "<>"

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

    fun arithopToString oper = (case oper
	   of P.IADD => "IADD"
	    | P.ISUB => "ISUB"
	    | P.IMUL => "IMUL"
	    | P.IDIV => "IDIV"
	    | P.IREM => "IREM"
	  (* end case *))

    fun branchToString oper = (case oper
	   of P.CMP{oper, signed, sz} => concat[
		  cmpopToString oper, if signed then "_i" else "_u",
		  Int.toString sz
		]
	    | P.FCMP{oper, sz} => concat[fcmpopToString oper, "_f", Int.toString sz]
	    | P.FSGN sz => concat["f", Int.toString sz, "sgn"]
	    | P.PEQL => "peql"
	    | P.PNEQ => "pneq"
	    | P.LIMIT n => concat["needGC(", Word.fmt StringCvt.DEC n, ")"]
	  (* end case *))

    fun allocToString P.SPECIAL = "special"
      | allocToString (P.RECORD{desc, mut=false}) =
	  concat["record[0x", IntInf.fmt StringCvt.HEX desc, "]"]
      | allocToString (P.RECORD{desc, mut=true}) =
	  concat["mut_record[0x", IntInf.fmt StringCvt.HEX desc, "]"]
      | allocToString (P.RAW_RECORD{desc, ...}) = concat[
	    "raw_record[0x", IntInf.fmt StringCvt.HEX desc, "]"
	  ]
      | allocToString (P.RAW_ALLOC{desc, align, len}) = concat(
	  "raw_" :: i2s align :: "_alloc[" ::
	  (case desc
	   of SOME d => ["0x", IntInf.fmt StringCvt.HEX d, ";"]
	    | _ => []
	  (* end case *)) @ [i2s len, "]"])

    fun setterToString P.UNBOXED_UPDATE = "unboxedupdate"
      | setterToString P.UPDATE = "update"
      | setterToString P.UNBOXED_ASSIGN = "unboxedassign"
      | setterToString P.ASSIGN = "assign"
      | setterToString (P.RAW_UPDATE{kind, sz}) =
	  concat("update_" :: numkind2s(kind, sz))
      | setterToString (P.RAW_STORE{kind, sz}) =
	  concat("store_" :: numkind2s(kind, sz))
      | setterToString P.SET_HDLR = "sethdlr"
      | setterToString P.SET_VAR = "setvar"

    fun lookerToString P.DEREF = "!"
      | lookerToString P.SUBSCRIPT = "array_sub"
      | lookerToString (P.RAW_SUBSCRIPT{kind, sz}) =
	  concat("array_sub_" :: numkind2s(kind, sz))
      | lookerToString (P.RAW_LOAD{kind, sz}) =
	  concat("load_" :: numkind2s(kind, sz))
      | lookerToString P.GET_HDLR = "gethdlr"
      | lookerToString P.GET_VAR = "getvar"

    fun cvtParams (prefix, from, to) =
	  concat[prefix, "_", i2s from, "_to_", i2s to]

    fun arithToString (P.ARITH{oper, sz}) = arithopToString oper ^ i2s sz
      | arithToString (P.FLOAT_TO_INT{mode, from, to}) = let
	  fun toS prefix = concat[prefix, i2s from, "_i", i2s to]
	  in
	    case mode
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

    fun pureToString (P.PURE_ARITH{oper, sz}) = pureopToString oper ^ i2s sz
      | pureToString (P.EXTEND{signed=true, from, to}) =
	  cvtParams ("sign_extend_", from, to)
      | pureToString (P.EXTEND{signed=false, from, to}) =
	  cvtParams ("zero_extend_", from, to)
      | pureToString (P.TRUNC{from, to}) = cvtParams ("trunc_", from, to)
      | pureToString (P.INT_TO_FLOAT{from, to}) = cvtParams ("float", from, to)
      | pureToString (P.FLOAT_TO_BITS{sz}) = concat["float_", i2s sz, "_to_bits"]
      | pureToString (P.BITS_TO_FLOAT{sz}) = concat["float_", i2s sz, "_from_bits"]
      | pureToString P.PURE_SUBSCRIPT = "vector_sub"
      | pureToString (P.PURE_RAW_SUBSCRIPT{kind, sz}) =
	  concat("vector_sub_" :: numkind2s(kind, sz))
      | pureToString (P.RAW_SELECT{kind, sz, offset}) =
	  concat("select_" :: numkind2s(kind, sz) @ ["@", i2s offset])

    fun space n = say(CharVector.tabulate(n, fn _ => #" "))

    fun expToString e = (case e
	   of C.VAR{name} => LV.toString name
	    | C.LABEL{name} => "L_" ^ LV.toString name
	    | C.NUM{iv, sz} =>
		concat["(i", i2s sz, ")", IntInf.toString iv]
	    | C.LOOKER{oper, args} => appToS(lookerToString oper, args)
	    | C.PURE{oper, args} => appToS(pureToString oper, args)
	    | C.SELECT{idx, arg} => appToS("#" ^ i2s idx, [arg])
	    | C.OFFSET{idx, arg} => appToS("@" ^ i2s idx, [arg])
	  (* end case *))

    and appToS (prefix, es) = String.concat[
	    prefix, "(", String.concatWithMap "," expToString es, ")"
	  ]

    fun sayv x = say(LV.toString x)

    fun sayList sayItem [] = say "()"
      | sayList sayItem [item] = (say "("; sayItem item; say ")")
      | sayList sayItem (fst::rest) = (
	  say "("; sayItem fst;
	  List.app (fn item => (say ","; sayItem item)) rest;
	  say ")")

    fun tyToString (C.NUMt{sz}) = "i" ^ Int.toString sz
      | tyToString (C.FLTt{sz}) = "f" ^ Int.toString sz
      | tyToString C.PTRt = "ptr"
      | tyToString C.LABt = "label"
      | tyToString C.TAGt = "int"

    fun sayTy cty = say(tyToString cty)

    fun paramToString {name, ty} = concat[LV.toString name, ":", tyToString ty]

    fun sayParam param = say (paramToString param)
    fun sayArg (e, ty) = (say(expToString e); say ":"; sayTy ty)

    fun sayArgs ([], []) = say "()"
      | sayArgs (arg::args, ty::tys) = (
	  say "("; sayArg (arg, ty);
	  ListPair.app (fn arg => (say ","; sayArg arg)) (args, tys);
	  say ")")
      | sayArgs _ = raise Match

    fun prStm n = let
	  fun sayExp e = say(expToString e)
	  fun sayApp (prefix, args) = (say(appToS(prefix, args)))
	  fun sayBr (P.LIMIT 0w0, []) = say "needsGC"
	    | sayBr (oper as P.LIMIT _, []) = say(branchToString oper)
	    | sayBr (oper, args) = sayApp (branchToString oper, args)
	  fun pr stm = (
		space n;
		case stm
		 of C.LET(e, x, stm) => (
		      say(expToString e); say " -> "; sayParam x; say "\n"; pr stm)
		  | C.ALLOC(p as P.RAW_ALLOC _, [], x, stm) => (
		      say (allocToString p); say " -> "; sayv x; say "\n"; pr stm)
		  | C.ALLOC(p, args, x, stm) => (
		      sayApp (allocToString p, args);
		      say " -> "; sayv x; say "\n"; pr stm)
		  | C.APPLY(f, args, tys) => (
		      say "apply "; sayExp f; sayArgs (args, tys); say "\n")
		  | C.THROW(f, args, tys) => (
		      say "throw "; sayExp f; sayArgs (args, tys); say "\n")
		  | C.GOTO(lab, args) => (
		      sayApp ("goto L_" ^ LV.toString lab, args); say "\n")
		  | C.SWITCH(arg, cases) => let
		      fun sayCase (i, e) = (
			    space (n+2); say "case "; say(i2s i);
			    say ":\n"; prStm (n+4) e)
		      in
			say "switch ("; say(expToString arg); say ") {\n";
			List.appi sayCase cases;
			space n; say "}\n"
		      end
		  | C.BRANCH(p, args, 0, stm1, stm2) => (
		      say "if "; sayBr (p, args); say " {\n";
		      prStm (n+2) stm1;
		      space n; say "} else {\n";
		      prStm (n+2) stm2;
		      space n; say "}\n")
		  | C.BRANCH(p, args, prob, stm1, stm2) => (
		      say "if "; sayBr (p, args);
		      say " { ["; say(Int.toString prob); say "/1000]\n";
		      prStm (n+2) stm1;
		      space n; say "} else { [";
		      say(Int.toString(100-prob)); say "/1000]\n";
		      prStm (n+2) stm2;
		      space n; say "}\n")
		  | C.ARITH(p, args, x, stm) => (
		      sayApp (arithToString p, args);
		      say " -> "; sayParam x; say "\n"; pr stm)
		  | C.SETTER(p, args, stm) => (
		      sayApp (setterToString p, args); say "\n"; pr stm)
		  | C.CALLGC(roots, newRoots, stm) => (
		      sayApp ("callgc", roots);
		      say " -> (";
		      say (String.concatWithMap "," LV.toString newRoots);
		      say ")\n";
		      pr stm)
		  | C.RCC{reentrant, linkage, proto, args, results, live, k} => (
		      if reentrant
			then say "reentrant c_call "
			else say "c_call ";
		      if linkage = "" then () else (say linkage; say " ");
		      sayList (fn e => say(expToString e)) args;
		      say " -> "; sayList sayParam results; say "\n";
(* FIXME: print live set too *)
		      pr k)
		(* end case *))
	  in
	    pr
	  end

    fun prFrag n (C.Frag{kind, lab, params, body}) = (
	  space n;
	  case kind
	   of C.STD_FUN => say "std_fun"
	    | C.STD_CONT => say "std_cont"
	    | C.KNOWN_FUN => say "known_fun"
	    | C.INTERNAL => say "frag"
	  (* end case *);
	  say " (L)"; sayv lab; say " "; sayList sayParam params; say " {\n";
	  prStm (n+2) body;
	  space n; say "}\n")

    fun prCluster (C.Cluster{attrs, frags}) = (
	  say ("# CLUSTER; align " ^ Int.toString(#alignHP attrs));
	  if (#needsBasePtr attrs) then say "; base-ptr" else ();
	  if (#hasTrapArith attrs) then say "; overflow" else ();
	  if (#hasRCC attrs) then say "; raw-cc" else ();
	  say "\n{\n";
	  List.app (prFrag 2) frags;
	  say "}\n")

    fun prCompUnit {srcFile, entry, fns} = (
	  say (concat["########## ", srcFile, "\n"]);
	  prCluster entry;
	  List.app (fn f => (say "#####\n"; prCluster f)) fns;
	  say "##########\n")

    fun main (_, [file]) = if OS.FileSys.access(file, [OS.FileSys.A_READ])
	  then let
	    val cu = ASDLFilePickle.fromFile CFGFilePickle.read_comp_unit file
		  handle exn => (
		    TextIO.output(TextIO.stdErr, concat[
			file, ": uncaught exception ", exnMessage exn, "\n"
		      ]);
		    OS.Process.exit OS.Process.failure)
	    in
	      prCompUnit cu;
	      OS.Process.success
	    end
	  else (
	    TextIO.output(TextIO.stdErr, concat[
		"pickle file \"", file, "\" not found\n"
	      ]);
	    OS.Process.failure)
      | main _ = (
	  TextIO.output(TextIO.stdErr, "usage: print-pkl <file>\n");
	  OS.Process.failure)

  end
