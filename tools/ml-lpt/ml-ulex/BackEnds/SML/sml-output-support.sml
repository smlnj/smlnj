(* sml-output-support.sml
 *
 * COPYRIGHT (c) 2005 
 * John Reppy (http://www.cs.uchicago.edu/~jhr)
 * Aaron Turon (adrassi@gmail.com)
 * All rights reserved.
 *
 * Some supporting code shared between different methods of 
 * SML output
 *)

structure SMLOutputSupport = 
  struct

    structure RE = RegExp
    structure Sym = RE.Sym
    structure SIS = RegExp.SymSet
    structure LO = LexOutputSpec

    datatype ml_exp = datatype ML.ml_exp
    datatype ml_pat = datatype ML.ml_pat

    fun ML_Sym s = ML_Raw [ML.Tok (RE.symToString s)]

    val inp = "inp"
    val inpVar = ML_Var inp

    fun idOf (LO.State {id, ...}) = id
    fun nameOf' i = "yyQ" ^ (Int.toString i)
    fun nameOf s = nameOf' (idOf s)
    fun actName i = "yyAction" ^ (Int.toString i)

  (* simple heuristic to avoid computing unused values *)
    local 
      val has = String.isSubstring
    in
    val hasyytext   = has "yytext"
    val hasyysubstr = has "yysubstr"
    val hasyyunicode= has "yyunicode"
    val hasREJECT   = has "REJECT"
    val hasyylineno = has "yylineno"
    val hasyycolno  = has "yycolno"
    end

  (* map over the intervals of a symbol set *)
    fun mapInt f syms = 
	  SIS.foldlInt (fn (i, ls) => (f i)::ls) [] syms

  (* generate code for an action *)
    fun mkAction (i, action, k) = let
          val updStrm = ML_RefPut (ML_Var "yystrm", ML_Var "strm")
	  val act = ML_Raw [ML.Tok action]
	  val seq = ML_Seq [updStrm, act]
	  val lets = if hasyysubstr action andalso not (!Options.lexCompat)
		     then ML_Let 
			    ("yysubstr", 
			     ML_App("yymksubstr", [ML_Var "strm"]), 
			     seq)
		     else seq
	  val lett = if hasyytext action 
		     then ML_Let 
			    ("yytext", 
			     ML_App("yymktext", [ML_Var "strm"]), 
			     lets)
 		     else lets
	  val letu = if hasyyunicode action 
		     then ML_Let 
			    ("yyunicode", 
			     ML_App("yymkunicode", [ML_Var "strm"]), 
			     lett)
		     else lett
	  val letl = if hasyylineno action
		     then ML_Let 
			    ("yylineno", 
			     ML_App("ref", 
				 [ML_App ("yygetlineNo", 
					  [ML_RefGet (ML_Var "yystrm")])]), 
			     letu)
		     else letu
	  val letc = if hasyycolno action
		     then ML_Let 
			    ("yycolno", 
			     ML_App("ref", 
				 [ML_App ("yygetcolNo", 
					  [ML_RefGet (ML_Var "yystrm")])]), 
			     letl)
		     else letl
	  val letr = if hasREJECT action
		     then ML_Let
			    ("oldStrm", ML_RefGet (ML_Var "yystrm"),
			     ML_Fun
			       ("REJECT", [],
				ML_Seq 
				  [ML_RefPut (ML_Var "yystrm", 
					      ML_Var "oldStrm"),
				   ML_App("yystuck", [ML_Var "lastMatch"])],
				letc))
		     else letc
	  in  
	    ML_NewGroup (ML_Fun (actName i, ["strm", "lastMatch : yymatch"], letr, k))
	  end

  (* output start state datatype *)
    fun startStatesHook spec strm = let
          val LO.Spec {startStates, ...} = spec
	  val machNames = #1 (ListPair.unzip startStates)
          in 
            TextIO.output (strm, String.concatWith " | " machNames)
          end

  (* output user declarations *)
    fun userDeclsHook spec strm = let
          val LO.Spec {decls, ...} = spec
          in 
            TextIO.output (strm, decls)
	  end

  (* output "header" -- the structure/functor definition of the lexer *)
    fun headerHook spec strm = let
          val LO.Spec {header, ...} = spec
          in 
            TextIO.output (strm, header)
	  end

  (* generate args parameter for the lexer *)
    fun argsHook spec strm = let
          val LO.Spec {arg, ...} = spec
	  val arg' = case (String.size arg, !Options.lexCompat)
		      of (0, true)  => "(yyarg as ())"
		       | (_, true)  => "(yyarg as " ^ arg ^ ") ()"
		       | (0, false) => ""
		       | (_, false) => "(yyarg as " ^ arg ^ ")"
          in 
            TextIO.output (strm, arg')
	  end

  (* ml-ulex mode only *)
    fun pargsHook spec strm = let
          val LO.Spec {arg, ...} = spec
          in 
            TextIO.output (strm, 
	      if String.size arg > 0 then "yyarg" else "")
	  end

    fun mkEOF (eofRules, innerExp) = 
	  if !Options.lexCompat 
	  then
            ML_If (ML_App("yyInput.eof", [ML_RefGet (ML_Var "yystrm")]), 
		   ML_App("UserDeclarations.eof", [ML_Var "yyarg"]),
		   innerExp)
	  else 
	    ML_If (
	    ML_App("ULexBuffer.eof", [ML_RefGet (ML_Var "yystrm")]), 
	    ML_Let("yycolno",  ML_App("ref", [ML_App ("yygetcolNo",  [ML_RefGet (ML_Var "yystrm")])]),
	    ML_Let("yylineno", ML_App("ref", [ML_App ("yygetlineNo", [ML_RefGet (ML_Var "yystrm")])]), 
	    ML_Case(ML_RefGet (ML_Var "yyss"),
	      map (fn ("_", act) => (ML_Wild,		 ML_Raw [ML.Tok ("(" ^ act ^ ")")])
		    | (ss,  act) => (ML_ConPat (ss, []), ML_Raw [ML.Tok ("(" ^ act ^ ")")]))
	          eofRules
	    ))),
	    innerExp)

  (* include the templates, which are defined in a compiler-specific way *)
    open Templates

  end