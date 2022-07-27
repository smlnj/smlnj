(* sml-output.sml
 *
 * COPYRIGHT (c) 2006
 * John Reppy (http://www.cs.uchicago.edu/~jhr)
 * Aaron Turon (http://www.cs.uchicago.edu/~adrassi)
 * All rights reserved.
 *
 * Back end for SML code, using first-class continuations for
 * Burke-Fisher-style error repair/recovery
 *)

structure SMLOutput =
  struct

    structure S = LLKSpec
    structure P = Predict

    structure TMap = Token.Map
    structure TSet = Token.Set
    structure NMap = Nonterm.Map

    structure NT = Nonterm

    datatype ml_exp = datatype ML.ml_exp
    datatype ml_pat = datatype ML.ml_pat
    datatype ml_decl = datatype ML.ml_decl
    datatype ml_fundecl = datatype ML.ml_fundecl
    datatype ml_fun_heading = datatype ML.ml_fun_heading

  (* the following functions compute names or small expressions
   * used throughout the backend
   *)

    fun NTFnName nt = NT.name nt ^ "_NT"
    fun NTFnVar nt = ML_Var (NTFnName nt)

    fun predFnName nt = NT.name nt ^ "_PRED"

    fun tokConName tok = "Tok." ^ Token.name tok
    fun tokConVar tok = ML_Var (tokConName tok)
    fun tokConPat tok = ML_ConPat (tokConName tok,
			    if Token.hasTy tok
			    then [ML_Wild]
			    else [])
    fun tokConPat' tok = ML_ConPat (Token.name tok,
			    if Token.hasTy tok
			    then [ML_Wild]
			    else [])
    fun tokMatch' tok = "match" ^ (Token.name tok)
    fun tokMatch tok = "" ^ tokMatch' tok

    fun tokExpected tok = ML_Raw [ML.Tok ("raise Fail \"expected " ^ (Token.name tok) ^ "\"")]

    val bindingSuffix = "_RES"
    val spanSuffix = "_SPAN"
    val fullSpan = "FULL_SPAN"
    val spanTySuffix = " : (Lex.pos * Lex.pos)"
    val rcSuffix = "_REFC"

    fun actionHeader (name, (bindings, formals), suffix, isPred, refcells, refSuffix) = let
	  val withSuffix = map (fn b => Atom.toString b ^ suffix)
			       (AtomSet.listItems (AtomSet.union (bindings, formals)))
	  val withSpan   = map (fn b => Atom.toString b ^ spanSuffix ^ spanTySuffix)
			       (AtomSet.listItems bindings)
	  val refs = map (fn (S.REFCELL {name, ...}) => name ^ refSuffix) refcells
	  val args = if isPred then
		       withSuffix @ refs
		     else
		       withSuffix @ withSpan @ [fullSpan ^ spanTySuffix] @ refs
          in
            String.concat [name, " (", String.concatWith ", " args, ")"]
          end

  (* make an expression that will pull the next token off the stream *)
    fun mkGet1 strm = ML_App ("lex", [ML_Var (strm)])
  (* make an expression that will pull the kth token off the stream *)
    fun mkGetk (strm, 1) = mkGet1 strm
      | mkGetk (strm, k) = ML_App ("lex", [ML_App ("#2", [mkGetk (strm, k-1)])])

    fun rawCode code = ML_Raw [ML.Tok code]

  (* make an expression for the given (polymorphic) decision tree *)
    fun mkPredict (pickFn, choiceFn, strm, tree, errAction) = let
          fun mkPredict (strm, P.Pick p) =
	        pickFn p
	    | mkPredict (strm, P.ByTok branches) = let
		val branches = List.concat (map mkMatch branches)
		val errCase = (ML_Wild, errAction)
	        in
	          ML_Case (mkGet1 strm, branches @ [errCase])
	        end
	    | mkPredict (strm, P.Choice prods) =
	        choiceFn prods
	  and mkMatch (set, tree) =
	        map (fn tok => (ML_TupPat [tokConPat tok, ML_Wild, ML_VarPat "strm'"],
				mkPredict ("strm'", tree)))
		    (TSet.listItems set)
          in
            mkPredict (strm, tree)
          end

  (* make a production *)
    fun mkProd (grm, pm) prod = let
          val rhs = Prod.items prod
	  val S.Grammar {refcells, ...} = grm
	  fun mkTok (t, strmExp, letFn) =
	        letFn (ML_App (tokMatch t, [strmExp]))
	  fun mkNT (nt, strmExp, args, letFn, item) = let
	        val name = (case (args, !Options.unitActions)
		       of (SOME args, false) => concat [
			      "(", NTFnName nt, " (",
			      actionHeader (
			       "UserCode.ARGS_" ^ Action.name args,
			       Item.bindingsLeftOf (item, prod),
			       bindingSuffix, true, refcells, rcSuffix),
			      "))"
			    ]
			| _ => NTFnName nt
		      (* end case *))
	        val innerExp = ML_App (name, [strmExp])
	        in
	          if NT.isSubrule nt
		  then letFn (mkNonterm (grm, pm) (nt, innerExp))
		  else letFn innerExp
	        end
	  fun mkEBNF (nt, strmExp, fname, letFn) = let
	        val predName = predFnName nt
	        val innerExp = letFn (ML_App (fname, [ML_Var predName, ML_Var (NTFnName nt), strmExp]))
		val Predict.PMaps {ebnfPredict, ...} = pm
		val predTree = ebnfPredict nt
		fun mkBool true = ML_Var "true"
		  | mkBool false = ML_Var "false"
		fun choiceFn _ = raise Fail "BUG: mkEBNF: backtracking choice unexpected"
		val errAction = ML_Var "false"
		val caseExp = mkPredict (mkBool, choiceFn, "strm", predTree, errAction)
		val predFn = ML_Funs ([(predName, ["strm"], caseExp)], innerExp)
		in
	          mkNonterm (grm, pm) (nt, predFn)
	        end
	  fun mkItem strm ((item, binding), k) = let
	        val strmExp = ML_Var strm
		fun mkLet e = ML_Let (String.concat
		      ["(", binding, bindingSuffix,
		       ", ", binding, spanSuffix, ", strm')"],
		      e, k)
	        in
	          case Item.sym item
		   of S.TOK t      => mkTok  (t,  strmExp, mkLet)
		    | S.NONTERM (nt, args)
				   => mkNT   (nt, strmExp, args, mkLet, item)
		    | S.CLOS nt    => mkEBNF (nt, strmExp, "EBNF.closure", mkLet)
		    | S.POSCLOS nt => mkEBNF (nt, strmExp, "EBNF.posclos", mkLet)
		    | S.OPT nt     => mkEBNF (nt, strmExp, "EBNF.optional", mkLet)
	        end
	  val itemBindings = Prod.itemBindings prod
	  fun debugCode () = concat[
		  "print \"", Nonterm.qualName (Prod.lhs prod), "\\n\""
		]
	  val action = if !Options.unitActions
		then "()"
		else (case Prod.action prod
		   of SOME _ => actionHeader (
			concat ["UserCode.", Prod.fullName prod, "_ACT"],
			Prod.bindingsAtAction prod, bindingSuffix, false,
			refcells, rcSuffix)
		    | NONE => let
			val bindings = ListPair.mapPartial
			      (fn (binding, hasValue) =>
				  if hasValue
				    then SOME (binding ^ bindingSuffix)
				    else NONE)
			      (itemBindings, Prod.itemYields prod)
			in
			  case bindings
			   of [] => "()"
			    | _ => concat["(", String.concatWith ", " bindings, ")"]
			  (* end case *)
			end
		  (* end case *))
	  fun innerExp strm = let
	        val strmVar = ML_Var (strm)
	        val span = if List.length itemBindings = 0 then
			     ML_Tuple [ML_App ("Err.getPos", [strmVar]),
				       ML_App ("Err.getPos", [strmVar])]
			   else
			     ML_Tuple [ML_App ("#1", [ML_Var (hd itemBindings ^ spanSuffix)]),
				       ML_App ("#2", [ML_Var (hd (rev itemBindings) ^ spanSuffix)])]
	        val act = ML_Tuple [ML_Raw [ML.Tok action], ML_Var fullSpan, strmVar]
		val act = if !Options.debug
		      then ML_Seq[
			  ML_Raw[ML.Tok(concat[
			      "print \"", Nonterm.qualName (Prod.lhs prod), "\\n\""
			    ])],
			  act
			]
		      else act
		val spanExp = ML_Let (fullSpan, span, act)
	        in case (Prod.pred prod, !Options.unitActions)
		    of (SOME pred, false) =>
		         ML_If (ML_Raw [ML.Tok ("("
				  ^ actionHeader
				      ("UserCode." ^ Prod.fullName prod ^ "_PRED",
				       Prod.bindingsAtAction prod, bindingSuffix, true,
				       refcells, rcSuffix)
				  ^ ")")],
				spanExp,
				ML_App ("fail", []))
		     | _ => spanExp
	        end
	  val parse = case (ListPair.zip (rhs, itemBindings))
		       of [] => innerExp "strm"
			| fst::rst =>
			    mkItem "strm"
			      (fst, List.foldr (mkItem "strm'") (innerExp "strm'") rst)
          in
            parse
          end

  (* make a group of productions, along with a decision tree to choose one of them *)
    and mknProds (grm, pm, nt) = let
	  fun mkProdFun (prod, k) = ML_Funs ([(Prod.name prod, ["strm"],
					    mkProd (grm, pm) prod)], k)
	  val Predict.PMaps {prodPredict, ...} = pm
	  val tree = prodPredict nt
	  fun pickFn prod = ML_App (Prod.name prod, [ML_Var "strm"])
	  fun choiceFn prods =
	        ML_App ("tryProds", [ML_Var "strm",
					ML_List (map (ML_Var o Prod.name) prods)])
	  val errAction = ML_App ("fail", [])
	  val caseExp = mkPredict (pickFn, choiceFn, "strm", tree, errAction)
          in
	    foldr mkProdFun caseExp (Nonterm.prods nt)
          end

    and mkNonterm' (grm, pm) nt = let
          val formals = (case (!Options.unitActions, Nonterm.formals nt)
		 of (false, _::_) => concat[
			" (",
			String.concatWithMap ", "
			  (fn f => Atom.toString f ^ bindingSuffix)
			    (Nonterm.formals nt),
			")"
		      ]
		  | _ => ""
		(* end case *))
	  val exp = (case Nonterm.prods nt
		 of [prod] => mkProd (grm, pm) prod
		  | _ => mknProds(grm, pm, nt)
		(* end case *))
          in
            (NTFnName nt ^ formals, ["strm"], exp)
          end
    and mkNonterm (grm, pm) (nt, k) = ML_Funs ([mkNonterm' (grm, pm) nt], k)

    fun mkNonterms (grm, pm) (nts, k) =
	  ML_Funs (map (mkNonterm' (grm, pm)) nts, k)

  (* output the main parser body *)
    fun parserHook spec strm = let
          val (grm as S.Grammar {toks, nterms, startnt, sortedTops, entryPoints, ...},
	       pm) = spec
          val ppStrm = TextIOPP.openOut {dst = strm, wid = 80}
	  val entries = map NTFnName (startnt :: entryPoints)
	  val entriesVal = "val (" ^ String.concatWith ", " entries ^ ") = "
	  val innerExp = ML_Tuple (map ML_Var entries)
	  val parser = List.foldl (mkNonterms (grm, pm)) innerExp sortedTops
	  fun optParam nt = if List.null (Nonterm.formals nt) then " " else " x "
	  fun optParamFn nt = if List.null (Nonterm.formals nt) then " " else " fn x => "
	  fun wrWrapParse nt = TextIO.output (strm, String.concat [
		  "val ", NTFnName nt, " = ", optParamFn nt,
		  "fn s => unwrap (Err.launch (eh, lexFn, ",
		  NTFnName nt, optParam nt, ", ",
		  if Nonterm.same (nt, startnt) then "true" else "false",
		  ") s)\n"
		])
	  fun wrEntry (name, nt) = TextIO.output (strm, String.concat [
		  "fun ", name, " lexFn ", optParam nt,
		  "s = let ", entriesVal, "mk lexFn in ", NTFnName nt,
		  if List.null (Nonterm.formals nt) then " " else " x ",
		  "s end\n\n"
		])
          in
            TextIO.output (strm, entriesVal ^ "\n");
            ML.ppML (ppStrm, parser);
	    TextIO.output (strm, "\n");
	    app wrWrapParse (startnt::entryPoints);
            TextIO.output (strm, concat [
		"\nin (",
		String.concatWith ", " entries,
		") end\n"
	      ]);
	    TextIO.output (strm, "  in\n");
	    wrEntry ("parse", startnt);
	    app (wrEntry o (fn x => ("parse" ^ Nonterm.name x, x))) entryPoints;
	    TextIO.output (strm, "  end\n")
          end

  (* make a match function for a token *)
    fun ppMatch (strm, ppStrm) t = let
          val matchCase =
	        (ML_TupPat
		   [ML_ConPat (tokConName t,
			       if Token.hasTy t
			       then [ML_VarPat "x"]
			       else []),
		    ML_VarPat "span",
		    ML_VarPat "strm'"],
		 if Token.hasTy t
		 then ML_Tuple [ML_Var "x", ML_Var "span", ML_Var "strm'"]
		 else ML_Tuple [ML_Var "()", ML_Var "span", ML_Var "strm'"])
	  val errCase = (ML_Wild, ML_App ("fail", []))
          val exp = ML_Case (mkGet1 "strm", [matchCase, errCase])
	  in
            TextIO.output (strm, "fun " ^ tokMatch' t ^ " strm = ");
	    ML.ppML (ppStrm, exp);
	    TextIO.output (strm, "\n")
          end

  (* a token trie used to organize the preferred changes *)
    datatype trie = TR of {
	replacements : S.token list list,	(* replacements *)
	kids : (S.token * trie) list
      }

    val emptyTrie = TR{replacements=[], kids=[]}

  (* build the token trie from the list of preferred changes; we quietly
   * eliminate duplicate changes as part of this building process.
   *)
    fun mkTrie (changes : (S.token list * S.token list) list) = let
	  fun insert ((old, new), tr) = let
		fun ins ([], tr as TR{replacements, kids}) = let
		      fun same repl = ListPair.allEq Token.same (new, repl)
		      in
		        if List.exists same replacements
			  then tr
			  else TR{replacements=new::replacements, kids=kids}
		      end
		  | ins (t::toks, TR{replacements, kids}) = let
		      fun find [] = [(t, ins(toks, emptyTrie))]
			| find ((t', tr)::rest) = if Token.same(t, t')
			    then (t', ins(toks, tr)) :: rest
			    else (t', tr) :: find rest
		      in
			TR{replacements = replacements, kids = find kids}
		      end
		in
		  ins (old, tr)
		end
	  in
	    List.foldl insert emptyTrie changes
	  end

  (* output the tokens structure *)
    fun tokenStructHook spec strm = let
          val (S.Grammar{name, toks, toksImport, changes, ...}, _) = spec
          val ppStrm = TextIOPP.openOut {dst = strm, wid = 80}
	  fun pr s = TextIO.output (strm, s)
	  fun prl s = TextIO.output (strm, concat s)
	  fun prDT () = let
		val first::rest = toks
		in
		  pr "    datatype token\n";
		  prl ["      = ", Token.def first, "\n"];
		  List.app (fn tok => prl ["      | ", Token.def tok, "\n"]) rest
		end
        (* list of all nullary and default token values for error repair *)
	  val allToks = let
		fun make (tok, ts) = if Token.hasTy tok
		      then (case Token.default tok
			 of SOME arg => concat[Token.name tok, "(", arg, ")"] :: ts
			  | NONE => ts
			(* end case *))
		      else Token.name tok :: ts
		in
		  List.foldr make [] toks
		end
	(* support for toString function *)
	  fun mkMat t = (ML_TupPat [tokConPat' t], rawCode (Token.quoted t))
          val casesExp = ML_Case (ML_Var "tok", List.map mkMat toks)
        (* support for isKW function *)
	  fun mkKWMat t = (ML_TupPat [tokConPat' t],
			   ML_Var (if Token.isKW t then "true" else "false"))
	  val kwCasesExp = ML_Case (ML_Var "tok", List.map mkKWMat toks)
	(* support for the changes function *)
(*****
	  val changesDT =
		"    datatype 'strm changes\n\
		\      = CHANGE of 'strm * int * token list * ('strm -> 'strm changes)\n\
		\      | NOCHANGE;\n"
	  fun genChanges [] = TextIO.output (strm, "    fun changes _ _ = NOCHANGE\n")
	    | genChanges changes = let
		val TR{replacements, kids} = mkTrie changes
		val noKids = List.null kids
	      (* map a token to an expression that evaluates to it *)
		fun tok2exp tok = if Token.hasTy tok
		      then (case Token.default tok
			 of SOME code => ML_App(Token.name tok, [ML_Raw[Tok code]])
			  | NONE => raise Fail(concat[
				"token '", Token.name tok, "' does not have a default value"
			      ])
			(* end case *))
		      else ML_Var(Token.name tok)
	      (* generate the insertion states first *)
		fun genInsFns (idx, [], fns) = if noKids
		      then (idx+1, (s idx, ["_"], ML_Var "NOCHANGE") :: fns)
		      else (idx+1, fns)
		  | genInsFns (idx, []::reps, fns) = (* ignore [] -> [] substitution *)
		      genInsFns (idx, reps, fns)
		  | genInsFns (idx, new::reps, fns) = let
		      val f = (s idx, ["strm"], ML_App("CHANGE", [
			      ML_Var "strm", ML_Int 0,
			      ML_List(List.map tok2exp new),
			      s(idx+1)
			    ]))
		      in
			genInsFns (idx+1, reps, f::fns)
		      end
		val (idx, fns) = genInsFns (0, replacements, [])
	      (* function to handle recursive cases *)
		fun gen (idx, depth, [], fns) = (idx, fns)
		  | gen (idx, depth, kids, fns) = let
		      fun genKid ((tok, tr), (idx, kfns, fns)) = let
			    val (idx', fns) = genKidFn (idx, depth+1, tr, fns)
			    in
			      (idx', (tok, s idx)::kfns, fns)
			    end
		      val (idx, kfns, fns) = List.foldl genKid (idx, [], fns) kids
		      in
		        (idx, fns)
		      end
		val (idx, fns) = gen (idx, 0, kids, fns)
		in
		  FUN(FUNHEAD("changes", [ML_TypePat(ML_VarPat "getTok", T_FUN())]),
		    ML_Funs(List.rev fns, ML_Var(s 0)))
		end
	  fun genChanges (idx, depth, TR{replacements, kids}) = let
		val noKids = List.null kids
		fun s idx = "s" ^ Int.toString idx

		fun gen
*****)
	  in
	    prl ["structure ", name, "Tokens"];
	    case toksImport
	     of NONE => (
		  pr " =\n";
		  pr "  struct\n";
		  prDT())
	      | SOME code => (
		  pr " : sig\n";
		  prDT();
		  List.app pr [
		      "    val allToks : token list\n",
		      "    val toString : token -> bool\n",
		      "    val isKW : token -> bool\n",
		      "    val isEOF : token -> bool\n",
                      "  end = struct\n"
		    ];
		  prl ["    datatype token = datatype ", Action.code code, "\n"])
	    (* end case *);
	  (* "allToks" value *)
	    prl [
		"    val allToks = [\n            ",
		String.concatWith ", " allToks, "\n           ]\n"
	      ];
	  (* "toString" function *)
            TextIO.output (strm, "    fun toString tok =\n");
	    ML.ppML (ppStrm, casesExp);
	    pr "\n";
	  (* "isKW" function *)
            TextIO.output (strm, "    fun isKW tok =\n");
	    ML.ppML (ppStrm, kwCasesExp);
	    pr "\n";
	  (* "isEOF" function *)
	    pr "    fun isEOF EOF = true\n";
	    pr "      | isEOF _ = false\n";
	  (* preferred "changes" support *)
	    (* TODO *)
	    prl ["  end (* ", name, "Tokens *)\n"]
	  end

    fun matchfnsHook spec strm = let
          val (S.Grammar {toks, ...}, _) = spec
          val ppStrm = TextIOPP.openOut {dst = strm, wid = 80}
          in
	    app (ppMatch (strm, ppStrm)) toks
          end

  (* output header *)
    fun headerHook (grm, _) strm = (case grm
	   of S.Grammar{header = SOME h, ...} => TextIO.output (strm, h)
	    | S.Grammar{name, ...} =>
		TextIO.output (strm, String.concat [
		    "functor ", name, "ParseFn (Lex : ANTLR_LEXER)"
		  ])
	  (* end case *))

  (* output tokens module name *)
    fun tokmodHook spec strm = let
          val (S.Grammar {name, ...}, _) = spec
          in
            TextIO.output (strm, name ^ "Tokens")
          end

  (* output user definitions *)
    fun defsHook spec strm = let
          val (S.Grammar {defs, ...}, _) = spec
	  fun output ss = TextIO.output (strm, String.concat ss)
          in
            TextIO.output (strm, Action.toString defs)
          end

  (* output user actions *)
    fun actsHook spec strm = let
          val (S.Grammar {prods, refcells, ...}, _) = spec
	  fun output ss = TextIO.output (strm, String.concat ss)
	  fun actionLevel (suffix, f, isPred) prod = (case f prod
            of SOME code => output [
	         "fun ",
		 actionHeader (
		   Prod.fullName prod ^ suffix,
		   Prod.bindingsAtAction prod, "", isPred, refcells, ""),
		 " = \n  (", Action.toString code, ")",
		  (case Nonterm.ty (Prod.lhs prod)
		    of NONE => ""
		     | SOME ty => " : " ^ ty),
		 "\n"]
	     | NONE => ())
	  fun args prod (itm as S.ITEM {sym = S.NONTERM (nt, SOME code), ...}) =
	        output ["fun ",
		  actionHeader (
		    "ARGS_" ^ Action.name code,
		    Item.bindingsLeftOf (itm, prod), "", true, refcells, ""),
		  " = \n  (", Action.toString code, ")\n"]
	    | args _ _ = ()
	  fun outCell (S.REFCELL {name, initCode, ty, ...}) = TextIO.output (strm,
		String.concat [
		  "fun mk", name ^ rcSuffix, "() : (", ty, ") ref = ref (",
		  Action.toString initCode, ")\n"])
          in
	    app (actionLevel ("_ACT", Prod.action, false)) prods;
	    app (actionLevel ("_PRED", Prod.pred, true)) prods;
	    app (fn prod => app (args prod) (Prod.items prod)) prods;
	    app outCell refcells
          end

    fun ehargsHook spec strm = let
	  fun out s = TextIO.output (strm, s)
          val (S.Grammar {refcells, ...}, _) = spec
	  val names = map (fn (S.REFCELL {name, ...}) => name) refcells
	  fun prepend pre s = pre ^ s
	  fun append post s = s ^ post
	  fun mkc (S.REFCELL {name, ...}) = String.concat [
		"val ", name, rcSuffix, " = UserCode.mk", name, rcSuffix, "()\n"]
          in
            app (out o mkc) refcells;
	    out (String.concat ["fun getS() = {",
		   String.concatWith ", "
		     (map (fn nm => nm ^ " = !" ^ nm ^ rcSuffix)
			  names),
		   "}\n"]);
	    out (String.concat ["fun putS{",
		   String.concatWith ", " names,
		   "} = (",
		   String.concatWith
		     "; " (map (fn nm => nm ^ rcSuffix ^ " := " ^ nm)
			       names),
		   ")\n"]);
	    out (String.concat ["fun unwrap (ret, strm, repairs) = ",
		   "(ret, strm, repairs",
		   if List.length names > 0 then ", getS()" else "",
		   ")\n"])
          end

    fun output (grm, pm, fname) =
          ExpandFile.expandTemplate {
	      src = SMLTemplate.template,
	      dst = fname ^ ".sml",
	      hooks = [
		  ("parser",   		parserHook (grm, pm)),
		  ("token-struct",	tokenStructHook (grm, pm)),
		  ("tokmod",   		tokmodHook (grm, pm)),
		  ("header",		headerHook (grm, pm)),
		  ("usrdefs",		defsHook (grm, pm)),
		  ("actions",		actsHook (grm, pm)),
		  ("ehargs",		ehargsHook (grm, pm)),
		  ("matchfns",		matchfnsHook (grm, pm))
		]
	    }

  end
