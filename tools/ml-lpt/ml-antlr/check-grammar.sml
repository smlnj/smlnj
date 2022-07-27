(* check-grammar.sml
 *
 * COPYRIGHT (c) 2005
 * John Reppy (http://www.cs.uchicago.edu/~jhr)
 * Aaron Turon (http://www.cs.uchicago.edu/~adrassi)
 * All rights reserved.
 *
 * Check a parse tree, returning a grammar.
 *)

structure CheckGrammar : sig

    val check : GrammarSyntax.grammar option -> LLKSpec.grammar

  end = struct

    structure Syn = GrammarSyntax
    structure S = LLKSpec
    structure ATbl = AtomTable
    structure AMap = AtomMap
    structure ASet = AtomSet

    fun nextId (r : int ref) () = let val id = !r in r := id+1; id end

  (* auto-number the bindings for repeated variable names *)
    local
      fun addNumbers(acc, _, _, []) = rev acc
	| addNumbers(acc, n, s, s'::bs) =
	    if s = s' then
	      addNumbers((s' ^ (Int.toString n))::acc, n+1, s, bs)
	    else addNumbers(s'::acc, n, s, bs)
      fun numberBindings([]) = []
	| numberBindings(b::bs) =
	    if List.exists (fn b' => b = b') bs
	    then (b ^ "1")::numberBindings(addNumbers([], 2, b, bs))
	    else b::(numberBindings bs)
      (* assign a binding to an item *)
      fun binding tokTbl (_, Syn.SYMBOL (name, _)) = (case ATbl.find tokTbl name
	    of SOME tok => (Token.name tok, Token.hasTy tok)
	     | NONE =>     (Atom.toString name, true)
           (* end case *))
	| binding tokTbl (_, Syn.SUBRULE _)	= ("SR", true)
	| binding tokTbl (_, Syn.CLOS itm)	= binding tokTbl itm
	| binding tokTbl (_, Syn.POSCLOS itm)	= binding tokTbl itm
	| binding tokTbl (_, Syn.OPT itm)	= binding tokTbl itm
    in
    (* assign bindings to a list of items *)
    fun bindings (userNames, items, tokTbl) = let
        (* all symbols with user-assigned names should yield a result
	 * for default actions
	 *)
          val userBindings = map (Option.map (fn nm => (nm, true))) userNames
	  val autoBindings = map (binding tokTbl) items
	  val (autoNames, autoYields) = ListPair.unzip autoBindings
	  val autoNames' = numberBindings autoNames
	  val autoBindings' = ListPair.zip (autoNames', autoYields)
          in
	    ListPair.map getOpt (userBindings, autoBindings')
          end
    end

    fun flatten [] = []
      | flatten ((_, Syn.IMPORT {filename, dropping}) :: ds) = let
	  val dropSet = ASet.fromList (map (fn (_, s) => s) dropping)
	  fun keep s = not (ASet.member (dropSet, s))
	  fun shouldImport (_, Syn.KEYWORD _)		 = true
	    | shouldImport (_, Syn.VALUE _)		 = true
	    | shouldImport (_, Syn.DEFS _)		 = true
	    | shouldImport (_, Syn.TOKEN (sym, _, SOME abbrev)) =
	        keep sym andalso keep abbrev
	    | shouldImport (_, Syn.TOKEN (sym, _, NONE)) = keep sym
	    | shouldImport (_, Syn.REFCELL _)		 = true
	    | shouldImport (_, Syn.RULE {lhs, ...})	 = keep lhs
	    | shouldImport _				 = false
	  val imp = case ParseFile.parse filename
		     of SOME ds' => flatten ds'
		      | NONE => []
	  in
	    (List.filter shouldImport imp) @ flatten ds
	  end
      | flatten (d::ds) = d :: flatten ds

  (* check a GrammarSyntax.grammar value for errors, while transforming
   * it into an LLKSpec.grammar suitable for analysis and parser generation.
   *)
    fun check (SOME (g : Syn.grammar)) = let
	  val _ = Err.status "checking grammar"
	  val ds = flatten g (* flatten import directives *)
(*	  val _ = print (Syn.ppGrammar ds) *)
	(* ref cells used to incrementally build grammar representation *)
	  val nextGlobalID = nextId (ref 0)
	  val tokTbl	: S.token ATbl.hash_table = ATbl.mkTable (128, Fail "tokens table")
	  val tokList	: S.token list ref = ref []
	  val keywords	: Err.span ATbl.hash_table = ATbl.mkTable (64, Fail "keywords table")
	  val prefers   : Err.span ATbl.hash_table = ATbl.mkTable (64, Fail "prefers table")
	  val changeList : (S.token list * S.token list) list ref = ref []
	  val defaults	: (Err.span * string) ATbl.hash_table = ATbl.mkTable (32, Fail "defaults table")
	  val entryPts	: Err.span ATbl.hash_table = ATbl.mkTable (16, Fail "entryPts table")
	  val name	: (string * Err.span) option ref = ref NONE
	  val header    : Syn.code option ref = ref NONE
	  val startSym	: (Atom.atom * Err.span) option ref = ref NONE
	  val defs	: Action.action ref = ref Action.empty
	  val toksImport : Action.action option ref = ref NONE
	  val refCells	: S.refcell ATbl.hash_table = ATbl.mkTable (4, Fail "refCells table")
	  val ntTbl	: (S.nonterm * (unit -> int)) ATbl.hash_table =
				ATbl.mkTable (64, Fail "ntTbl table")
	  val ntList	: S.nonterm list ref = ref []
	  val prodList	: S.prod list ref = ref []
	(* error message for duplicate declarations *)
	  fun dupeErr (origSpan, newSpan, whatDupe) =
	        Err.spanErr (newSpan,
		  "duplicate " :: whatDupe
		  @ [", originally declared at ", Err.span2str origSpan])
	(* PHASE 1: record basic directives, checking for duplicates *)
	  fun doDecl1 (span, Syn.NAME n) = (case !name
		  of SOME (_, span') => dupeErr (span', span, ["%name declaration"])
		   | NONE => name := SOME (n, span)
		(* end case *))
	    | doDecl1 (span, Syn.HEADER h) = (case !header
		  of SOME(span', _) => dupeErr (span', span, ["%header declaration"])
		   | NONE => header := SOME h
		(* end case *))
	    | doDecl1 (span, Syn.START sym) = (case !startSym
		  of SOME (_, span') => dupeErr (span', span, ["%start declaration"])
		   | NONE => startSym := SOME (sym, span)
		(* end case *))
	    | doDecl1 (span, Syn.ENTRY sym) = (case ATbl.find entryPts sym
		  of NONE => ATbl.insert entryPts (sym, span)
		   | SOME span' => dupeErr (span', span, [
			"%entry declaration for '", Atom.toString sym, "'"
		      ])
		(* end case *))
	    | doDecl1 (span, Syn.KEYWORD sym) = (case ATbl.find keywords sym
		 of NONE => ATbl.insert keywords (sym, span)
		  | SOME span' => dupeErr (span', span, [
			"%keyword declaration for '", Atom.toString sym, "'"
		      ])
		(* end case *))
	    | doDecl1 (span, Syn.PREFER sym) = (case ATbl.find prefers sym
		 of NONE => ATbl.insert prefers (sym, span)
		  | SOME span' => dupeErr (span', span, [
			"%prefer declaration for '", Atom.toString sym, "'"
		      ])
		(* end case *))
	    | doDecl1 (span, Syn.VALUE(sym, (_, code))) = (case ATbl.find defaults sym
		 of NONE => ATbl.insert defaults (sym, (span, code))
		  | SOME(span', _) => dupeErr (span', span, [
			"%value declaration for '", Atom.toString sym, "'"
		      ])
		(* end case *))
	    | doDecl1 (span, Syn.REFCELL (name, ty, code)) = (
		case ATbl.find refCells (Atom.atom name)
		 of NONE => ATbl.insert refCells
			      (Atom.atom name,
			       S.REFCELL {
				 name = name, ty = ty,
				 initCode = Action.action code, loc = span})
		  | SOME (S.REFCELL {loc, ...}) =>
		      dupeErr (loc, span, ["%refcell declaration for '", name, "'"])
		(* end case *))
	    | doDecl1 (span, Syn.DEFS code) =
	        defs := Action.concat (!defs, Action.action code)
	    | doDecl1 (span, Syn.TOKENTYPE ty) = (case !toksImport
		 of NONE => toksImport := SOME(Action.action(span, ty))
		  | SOME act => dupeErr (span, Action.span act, ["%tokentype declaration"])
		(* end case *))
	    | doDecl1 _ = ()
	  val _ = app doDecl1 ds
	(* PHASE 2: record %tokens declarations *)
	  val kwSet = ASet.addList (ASet.empty, map (fn (x, _) => x) (ATbl.listItemsi keywords))
	  fun isKW s = ASet.member (kwSet, s)
	  fun doDecl2 (span, Syn.TOKEN (sym, tyOpt, abbrevOpt)) = (
	        case ATbl.find tokTbl sym
		 of NONE => let
		      val _ = (case abbrevOpt
			     of SOME a => (case ATbl.find tokTbl a
				   of SOME (S.T{loc, ...}) =>
					dupeErr (loc, span, [
					    "%tokens declaration with abbreviation ",
					    Atom.toString a
				       ])
				    | NONE => ()
				  (* end case *))
			      | NONE => ()
			    (* end case *))
		    (* check for default argument for non-nullary tokens *)
		      val dfltOpt = (case ATbl.find defaults sym
			     of SOME(span, code) => if Option.isSome tyOpt
				  then SOME code
				  else (
				    Err.spanErr (span, [
					"value declaration for nullary token '",
					Atom.toString sym, "'"
				      ]);
				    NONE)
			      | NONE => NONE
			    (* end case *))
		      val tok = S.T {
			      id = nextGlobalID(),
			      name = sym,
			      loc = span,
			      ty = tyOpt,
			      abbrev = abbrevOpt,
			      keyword = isKW sym orelse getOpt(Option.map isKW abbrevOpt, false),
			      default = dfltOpt
			    }
		      in
			ATbl.insert tokTbl (sym, tok);
			Option.app (fn a => ATbl.insert tokTbl (a, tok)) abbrevOpt;
			tokList := tok :: !tokList
		      end
		  | SOME (S.T{loc, ...}) =>
		      dupeErr (loc, span, ["%tokens declaration for '", Atom.toString sym, "'"])
		(* end case *))
	    | doDecl2 _ = ()
	  val _ = app doDecl2 ds
	  val _ = if List.length (!tokList) = 0 then
		    Err.errMsg ["Error: no tokens defined"]
		  else ()
	  val eofTok = S.T {
		  id = nextGlobalID(), name = Atom.atom "EOF", loc = Err.emptySpan, ty = NONE,
		  abbrev = NONE, keyword = false, default = NONE
		}
	  val _ = (ATbl.insert tokTbl (Atom.atom "EOF", eofTok);
		   tokList := eofTok :: !tokList)
	(* check for symbols in a %keyword, %prefer, %value, or %change declaration that
	 * are note defined as tokens.
	 *)
	  val tokSet = ASet.addList (ASet.empty, #1 (ListPair.unzip (ATbl.listItemsi tokTbl)))
	  fun undefErr spanOf kind name =
		Err.spanErr (spanOf name, [kind, " '", Atom.toString name, "' is not declared as a token"])
	  val _ = ASet.app (undefErr (fn name => ATbl.lookup keywords name) "keyword") (ASet.difference (kwSet, tokSet))
	  val prefSet = ASet.addList (ASet.empty, map (fn (x, _) => x) (ATbl.listItemsi prefers))
	  val _ = ASet.app (undefErr (fn name => ATbl.lookup prefers name) "preferred token") (ASet.difference (prefSet, tokSet))
	  val dfltSet = ASet.addList (ASet.empty, map (fn (x, _) => x) (ATbl.listItemsi defaults))
	  val _ = ASet.app (undefErr (fn name => #1(ATbl.lookup defaults name)) "value") (ASet.difference (dfltSet, tokSet))
	  fun doChange (span, Syn.CHANGE(fromToks, toToks)) = let
		fun toTok name = (case ATbl.find tokTbl name
		       of NONE => (
			    Err.spanErr (span, [
				"'", Atom.toString name,
				"' in %change declaration is not declared as a token"
			      ]);
			    eofTok)
			| SOME tok => tok
		      (* end case *))
		val change = (List.map toTok fromToks, List.map toTok toToks)
		in
		  changeList := change :: !changeList
		end
	    | doChange _ = ()
	  val _ = List.app doChange ds
(* FIXME: need to check that any value-carrying token that might be inserted has a default value *)
	(* PHASE 3: load nonterminals *)
	  fun insNTerm (nt as S.NT{name, ...}) = let
	        val nid = nextId (ref 1)
	        in
	          ATbl.insert ntTbl (name, (nt, nid));
		  ntList := nt :: !ntList;
		  (nt, nid)
	        end
	(* map a non-terminal name to its info record, creating a new nonterminal
	 * record if none is found.
	 *)
	  fun lookupNTerm name = (case ATbl.find ntTbl name
		 of NONE => (
		      if ASet.member (tokSet, name) then
			Err.errMsg ["Error: symbol ", Atom.toString name,
				    " defined as both a token and a nonterminal."]
		      else ();
		      insNTerm (S.NT{name = name, id = nextGlobalID(), formals = ref [], ty = ref NONE,
				     binding = S.TOP, prods = ref[], isEBNF = false, loc = ref NONE}))
		  | SOME info => info
		(* end case *))
	(* check and load a rules and type annotations *)
          fun doDecl3 (span, Syn.RULE {lhs, formals = newFormals, rhs}) = let
		val (nt as S.NT{prods, formals, loc = ntLoc, ...}, nextProdID) =
		      lookupNTerm lhs
		val nextSRID = nextId (ref 1)
		val prodName = concat [Nonterm.name nt, "_PROD_", Int.toString (nextProdID())]
             (* check the rhs, creating a production *)
		val Syn.RHS {items, action, try, predicate, loc = rhsLoc} = rhs
		val (userNames, items) = ListPair.unzip items
		val finishedRHS : S.item list ref = ref []
		val prod = S.PROD{
			     name = prodName,
			     lhs = nt,
			     rhs = finishedRHS,
			     rhsBindings = bindings (userNames, items, tokTbl),
			     try = try,
			     id = nextGlobalID(),
			     action = Option.map Action.action action,
			     pred = Option.map Action.action predicate,
			     loc = rhsLoc
			   }
		fun doPreitem (Syn.SYMBOL (name, args)) =
		      if ATbl.inDomain tokTbl name
		      then if not (isSome args)
			   then S.TOK(valOf (ATbl.find tokTbl name))
			   else (Err.errMsg ["Attempted to apply arguments to token ",
					     Atom.toString name, "."];
				 S.TOK eofTok)
		      else S.NONTERM(#1 (lookupNTerm name), Option.map Action.action args)
		  | doPreitem (Syn.SUBRULE alts) = S.NONTERM(doSubrule (false, alts), NONE)
		  | doPreitem (Syn.CLOS itm)     = S.CLOS(doSubrule (true, mkAlts itm))
		  | doPreitem (Syn.POSCLOS itm)  = S.POSCLOS(doSubrule (true, mkAlts itm))
		  | doPreitem (Syn.OPT itm)      = S.OPT(doSubrule (true, mkAlts itm))
		and doItem (span, s) = S.ITEM {sym = doPreitem s,
					       id = nextGlobalID(),
					       loc = span}
		and mkAlts (_, Syn.SUBRULE alts) = alts
		  | mkAlts (span, itm) = [Syn.RHS {
		        items = [(NONE, (span, itm))], loc = span,
		        action = NONE, try = false, predicate = NONE
		      }]
		and doSubrule (isEBNF, alts) = let
		      val prods = ref []
		      val srName = Atom.atom (concat [prodName,
				    "_SUBRULE_", Int.toString (nextSRID())])
		      val sr = S.NT{
			name = srName, formals = ref [], binding = S.WITHIN prod,
			id = nextGlobalID(), prods = prods, isEBNF = isEBNF,
			loc = ref NONE, ty = ref NONE}
		      fun altToRule alt = let
			    val Syn.RHS {loc, ...} = alt
		            in
			      (loc, Syn.RULE {lhs = srName, formals = [], rhs = alt})
		            end
		      in
		        ignore (insNTerm sr);
		        app (doDecl3 o altToRule) alts;
			sr
		      end
	        in
	          finishedRHS := map doItem items;	(* actually process the RHS *)
		  prodList := prod :: !prodList;	(* add to global prod list *)
		  prods := prod :: !prods;		(* add to lhs's prod list *)
		  formals := (map Atom.atom newFormals)	(* TODO: check for agreement *)
                end
	    | doDecl3 (span, Syn.NONTERM (nt, tyAnn)) = let
		val (S.NT{ty, ...}, _) = lookupNTerm nt
	        in case !ty
		    of NONE => ty := SOME tyAnn
		     | SOME _ => Err.spanErr (span, [
			 "duplicate type annotation for nonterminal ",
			 Atom.toString nt, "."])
	        end
	    | doDecl3 _ = ()
	  val _ = app doDecl3 ds
	  val nterms = rev(!ntList)
        (* check the grammar *)
	  val _ = if List.length (!prodList) = 0 then (
		    Err.errMsg ["Error: no rules defined."];
		    raise Err.Abort)
		  else ()
	(* check for undefined nonterminals, while reversing the order of productions *)
	  fun chkNT (S.NT{name, prods, ...}) = (case !prods
	        of [] => Err.errMsg ["Error: symbol ", Atom.toString name, " is not defined."]
		 | l => prods := List.rev l
               (* end case *))
	  val _ = app chkNT nterms
	(* note: safe to assume length nterms > 0, otherwise aborted above *)
	  fun findNT errStr (sym, span) = (case ATbl.find ntTbl sym
		of NONE => (Err.spanErr (span, ["Error: ", errStr, " symbol ",
						Atom.toString sym,
						" is not defined."]);
		            hd nterms)
		 | SOME (nt, _) => nt
	       (* end case *))
	  val startnt = (case !startSym
		 of NONE => hd nterms
		  | SOME s => findNT "%start" s
		(* end case *))
	  val entryPoints = map (findNT "%entry") (ATbl.listItemsi entryPts)
	  val sortedTops = Nonterm.topsort (startnt::entryPoints)
	  val topsSet = AtomSet.addList
			  (AtomSet.empty,
			   map (Atom.atom o Nonterm.name) (List.concat sortedTops))
	(* check that all defined nonterminals are used *)
	  fun checkNTInTops nt =
	        if Nonterm.isSubrule nt = false
		  andalso AtomSet.member (topsSet, Atom.atom (Nonterm.name nt)) = false
		then Err.warning ["Warning: nonterminal ", Nonterm.name nt,
				  " is not reachable from any entry point."]
		else ()
	  val _ = app checkNTInTops nterms
	  val _ = Err.abortIfErr()
	  in
	    S.Grammar{
		name = getOpt (Option.map #1 (!name), ""),
		header = Option.map #2 (!header),
		defs = !defs,
		toks = List.rev (!tokList),
		toksImport = !toksImport,
		changes = let (* add the %prefer tokens to the changes list *)
		  fun preferChange (tok, _, changes) =
			([], [ATbl.lookup tokTbl tok]) :: changes
		  in
		    ATbl.foldi preferChange (List.rev (!changeList)) prefers
		  end,
		nterms = nterms,
		prods = List.rev(!prodList),
		eof = eofTok,
		sortedTops = sortedTops,
		startnt = startnt,
		entryPoints = entryPoints,
		refcells = ATbl.listItems refCells
	      }
	  end
      | check NONE = raise Err.Abort

  end
