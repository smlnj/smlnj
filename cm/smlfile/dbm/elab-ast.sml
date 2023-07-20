(* cm/smlfile/dbm/elab-ast.sml
 *
 * COPYRIGHT (c) 2023 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure CM_Elaborate =
struct

local

  structure S = Symbol
(*  structure SP = SymPath ? *)
  structure SS = SymbolSet
  structure A = Ast
  structure SK = Skeleton
(*  structure EM = ErrorMsg *)

  type symbol = S.symbol  (* = SS.item *)
  type path = symbol list  (* ASSERT: non-null *)
  type symset = SS.set
  type mexp = SK.exp
  type decls = SK.decl list
  type bind = symbol * mexp
  type bindings = bind list
  type ssexp = symset * mexp

  open A (* should avoid open! *)

  fun error (msg: string) = raise Fail msg

in


    (* The main idea is to collect lists of decl ("dl"s).
     * Normally, a dl will eventually become an argument to seq or par.
     * As an important optimization, we always try to keep any "Ref s"
     * at the front (but we don't try too hard and only do it where
     * it is reasonably convenient). *)

    (* o' : (('b * 'c) -> 'd) * ('a -> 'b) -> ('a * 'c) -> 'd) *)
    (* function composition suitable for fold[lr]-arguments *)
    infix o'
    fun (f o' g) (x, y) = f (g x, y)

    (* symsetUnion : symset list -> symset *)
    fun symsetUnion (symsets: symset list) = foldl SS.union SS.empty symsets

    (* addPathHead [s_addP] : path * symset -> symset *)
    (* add the head of a symbol path to a given symset, unconditionally.
     * This is used for paths occurring in structure (/signature) expressions,
     * where a singleton path can denote a structure (/signature). *)
    fun addPathHead ([], set) = set  (* impossible? error? *)
      | addPathHead (head :: _, set) = SS.add (set, head)

    (* addStrPathHead [s_addMP] : path * symset -> symset *)
    (* same as addPathHead except we ignore paths of length 1 because they
     * do not involve module access(?).
     * This is used for paths that occur in core exps, types, etc., where a
     * singleton path is just a (local) variable. *)
    fun addStrPathHead ([], set) = set 	(* impossible? error? *)
      | addStrPathHead ([only], set) = set (* ignore singleton path: assumed core symbol *)
      | addStrPathHead (head :: _, set) =
	  SS.add (set, head)  (* add head of path, a structure name *)

    (* dl_addSym : symbol * decls -> decls *)
    (* add a free reference to a symbol to a new or old Ref at head of decls *)
    fun dl_addSym (sym, SK.Ref syms :: decls) = SK.Ref (SS.add (syms, sym)) :: decls
      | dl_addSym (sym, decls) = SK.Ref (SS.singleton sym) :: decls

    (* dl_addP : path * decls -> decls *)
    (* add the first element of a path to a dl *)
    fun dl_addP ([], dl) = dl
      | dl_addP (head :: _, dl) = dl_addSym (head, dl)

    (* dl_addMP : path * decls -> decls *)
    (* add the first element of a path to a dl -- except if that element is
     * the only one on the path, in which case is should not be a structure symbol *)
    fun dl_addMP ([], dl) = dl
      | dl_addMP ([only], dl) = dl
      | dl_addMP (head :: _, dl) = dl_addSym (head, dl)

    (* dl_addS : symset * decl list -> decl list *)
    (* given a symset (a set of free path heads) and a decl list, 
       add the symset onto the decl list, amalgamating it with any leading Ref decl *)
    fun dl_addS (ss: symset, dl: decls) =
	if SS.isEmpty ss then dl
	else (case dl
	        of SK.Ref ss' :: dl' => SK.Ref (SS.union (ss, ss')) :: dl'
		 | _ => SK.Ref ss :: dl)

    (* seq : decl list -> decl *)
    (* make a Seq node when necessary -- only appearance of SK.Seq *)
    fun seq [] = SK.Ref SS.empty
      | seq [only] = only
      | seq dl = SK.Seq dl

    (* split_dl : decl list -> symset * decl list *)
    (* split initial ref symset from a decl list, if it starts with a Ref.
     * What happens if a Ref exists further down the decl list? Nothing. *)
    fun split_dl (SK.Ref ss :: decls) = (ss, decls)
      | split_dl decls = (SS.empty, decls)  (* no initial Ref to split off *)

    (* join_dl : decl list * decl list -> decl list *)
    (* join two decl lists.
     * This can result in a Ref decl in the middle of the decl list returned. *)
    fun join_dl ([], decls) = decls
      | join_dl ([SK.Ref s], decls) = dl_addS (s, decls)
      | join_dl (h :: t, d) = h :: join_dl (t, d)

    (* local_dl : decl list * decl list * decl list -> decl list *)
    (* local definitions *)
    fun local_dl ([], body_decls, decls) =
	  join_dl (body_decls, decls)  (* no local decls *)
      | local_dl (SK.Ref s :: rest, body, decls) =  (* local decls start with a Ref *)
	  dl_addS (s, local_dl (rest, body, decls))
      | local_dl (local_decls, body_decls, decls) =
	  SK.Local (seq local_decls, seq body_decls) :: decls

    (* letexp : decl list * (symset * mexp) -> symset * mexp *)
    (* build a let expression for letStr, letFct
     * looks like a folder function, but not used as one. *)
    fun letexp (dl, (ss, exp)) =
	  (case split_dl dl
	     of (ss', []) => (SS.union (ss', ss), exp)
	      | (ss', dl') =>
		  let val dl'' =
			  if SS.isEmpty ss
			  then dl'
			  else rev (dl_addS (ss, rev dl'))
		   in (ss', SK.Let (dl'', exp))
		  end)

    (* pair : ssexp * ssexp -> ssexp *)
    (* making a Pair *)
    fun pair ((ss1, exp1), (ss2, exp2)) = (SS.union (ss1, ss2), SK.Pair (exp1, exp2))

    (* pairOp : ssexp * ssexp option -> ssexp *)
    (* making an Pair, conditionally on the second argument *)
    fun pairOp (ssexp, NONE) = ssexp
      | pairOp (ssexp1, SOME ssexp2) = pair (ssexp1, ssexp2)

    (* open' : mexp * decls -> decls *)
    (* Open "cancels" the exp Decl constructor *)
    fun open' (SK.Decl decls, decls') = join_dl (decls, decls')
      | open' (e, dl) = SK.Open e :: dl

    (* par : decls * decls -> decls *)
    (* treat (nontrivial) decls1 as a Par decl and add to decls2 *)
    fun par ([], decls) = decls
      | par ([only], decls) = only :: decls
      | par (decls1, decls2) = SK.Par decls1 :: decls2

    (* parbindcons : bindings * decls -> decls *)
    (* Given a "bindings", stick a parallel Bind in front of decl, the given decl list.
     * While doing so, if a Ref occured at the front of the dl, move it to the front
     * past the bindings while removing the symbols bound in the binds.
     * I.e. move the decls into the scope of binds, deleting symbols that have become bound
     * from a leading Ref decl. *)
    fun parbindcons (bindings: bindings, SK.Ref ss :: decls) =
	  let val bs = SS.addList (SS.empty, map #1 bindings)
		      (* the set of "bound symbols" *)
           in dl_addS (SS.difference (ss, bs), par (map SK.Bind bindings, decls))
          end
      | parbindcons (binds, decls) = par (map SK.Bind binds, decls)

    (* parbind : [collector:]('x * (symset * bindings) -> (symset * bindings))
                 -> [xs:] 'x list
		 -> [decls:] decls
		 -> decls *)
    (* generate a set of "parallel" bindings *)
    fun parbind collector (xs: 'x list) (decls: decls) =
	let val (ss: symset, binds: bindings) =
		foldl collector (SS.empty, nil : bindings) xs
	 in dl_addS (ss, parbindcons (binds, decls))
	end

    (* tyFree : Ast.ty -> symset *)
    fun tyFree (ty : Ast.ty) =
	let (* free : Ast.ty * symset -> symset *)
	    (* (free) path heads of a type expression *)
	    fun free (VarTy _, set) = set
	      | free (ConTy (cn, l), set) = addStrPathHead (cn, foldl free set l)
	      | free (RecordTy l, set) = foldl (free o' #2) set l
	      | free (TupleTy l, set) = foldl free set l
	      | free (MarkTy (arg, _), set) = free (arg, set)
	 in free (ty, SS.empty)
	end

    (* tyOpFree : Ast.ty option -> symset *)
    (* (free) path heads of a type option (empty for NONE) *)
    fun tyOpFree NONE = SS.empty
      | tyOpFree (SOME ty) = tyFree ty

    (* patFree : Ast.pat -> symset *)
    (* (free) path heads of a pattern *)
    fun patFree (VarPat _) = SS.empty  (* arg is val symbol, not path *)
      | patFree (RecordPat { def, ... }) =
	  symsetUnion (map (fn (_, p) => patFree p) def)
      | patFree (ListPat pats | TuplePat pats | VectorPat pats | OrPat pats) =
	  symsetUnion (map patFree pats)
      | patFree (FlatAppPat pats) =
	  symsetUnion (map patFree pats)
      | patFree (AppPat { constr, argument }) =
	  addStrPathHead (constr, patFree argument)
      | patFree (ConstraintPat { pattern, constraint }) =
	  SS.union (patFree pattern, tyPatFree constraint)
      | patFree (LayeredPat { varPat, expPat }) = patFree expPat
      | patFree (MarkPat (arg, _)) = patFree arg
      | patFree (WildPat | IntPat _| WordPat _ | StringPat _ | CharPat _) = SS.empty

    (* ebFree : Ast.eb -> symset *)
    (* (free) path heads of an exception binding *)
    fun ebFree (EbGen { exn, etype }) = tyOpFree etype
      | ebFree (EbDef { exn, edef }) = s_addMP (edef, SS.empty)
      | ebFree (MarkEb (arg, _)) = ebFree arg

    (* dbFree : Ast.db -> symset *)
    (* (free) path heads of a datatype binding *)
    fun dbFree (Db { rhs, ... }) =
	  symsetUnion (map (fn (_, tyOp) => tyOpFree tyOp) rhs)
      | dbFree (MarkDb (db, _)) = dbFree db

    (* tbFree : Ast.tb -> symset *)
    (* (free) path heads of a type binding *)
    fun tbFree (Tb { def, ... }) = tyFree def
      | tbFree (MarkTb (tb, _)) = tbFree tb

    (* elabExp : env -> Ast.exp -> imports? *)
    (* imports for an expression
     * env contains (1) structure "bindings", (2) currently opened paths 
     * -- the core decs in a let dec cannot mask structure names introduced by
     *    an open dec within the let dec, but the open dec can cancel "free"
     *    occurrences of its component substructure names.
     * -- only open decs within a let dec can change the env for the body
     *    (and it matterns whether an opened path is locally defined, and
     *     whether it is "complete"? -- do we leave dealing with these
     *     distinctions to the 2nd pass?)
     *)
    fun elabExp env exp =
	let val elabExp0 : Ast.exp -> imports = elabExp env
	    (* elabExp1 : Ast.exp * imports -> imports ? *)
	    fun elabExp1 (exp, imports) = elabExp0 exp @ imports
	 in case exp
	      of VarExp path => addStrPathHead (path, env)
	       | FnExp rules => foldr (elabRule env) nil rules
	       | FlatAppExp exps => foldr elabExp1 nil exps
	       | AppExp { function, argument } =>
		   elabExp0 (function, elabExp0 argument)
	       | CaseExp { expr, rules } =>
		   elabExp1 (expr, foldr (elabRule env) nil rules)
	       | LetExp { dec, expr } =>
		   local_dl (dec_dl (dec, nil), elabExp (expr, nil), dl)
	       | (SeqExp exos | ListExp exps | TupleExp exps | VectorExp exps) =>
		   foldl elabExp1 nil exps
	       | RecordExp fields => foldl (elabExp1 o' #2) nil fields
	       | SelectorExp _ => nil
	       | ConstraintExp { expr, constraint } =>
		   dl_addS (tyFree constraint, elabExp (expr, dl))
	       | HandleExp { expr, rules } =>
		   elabExp (expr, foldr ElabRule nil rules)
	       | RaiseExp exp => elabExp0 exp
	       | IfExp { test, thenCase, elseCase } =>
	           elabExp1 (elseCase, elabExp1 (thenCase, elabExp0 test))
	       | (AndalsoExp (e1, e2) | OrelseExp (e1, e2)) =>
		   elabExp1 (e1, elabExp0 e2)
	       | WhileExp { test, expr } =>
		   elabExp1 (test, elabExp0 expr)
	       | MarkExp (exp, _) = elabExp0 exp
	       | (IntExp _|WordExp _|RealExp _|StringExp _|CharExp _), env) => nil
        end (* fun elabExp *)

    (* exp_dl : Ast.exp * decls -> decls *)
    (* get a dl from an expression... *)
    fun exp_dl (VarExp p, dl) = dl_addMP (p, dl)
      | exp_dl (FnExp rl, dl) = foldr rule_dl dl rl
      | exp_dl (FlatAppExp exps, dl) = foldr (exp_dl) dl exps
      | exp_dl (AppExp { function, argument }, dl) =
	  exp_dl (function, exp_dl (argument, dl))
      | exp_dl (CaseExp { expr, rules }, dl) =
	exp_dl (expr, foldr rule_dl dl rules)
      | exp_dl (LetExp { dec, expr }, dl) =
	  local_dl (dec_dl (dec, nil), exp_dl (expr, nil), dl)
      | exp_dl ((SeqExp exos | ListExp exps | TupleExp exps | VectorExp exps), dl) =
	  foldl exp_dl dl exps
      | exp_dl (RecordExp l, dl) = foldl (exp_dl o' #2) dl l
      | exp_dl (SelectorExp _, dl) = dl
      | exp_dl (ConstraintExp { expr, constraint }, dl) =
	  dl_addS (tyFree constraint, exp_dl (expr, dl))
      | exp_dl (HandleExp { expr, rules }, dl) =
	  exp_dl (expr, foldl rule_dl dl rules)
      | exp_dl (RaiseExp e, dl) = exp_dl (e, dl)
      | exp_dl (IfExp { test, thenCase, elseCase }, dl) =
	  exp_dl (test, exp_dl (thenCase, exp_dl (elseCase, dl)))
      | exp_dl ((AndalsoExp (e1, e2) | OrelseExp (e1, e2)), dl) =
	  exp_dl (e1, exp_dl (e2, dl))
      | exp_dl (WhileExp { test, expr }, dl) = exp_dl (test, exp_dl (expr, dl))
      | exp_dl (MarkExp (arg, _), dl) = exp_dl (arg, dl)
      | exp_dl ((IntExp _|WordExp _|RealExp _|StringExp _|CharExp _), dl) = dl

    (* rule_dl : Ast.rule * decls -> decls *)
    and rule_dl (Rule { pat, exp }, decls) =
	dl_addS (patFree pat, exp_dl (exp, decls))

    (* clause_dl : Ast.clause * decls -> decls *)
    and clause_dl (Clause {pats, resultty, exp}, decls) =
	dl_addS (symsetUnion (tyOpFree resultty :: map patFree pats),
		 exp_dl (exp, decls))

    (* fb_dl : Ast.fb * decls -> decls *)
    and fb_dl (Fb (clauses, _), decls) = foldr clause_dl decls clauses
      | fb_dl (MarkFb (arg, _), decls) = fb_dl (arg, decls)

    (* vb_dl : Ast.vb * decls -> decls *)
    and vb_dl (Vb {pat, exp, ...}, decls) =
	  dl_addS (patFree pat, exp_dl (exp, decls))
      | vb_dl (MarkVb (arg, _), decls) = vb_dl (arg, decls)

    (* rvb_dl : Ast.rvb * decls -> decls *)
    and rvb_dl (Rvb {var, exp, resultty, ...}, decls) =
	  dl_addS (tyOpFree resultty, exp_dl (exp, decls))
      | rvb_dl (MarkRvb (arg, _), decls) = rvb_dl (arg, decls)

    (* spec_dl : Ast.spec * decls -> decls *)
    and spec_dl (MarkSpec (arg, _), decls) = spec_dl (arg, decls)
      | spec_dl (StrSpec strs, decls) =
	  let (* strange case -- structure optional, signature mandatory *)
	      fun folder ((name, sigexp, pathOp), (ss, binds)) =
		    let val (ss', mexp) = sigexp_p sigexp
			val ss'' = SS.union (ss, ss')
			val bind =
			    (case pathOp
			       of NONE => (name, mexp)
			        | SOME path => (name, SK.Pair (SK.Var path, mexp)))
		     in (ss'', bind :: binds)
		    end
	      val (ss, binds) = foldr folder (SS.empty, []) strs
           in dl_addS (ss, parbindcons (binds, decls))
          end
      | spec_dl (TycSpec (l, _), decls) =
	  let fun mapper (_, _, SOME ty) = tyFree ty
		| mapper _ = SS.empty
	   in dl_addS (symsetUnion (map mapper l), decls)
	  end
      | spec_dl (FctSpec l, decls) =
	  let fun folder ((n, g), (ss, bl)) =
		    let val (ss', e) = fsigexp_p g
		     in (SS.union (ss, ss'), (n, e) :: bl)
		    end
	      val (ss, bl) = foldr folder (SS.empty, []) l
	   in dl_addS (ss, parbindcons (bl, decls))
	  end
      | spec_dl (ValSpec l, decls) =
	  dl_addS (symsetUnion (map (fn (_,ty) =>  tyFree ty) l), decls)
      | spec_dl (DataSpec { datatycs, withtycs }, decls) =
	  let val wfree = symsetUnion (map tbFree withtycs)
	      val dfree = symsetUnion (map dbFree datatycs)
	      val free = SS.union (wfree, dfree)
	   in dl_addS (free, decls)
	  end
      | spec_dl (DataReplSpec(_,cn), decls) =
	  dl_addS (s_addMP (cn, SS.empty), decls)
      | spec_dl (ExceSpec exnspecs, decls) =
	  dl_addS (symsetUnion (map (fn (_, tyOp) => tyOpFree tyOp) exnspecs), decls)
      | spec_dl (ShareStrSpec paths, decls) = (* all paths are str paths *)
	  foldl dl_addP decls paths
      | spec_dl (ShareTycSpec paths, decls) =
	  dl_addS (foldl s_addMP SS.empty paths, decls)
      | spec_dl (IncludeSpec g, decls) =
	  let val (ss, e) = sigexp_p g
	   in dl_addS (ss, open' (e, decls))
	  end

    (* sigexp_p : Ast.sigexp -> ssexp *)
    and sigexp_p (VarSig s) = (SS.empty, SK.Var [s])
      | sigexp_p (AugSig (sigexp, whspecs)) =
	  let fun whspecFree (WhType (_, _, ty)) = tyFree ty
		| whspecFree (WhStruct (_, path)_ = SS.singleton (hd path)
	      val (ss, mexp) = sigexp_p sigexp
	   in (symsetUnion (ss :: map whspecFree whspecs), mexp)
	  end
      | sigexp_p (BaseSig specs) =
	  let val (ss, decls) = split_dl (foldr spec_dl [] specs)
	   in (ss, SK.Decl decls)
	  end
      | sigexp_p (MarkSig (arg, _)) = sigexp_p arg

    (* fsigexp_p : Ast.fsigexp -> ssexp *)
    and fsigexp_p (VarFsig s) = (SS.empty, SK.Var [s])
      | fsigexp_p (BaseFsig { param, result }) =
	  letexp (foldr fparam_d [] param, sigexp_p result)
      | fsigexp_p (MarkFsig (arg, _)) = fsigexp_p arg

    (* fparam_d : (sym op * Ast.sigexp) * decls -> decls *)
    and fparam_d ((symOp, sigexp), decls) =
	  let val (ss, exp) = sigexp_p sigexp
	   in case symOp
	        of NONE => dl_addS (ss, open' (exp, decls))
		 | SOME sym => dl_addS (ss, SK.Bind (sym, exp) :: decls)
	  end

    (* sigexpc_p : Ast.sigexp Ast.sigConst -> ssexp option *)
    and sigexpc_p NoSig = NONE
      | sigexpc_p (Transparent sigexp | Opaque sigexp) = SOME (sigexp_p sigexp)

    (* sigexpc_p : Ast.fsigexp Ast.sigConst -> ssexp option *)
    and fsigexpc_p NoSig = NONE
      | fsigexpc_p (Transparent fsigexp | Opaque fsigexp) = SOME (fsigexp_p fsigexp)

    (* fctexp_p : Ast.fctexp -> ssexp *)
    and fctexp_p (VarFct (path, constraint)) =
	  pairOp ((SS.empty, SK.Var path), fsigexpc_p constraint)
      | fctexp_p (BaseFct { params, body, constraint }) =
	  letexp (foldr fparam_d [] params,
		  pairOp (strexp_p body, sigexpc_p constraint))
      | fctexp_p (AppFct (p, l, c)) =
	  let fun folder ((str, _), (s, el)) =
		    let val (s', e) = strexp_p str
		     in (SS.union (s, s'), e :: el)
		    end
	      val (s, el) = foldl folder (SS.empty, nil) l
	      val (s', e) = pairOp ((SS.empty, SK.Var p), fsigexpc_p c)
	   in (SS.union (s, s'), foldl SK.Pair e el)
	  end
      | fctexp_p (LetFct (bdg, b)) = letexp (dec_dl (bdg, nil), fctexp_p b)
      | fctexp_p (MarkFct (arg, _)) = fctexp_p arg

    (* strexp_p : Ast.strexp -> ssexp *)
    and strexp_p (VarStr path) = (SS.empty, SK.Var path)
      | strexp_p (BaseStr dec) =
	  let val (ss, decls) = split_dl (dec_dl (dec, nil))
	   in (ss, SK.Decl decls)
	  end
      | strexp_p (ConstrainedStr (strexp, constraint)) =
	  pairOp (strexp_p strexp, sigexpc_p constraint)
      | strexp_p (AppStr (fctpath, args) | AppStrI (fctpath, args)) =
	  (* reverses order of args in building Pair "list", but no matter *)
	  let fun folder ((strexp, _), (ss, mexps)) =
		    let val (ss', mexp) = strexp_p strexp
		     in (SS.union (ss, ss'), mexp :: mexps)
		    end
	      val (ss, exps) = foldl folder (SS.empty, nil) args
	   in (ss, foldl SK.Pair (SK.Var fctpath) exps)
	  end
      | strexp_p (LetStr (bdg, b)) = letexp (dec_dl (bdg, []), strexp_p b)
      | strexp_p (MarkStr (s, _)) = strexp_p s

    (* dec_dl : Ast.dec * decls -> decls *)
    and dec_dl (ValDec (vbs, _), decls) = foldl vb_dl decls vbs
      | dec_dl (ValrecDec (rvbs, _), decls) = foldl rvb_dl decls rvbs
      | dec_dl (DoDec exp, decls) = exp_dl (exp, decls)
      | dec_dl (FunDec (l, _), decls) = foldl fb_dl decls l
      | dec_dl (TypeDec l, decls) = dl_addS (foldl tbFree SS.empty l, decls)
      | dec_dl (DatatypeDec { datatycs, withtycs }, decls) =
	  dl_addS (foldl dbFree (foldl tbFree SS.empty withtycs) datatycs, decls)
      | dec_dl (DataReplDec (_,cn), decls) =
	  dl_addS (s_addMP (cn, SS.empty), decls)
      | dec_dl (AbstypeDec { abstycs, withtycs, body }, decls) =
	  dl_addS (foldl dbFree (foldl tbFree SS.empty withtycs) abstycs,
		   dec_dl (body, decls))
      | dec_dl (ExceptionDec l, decls) = dl_addS (foldl ebFree SS.empty l, decls)
      | dec_dl ((StrDec strbs), decls) =
	  let (* ss_binds : Ast.strb * (symset * bindings) -> (symset * bindings) *)
	      fun ss_binds (MarkStrb (arg, _), x) = ss_binds (arg, x)
		| ss_binds (Strb { name, def, constraint }, (ss, binds)) =
		    let val (ss', mexp) = pairOp (strexp_p def, sigexpc_p constraint)
		     in (SS.union (ss, ss'), (name, mexp) :: binds)
		    end
	   in parbind ss_binds strbs decls
	  end
      | dec_dl (FctDec l, decls) =
	  let fun ss_binds (MarkFctb (arg, _), x) = ss_binds (arg, x)
		| ss_binds (Fctb { name, def }, (s, bl)) =
		    let val (s', e) = fctexp_p def
		     in (SS.union (s, s'), (name, e) :: bl)
		    end
	   in parbind ss_binds l decls
	  end
      | dec_dl (SigDec l, decls) =
	  let fun one (MarkSigb (arg, _), x) = one (arg, x)
		| one (Sigb { name, def }, (s, bl)) =
		    let val (s', e) = sigexp_p def
		     in (SS.union (s, s'), (name, e) :: bl)
		    end
	   in parbind one l decls
	  end
      | dec_dl (FsigDec l, decls) =
	  let fun one (MarkFsigb (arg, _), x) = one (arg, x)
		| one (Fsigb { name, def }, (s, bl)) =
		    let val (s', e) = fsigexp_p def
		     in (SS.union (s, s'), (name, e) :: bl)
		    end
	   in parbind one l decls
	  end
      | dec_dl (LocalDec (bdg, body), decls) =
	  local_dl (dec_dl (bdg, []), dec_dl (body, []), decls)
      | dec_dl (SeqDec decs, decls) = foldr dec_dl decls decs
      | dec_dl (OpenDec paths, decls) = par (map (SK.Open o SK.Var) paths, decls)
      | dec_dl (OvldDec (_, l), decls) = foldl exp_dl decls l
(*      | dec_dl (FixDec _, decls) = decls *)
      | dec_dl (MarkDec (dec, _), decls) = dec_dl (dec, decls)

    (* c_dec : Ast.dec -> decl *)
    fun c_dec (dec: Ast.dec) = seq (dec_dl (dec, []))

    (* checkDec : SM.region -> Ast.dec -> (unit -> unit)? *)
    (* complain about any top-level non-module declarations *)
    fun checkDec reg =
	let fun sameReg (LocalDec (_, body), k) = sameReg (body, k)
	      | sameReg (SeqDec l, k) = foldl sameReg k l
	      | sameReg (MarkDec (arg, reg), k) = checkDec reg (arg, k) (* reset region *)
	      | sameReg ((StrDec _ | FctDec _ | SigDec _ | FsigDec _), k) = k
	      | sameReg (OpenDec _, k) =
		  (fn () => (k ();
			     (* err EM.COMPLAIN reg "toplevel open" *)
			     error "toplevel open"))
	      | sameReg (_, k) =
		 (fn () => (k ();
			    (* err EM.WARN reg "definition not tracked by CM" *)
			     error "definition not tracked by CM"))

	 in sameReg
	end

    (* convert : Ast.dec -> SK.decl *)
    val convert : Ast.dec -> SK.decl = c_dec

end (* top local *)
end (* structure CM_Elaborate *)

(* NOTES

1. deal with error detection and error messages later (e.g. for the top-level convert
function. checkDec is a version of the former "complainCM" check and error message 
generator.

2. elaboration produces two products:
   a. provisional imports (imports qualified by structure paths in scope)
   b. a structural representation of (module) declarations (CM top-level decs)

3. "opened paths" appear both
   a. as qualifiers in the (provisional) imports, and
   b. in the structure (binding) representation, where they are needed to
      determine the str component names of a structure, in case _it_ is opened
      downstream in its declaration scope.
  

*)
