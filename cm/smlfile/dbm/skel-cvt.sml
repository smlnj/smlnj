(* cm/smlfile/dbm/skel-cvt.sml
 *
 * COPYRIGHT (c) 2023 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Convert ASTs to CM's simplified, summary version thereof ("skeletons").
 *
 *   The ideas here are based on those found in the original SC and
 *   also in an older version of CM (before 1999).  However, nearly
 *   all aspects have been changed radically, and the code has been
 *   re-written from scratch.
 *
 *   The skeletons generated by this module are typically smaller
 *   than the "decl"s in SC or old versions of CM.  This should
 *   make dependency analysis somewhat faster (but is probably not
 *   very noticeable).
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *
 * The copyright notices of the earlier versions are:
 *   Copyright (c) 1999 by Lucent Technologies, Bell Laboratories
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *   Copyright (c) 1993 by Carnegie Mellon University,
 *                         School of Computer Science
 *                         contact: Gene Rollins (rollins+@cs.cmu.edu)
 *)

signature SKELCVT =
sig

  val convert : Ast.dec -> {skeleton : Skeleton.decl, complain : unit -> unit}

end (* signature SKELCVT *)

structure SkelCvt :> SKELCVT =
struct

local

  structure S = Symbol
  structure SP = SymPath
  structure SS = SymbolSet
  structure EM = ErrorMsg

  type symbol = Symbol.symbol
  type path = symbol list

  open Ast Skeleton (* avoid open! *)

in

    (* The main idea is to collect lists of decl ("dl"s).
     * Normally, a dl will eventually become an argument to seq or par.
     * As an important optimization, we always try to keep any "Ref s"
     * at the front (but we don't try too hard and only do it where
     * it is reasonably convenient). *)

    (* function composition suitable for fold[lr]-arguments *)
    infix o'
    fun (f o' g) (x, y) = f (g x, y)

    (* add the head of a symbol path to a given set *)
    fun s_addP ([], set) = set
      | s_addP (head :: _, set) = SS.add (set, head)

    (* same as s_addP except we ignore paths of length 1 because they
     * do not involve module access. *)
    fun s_addMP ([], set) = set		(* can this happen at all? *)
      | s_addMP ([only], set) = set	(* no module name here *)
      | s_addMP (head :: _, set) = SS.add (set, head)

    (* add a reference to a symbol to a dl *)
    fun dl_addSym (sy, []) = [Ref (SS.singleton sy)]
      | dl_addSym (sy, Ref s :: dl) = Ref (SS.add (s, sy)) :: dl
      | dl_addSym (sy, dl) = Ref (SS.singleton sy) :: dl

    (* add the first element of a path to a dl *)
    fun dl_addP ([], d) = d
      | dl_addP (head :: _, d) = dl_addSym (head, d)

    (* add the first element of a path to a dl -- except if that element is
     * the only one on the path*)
    fun dl_addMP ([], dl) = dl
      | dl_addMP ([only], dl) = dl
      | dl_addMP (head :: _, dl) = dl_addSym (head, dl)

    (* given a set of module references, add it to a decl list *)
    fun dl_addS (s, dl) =
	if SS.isEmpty s then dl
	else case dl of
	    [] => [Ref s]
	  | Ref s' :: dl' => Ref (SS.union (s, s')) :: dl'
	  | _ => Ref s :: dl

    (* make a Seq node when necessary *)
    fun seq [] = Ref SS.empty
      | seq [only] = only
      | seq l = Seq l

    (* make a Par node when necessary and stick it in front of a given dl *)
    fun parcons ([], d) = d
      | parcons ([only], d) = only :: d
      | parcons (l, d) = Par l :: d

    (* Given a "bind list", stick a parallel Bind in front of a given dl.
     * While doing so, if a Ref occured at the front of the dl, move it
     * past the bind list (shrinking it appropriately). *)
    fun parbindcons (bl, Ref s :: d) = let
	    val bs = SS.addList (SS.empty, map #1 bl)
        in
	    dl_addS (SS.difference (s, bs), parcons (map Bind bl, d))
        end
      | parbindcons (bl, d) = parcons (map Bind bl, d)

    (* split initial ref set from a decl list *)
    fun split_dl [] = (SS.empty, [])
      | split_dl (Ref s :: d) = (s, d)
      | split_dl d = (SS.empty, d)

    (* join two definition sequences *)
    fun join_dl ([], d) = d
      | join_dl ([Ref s], d) = dl_addS (s, d)
      | join_dl (h :: t, d) = h :: join_dl (t, d)

    (* local definitions *)
    fun local_dl ([], b, d) = join_dl (b, d)
      | local_dl (Ref s :: t, b, d) = dl_addS (s, local_dl (t, b, d))
      | local_dl (l, b, d) = Local (seq l, seq b) :: d

    (* build a let expression *)
    fun letexp (dl, (s, e)) =
	case split_dl dl of
	    (s', []) => (SS.union (s', s), e)
	  | (s', dl') => let
		val dl'' = if SS.isEmpty s then dl'
			   else rev (dl_addS (s, rev dl'))
	    in
		(s', Let (dl'', e))
	    end

    (* making an Ign1 where necessary ... *)
    fun ign (p1, NONE) = p1
      | ign ((s1, e1), SOME (s2, e2)) = (SS.union (s1, s2), Ign1 (e1, e2))

    (* Open cancels Decl *)
    fun open' (Decl dl, dl') = join_dl (dl, dl')
      | open' (e, dl) = Open e :: dl

    (* generate a set of "parallel" bindings *)
    fun parbind f l d = let
	val (s, bl) = foldl f (SS.empty, []) l
    in
	dl_addS (s, parbindcons (bl, d))
    end

    (* get the ref set from a type *)
    fun ty_s (VarTy _, set) = set
      | ty_s (ConTy (cn, l), set) = s_addMP (cn, foldl ty_s set l)
      | ty_s (RecordTy l, set) = foldl (ty_s o' #2) set l
      | ty_s (TupleTy l, set) = foldl ty_s set l
      | ty_s (MarkTy (arg, _), set) = ty_s (arg, set)

    (* ... from a type option *)
    fun tyopt_s (NONE, set) = set
      | tyopt_s (SOME t, set) = ty_s (t, set)

    (* ... from a pattern *)
    fun pat_s (VarPat p, set) = s_addMP (p, set)
      | pat_s (RecordPat { def, ... }, set) = foldl (pat_s o' #2) set def
      | pat_s ((ListPat l | TuplePat l | VectorPat l | OrPat l), set) =
	foldl pat_s set l
      | pat_s (FlatAppPat l, set) = foldl (pat_s o' #item) set l
      | pat_s (AppPat { constr, argument }, set) =
	pat_s (constr, pat_s (argument, set))
      | pat_s (ConstraintPat { pattern, constraint }, set) =
	pat_s (pattern, ty_s (constraint, set))
      | pat_s (LayeredPat { varPat, expPat }, set) =
	pat_s (varPat, pat_s (expPat, set))
      | pat_s (MarkPat (arg, _), set) = pat_s (arg, set)
      | pat_s ((WildPat|IntPat _|WordPat _|StringPat _|CharPat _), set) = set

    (* ... from an exception binding *)
    fun eb_s (EbGen { exn, etype }, set) = tyopt_s (etype, set)
      | eb_s (EbDef { exn, edef }, set) = s_addMP (edef, set)
      | eb_s (MarkEb (arg, _), set) = eb_s (arg, set)

    (* ... *)

    fun db_s (Db { tyc, tyvars, rhs, lazyp }, set) = foldl (tyopt_s o' #2) set rhs
      | db_s (MarkDb (arg, _), set) = db_s (arg, set)

    fun tb_s (Tb { tyc, def, tyvars }, set) = ty_s (def, set)
      | tb_s (MarkTb (arg, _), set) = tb_s (arg, set)

    (* get a dl from an expression... *)
    fun exp_dl (VarExp p, d) = dl_addMP (p, d)
      | exp_dl (FnExp rl, d) = foldr rule_dl d rl
      | exp_dl (FlatAppExp l, d) = foldr (exp_dl o' #item) d l
      | exp_dl (AppExp { function, argument }, d) =
	exp_dl (function, exp_dl (argument, d))
      | exp_dl (CaseExp { expr, rules }, d) =
	exp_dl (expr, foldr rule_dl d rules)
      | exp_dl (LetExp { dec, expr }, d) =
	local_dl (dec_dl (dec, []), exp_dl (expr, []), d)
      | exp_dl ((SeqExp l | ListExp l | TupleExp l | VectorExp l), d) =
	foldl exp_dl d l
      | exp_dl (RecordExp l, d) = foldl (exp_dl o' #2) d l
      | exp_dl (SelectorExp _, d) = d
      | exp_dl (ConstraintExp { expr, constraint }, d) =
	dl_addS (ty_s (constraint, SS.empty), exp_dl (expr, d))
      | exp_dl (HandleExp { expr, rules }, d) =
	exp_dl (expr, foldl rule_dl d rules)
      | exp_dl (RaiseExp e, d) = exp_dl (e, d)
      | exp_dl (IfExp { test, thenCase, elseCase }, d) =
	exp_dl (test, exp_dl (thenCase, exp_dl (elseCase, d)))
      | exp_dl ((AndalsoExp (e1, e2) | OrelseExp (e1, e2)), d) =
	exp_dl (e1, exp_dl (e2, d))
      | exp_dl (WhileExp { test, expr }, d) = exp_dl (test, exp_dl (expr, d))
      | exp_dl (MarkExp (arg, _), d) = exp_dl (arg, d)
      | exp_dl ((IntExp _|WordExp _|RealExp _|StringExp _|CharExp _), d) = d

    and rule_dl (Rule { pat, exp }, d) =
	dl_addS (pat_s (pat, SS.empty), exp_dl (exp, d))

    and clause_dl (Clause { pats = p, resultty = t, exp = e }, d) =
	dl_addS (foldl (pat_s o' #item) (tyopt_s (t, SS.empty)) p,
		exp_dl (e, d))

    and fb_dl (Fb (l, _), d) = foldr clause_dl d l
      | fb_dl (MarkFb (arg, _), d) = fb_dl (arg, d)

    and vb_dl (Vb { pat, exp, lazyp }, d) =
	dl_addS (pat_s (pat, SS.empty), exp_dl (exp, d))
      | vb_dl (MarkVb (arg, _), d) = vb_dl (arg, d)

    and rvb_dl (Rvb { var, exp, resultty, ... }, d) =
	dl_addS (tyopt_s (resultty, SS.empty), exp_dl (exp, d))
      | rvb_dl (MarkRvb (arg, _), d) = rvb_dl (arg, d)

    and spec_dl (MarkSpec (arg, _), d) = spec_dl (arg, d)
      | spec_dl (StrSpec l, d) = let
	    (* strange case - optional: structure, mandatory: signature *)
	    fun one ((n, g, c), (s, bl)) = let
		val (s', e) = sigexp_p g
		val s'' = SS.union (s, s')
	    in
		case c of
		    NONE => (s'', (n, e) :: bl)
		  | SOME p => (s'', (n, Ign1 (Var (SP.SPATH p), e)) :: bl)
	    end
	    val (s, bl) = foldr one (SS.empty, []) l
        in
	    dl_addS (s, parbindcons (bl, d))
        end
      | spec_dl (TycSpec (l, _), d) = let
	    fun one_s ((_, _, SOME t), s) = ty_s (t, s)
	      | one_s (_, s) = s
	in
	    dl_addS (foldl one_s SS.empty l, d)
	end
      | spec_dl (FctSpec l, d) = let
	    fun one ((n, g), (s, bl)) = let
		val (s', e) = fsigexp_p g
	    in
		(SS.union (s, s'), (n, e) :: bl)
	    end
	    val (s, bl) = foldr one (SS.empty, []) l
	in
	    dl_addS (s, parbindcons (bl, d))
	end
      | spec_dl (ValSpec l, d) = dl_addS (foldl (ty_s o' #2) SS.empty l, d)
      | spec_dl (DataSpec { datatycs, withtycs }, d) =
	dl_addS (foldl db_s (foldl tb_s SS.empty withtycs) datatycs, d)
      | spec_dl (DataReplSpec(_,cn), d) =
	dl_addS (s_addMP (cn, SS.empty), d)
      | spec_dl (ExceSpec l, d) = dl_addS (foldl (tyopt_s o' #2) SS.empty l, d)
      | spec_dl (ShareStrSpec l, d) = foldl dl_addP d l
      | spec_dl (ShareTycSpec l, d) = dl_addS (foldl s_addMP SS.empty l, d)
      | spec_dl (IncludeSpec g, d) = let
	    val (s, e) = sigexp_p g
	in
	    dl_addS (s, open' (e, d))
	end

    and sigexp_p (VarSig s) = (SS.empty, Var (SP.SPATH [s]))
      | sigexp_p (AugSig (g, whspecs)) = let
	    fun one_s (WhType (_, _, ty), s) = ty_s (ty, s)
	      | one_s (WhStruct (_, p), s) = s_addP (p, s)
	    val (s, e) = sigexp_p g
	in
	    (foldl one_s s whspecs, e)
	end
      | sigexp_p (BaseSig l) = let
	    val (s, d) = split_dl (foldr spec_dl [] l)
	in
	    (s, Decl d)
	end
      | sigexp_p (MarkSig (arg, _)) = sigexp_p arg

    and fsigexp_p (VarFsig s) = (SS.empty, Var (SP.SPATH [s]))
      | fsigexp_p (BaseFsig { param, result }) =
	letexp (foldr fparam_d [] param, sigexp_p result)
      | fsigexp_p (MarkFsig (arg, _)) = fsigexp_p arg

    and fparam_d ((nopt, g), d) = let
	val (s, e) = sigexp_p g
    in
	case nopt of
	    NONE => dl_addS (s, open' (e, d))
	  | SOME n => dl_addS (s, Bind (n, e) :: d)
    end

    and sigexpc_p NoSig = NONE
      | sigexpc_p (Transparent g | Opaque g) = SOME (sigexp_p g)

    and fsigexpc_p NoSig = NONE
      | fsigexpc_p (Transparent fg | Opaque fg) = SOME (fsigexp_p fg)

    and fctexp_p (VarFct (p, c)) =
	ign ((SS.empty, Var (SP.SPATH p)), fsigexpc_p c)
      | fctexp_p (BaseFct { params, body, constraint }) =
	letexp (foldr fparam_d [] params,
		ign (strexp_p body, sigexpc_p constraint))
      | fctexp_p (AppFct (p, l, c)) = let
	    fun one ((str, _), (s, el)) = let
		val (s', e) = strexp_p str
	    in
		(SS.union (s, s'), e :: el)
	    end
	    val (s, el) = foldl one (SS.empty, []) l
	    val (s', e) = ign ((SS.empty, Var (SP.SPATH p)), fsigexpc_p c)
	in
	    (SS.union (s, s'), foldl Ign1 e el)
	end
      | fctexp_p (LetFct (bdg, b)) = letexp (dec_dl (bdg, []), fctexp_p b)
      | fctexp_p (MarkFct (arg, _)) = fctexp_p arg

    and strexp_p (VarStr p) = (SS.empty, Var (SP.SPATH p))
      | strexp_p (BaseStr dec) = let
	    val (s, dl) = split_dl (dec_dl (dec, []))
	in
	    (s, Decl dl)
	end
      | strexp_p (ConstrainedStr (s, c)) = ign (strexp_p s, sigexpc_p c)
      | strexp_p (AppStr (p, l) | AppStrI (p, l)) = let
	    fun one ((str, _), (s, el)) = let
		val (s', e) = strexp_p str
	    in
		(SS.union (s, s'), e :: el)
	    end
	    val (s, el) = foldl one (SS.empty, []) l
	in
	    (s, foldl Ign1 (Var (SP.SPATH p)) el)
	end
      | strexp_p (LetStr (bdg, b)) = letexp (dec_dl (bdg, []), strexp_p b)
      | strexp_p (MarkStr (s, _)) = strexp_p s

    and dec_dl (ValDec (l, _), d) = foldl vb_dl d l
      | dec_dl (ValrecDec (l, _), d) = foldl rvb_dl d l
      | dec_dl (DoDec exp, d) = exp_dl (exp, d)
      | dec_dl (FunDec (l, _), d) = foldl fb_dl d l
      | dec_dl (TypeDec l, d) = dl_addS (foldl tb_s SS.empty l, d)
      | dec_dl (DatatypeDec { datatycs, withtycs }, d) =
	dl_addS (foldl db_s (foldl tb_s SS.empty withtycs) datatycs, d)
      | dec_dl (DataReplDec (_,cn), d) =
	dl_addS (s_addMP (cn, SS.empty), d)
      | dec_dl (AbstypeDec { abstycs, withtycs, body }, d) =
	dl_addS (foldl db_s (foldl tb_s SS.empty withtycs) abstycs,
		 dec_dl (body, d))
      | dec_dl (ExceptionDec l, d) = dl_addS (foldl eb_s SS.empty l, d)
      | dec_dl ((StrDec l), d) = let
	    fun one (MarkStrb (arg, _), x) = one (arg, x)
	      | one (Strb { name, def, constraint }, (s, bl)) = let
		    val (s', e) = ign (strexp_p def, sigexpc_p constraint)
		in
		    (SS.union (s, s'), (name, e) :: bl)
		end
	in
	    parbind one l d
	end
      | dec_dl (FctDec l, d) = let
	    fun one (MarkFctb (arg, _), x) = one (arg, x)
	      | one (Fctb { name, def }, (s, bl)) = let
		    val (s', e) = fctexp_p def
		in
		    (SS.union (s, s'), (name, e) :: bl)
		end
	in
	    parbind one l d
	end
      | dec_dl (SigDec l, d) = let
	    fun one (MarkSigb (arg, _), x) = one (arg, x)
	      | one (Sigb { name, def }, (s, bl)) = let
		    val (s', e) = sigexp_p def
		in
		    (SS.union (s, s'), (name, e) :: bl)
		end
	in
	    parbind one l d
	end
      | dec_dl (FsigDec l, d) = let
	    fun one (MarkFsigb (arg, _), x) = one (arg, x)
	      | one (Fsigb { name, def }, (s, bl)) = let
		    val (s', e) = fsigexp_p def
		in
		    (SS.union (s, s'), (name, e) :: bl)
		end
	in
	    parbind one l d
	end
      | dec_dl (LocalDec (bdg, body), d) =
	local_dl (dec_dl (bdg, []), dec_dl (body, []), d)
      | dec_dl (SeqDec l, d) = foldr dec_dl d l
      | dec_dl (OpenDec l, d) = parcons (map (Open o Var o SP.SPATH) l, d)
      | dec_dl (OvldDec (_, l), d) = foldl exp_dl d l
      | dec_dl (FixDec _, d) = d
      | dec_dl (MarkDec (arg, _), d) = dec_dl (arg, d)

    fun c_dec d = seq (dec_dl (d, []))

    fun convert { tree, err } =
        let
	    (* build a function that will complain (once you call it)
	     * about any existing restriction violations *)
	    fun complainCM reg =
	        let fun sameReg (LocalDec (_, body), k) = sameReg (body, k)
		      | sameReg (SeqDec l, k) = foldl sameReg k l
		      | sameReg (OpenDec _, k) =
		          (fn () => (k (); err EM.COMPLAIN reg "toplevel open"))
		      | sameReg (MarkDec (arg, reg), k) = complainCM reg (arg, k)
		      | sameReg ((StrDec _ | FctDec _ | SigDec _ | FsigDec _), k) = k
		      | sameReg (_, k) =
		         (fn () => (k (); err EM.WARN reg "definition not tracked by CM"))

		in sameReg
		end

	    fun warn0 () = ()

	    val complain = complainCM SourceMap.NULLregion (tree, warn0)

	 in { complain = complain, skeleton = c_dec tree }
	end

end (* structure SkelCvt *)
