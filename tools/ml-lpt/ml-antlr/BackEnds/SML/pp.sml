(* pp.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure MLPP =
struct
  structure PP = TextIOPP
  structure ABS = MLABS

  fun ppAtom (ppStrm, a) = PP.string ppStrm (Atom.toString a)
    fun ppList {lb, rb, sep, item} (ppStrm, l) = let
	  fun pp [] = ()
	    | pp [x] = item(ppStrm, x)
	    | pp (x::r) = (item(ppStrm, x); sep ppStrm; pp r)
	  in
	    lb ppStrm; pp l; rb ppStrm
	  end

    fun ppAtomList (ppStrm, als) = ppList {
            lb = fn ppStrm => (PP.openVBox ppStrm (PP.Abs 0);
                               PP.string ppStrm "("),
	    rb = fn ppStrm => (PP.closeBox ppStrm; PP.string ppStrm ")"),
	    sep = fn ppStrm => (PP.string ppStrm ", "),
	    item = fn (ppStrm, a) => ppAtom (ppStrm, a)
	  } (ppStrm, als)

    fun ppNullaryConsList(ppStrm, als) = ppList {
            lb = fn ppStrm => (PP.openVBox ppStrm (PP.Abs 0)),
	    rb = fn ppStrm => (PP.closeBox ppStrm),
	    sep = fn ppStrm => (PP.string ppStrm ", "),
	    item = fn (ppStrm, a) => ppAtom (ppStrm, a)
	  } (ppStrm, als)

    fun ppGuid (ppStrm, guid) = PP.string ppStrm (concat["(\"", guid, "\")"])

    fun ppStringLit (ppStrm, s) = PP.string ppStrm (concat [
	    "(\"", String.toCString s, "\")"
	  ])

    fun ppTyFormals(ppStrm, als) = ppList {
            lb = fn ppStrm => (PP.openVBox ppStrm (PP.Abs 0);
                               PP.string ppStrm "["),
	    rb = fn ppStrm => (PP.closeBox ppStrm; PP.string ppStrm "] "),
	    sep = fn ppStrm => (PP.string ppStrm ", "),
	    item = fn (ppStrm, a) => ppAtom (ppStrm, a)
	  } (ppStrm, als)

    fun ppLongUid (ppStrm, uids) = ppList {
            lb = fn ppStrm => (PP.openVBox ppStrm (PP.Abs 0)),
	    rb = fn ppStrm => (PP.closeBox ppStrm),
	    sep = fn ppStrm => (PP.string ppStrm "."),
	    item = fn (ppStrm, uid) => ppAtom (ppStrm, uid)
	  } (ppStrm, uids)


    fun ppAnd f (ppStrm, (a::alist))
      = ppList {lb= fn ppStrm => (),
		rb= fn ppStrm => (),
		sep= fn ppStrm => (PP.string ppStrm "and "),
		item = f} (ppStrm, a::alist)
      | ppAnd _ _ = raise Fail "empty list"

    fun ppBar f (ppStrm, (a::alist))
      = ppList {lb= fn ppStrm => (),
		rb= fn ppStrm => (),
		sep= fn ppStrm => (PP.string ppStrm "| "),
		item = f} (ppStrm, a::alist)
      | ppBar _ _ = raise Fail "empty list"
(*
    fun ppConst (ppStrm, const)
      = case const of
	    ABS.C_INT i => PP.string ppStrm (Int.toString i)
	  | ABS.C_REAL r => PP.string ppStrm (Real.toString r)
	  | ABS.C_STRING s => PP.string ppStrm s

    fun ppBoolop (ppStrm, ABS.B_AND)
      = PP.string ppStrm "andalso"
      | ppBoolop (ppStrm, ABS.B_OR)
      = PP.string ppStrm "orelse"
*)
    fun ppType (ppStrm, ty)
      = case ty of
	    ABS.T_VAR var => ppAtom (ppStrm, var)
	  | ABS.T_TUPLE tys_nonempty
	    => ppList {lb= fn ppStrm => (),
		       rb= fn ppStrm => (),
		       sep= fn ppStrm => (PP.string ppStrm "*"),
		       item = ppType} (ppStrm, tys_nonempty)
	  | ABS.T_FUN (t1, t2) => (ppType (ppStrm, t1);
				   PP.string ppStrm "->";
				   ppType (ppStrm, t2))
	  | ABS.T_RECORD fields
	     => let fun ppField (ppStrm, (labl, ty))
		      = (ppAtom (ppStrm, labl);
			 PP.string ppStrm ":";
			 ppType (ppStrm, ty))
		in
		    ppList{lb= fn ppStrm => (PP.string ppStrm "{"),
			   rb= fn ppStrm => (PP.string ppStrm "}"),
			   sep= fn ppStrm => (PP.string ppStrm ", "),
			   item = ppField
			   } (ppStrm, fields)
		end
	  | ABS.T_PAREN t => (PP.string ppStrm "(";
			      ppType (ppStrm, t);
			      PP.string ppStrm ")")

    fun ppTypeBinding (ppStrm, ABS.TypeBind andlist)
      = let fun try (ppStrm, (varlist, Type))
	      = (ppAtomList (ppStrm, varlist);
		 PP.string ppStrm "=";
		 ppType (ppStrm, Type))
	in
	    ppAnd try (ppStrm, andlist)
	end

    fun ppDatatypeBinding (ppStrm, ABS.DatatypeBind andlist)
      = let fun trybar (ppStrm, (id, ty))
	      = (ppAtom (ppStrm, id);
		 case ty
		  of NONE => ()
		   | SOME ty => (PP.string ppStrm " of ";
				 ppType (ppStrm, ty)))

	    fun try (ppStrm, (varlist, barlist))
	      = (ppAtomList (ppStrm, varlist);
		 PP.string ppStrm "=";
		 ppBar trybar (ppStrm, barlist))
	in
	    ppAnd try (ppStrm, andlist)
	end

    fun ppSimpleDecl (ppStrm, simple_decl)
      = case simple_decl
	  of ABS.D_VAL val_andlist
	     => let
		 fun ppVal (ppStrm, (pattern, exp))
		   = (ppPat (ppStrm, pattern);
		      PP.string ppStrm "=";
		      ppExp (ppStrm, exp))
	     in
		 PP.string ppStrm "val ";
		 ppAnd ppVal (ppStrm, val_andlist)
	     end
	   | ABS.D_FUN fun_andlist
	     => (PP.string ppStrm "fun ";
		 ppAnd ppFunDecl (ppStrm, fun_andlist))
	   | ABS.D_TYPE typebinding
	     => (PP.string ppStrm "type ";
		 ppTypeBinding (ppStrm, typebinding))
	   | ABS.D_ABS_DATATYPE (dataTBind, TBindOp)
	     => (PP.string ppStrm "datatype ";
		 ppDatatypeBinding (ppStrm, dataTBind);
		 case TBindOp
		  of NONE => ()
		   | SOME b => (PP.string ppStrm "withtype ";
				ppTypeBinding (ppStrm, b)))
	   | ABS.D_ABS_TYPE (dataTBind, TBindOp, decl)
	     => (PP.string ppStrm "abstype ";
		 ppDatatypeBinding (ppStrm, dataTBind);
		 case TBindOp
		  of NONE => ()
		   | SOME b => (PP.string ppStrm "withtype ";
				ppTypeBinding (ppStrm, b));
		 PP.string ppStrm "with ";
		 ppDecl (ppStrm, decl);
		 PP.string ppStrm "end")

    and ppDecl (ppStrm, dlist_nonempty)
      =  ppList {lb= fn ppStrm => (),
			rb= fn ppStrm => (),
			sep= fn ppStrm => (),
			item = ppSimpleDecl} (ppStrm, dlist_nonempty)


    and ppMatch (ppStrm, matchlist)
      = let fun try (ppStrm, (pattern, exp))
	      = (ppPat (ppStrm, pattern);
		 PP.string ppStrm "=> ";
		 ppExp (ppStrm, exp))
	in ppBar try (ppStrm, matchlist)
	end

    and ppFunDecl (ppStrm, ABS.FUN barlist)
      = let fun trybar (ppStrm, (fun_heading, tyop, exp))
	      = (ppFunHeading (ppStrm, fun_heading);
		 case tyop of
		     NONE => ()
		   | SOME ty => (PP.string ppStrm ": ";
				 ppType (ppStrm, ty));
		 ppExp (ppStrm, exp))
	in
	    PP.string ppStrm "fun ";
	    ppBar trybar (ppStrm, barlist)
	end

    and ppFunHeading (ppStrm, ABS.FUNHEAD (s, patlist_nonempty))
      = (ppAtom(ppStrm, s);
	 ppList {lb= fn ppStrm => (),
		 rb= fn ppStrm => (),
		 sep= fn ppStrm => (PP.string ppStrm " "),
		 item = ppPat} (ppStrm, patlist_nonempty))

    and ppSigna (ppStrm, ABS.SG s)
	(* fix this *)
      = case s of
	    ABS.SP_EMPTY => ()

    and ppStruc (ppStrm, ABS.ST_STRUCT object_decl)
      = (PP.string ppStrm "struct";
	 ppObject (ppStrm, object_decl);
	 PP.string ppStrm "end")

    and ppObject (ppStrm, object_decl)
      = case object_decl
	  of ABS.DECL r => ppDecl (ppStrm, r)
	   | ABS.STRUCTURE (SOME sg, st)
	     => (PP.string ppStrm "structure :sig";
		 ppSigna (ppStrm, sg);
		 PP.string ppStrm "end = ";
		 ppStruc (ppStrm, st))
	   | ABS.STRUCTURE (NONE, st)
	     => (PP.string ppStrm "structure ";
		 ppStruc (ppStrm, st))
	   | ABS.LOCAL (o1, o2)
	     => (PP.string ppStrm "local";
		 ppObject (ppStrm, o1);
		 PP.string ppStrm "in";
		 ppObject (ppStrm, o2);
		 PP.string ppStrm "end")
		(* end case *)


    and ppFile (ppStrm, ABS.OBJECT r) = (
          PP.openHBox ppStrm;
          ppObject (ppStrm, r);
	  PP.closeBox ppStrm)

    and ppExp (ppStrm, e)
      = let
	  fun str s = PP.string ppStrm s
	  fun sp () = PP.space ppStrm 1
	  fun nl () = PP.newline ppStrm
	  fun hbox () = PP.openHBox ppStrm
	  fun vbox () = PP.openVBox ppStrm (PP.Abs 2)
	  fun close () = PP.closeBox ppStrm
	  fun letBody (true, pp)
	    = (nl();
	       str "in";
	       vbox(); nl(); pp(); close();
	       nl();
	       str "end")
	    | letBody (false, pp) = pp()
	  fun ppE (inLet, prevFn, e)
	    = (case e
		of (ABS.ML_Var x) => letBody(inLet, fn () => str x)
		 | (ABS.ML_Int n) => letBody(inLet, fn () => str(IntInf.toString n))
		 | (ABS.ML_Cmp (cop, e1, e2))
		   => letBody(inLet,
			      fn ()
				 => (
				     ppExp' e1;
				     sp();
				     str (case cop
					   of ABS.LT => "<"
					    | ABS.GT => ">"
					    | ABS.EQ => "="
					    | ABS.LEQ => "<="
					    | ABS.GEQ => ">=");
				     sp();
				     ppExp' e2))
		 | (ABS.ML_Bool (bop, e1, e2))
		   => letBody(inLet,
			      fn () => (
					ppExp' e1;
					sp();
					str (case bop
					      of ABS.AND => "andalso"
					       | ABS.OR  => "orelse");
					sp();
					ppExp' e2))
		 | (ABS.ML_Case(arg, pl)) =>
		   letBody(inLet,
			   fn () => (
				     hbox();
				     str "(case"; sp(); str "("; ppExp' arg; str ")";
				     close();
				     doCases (false, true, pl);
				     nl(); str "(* end case *))"))
		 | (ABS.ML_App(f, args)) =>
		   letBody(inLet,
			   fn () => (
				     hbox();
				     str f; str "(";
				     case args
				      of [] => ()
				       | [e] => ppExp' e
				       | (e::r) => (
						    ppExp' e; app (fn e => (str ","; sp(); ppExp' e)) r)
				     (* end case *);
				     str ")";
				     close()))
		 | (ABS.ML_If(e1, e2, e3 as ABS.ML_If _)) =>
		   letBody(inLet,
			   fn () => (
				     PP.openVBox ppStrm (PP.Abs 0);
				     vbox();
				     hbox(); str "if"; sp(); ppExp' e1; close(); nl();
				     hbox(); str "then"; sp();
				     vbox(); ppExp' e2; close();
				     close();
				     close(); nl();
				     hbox(); str "else"; sp();
				     ppExp' e3;
				     close();
				     close()))
		 | (ABS.ML_If(e1, e2, e3)) =>
		   letBody(inLet,
			   fn () => (
				     vbox();
				     hbox(); str "if"; sp(); ppExp' e1; close(); nl();
				     hbox(); str "then"; sp();
				     vbox(); ppExp' e2; close();
				     close(); nl();
				     hbox(); str "else"; sp();
				     vbox(); ppExp' e3; close();
				     close();
				     close()))
		 | (ABS.ML_Let(x, e1, e2)) => let
		       fun pp () = (
				    nl();
				    hbox();
				    str "val"; sp(); str x; sp(); str "="; sp();
				    ppExp' e1;
				    close();
				    ppE (true, false, e2))
		   in
		       if inLet
		       then pp()
		       else (
			     str "let";
			     PP.openVBox ppStrm (PP.Abs 0);
			     pp();
			     close())
		   end
		 | (ABS.ML_Funs([], e)) =>
		   ppE (inLet, false, e)
		 | (ABS.ML_Funs((f, params, body)::fs, e)) => let
		       fun pp prefix = (
					nl();
					hbox();
					str prefix; sp(); str f; sp();
					str "(";
					case params
					 of [] => ()
					  | [x] => str x
					  | (x::r) => (
						       str x; app (fn x => (str ","; sp(); str x)) r)
					(* end case *);
					str ")"; sp(); str "="; sp();
					PP.openVBox ppStrm (PP.Abs 6);
					ppExp' body;
					close();
					close();
					ppE (true, true, ABS.ML_Funs(fs, e)))
		   in
		       if inLet
		       then if prevFn then pp "and" else pp "fun"
		       else (
			     PP.openVBox ppStrm (PP.Abs 0);
			     str "let";
			     pp "fun";
			     close())
		   end
		 | (ABS.ML_Seq[]) => letBody(inLet, fn () => str "()")
		 | (ABS.ML_Seq[e]) => ppE(inLet, prevFn, e)
		 | (ABS.ML_Seq(e::r)) => let
		       fun pp () = (
				    ppExp' e;
				    app (fn e => (str ";"; sp(); ppExp' e)) r)
		   in
		       if inLet
		       then (
			     nl(); str "in";
			     PP.openBox ppStrm (PP.Abs 2);
			     nl(); pp();
			     close();
			     nl();
			     str "end")
		       else (
			     PP.openBox ppStrm (PP.Abs 0);
			     str "("; pp(); str ")";
			     close())
		   end
		 | (ABS.ML_Tuple[]) => letBody(inLet, fn () => str "()")
		 | (ABS.ML_Tuple(e::r)) =>
		   letBody (inLet, fn () => (
					     PP.openBox ppStrm (PP.Abs 2);
					     str "(";
					     ppExp' e;
					     app (fn e => (str ","; sp(); ppExp' e)) r;
					     str ")";
					     close()))
		 | (ABS.ML_List[]) => letBody(inLet, fn () => str "[]")
		 | (ABS.ML_List(e::r)) =>
		   letBody (inLet, fn () => (
					     PP.openBox ppStrm (PP.Abs 2);
					     str "[";
					     ppExp' e;
					     app (fn e => (str ","; sp(); ppExp' e)) r;
					     str "]";
					     close()))
		 | (ABS.ML_RefGet e) => letBody(inLet, fn () => (
							     str "!(";
							     ppExp' e;
							     str ")"))
		 | (ABS.ML_RefPut (e1, e2)) =>
		   letBody(inLet, fn () => (
					    ppExp' e1;
					    str " := ";
					    ppExp' e2))
		 | (ABS.ML_Raw toks) =>
		   letBody(inLet,
			   fn () => (hbox(); app (fn (ABS.Tok s) => str s) toks; close()))
		 | (ABS.ML_Handle (exp, cases)) =>
		   (
		    ppE (inLet, prevFn, exp);
		    nl(); str "handle";
		    doCases (true, true, cases))
		   (* end case *))
	  and ppExp' e = ppE (false, false, e)
	  and doCases (_,_, []) = ()
	    | doCases (isExn, isFirst, (p, e)::r) =
	      (
	       nl();
	       (* NOTE: the following seems to trigger a bug in the PP library (bad indent) *)
	       PP.openHOVBox ppStrm (PP.Abs 6);
 	       hbox();
	       if isFirst
	       then if isExn
		    then (sp(); sp(); sp())
		    else (sp(); str "of")
	       else (PP.space ppStrm 2; str "|");
	       sp();
	       ppPat (ppStrm, p); sp(); str "=>";
	       close();
	       sp();
	       hbox();
	       PP.openVBox ppStrm (PP.Abs 0);
	       ppExp' e;
	       close();
	       close();
	       close();
	       doCases (isExn, false, r))
      in
	  ppE(false, false, e)
      end

    and ppPat (ppStrm, p) = let
	fun str s = PP.string ppStrm s
	fun sp () = PP.space ppStrm 1
	fun nl () = PP.newline ppStrm
	fun hbox () = PP.openHBox ppStrm
	fun vbox () = PP.openVBox ppStrm (PP.Abs 2)
	fun close () = PP.closeBox ppStrm
	fun pp (ABS.ML_Wild) = str "_"
	  | pp (ABS.ML_VarPat x) = str x
	  | pp (ABS.ML_IntPat n) = str(IntInf.toString n)
	  | pp (ABS.ML_ConPat(c, [])) = str c
	  | pp (ABS.ML_ConPat(c, [p])) = (
				      str c; str "("; pp p; str ")")
	  | pp (ABS.ML_ConPat(c, p::r)) = (
				       str c; str "("; pp p;
				       app (fn p => (str ","; sp(); pp p)) r;
				       str ")")
	  | pp (ABS.ML_TupPat []) = str "()"
	  | pp (ABS.ML_TupPat (p::r)) = (
				     str "("; pp p;
				     app (fn p => (str ","; sp(); pp p)) r;
				     str ")")
	  | pp (ABS.ML_ListPat []) = str "[]"
	  | pp (ABS.ML_ListPat (p::r))
	    = (str "["; pp p;
	       app (fn p => (str ","; sp(); pp p)) r;
	       str "]")
	  | pp (ABS.ML_TypePat (p, ty))
	    = (pp p;
	       str ":";
	       ppType (ppStrm, ty))
    in
	hbox(); pp p; close()
    end

end
