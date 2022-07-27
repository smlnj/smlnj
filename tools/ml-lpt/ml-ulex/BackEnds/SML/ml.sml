(* ml.sml
 *
 * COPYRIGHT (c) 1999 Bell Labs, Lucent Technologies
 * (Used and modified with permission)
 * Aaron Turon (http://www.cs.uchicago.edu/~adrassi)
 *
 * ML core language representation and pretty-printing
 *)

structure ML =
  struct

    datatype raw_ml = Raw of ml_token list

    and ml_token = Tok of string

    datatype cmp_op = LT | GT | EQ | LEQ | GEQ
    datatype bool_op = AND | OR

  (* a subset of ML expressions and patterns that we use to represent the
   * match DFA
   *)
    datatype ml_exp
      = ML_Var of string
      | ML_Cmp of (cmp_op * ml_exp * ml_exp)
      | ML_Bool of (bool_op * ml_exp * ml_exp)
      | ML_Case of ml_exp * (ml_pat * ml_exp) list
      | ML_If of ml_exp * ml_exp * ml_exp
      | ML_App of (string * ml_exp list)
      | ML_Let of (string * ml_exp * ml_exp)
      | ML_Fun of (string * string list * ml_exp * ml_exp)
      | ML_Seq of ml_exp list
      | ML_Tuple of ml_exp list
      | ML_List of ml_exp list
      | ML_RefGet of ml_exp
      | ML_RefPut of ml_exp * ml_exp
      | ML_Raw of ml_token list
      | ML_NewGroup of ml_exp

    and ml_pat
      = ML_Wild
      | ML_VarPat of string
      | ML_IntPat of RegExp.Sym.point
      | ML_ConPat of string * ml_pat list

    local
      structure PP = TextIOPP
    in
    fun ppML (ppStrm, e) = let
	  fun str s = PP.string ppStrm s
	  fun sp () = PP.space ppStrm 1
	  fun nl () = PP.newline ppStrm
	  fun hbox () = PP.openHBox ppStrm
	  fun vbox () = PP.openVBox ppStrm (PP.Abs 2)
	  fun close () = PP.closeBox ppStrm
	  fun letBody (true, pp) = (
		nl();
		str "in";
		vbox(); nl(); pp(); close();
		nl();
		str "end")
	    | letBody (false, pp) = pp()
	  fun ppExp (inLet, prevFn, e) = (case e
		 of (ML_Var x) => letBody(inLet, fn () => str x)
		  | (ML_Cmp (cop, e1, e2)) => letBody(inLet, fn () => (
		      ppExp' e1;
		      sp();
		      str (case cop
			    of LT => "<"
			     | GT => ">"
			     | EQ => "="
			     | LEQ => "<="
			     | GEQ => ">=");
		      sp();
		      ppExp' e2))
		  | (ML_Bool (bop, e1, e2)) => letBody(inLet, fn () => (
		      ppExp' e1;
		      sp();
		      str (case bop
			    of AND => "andalso"
			     | OR  => "orelse");
		      sp();
		      ppExp' e2))
		  | (ML_Case(arg, pl)) => let
		      fun doCases (_, []) = ()
			| doCases (isFirst, (p, e)::r) = (
			    nl();
(* NOTE: the following seems to trigger a bug in the PP library (bad indent) *)
			    PP.openHOVBox ppStrm (PP.Abs 6);
			      hbox();
			        if isFirst
				  then (sp(); str "of")
				  else (PP.space ppStrm 2; str "|");
			        sp();
			        ppPat p; sp(); str "=>";
			      close();
			      sp();
			      hbox();
			        PP.openVBox ppStrm (PP.Abs 0);
				  ppExp' e;
			        close();
			      close();
			    close();
			    doCases (false, r))
		      in
			letBody(inLet, fn () => (
			  hbox();
			    str "(case"; sp(); str "("; ppExp' arg; str ")";
			  close();
			  doCases (true, pl);
			  nl(); str "(* end case *))"))
		      end
		  | (ML_App(f, args)) => letBody(inLet, fn () => (
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
		  | (ML_If(e1, e2, e3 as ML_If _)) => letBody(inLet, fn () => (
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
		  | (ML_If(e1, e2, e3)) => letBody(inLet, fn () => (
		      vbox();
			hbox(); str "if"; sp(); ppExp' e1; close(); nl();
			hbox(); str "then"; sp();
			  vbox(); ppExp' e2; close();
			close(); nl();
			hbox(); str "else"; sp();
			  vbox(); ppExp' e3; close();
			close();
		      close()))
		  | (ML_Let(x, e1, e2)) => let
		      fun pp () = (
			    nl();
			    hbox();
			      str "val"; sp(); str x; sp(); str "="; sp();
			      ppExp' e1;
			    close();
			    ppExp (true, false, e2))
		      in
			if inLet
			  then pp()
			  else (
			    str "let";
			    PP.openVBox ppStrm (PP.Abs 0);
			      pp();
			    close())
		      end
		  | (ML_Fun(f, params, body, e)) => let
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
			    ppExp (true, true, e))
		      in
			if inLet
			  then if prevFn then pp "and" else pp "fun"
			  else (
			    PP.openVBox ppStrm (PP.Abs 0);
			    str "let";
			    pp "fun";
			    close())
		      end
		  | (ML_Seq[]) => letBody(inLet, fn () => str "()")
		  | (ML_Seq[e]) => ppExp(inLet, prevFn, e)
		  | (ML_Seq(e::r)) => let
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
		  | (ML_Tuple[]) => letBody(inLet, fn () => str "()")
		  | (ML_Tuple(e::r)) => letBody (inLet, fn () => (
			PP.openBox ppStrm (PP.Abs 2);
			    str "(";
			    ppExp' e;
			    app (fn e => (str ","; sp(); ppExp' e)) r;
			    str ")";
			  close()))
		  | (ML_List[]) => letBody(inLet, fn () => str "[]")
		  | (ML_List(e::r)) => letBody (inLet, fn () => (
			PP.openBox ppStrm (PP.Abs 2);
			    str "[";
			    ppExp' e;
			    app (fn e => (str ","; sp(); ppExp' e)) r;
			    str "]";
			  close()))
		  | (ML_RefGet e) => letBody(inLet, fn () => (
		        str "!(";
			ppExp' e;
			str ")"))
		  | (ML_RefPut (e1, e2)) => letBody(inLet, fn () => (
		        ppExp' e1;
			str " := ";
			ppExp' e2))
		  | (ML_Raw toks) => letBody(inLet, fn () => (
		      hbox(); app (fn (Tok s) => str s) toks; close()))
		  | (ML_NewGroup e) => ppExp(inLet, false, e)
		(* end case *))
	  and ppExp' e = ppExp(false, false, e)
	  and ppPat p = let
		fun pp (ML_Wild) = str "_"
		  | pp (ML_VarPat x) = str x
		  | pp (ML_IntPat n) = str(RegExp.symToString n)
		  | pp (ML_ConPat(c, [])) = str c
		  | pp (ML_ConPat(c, [p])) = (
		      str c; str "("; pp p; str ")")
		  | pp (ML_ConPat(c, p::r)) = (
		      str c; str "("; pp p;
		      app (fn p => (str ","; pp p)) r;
		      str ")")
		in
		  hbox(); pp p; close()
		end
	  in
	    ppExp (false, false, e)
	  end
    end (* local *)

  end
