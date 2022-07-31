(* sexp-pp.sml
 *
 * COPYRIGHT (c) 2022 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * A pretty printer for SExp values.
 *)

structure SExpPP : sig

    val output : TextIOPP.stream * SExp.value -> unit

  end = struct

    structure S = SExp
    structure PP = TextIOPP
    structure F = Format

    fun output (strm, sexp) = let
	  val str = PP.string strm
	  fun sp () = PP.space strm 1
	  fun ppList [] = str "()"
	    | ppList [v] = (
		PP.openHBox strm;
		  str "("; ppVal v; str ")";
		PP.closeBox strm)
	    | ppList (v1::v2::vr) = (
		PP.openHBox strm;
		  str "("; ppVal v1; sp();
		  PP.openHOVBox strm (PP.Rel 0);
		    ppVal v2;
		    List.app (fn v => (sp(); ppVal v)) vr;
		    str ")";
		  PP.closeBox strm;
		PP.closeBox strm)
	  and ppVal (S.SYMBOL value) = str (Atom.toString value)
	    | ppVal (S.BOOL value) = str (if value then "#t" else "#f")
	    | ppVal (S.INT value) = str (F.format "%d" [F.LINT value])
	    | ppVal (S.FLOAT value) = str (F.format "%g" [F.REAL value])
	    | ppVal (S.STRING value) = str (SExpStringUtil.toString value)
	    | ppVal (S.QUOTE value) = (str "'"; ppVal value)
	    | ppVal (S.LIST values) = ppList values
	  in
	    PP.openVBox strm (PP.Abs 0);
	      ppVal sexp;
	      PP.newline strm;
	    PP.closeBox strm
	  end

  end
