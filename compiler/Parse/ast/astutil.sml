(* astutil.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure AstUtil : ASTUTIL =
  struct

    structure Sym = Symbol
    structure Err = ErrorMsg

    val unitPat = Ast.RecordPat{def=nil,flexibility=false}
    val unitExp = Ast.RecordExp nil
    val trueDcon = [Sym.varSymbol "true"]
    val falseDcon = [Sym.varSymbol "false"]
    val quoteDcon = [Sym.strSymbol "SMLofNJ", Sym.varSymbol "QUOTE"]
    val antiquoteDcon = [Sym.strSymbol "SMLofNJ", Sym.varSymbol "ANTIQUOTE"]
    val arrowTycon = Sym.tycSymbol "->"
    val exnID = Sym.tycSymbol "exn"
    val bogusID = Sym.varSymbol "BOGUS"
    val symArg = Sym.strSymbol "<Parameter>"
    val itsym = [Sym.varSymbol "it"]

    fun checkFix (i, err) =
	  if (i < 0) orelse (9 < i)
	    then (
	      err Err.COMPLAIN "fixity precedence must be between 0 and 9" Err.nullErrorBody;
	      9)
	    else IntInf.toInt i

    (* layered patterns *)

    fun lay3 ((x as Ast.VarPat _), y, _) = Ast.LayeredPat{varPat=x,expPat=y}
      | lay3 (Ast.ConstraintPat{pattern,constraint}, y, err) =
	     (err Err.COMPLAIN "illegal (multiple?) type constraints in AS pattern"
			   Err.nullErrorBody;
	      case lay3 (pattern,y,err)
	       of Ast.LayeredPat{varPat,expPat} =>
		 Ast.LayeredPat{varPat=varPat,
			    expPat=Ast.ConstraintPat{pattern=expPat, constraint=constraint}}
		| pat => pat)
      | lay3 (Ast.MarkPat(x,_),y, err) = lay3 (x,y,err)
      | lay3 (Ast.FlatAppPat[x],y,err) = (err Err.COMPLAIN "parentheses illegal around variable in AS pattern" Err.nullErrorBody; y)
      | lay3 (x,y,err) = (err Err.COMPLAIN "pattern to left of AS must be variable"
				Err.nullErrorBody; y)

    fun lay2 (Ast.ConstraintPat{pattern,constraint}, y, err) =
	     (err Err.COMPLAIN "illegal (multiple?) type constraints in AS pattern"
			   Err.nullErrorBody;
	      case lay2 (pattern,y,err)
	       of Ast.LayeredPat{varPat,expPat} =>
		 Ast.LayeredPat{varPat=varPat,
			    expPat=Ast.ConstraintPat{pattern=expPat,
						 constraint=constraint}}
		| pat => pat)
      | lay2 (Ast.MarkPat(x,_),y, err) = lay2 (x,y,err)
      | lay2 (Ast.FlatAppPat[{item,...}],y,err) = lay3(item,y,err)
      | lay2 p = lay3 p

    fun lay (Ast.ConstraintPat{pattern,constraint}, y, err) =
	     (case lay2 (pattern,y,err)
	       of Ast.LayeredPat{varPat,expPat} =>
		 Ast.LayeredPat{varPat=varPat,
			    expPat=Ast.ConstraintPat{pattern=expPat,
						 constraint=constraint}}
		| pat => pat)
      | lay (Ast.MarkPat(x,_),y, err) = lay (x,y,err)
      | lay p = lay2 p

    val layered = lay

    (* sequence of declarations *)
    fun makeSEQdec (Ast.SeqDec a, Ast.SeqDec b) = Ast.SeqDec(a@b)
      | makeSEQdec (Ast.SeqDec a, b) = Ast.SeqDec(a@[b])
      | makeSEQdec (a, Ast.SeqDec b) = Ast.SeqDec(a::b)
      | makeSEQdec (a,b) = Ast.SeqDec[a,b]


    fun quoteExp s = Ast.AppExp{function=Ast.VarExp quoteDcon,argument=Ast.StringExp s}
    fun antiquoteExp e = Ast.AppExp{function=Ast.VarExp antiquoteDcon,argument= e}

  end (* structure *)
