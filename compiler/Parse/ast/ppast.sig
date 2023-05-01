(* compiler/Parse/ast/ppast.sig *)
(* Copyright 2003 by the Fellowship of SML/NJ *)
(* Jing Cao and Lukasz Ziarek, 2003; DBM 2022 *)

signature PPAST =

sig

   val fmtExp	    : Source.source option -> Ast.exp * int       -> Formatting.format 
   val fmtPat	    : Source.source option -> Ast.pat * int       -> Formatting.format
   val fmtStrExp    : Source.source option -> Ast.strexp * int    -> Formatting.format
   val fmtFctExp    : Source.source option -> Ast.fctexp * int    -> Formatting.format
   val fmtWhereSpec : Source.source option -> Ast.wherespec * int -> Formatting.format
   val fmtSigExp    : Source.source option -> Ast.sigexp * int    -> Formatting.format
   val fmtFsigExp   : Source.source option -> Ast.fsigexp * int   -> Formatting.format
   val fmtSpec	    : Source.source option -> Ast.spec * int      -> Formatting.format 
   val fmtDec	    : Source.source option -> Ast.dec * int       -> Formatting.format
   val fmtVb   	    : Source.source option -> Ast.vb * int        -> Formatting.format
   val fmtRvb  	    : Source.source option -> Ast.rvb * int       -> Formatting.format
   val fmtFb	    : Source.source option -> Ast.fb * int        -> Formatting.format
   val fmtClause    : Source.source option -> Ast.clause * int    -> Formatting.format
   val fmtTb	    : Source.source option -> Ast.tb * int        -> Formatting.format
   val fmtDb	    : Source.source option -> Ast.db * int        -> Formatting.format  
   val fmtDbrhs	    : Source.source option -> (Symbol.symbol * Ast.ty option) list * int -> Formatting.format
   val fmtEb	    : Source.source option -> Ast.eb * int        -> Formatting.format
   val fmtStrb	    : Source.source option -> Ast.strb * int      -> Formatting.format
   val fmtFctb	    : Source.source option -> Ast.fctb * int      -> Formatting.format
   val fmtTyvar	    : Ast.tyvar                                   -> Formatting.format
   val fmtTy	    : Source.source option -> Ast.ty * int        -> Formatting.format 

end (* signature PPAST *)
