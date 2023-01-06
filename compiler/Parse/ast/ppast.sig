(* compiler/Parse/ast/ppast.sig *)
(* Copyright 2003 by the Fellowship of SML/NJ *)
(* Jing Cao and Lukasz Ziarek, 2003; DBM 2022 *)

signature PPAST =

sig

   val fmtExp	    : Source.source option -> Ast.exp * int       -> PrettyPrint.format 
   val fmtPat	    : Source.source option -> Ast.pat * int       -> PrettyPrint.format
   val fmtStrExp    : Source.source option -> Ast.strexp * int    -> PrettyPrint.format
   val fmtFctExp    : Source.source option -> Ast.fctexp * int    -> PrettyPrint.format
   val fmtWhereSpec : Source.source option -> Ast.wherespec * int -> PrettyPrint.format
   val fmtSigExp    : Source.source option -> Ast.sigexp * int    -> PrettyPrint.format
   val fmtFsigExp   : Source.source option -> Ast.fsigexp * int   -> PrettyPrint.format
   val fmtSpec	    : Source.source option -> Ast.spec * int      -> PrettyPrint.format 
   val fmtDec	    : Source.source option -> Ast.dec * int       -> PrettyPrint.format
   val fmtVb   	    : Source.source option -> Ast.vb * int        -> PrettyPrint.format
   val fmtRvb  	    : Source.source option -> Ast.rvb * int       -> PrettyPrint.format
   val fmtFb	    : Source.source option -> Ast.fb * int        -> PrettyPrint.format
   val fmtClause    : Source.source option -> Ast.clause * int    -> PrettyPrint.format
   val fmtTb	    : Source.source option -> Ast.tb * int        -> PrettyPrint.format
   val fmtDb	    : Source.source option -> Ast.db * int        -> PrettyPrint.format  
   val fmtDbrhs	    : Source.source option -> (Symbol.symbol * Ast.ty option) list * int -> PrettyPrint.format
   val fmtEb	    : Source.source option -> Ast.eb * int        -> PrettyPrint.format
   val fmtStrb	    : Source.source option -> Ast.strb * int      -> PrettyPrint.format
   val fmtFctb	    : Source.source option -> Ast.fctb * int      -> PrettyPrint.format
   val fmtTyvar	    : Ast.tyvar                                   -> PrettyPrint.format
   val fmtTy	    : Source.source option -> Ast.ty * int        -> PrettyPrint.format 

end (* signature PPAST *)
