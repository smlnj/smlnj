(* src/Elaborator/print/ppast.sig *)
(* Copyright 2003 by the Fellowship of SML/NJ *)
(* Jing Cao and Lukasz Ziarek, 2003; DBM 2022 *)

signature PPAST =

sig

   val fmtExp	    : Source.source option -> Ast.exp * int       -> NewPrettyPrint.format 
   val fmtPat	    : Source.source option -> Ast.pat * int       -> NewPrettyPrint.format
   val fmtStrExp    : Source.source option -> Ast.strexp * int    -> NewPrettyPrint.format
   val fmtFctExp    : Source.source option -> Ast.fctexp * int    -> NewPrettyPrint.format
   val fmtWhereSpec : Source.source option -> Ast.wherespec * int -> NewPrettyPrint.format
   val fmtSigExp    : Source.source option -> Ast.sigexp * int    -> NewPrettyPrint.format
   val fmtFsigExp   : Source.source option -> Ast.fsigexp * int   -> NewPrettyPrint.format
   val fmtSpec	    : Source.source option -> Ast.spec * int      -> NewPrettyPrint.format 
   val fmtDec	    : Source.source option -> Ast.dec * int       -> NewPrettyPrint.format
   val fmtVb   	    : Source.source option -> Ast.vb * int        -> NewPrettyPrint.format
   val fmtRvb  	    : Source.source option -> Ast.rvb * int       -> NewPrettyPrint.format
   val fmtFb	    : Source.source option -> Ast.fb * int        -> NewPrettyPrint.format
   val fmtClause    : Source.source option -> Ast.clause * int    -> NewPrettyPrint.format
   val fmtTb	    : Source.source option -> Ast.tb * int        -> NewPrettyPrint.format
   val fmtDb	    : Source.source option -> Ast.db * int        -> NewPrettyPrint.format  
   val fmtDbrhs	    : Source.source option -> (Symbol.symbol * Ast.ty option) list * int -> NewPrettyPrint.format
   val fmtEb	    : Source.source option -> Ast.eb * int        -> NewPrettyPrint.format
   val fmtStrb	    : Source.source option -> Ast.strb * int      -> NewPrettyPrint.format
   val fmtFctb	    : Source.source option -> Ast.fctb * int      -> NewPrettyPrint.format
   val fmtTyvar	    : Ast.tyvar                                   -> NewPrettyPrint.format
   val fmtTy	    : Source.source option -> Ast.ty * int        -> NewPrettyPrint.format 

end (* signature PPAST *)
