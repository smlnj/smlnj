(* Copyright 2003 by University of Chicago *)
(* src/Elaborator/print/ppast.sig *)
(* Jing Cao and Lukasz Ziarek *)

signature PPAST =

sig

   val fmtExp	    : Source.source option -> Ast.exp * int       -> NewPP.format 
   val fmtPat	    : Source.source option -> Ast.pat * int       -> NewPP.format
   val fmtStrExp    : Source.source option -> Ast.strexp * int    -> NewPP.format
   val fmtFctExp    : Source.source option -> Ast.fctexp * int    -> NewPP.format
   val fmtWhereSpec : Source.source option -> Ast.wherespec * int -> NewPP.format
   val fmtSigExp    : Source.source option -> Ast.sigexp * int    -> NewPP.format
   val fmtFsigExp   : Source.source option -> Ast.fsigexp * int   -> NewPP.format
   val fmtSpec	    : Source.source option -> Ast.spec * int      -> NewPP.format 
   val fmtDec	    : Source.source option -> Ast.dec * int       -> NewPP.format
   val fmtVb   	    : Source.source option -> Ast.vb * int        -> NewPP.format
   val fmtRvb  	    : Source.source option -> Ast.rvb * int       -> NewPP.format
   val fmtFb	    : Source.source option -> Ast.fb * int        -> NewPP.format
   val fmtClause    : Source.source option -> Ast.clause * int    -> NewPP.format
   val fmtTb	    : Source.source option -> Ast.tb * int        -> NewPP.format
   val fmtDb	    : Source.source option -> Ast.db * int        -> NewPP.format  
   val fmtDbrhs	    : Source.source option -> (Symbol.symbol * Ast.ty option) list * int -> NewPP.format
   val fmtEb	    : Source.source option -> Ast.eb * int        -> NewPP.format
   val fmtStrb	    : Source.source option -> Ast.strb * int      -> NewPP.format
   val fmtFctb	    : Source.source option -> Ast.fctb * int      -> NewPP.format
   val fmtTyvar	    : Ast.tyvar                                        -> NewPP.format
   val fmtTy	    : Source.source option -> Ast.ty * int        -> NewPP.format 

end (* signature PPAST *)
