(* Copyright 2003 by University of Chicago *)
(* src/Elaborator/print/ppast.sig *)
(* Jing Cao and Lukasz Ziarek *)

signature PPAST =

sig

   val fmtExp	    : Source.inputSource option -> Ast.exp * int       -> NewPP.format 
   val fmtPat	    : Source.inputSource option -> Ast.pat * int       -> NewPP.format
   val fmtStrExp    : Source.inputSource option -> Ast.strexp * int    -> NewPP.format
   val fmtFctExp    : Source.inputSource option -> Ast.fctexp * int    -> NewPP.format
   val fmtWhereSpec : Source.inputSource option -> Ast.wherespec * int -> NewPP.format
   val fmtSigExp    : Source.inputSource option -> Ast.sigexp * int    -> NewPP.format
   val fmtFsigExp   : Source.inputSource option -> Ast.fsigexp * int   -> NewPP.format
   val fmtSpec	    : Source.inputSource option -> Ast.spec * int      -> NewPP.format 
   val fmtDec	    : Source.inputSource option -> Ast.dec * int       -> NewPP.format
   val fmtVb   	    : Source.inputSource option -> Ast.vb * int        -> NewPP.format
   val fmtRvb  	    : Source.inputSource option -> Ast.rvb * int       -> NewPP.format
   val fmtFb	    : Source.inputSource option -> Ast.fb * int        -> NewPP.format
   val fmtClause    : Source.inputSource option -> Ast.clause * int    -> NewPP.format
   val fmtTb	    : Source.inputSource option -> Ast.tb * int        -> NewPP.format
   val fmtDb	    : Source.inputSource option -> Ast.db * int        -> NewPP.format  
   val fmtDbrhs	    : Source.inputSource option -> (Symbol.symbol * Ast.ty option) list * int -> NewPP.format
   val fmtEb	    : Source.inputSource option -> Ast.eb * int        -> NewPP.format
   val fmtStrb	    : Source.inputSource option -> Ast.strb * int      -> NewPP.format
   val fmtFctb	    : Source.inputSource option -> Ast.fctb * int      -> NewPP.format
   val fmtTyvar	    : Ast.tyvar                                        -> NewPP.format
   val fmtTy	    : Source.inputSource option -> Ast.ty * int        -> NewPP.format 

end (* signature PPAST *)
