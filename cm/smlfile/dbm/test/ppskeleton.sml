(* cm/smlfile/dbm/test/ppskeleton.sml *)

(* prettyprinter for skeletons *)

structure PPSkel =
struct
local
  structure SS = SymbolSet
  structure SK = Skeleton
  structure F = Formatting
  structure PF = PrintFormat
in

(* fmtPath : string list -> F.format *)
fun fmtPath (path : string list) =
    F.csequence (F.period) (map F.text path)

(* flattenExp : SK.exp -> SK.exp list *)
fun flattenExp (SK.Pair (exp1, exp2)) =
      exp1 :: flattenExp exp2
  | flattenExp exp = [exp]

fun fmtDecl (decl: SK.decl) =
    (case decl
       of SK.Bind (sym, exp) =>
	    F.hblock [F.text sym, F.equal, fmtExp exp]
	| SK.Local (decl1, decl2) => 
	    F.vblock
	      [F.text "local",
	       F.vblock
	         [fmtDecl decl1,
	          fmtDecl decl2]]
	| SK.Par decls => 
 	    F.pblock [F.text "Par",
		      F.indent 2 (F.list (map fmtDecl decls))]
	| SK.Seq decls => 
 	    F.pblock [F.text "Seq",
		      F.indent 2 (F.list (map fmtDecl decls))]
	| SK.Open exp =>
	    F.hblock [F.text "Open", fmtExp exp]
	| SK.Ref symset => 
	    F.braces (F.hsequence F.comma (map F.text (SS.toList symset))))

and fmtExp (exp: SK.exp) =
    (case exp
       of SK.Var path => fmtPath path
	| SK.Decl decls =>
	    F.hblock [F.text "Decl", F.list (map fmtDecl decls)]
	| SK.Let (decls, exp) => 
	    F.vblock
              [F.text "Let",
	       F.indent 2 (F.list (map fmtDecl decls)),
	       F.indent 2 (fmtExp exp)]
	| SK.Pair _ => 
	    F.braces (F.hsequence F.comma (map fmtExp (flattenExp exp))))

fun ppDecl (decl: SK.decl) =
    PF.printFormatNL (fmtDecl decl)

fun ppExp (exp: SK.exp) =
    PF.printFormatNL (fmtExp exp)

end (* top local *)
end (* structure PPSkel *)
