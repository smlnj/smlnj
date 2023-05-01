(* pplexp.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(* Pretty printing of plambda lexps, using the new PrettyPrint library. *)

signature PPLEXP =
sig

  val conToString : PLambda.con -> string
  val lexpTag : PLambda.lexp -> string

  val fmtLexp: int -> PLambda.lexp -> Formatting.format
  val ppLexp: int -> PLambda.lexp -> unit

end (* signature PPLEXP *)


structure PPLexp : PPLEXP =
struct

local
  structure LV = LambdaVar
  structure DA = Access
  structure S = Symbol
  structure PP = Formatting
  structure PF = PrintFormat
  structure SF = StringFormats

  fun bug s = ErrorMsg.impossible ("PPLexp: "^s)

  open PLambda

in

(* fun lexpTag : lexp -> string *)
(* for holophrastic printing constroled by print depth parameter *)
fun lexpTag (VAR _) = "VAR"
  | lexpTag (INT _) = "INT"
  | lexpTag (WORD _) = "WORD"
  | lexpTag (REAL _) = "REAL"
  | lexpTag (STRING _) = "STRING"
  | lexpTag (PRIM _) = "PRIM"
  | lexpTag (GENOP _) = "GENOP"
  | lexpTag (FN _) = "FN"
  | lexpTag (FIX _) = "FIX"
  | lexpTag (APP _) = "APP"
  | lexpTag (LET _) = "LET"
  | lexpTag (TFN _) = "TFN"
  | lexpTag (TAPP _) = "TAPP"
  | lexpTag (ETAG _) = "ETAG"
  | lexpTag (RAISE _) = "RAISE"
  | lexpTag (HANDLE _) = "HANDLE"
  | lexpTag (CON _) = "CON"
  | lexpTag (SWITCH _) = "SWITCH"
  | lexpTag (VECTOR _) = "VECTOR"
  | lexpTag (RECORD _) = "RECORD"
  | lexpTag (SRECORD _) = "SRECORD"
  | lexpTag (SELECT _) = "SELECT"
  | lexpTag (WRAP _) = "WRAP"
  | lexpTag (UNWRAP _) = "UNWRAP"

(* conToString : PLambda.con -> string *)
fun conToString (DATAcon((sym, _, _), _, lvar)) = ((S.name sym) ^ "." ^ (LV.lvarName lvar))
  | conToString (INTcon{ival, ty=0}) =
      String.concat ["(II)", IntInf.toString ival]
  | conToString (INTcon{ival, ty}) =
      String.concat ["(I", Int.toString ty, ")", IntInf.toString ival]
  | conToString (WORDcon{ival, ty}) =
      String.concat ["(W", Int.toString ty, ")", IntInf.toString ival]
  | conToString (STRINGcon s) = SF.formatString s


(* fmtLexp : int -> lexp -> format
 * holophrastic PLambda.lexp formatter, with depth limited by pd argument *)
fun fmtLexp (pd:int) (l: lexp): PP.format =
    if pd < 1 then PP.text (lexpTag l) else
    let 
        val fmtLexp' = fmtLexp (pd-1)
        val fmtLty' = PPLty.fmtLty (pd-1)
        val fmtTyc' = PPLty.fmtTyc (pd-1)
        val fmtTKind' = PPLty.fmtTKind (pd-1)

        (* fmtI : PLambda.lexp -> format *)
        fun fmtI (VAR v) = PP.text (LV.lvarName v)
	  | fmtI (INT{ival, ty=0}) = PP.hblock [PP.text "(II)", PP.text (IntInf.toString ival)]
	  | fmtI (INT{ival, ty}) =
	      PP.cblock [PP.parens (PP.cblock [PP.text "I", PP.integer ty]), PP.text (IntInf.toString ival)]
	  | fmtI (WORD{ival, ty}) =
	      PP.cblock [PP.parens (PP.cblock [PP.text "W", PP.integer ty]), PP.text (IntInf.toString ival)]
          | fmtI (REAL{rval, ty}) =
	      PP.cblock [PP.parens (PP.cblock [PP.text "R", PP.integer ty]), PP.text (RealLit.toString rval)]
          | fmtI (STRING s) = PP.string s
          | fmtI (ETAG (lexp,_)) = PP.cblock [PP.text "ETAG", PP.parens (fmtLexp' lexp)]

          | fmtI (RECORD lexps) = PP.cblock [PP.text "REC", PP.tuple (map fmtLexp' lexps)]

          | fmtI (SRECORD lexps) = PP.cblock [PP.text "SREC", PP.tuple (map fmtLexp' lexps)]

          | fmtI (VECTOR (lexps, _)) = PP.cblock [PP.text "VEC", PP.tuple (map fmtLexp' lexps)]

          | fmtI (PRIM(p,t,ts)) =
              PP.cblock
		[PP.text "PRIM",
		   PP.parens
		     (PP.pblock
			[PP.cblock [PP.text (PrimopUtil.toString p), PP.comma],
			 PP.cblock [fmtLty' t, PP.comma],
			 PP.tuple (map fmtTyc' ts)])]

          | fmtI (l as SELECT(i, _)) =
	      let fun gather(SELECT(i,l)) =
			let val (more,root) = gather l
			 in  (i :: more,root)
			end
		    | gather l = (nil, l)
		  val (path, root) = gather l
	       in PP.cblock [fmtLexp' root, PP.list (map PP.integer (rev path))]
	      end

          | fmtI (FN(v,t,body)) =
	      PP.pblock [PP.cblock [PP.text "FN(", PP.text (LV.lvarName v), PP.colon, fmtLty' t, PP.text ") => "],
		    PP.indent 4 (fmtLexp' body)]

          | fmtI (CON ((s, c, lt), ts, l)) =
	      PP.pblock
		[PP.text "CON",
		 PP.parens
		   (PP.pblock
		     [PP.parens
			(PP.pblock
			   [PP.hblock
			      [PP.cblock [PP.text (S.name s), PP.comma],
			       PP.cblock [PP.text (DA.conrepToString c), PP.comma]],
			    fmtLty' lt,
			    PP.comma]),
		      PP.cblock [PP.list (map fmtTyc' ts), PP.comma],
		      PP.indent 4 (fmtLexp' l)])]

          | fmtI (APP (FN (lvar, _, body), r)) =
              PP.cblock [PP.text "APP-", fmtLexp' (LET(lvar, r, body))]

          | fmtI (LET(v, r, l)) =
              PP.vblock 
 	        [PP.pblock
		   [PP.hblock [PP.text "LET", PP.text (LV.lvarName v), PP.text "="],
		    PP.indent 4 (fmtLexp' r)],
		 PP.indent 1 (PP.hblock [PP.text "IN", fmtLexp' l])]

          | fmtI (APP(l, r)) =
	      PP.cblock [PP.text "APP",
                    PP.parens (PP.tryFlat (PP.vblock [PP.cblock [fmtLexp' l, PP.comma], fmtLexp' r]))]

          | fmtI (TFN(ks, b)) =
	      PP.cblock [PP.text "TFN",
		    PP.parens (PP.pblock [PP.tuple (map fmtTKind' ks), PP.indent 3 (fmtLexp' b)])]

          | fmtI (TAPP(l, ts)) =
	      PP.cblock [PP.text "TAPP",
		    PP.parens (PP.pblock [fmtLexp' l, PP.tuple (map fmtTyc' ts)])]

          | fmtI (GENOP(dict, p, t, ts)) =
	      PP.cblock
		[PP.text "GENOP",
		 PP.parens (PP.pblock
			      [PP.cblock [PP.text (PrimopUtil.toString p), PP.comma],
			       PP.cblock [fmtLty' t, PP.comma],
			       PP.tuple (map fmtTyc' ts)])]

          | fmtI (SWITCH (l,_,llist,default)) =
            let fun switchCase (c,l) =
                      PP.pblock [PP.hblock [PP.text (conToString c), PP.text "=>"], PP.indent 4 (fmtLexp' l)]
		val defaultCase =
		    (case default
		      of NONE => nil
		       | SOME lexp => [PP.pblock [PP.text "_ =>", PP.indent 4 (fmtLexp' lexp)]])
             in PP.vblock
		  [PP.hblock [PP.text "SWITCH ", fmtLexp' l],
		   PP.indent 2 (PP.hblock [PP.text "of", PP.vblock (map switchCase llist @ defaultCase)])]
            end

          | fmtI (FIX (varlist, ltylist, lexplist, body)) =
            let fun ffun (v, t, l) =
                      PP.vblock [PP.hblock [PP.text (LV.lvarName v), PP.text ":", fmtLty' t, PP.text "=="],
			    PP.indent 2 (fmtLexp' l)]
             in PP.vblock [PP.hblock [PP.text "FIX",
		            PP.vblock (map ffun (List3.zip3Eq (varlist, ltylist, lexplist)))],
		      PP.hblock [PP.text "IN", fmtLexp' body]]
            end

          | fmtI (RAISE(l,t)) =
	      PP.cblock [PP.text "RAISE", 
		    PP.parens (PP.pblock [PP.cblock [fmtLty' t, PP.comma], fmtLexp' l])]

          | fmtI (HANDLE (lexp, withlexp)) =
              PP.vblock
                [PP.hblock [PP.text "HANDLE", fmtLexp' lexp],
		 PP.hblock [PP.text "WITH", fmtLexp' withlexp]]

          | fmtI (WRAP(t, _, l)) =
	      PP.cblock [PP.text "WRAP",
		    PP.parens (PP.vblock [PP.cblock [fmtTyc' t, PP.comma], fmtLexp' l])]

          | fmtI (UNWRAP(t, _, l)) =
	      PP.cblock [PP.text "UNWRAP",
		    PP.parens (PP.vblock [PP.cblock [fmtTyc' t, PP.comma], fmtLexp' l])]

   in fmtI l
  end

fun ppLexp (pd: int) (lexp : PLambda.lexp) =
    PF.printFormat (fmtLexp pd lexp)			 

end (* toplevel local *)
end (* structure PPLexp *)
