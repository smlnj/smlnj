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

  val fmtLexp: int -> PLambda.lexp -> PrettyPrint.format
  val ppLexp: int -> PLambda.lexp -> unit

end (* signature PPLEXP *)


structure PPLexp : PPLEXP =
struct

local
  structure LV = LambdaVar
  structure A = Absyn
  structure DA = Access
  structure S = Symbol
  structure PP = PrettyPrint

  open PLambda PrettyPrint

  (* zipEq3: zipEq for 3 lists *)
  fun zipEq3 (x::xs, y::ys, z::zs) =
        (x, y, z) :: zipEq3 (xs, ys, zs)
    | zipEq3 (nil,nil,nil) = nil
    | zipEq3 _ = raise ListPair.UnequalLengths

  fun bug s = ErrorMsg.impossible ("PPLexp: "^s)

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
  | conToString (STRINGcon s) = PrintUtil.formatString s


(* fmtLexp : int -> lexp -> format
 * holophrastic PLambda.lexp formatter, with depth limited by pd argument *)
fun fmtLexp (pd:int) (l: lexp): format =
    if pd < 1 then text (lexpTag l) else
    let 
        val fmtLexp' = fmtLexp (pd-1)
        val fmtLty' = PPLty.fmtLty (pd-1)
        val fmtTyc' = PPLty.fmtTyc (pd-1)
        val fmtTKind' = PPLty.fmtTKind (pd-1)

        (* fmtI : PLambda.lexp -> format *)
        fun fmtI (VAR v) = text (LV.lvarName v)
	  | fmtI (INT{ival, ty=0}) = hcat [text "(II)", text (IntInf.toString ival)]
	  | fmtI (INT{ival, ty}) =
	      ccat [parens (ccat [text "I", integer ty]), text (IntInf.toString ival)]
	  | fmtI (WORD{ival, ty}) =
	      ccat [parens (ccat [text "W", integer ty]), text (IntInf.toString ival)]
          | fmtI (REAL{rval, ty}) =
	      ccat [parens (ccat [text "R", integer ty]), text (RealLit.toString rval)]
          | fmtI (STRING s) = string s
          | fmtI (ETAG (lexp,_)) = ccat [text "ETAG", parens (fmtLexp' lexp)]

          | fmtI (RECORD lexps) = ccat [text "REC", tuple (map fmtLexp' lexps)]

          | fmtI (SRECORD lexps) = ccat [text "SREC", tuple (map fmtLexp' lexps)]

          | fmtI (VECTOR (lexps, _)) = ccat [text "VEC", tuple (map fmtLexp' lexps)]

          | fmtI (PRIM(p,t,ts)) =
              ccat [text "PRIM",
		    parens
		      (pcat [ccat [text (PrimopUtil.toString p), comma],
			       ccat [fmtLty' t, comma],
			       tuple (map fmtTyc' ts)])]

          | fmtI (l as SELECT(i, _)) =
	      let fun gather(SELECT(i,l)) =
			let val (more,root) = gather l
			 in  (i :: more,root)
			end
		    | gather l = (nil, l)
		  val (path, root) = gather l
	       in ccat [fmtLexp' root, list (map integer (rev path))]
	      end

          | fmtI (FN(v,t,body)) =
	      pcat [ccat [text "FN(", text (LV.lvarName v), colon, fmtLty' t, text ") => "],
		    indent 4 (fmtLexp' body)]

          | fmtI (CON ((s, c, lt), ts, l)) =
	      pcat
		[text "CON",
		 parens (pcat [hcat [ccat [text (S.name s), comma],
				       ccat [text (DA.conrepToString c), comma]],
				 fmtLty' lt]),
		comma,
		list (map fmtTyc' ts),
		comma,
		indent 4 (fmtLexp' l),
		rparen]

          | fmtI (APP (FN (lvar, _, body), r)) =
              ccat [text "APP-", fmtLexp' (LET(lvar, r, body))]

          | fmtI (LET(v, r, l)) =
              vcat 
 	        [pcat
		   [hcat [text "LET", text (LV.lvarName v), text "="],
		    indent 4 (fmtLexp' r)],
		 indent 1 (hcat [text "IN", fmtLexp' l])]

          | fmtI (APP(l, r)) =
	      ccat [text "APP",
                    parens (tryFlat (vcat [ccat [fmtLexp' l, comma], fmtLexp' r]))]

          | fmtI (TFN(ks, b)) =
	      ccat [text "TFN",
		    parens (pcat [tuple (map fmtTKind' ks), indent 3 (fmtLexp' b)])]

          | fmtI (TAPP(l, ts)) =
	      ccat [text "TAPP",
		    parens (pcat [fmtLexp' l, tuple (map fmtTyc' ts)])]

          | fmtI (GENOP(dict, p, t, ts)) =
	      ccat [text "GENOP",
		    parens (pcat
			      [ccat [text (PrimopUtil.toString p), comma],
			       ccat [fmtLty' t, comma],
			       tuple (map fmtTyc' ts)])]

          | fmtI (SWITCH (l,_,llist,default)) =
            let fun switchCase (c,l) =
                      pcat [hcat [text (conToString c), text "=>"], indent 4 (fmtLexp' l)]
		val defaultCase =
		    (case default
		      of NONE => nil
		       | SOME lexp => [pcat [text "_ =>", indent 4 (fmtLexp' lexp)]])
             in vcat
		  [hcat [text "SWITCH ", fmtLexp' l],
		   indent 2 (hcat (text "of", vcat (map switchCase llist @ defaultCase)))]
            end

          | fmtI (FIX (varlist, ltylist, lexplist, body)) =
            let fun ffun (v, t, l) =
                      vcat [hcat [text (LV.lvarName v), text ":", fmtLty' t, text "=="],
			    indent 2 (fmtLexp' l)]
             in vcat [hcat [text "FIX",
		            vcat (map ffun (zipEq3 (varlist, ltylist, lexplist)))],
		      hcat [text "IN", fmtLexp' body]]
            end

          | fmtI (RAISE(l,t)) =
	      ccat [text "RAISE", 
		    parens (pcat [ccat [fmtLty' t, comma], fmtLexp' l])

          | fmtI (HANDLE (lexp, withlexp)) =
              vcat
                [hcat [text "HANDLE", fmtLexp' lexp],
		 hcat [text "WITH", fmtLexp' withlexp]]

          | fmtI (WRAP(t, _, l)) =
	      ccat [text "WRAP",
		    parens (vcat [ccat [fmtTyc' t, comma], fmtLexp' l])]

          | fmtI (UNWRAP(t, _, l)) =
	      ccat [text "UNWRAP",
		    parens (vcat [ccat [fmtTyc' t, comma], fmtLexp' l])]

   in fmtI l
  end

fun ppLexp (pd: int) (lexp : PLambda.lexp) =
    printFormat (fmtLexp pd lexp)			 

end (* toplevel local *)
end (* structure PPLexp *)
