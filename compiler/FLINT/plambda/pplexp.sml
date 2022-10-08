(* pplexp.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(* Pretty printing of plambda lexps, using the NewPP prettyprinter. *)

signature PPLEXP =
sig

  val conToString : PLambda.con -> string
  val lexpTag : PLambda.lexp -> string

  val fmtLexp: int -> PLambda.lexp -> NewPP.format
  val ppLexp: int -> PLambda.lexp -> unit

end (* signature PPLEXP *)


structure PPLexp : PPLEXP =
struct

local
  structure LV = LambdaVar
  structure A = Absyn
  structure DA = Access
  structure S = Symbol
  structure PP = NewPP  (* New Prettyprinter *)

  open PLambda NewPP

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
	  | fmtI (INT{ival, ty=0}) = hcat (text "(II)", text (IntInf.toString ival))
	  | fmtI (INT{ival, ty}) =
	      text (String.concat ["(I", Int.toString ty, ")", IntInf.toString ival])
	  | fmtI (WORD{ival, ty}) =
	      text (String.concat ["(W", Int.toString ty, ")", IntInf.toString ival])
          | fmtI (REAL{rval, ty}) =
	      text (String.concat ["(R", Int.toString ty, ")", RealLit.toString rval])
          | fmtI (STRING s) = string s
          | fmtI (ETAG (lexp,_)) =
              enclose {front = text "ETAG(", back = rparen} (fmtLexp' lexp)

          | fmtI (RECORD lexps) =
              ccat (text "REC", tuple fmtLexp' lexps)

          | fmtI (SRECORD lexps) =
              ccat (text "SREC", tuple fmtLexp' lexps)

          | fmtI (VECTOR (lexps, _)) =
              ccat (text "VEC", tuple fmtLexp' lexps)

          | fmtI (PRIM(p,t,ts)) =
              enclose {front = text "PRIM(", back = rparen}
		 (pblock
		   [ccat (text (PrimopUtil.toString p), comma),
		    ccat (fmtLty' t, comma),
		    tuple fmtTyc' ts])

          | fmtI (l as SELECT(i, _)) =
	      let fun gather(SELECT(i,l)) =
			let val (more,root) = gather l
			 in  (i :: more,root)
			end
		    | gather l = (nil, l)
		  val (path, root) = gather l
	       in ccat (fmtLexp' root, list integer (rev path))
	      end

          | fmtI (FN(v,t,body)) =
	      pcat (cblock [text "FN(", text (LV.lvarName v), colon, fmtLty' t, text ") => "],
		    softIndent 4 (fmtLexp' body))

          | fmtI (CON ((s, c, lt), ts, l)) =
	      pblock
		[text "CON",
		 parens (pblock [hcat (ccat (text (S.name s), comma),
				       ccat (text (DA.conrepToString c), comma)),
				 fmtLty' lt]),
		comma,
		list fmtTyc' ts,
		comma,
		softIndent 4 (fmtLexp' l),
		rparen]

          | fmtI (APP (FN (lvar, _, body), r)) =
            ccat
              (text "APP-",
               fmtLexp' (LET(lvar, r, body)))

          | fmtI (LET(v, r, l)) =
            vcat 
 	      (pcat
		 (hblock [text "LET", text (LV.lvarName v), text "="],
		  softIndent 4 (fmtLexp' r)),
               hardIndent 1 (hcat (text "IN", fmtLexp' l)))

          | fmtI (APP(l, r)) =
	      enclose {front = text "APP(", back = rparen}
                (tryFlat (vcat (ccat (fmtLexp' l, comma), fmtLexp' r)))

          | fmtI (TFN(ks, b)) =
              enclose {front = text "TFN(", back = rparen}
		(pcat (tuple fmtTKind' ks,
		       softIndent 3 (fmtLexp' b)))

          | fmtI (TAPP(l, ts)) =
              enclose {front=text "TAPP(", back=rparen}
	        (pcat (fmtLexp' l,
		       tuple fmtTyc' ts))

          | fmtI (GENOP(dict, p, t, ts)) =
              enclose {front=text "GEN(", back=rparen}
                (pblock
                   [ccat (text (PrimopUtil.toString p), comma),
                    ccat (fmtLty' t, comma),
                    tuple fmtTyc' ts])

          | fmtI (SWITCH (l,_,llist,default)) =
            let fun switchCase (c,l) =
                      pcat (hcat (text (conToString c), text " =>"), softIndent 4 (fmtLexp' l))
		val defaultCase =
		    (case default
		      of NONE => nil
		       | SOME lexp => [pcat (text "_ =>", softIndent 4 (fmtLexp' lexp))])
             in vblock
		  [hcat (text "SWITCH ", fmtLexp' l),
		   hardIndent 2 (hcat (text "of", vblock (map switchCase llist @ defaultCase)))]
            end

          | fmtI (FIX (varlist, ltylist, lexplist, body)) =
            let fun ffun (v, t, l) =
                      ccat (hblock [text (LV.lvarName v), text ":", fmtLty' t, text "=="],
			    hardIndent 2 (fmtLexp' l))
             in vcat (hcat (text "FIX",
		            vblock (map ffun (zipEq3 (varlist, ltylist, lexplist)))),
		      hcat (text "IN",
			    fmtLexp' body))
            end

          | fmtI (RAISE(l,t)) =
	      enclose {front = text "RAISE(", back = rparen}
                (pcat (ccat (fmtLty' t, comma), fmtLexp' l))

          | fmtI (HANDLE (lexp, withlexp)) =
            vblock
              [hcat (text "HANDLE", fmtLexp' lexp),
               hcat (text "WITH", fmtLexp' withlexp)]

          | fmtI (WRAP(t, _, l)) =
              enclose {front = text "WRAP(", back = rparen}
		(vcat
                   (ccat (fmtTyc' t, comma),
		    fmtLexp' l))

          | fmtI (UNWRAP(t, _, l)) =
              enclose {front = text "UNWRAP(", back = rparen}
		(vcat
                   (ccat (fmtTyc' t, comma),
		    fmtLexp' l))

   in fmtI l
  end

fun ppLexp (pd: int) (lexp : PLambda.lexp) =
    printFormat (fmtLexp pd lexp)			 

end (* toplevel local *)
end (* structure PPLexp *)
