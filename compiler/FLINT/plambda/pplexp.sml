(* pplexp.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(* _Real_ pretty printing for plambda lexp *)

signature PPLEXP =
sig

  val conToString : PLambda.con -> string
  val ppLexp : int -> PrettyPrint.stream -> PLambda.lexp -> unit
  val ppFun : PrettyPrint.stream -> PLambda.lexp -> LambdaVar.lvar -> unit

  val stringTag : PLambda.lexp -> string

end (* signature PPLEXP *)


structure PPLexp : PPLEXP =
struct

local structure A = Absyn
      structure DA = Access
      structure S = Symbol
      structure PP = PrettyPrint
      structure PU = PPUtil
      open PLambda
in

fun bug s = ErrorMsg.impossible ("PPLexp: "^s)

val lvarName = LambdaVar.lvarName

fun app2(f, [], []) = ()
  | app2(f, a::r, b::z) = (f(a, b); app2(f, r, z))
  | app2(f, _, _) = bug "unexpected list arguments in function app2"

fun conToString (DATAcon((sym, _, _), _, v)) = ((S.name sym) ^ "." ^ (lvarName v))
  | conToString (INTcon{ival, ty=0}) =
      concat["(II)", IntInf.toString ival]
  | conToString (INTcon{ival, ty}) =
      concat["(I", Int.toString ty, ")", IntInf.toString ival]
  | conToString (WORDcon{ival, ty}) =
      concat["(W", Int.toString ty, ")", IntInf.toString ival]
  | conToString (STRINGcon s) = PrintUtil.formatString s
(*  | conToString (VLENcon n) = Int.toString n *)

(* complex : lexp -> bool *)
(* An lexp is "complex" if it contains a subexpression of form
 * FIX, LET, TAPP, GENOP, SWITCH, CON, or HANDLE.
 * Use of complex in ppLexp may lead to stupid n^2 behavior. *)
fun complex (le: lexp) : bool =
    case le
     of FN(_, _, b) => complex b
      | FIX(vl, _, ll, b) => true
      | APP(FN _, _) => true
      | APP(l, r) => complex l orelse complex r
      | LET _ => true
      | TFN(_, b) => complex b
      | TAPP(l, []) => complex l
      | TAPP(l, _) => true
      | GENOP(_,_,_,_) => true

      | (RECORD l | SRECORD l | VECTOR(l, _)) => List.exists complex l
      | SELECT(_, l) => complex l

      | SWITCH _ => true
      | CON(_, _, l) => true

      | HANDLE _ => true
      | RAISE(l, _) => complex l

      | WRAP(_, _, l) => complex l
      | UNWRAP(_, _, l) => complex l
      | _ => false

fun ppLexp (pd:int) ppstrm (l: lexp): unit =
    let val {openHOVBox, openHVBox, closeBox, break, newline, pps, ppi, ...} =
            PU.en_pp ppstrm

        val ppLexp' = ppLexp (pd-1) ppstrm
        val ppLty' = PPLty.ppLty (pd-1) ppstrm
        val ppTyc' = PPLty.ppTyc (pd-1) ppstrm
        fun br0 n = PP.break ppstrm {nsp=0,offset=n}
        fun br1 n = PP.break ppstrm {nsp=1,offset=n}
        fun br(n,m) = PP.break ppstrm {nsp=n,offset=m}
        fun ppClosedSeq (start,sep,close) ppfn elems =
            PU.ppClosedSequence ppstrm
              {front = (fn s => PP.string s start),
               back = (fn s => PP.string s close),
               sep = PPUtil.sepWithCut sep,
               pr = ppfn,
               style = PU.INCONSISTENT}
              elems

        fun ppl pd (VAR v) = pps (lvarName v)
	  | ppl pd (INT{ival, ty=0}) = (pps "(II)"; pps(IntInf.toString ival))
	  | ppl pd (INT{ival, ty}) =
	      pps(concat["(I", Int.toString ty, ")", IntInf.toString ival])
	  | ppl pd (WORD{ival, ty}) =
	      pps(concat["(W", Int.toString ty, ")", IntInf.toString ival])
          | ppl pd (REAL{rval, ty}) =
	      pps(concat["(R", Int.toString ty, ")", RealLit.toString rval])
          | ppl pd (STRING s) = PU.ppString ppstrm s
          | ppl pd (ETAG (l,_)) = (pps "ETAG("; ppl pd l; pps ")")

          | ppl pd (RECORD l) =
            if pd < 1 then pps "<REC>" else
              (openHOVBox 3;
               pps "RCD";
               ppClosedSeq ("(",",",")") (fn s => ppl (pd-1)) l;
               closeBox ())

          | ppl ps (SRECORD l) =
            if pd < 1 then pps "<SREC>" else
              (openHOVBox 4;
               pps "SRCD";
               ppClosedSeq ("(",",",")") (fn s => ppl (pd-1)) l;
               closeBox ())

          | ppl pd (le as VECTOR (l, _)) =
            if pd < 1 then pps "<VEC>" else
              (openHOVBox 3;
               pps "VEC";
               ppClosedSeq ("(",",",")") (fn s => ppl (pd-1)) l;
               closeBox ())

          | ppl pd (PRIM(p,t,ts)) =
            if pd < 1 then pps "<PRIM>" else
            (openHOVBox 4;
              pps "PRM(";
              openHOVBox 0;
               pps(PrimopUtil.toString p); pps ","; br1 0;
               ppLty' t;
	       pps ",";
	       br1 0;
               ppClosedSeq ("[",",","]") (PPLty.ppTyc (pd-1)) ts;
              closeBox();
              pps ")";
             closeBox ())

          | ppl pd (l as SELECT(i, _)) =
            if pd < 1 then pps "<SEL>" else
            let fun gather(SELECT(i,l)) =
                      let val (more,root) = gather l
                       in  (i :: more,root)
                      end
                  | gather l = (nil, l)

                val (path,root) = gather l
                fun ipr (i:int) = pps(Int.toString i)
             in openHOVBox 2;
                ppl (pd-1) root;
                ppClosedSeq ("[",",","]") (fn s => ppi) (rev path);
                closeBox ()
            end

          | ppl pd (FN(v,t,l)) =
            if pd < 1 then pps "<FN>" else
            (openHOVBox 3; pps "FN(";
              pps(lvarName v); pps ":"; br1 0; ppLty' t; pps ",";
              if complex l then newline() else ();
              ppl (pd-1) l; pps ")";
             closeBox())

          | ppl pd (CON((s, c, lt), ts, l)) =
            if pd < 1 then pps "<CON>" else
            (openHOVBox 4;
              pps "CON(";
              openHOVBox 1; pps "("; pps(S.name s); pps ",";
               pps(DA.prRep c); pps ",";
               ppLty' lt; pps ")";
              closeBox ();
              pps ","; br1 0;
              ppClosedSeq ("[",",","]") (PPLty.ppTyc (pd-1)) ts;
              pps ","; br1 0;
              ppl (pd-1) l; pps ")";
             closeBox())

          | ppl pd (APP(FN(v,_,l),r)) =
            if pd < 1 then pps "<LET*>" else
            (openHOVBox 5;
              pps "(APP)";
              ppl (pd-1) (LET(v, r, l));
             closeBox())

          | ppl pd (LET(v, r, l)) =
            if pd < 1 then pps "<LET>" else
            (openHVBox 2;
              openHOVBox 4;
               pps (lvarName v); br1 0; pps "="; br1 0; ppl (pd-1) r;
              closeBox();
              newline();
              ppl (pd-1) l;
             closeBox())

          | ppl pd (APP(l, r)) =
            if pd < 1 then pps "<APP>" else
            (pps "APP(";
             openHVBox 0;
             ppl (pd-1) l; pps ","; br1 0; ppl (pd-1) r;
             closeBox();
             pps ")")

          | ppl pd (TFN(ks, b)) =
            if pd < 1 then pps "<TFN>" else
            (openHOVBox 0;
             pps "TFN(";
             openHVBox 0;
             ppClosedSeq ("(",",",")") (PPLty.ppTKind (pd-1)) ks; br1 0;
             ppl (pd-1) b;
             closeBox();
             pps ")";
             closeBox())

          | ppl pd (TAPP(l, ts)) =
            if pd < 1 then pps "<TAP>" else
            (openHOVBox 0;
              pps "TAPP(";
              openHVBox 0;
               ppl (pd-1) l; br1 0;
               ppClosedSeq ("[",",","]") (PPLty.ppTyc (pd-1)) ts;
              closeBox();
              pps ")";
             closeBox())

          | ppl pd (GENOP(dict, p, t, ts)) =
            if pd < 1 then pps "<GEN>" else
            (openHOVBox 4;
              pps "GEN(";
              openHOVBox 0;
               pps(PrimopUtil.toString p); pps ","; br1 0;
               ppLty' t; br1 0;
               ppClosedSeq ("[",",","]") (PPLty.ppTyc (pd-1)) ts;
              closeBox();
              pps ")";
             closeBox ())

          | ppl pd (SWITCH (l,_,llist,default)) =
            if pd < 1 then pps "<SWI>" else
            let fun switch [(c,l)] =
                      (openHOVBox 2;
                       pps (conToString c); pps " =>"; br1 0; ppl (pd-1) l;
                       closeBox())
                  | switch ((c,l)::more) =
                      (openHOVBox 2;
                       pps (conToString c); pps " =>"; br1 0; ppl (pd-1) l;
                       closeBox();
                       newline();
                       switch more)
                  | switch [] = () (* bug "unexpected case in switch" *)

             in openHOVBox 3;
                pps "SWI ";
                ppl (pd-1) l; newline();
                pps "of ";
                openHVBox 0;
                switch llist;
                case (default,llist)
                 of (NONE,_) => ()
                  | (SOME l,nil) => (openHOVBox 2; pps "_ =>"; br1 0; ppl (pd-1)l;
                                     closeBox())
                  | (SOME l,_) => (newline();
                                   openHOVBox 2;
                                   pps "_ =>"; br1 0; ppl (pd-1) l;
                                   closeBox());
                closeBox();
                closeBox()
            end

          | ppl pd (FIX(varlist,ltylist,lexplist,lexp)) =
            if pd < 1 then pps "<FIX>" else
            let fun flist([v],[t],[l]) =
                      let val lv = lvarName v
                       in pps lv; pps ": "; ppLty' t; pps " == ";
			  newline();
                          ppl (pd-1) l
                      end
                  | flist(v::vs,t::ts,l::ls) =
                      let val lv = lvarName v
                       in pps lv; pps ": "; ppLty' t; pps " == ";
                          newline();
			  ppl (pd-1) l;
			  newline();
                          flist(vs,ts,ls)
                      end
                  | flist(nil,nil,nil) = ()
                  | flist _ = bug "unexpected cases in flist"

             in openHOVBox 0;
                pps "FIX(";
                openHVBox 0; flist(varlist,ltylist,lexplist); closeBox();
                newline(); pps "IN ";
                ppl (pd-1) lexp;
                pps ")";
                closeBox()
            end

          | ppl pd (RAISE(l,t)) =
            if pd < 1 then pps "<RAISE>" else
            (openHOVBox 0;
              pps "RAISE(";
              openHVBox 0;
               ppLty' t; pps ","; br1 0; ppl (pd-1) l;
              closeBox();
              pps ")";
             closeBox())

          | ppl pd (HANDLE (lexp,withlexp)) =
            if pd < 1 then pps "<HANDLE>" else
            (openHOVBox 0;
             pps "HANDLE"; br1 0; ppl (pd-1) lexp;
             newline();
             pps "WITH"; br1 0; ppl (pd-1) withlexp;
             closeBox())

          | ppl pd (WRAP(t, _, l)) =
            if pd < 1 then pps "<WRAP>" else
            (openHOVBox 0;
              pps "WRAP("; ppTyc' t; pps ",";
              newline();
              ppl (pd-1) l;
              pps ")";
             closeBox())

          | ppl pd (UNWRAP(t, _, l)) =
            if pd < 1 then pps "<WRAP>" else
            (openHOVBox 0;
              pps "UNWRAP("; ppTyc' t; pps ",";
              newline();
              ppl (pd-1) l;
              pps ")";
             closeBox())

   in ppl pd l; newline(); newline()
  end

fun ppFun ppstrm l v =
  let fun last (DA.LVAR x) = x
        | last (DA.PATH(r,_)) = last r
        | last _ = bug "unexpected access in last"

      fun find le =
        case le
          of VAR w =>
               if (v=w)
               then PU.pps ppstrm ("VAR " ^ lvarName v ^ " is free in <lexp>\n")
               else ()
           | l as FN(w,_,b) => if v=w then ppLexp 20 ppstrm l else find b
           | l as FIX(vl,_,ll,b) =>
             if List.exists (fn w => v=w) vl then ppLexp 20 ppstrm l
             else (app find ll; find b)
           | APP(l,r) => (find l; find r)
           | LET(w,l,r) => (if v=w then ppLexp 20 ppstrm l else find l; find r)
           | TFN(_, r) => find r
           | TAPP(l, _) => find l
           | SWITCH (l,_,ls,d) =>
             (find l; app (fn(_,l) => find l) ls;
              case d of NONE => () | SOME l => find l)
           | RECORD l => app find l
           | SRECORD l => app find l
           | VECTOR (l, _) => app find l
           | SELECT(_,l) => find l
           | CON((_, DA.EXN p, _), _, e) => (find(VAR(last p)); find e)
           | CON(_,_,e) => find e
           | HANDLE(e,h) => (find e; find h)
           | RAISE(l,_) => find l
           | INT _ => ()
           | WORD _ => ()
	   | REAL _ => ()
           | STRING _ => ()
           | ETAG (e,_) => find e
           | PRIM _ => ()
           | GENOP ({default=e1,table=es}, _, _, _) =>
             (find e1; app (fn (_, x) => find x) es)
           | WRAP(_, _, e) => find e
           | UNWRAP(_, _, e) => find e

   in find l
  end

fun stringTag (VAR _) = "VAR"
  | stringTag (INT _) = "INT"
  | stringTag (WORD _) = "WORD"
  | stringTag (REAL _) = "REAL"
  | stringTag (STRING _) = "STRING"
  | stringTag (PRIM _) = "PRIM"
  | stringTag (GENOP _) = "GENOP"
  | stringTag (FN _) = "FN"
  | stringTag (FIX _) = "FIX"
  | stringTag (APP _) = "APP"
  | stringTag (LET _) = "LET"
  | stringTag (TFN _) = "TFN"
  | stringTag (TAPP _) = "TAPP"
  | stringTag (ETAG _) = "ETAG"
  | stringTag (RAISE _) = "RAISE"
  | stringTag (HANDLE _) = "HANDLE"
  | stringTag (CON _) = "CON"
  | stringTag (SWITCH _) = "SWITCH"
  | stringTag (VECTOR _) = "VECTOR"
  | stringTag (RECORD _) = "RECORD"
  | stringTag (SRECORD _) = "SRECORD"
  | stringTag (SELECT _) = "SELECT"
  | stringTag (WRAP _) = "WRAP"
  | stringTag (UNWRAP _) = "UNWRAP"

end (* toplevel local *)
end (* struct PPLexp *)
