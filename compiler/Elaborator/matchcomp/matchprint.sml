(* FLINT/trans/matchprint.sml *)
(* pattern printing (using PPAbsyn.ppPat) *)

structure MatchPrint =
struct

local
  structure PP = PrettyPrint
  open PrintUtil
  val printDepth = Control_Print.printDepth

  fun bug msg = ErrorMsg.impossible ("MatchPrint: "^ msg)
in

(* matchPrint: StaticEnv.staticEnv * (AS.pat * AS.exp) list * int list -> PP.stream -> () *)
fun matchPrint (env,rules,unused) ppstrm =
  let fun matchPrint' ([],_,_) = ()
        | matchPrint' ((pat,_)::more,[],_) =
           (PP.string ppstrm "        ";
            PPAbsyn.ppPat env ppstrm (pat,!printDepth);
            PP.string ppstrm " => ...";
            PP.newline ppstrm;
            matchPrint' (more,[],0))
        | matchPrint' ((pat,_)::more,(taglist as (tag::tags)),i) =
           if i = tag then
            (PP.string ppstrm "  -->   ";
             PPAbsyn.ppPat env ppstrm (pat,!printDepth);
             PP.string ppstrm " => ...";
             PP.newline ppstrm;
             matchPrint'(more,tags,i+1))
           else
            (PP.string ppstrm "        ";
             PPAbsyn.ppPat env ppstrm (pat,!printDepth);
             PP.string ppstrm " => ...";
             PP.newline ppstrm;
             matchPrint'(more,taglist,i+1))
   in PP.newline ppstrm;
      PP.openHVBox ppstrm (PP.Rel 0);
      matchPrint'(rules,unused,0);
      PP.closeBox ppstrm
  end

(* bindPrint : StaticEnv.staticEnv * (AS.pat * AS.exp) list -> PP.stream -> () *)
fun bindPrint (env,(pat,_)::_) ppstrm =
      (PP.newline ppstrm; PP.string ppstrm "        ";
       PPAbsyn.ppPat env ppstrm (pat, !printDepth);
       PP.string ppstrm " = ...")
  | bindPrint _ _ = bug "bindPrint -- unexpected args"

end (* local printutil *)

end (* structure MatchPrint *)
