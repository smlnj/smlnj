(* allocprof.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * NOTE: because pseudo registers are no longer supported by the backend,
 * this code won't work.  Therefore, I've removed it from the sources list
 * and its uses in spill.sml and closure.sml.  [JHR; 2018-10-10]
 *)

structure AllocProf =
struct

local open CPS

structure CGoptions = Control.CG
structure P = CPS.P (* to avoid confusing SourceGroup *)
val mkLvar = LambdaVar.mkLvar

val ARRAYS =          0
val ARRAYSIZE =       1
val STRINGS =         2
val STRINGSIZE =      3
val REFCELLS =        4
val REFLISTS =        5
val CLOSURES =        6
val CLOSURESLOTS =    11
val CLOSUREOVFL =     (CLOSURES + CLOSURESLOTS)
val KCLOSURES =       (CLOSUREOVFL + 1)
val KCLOSURESLOTS =   11
val KCLOSUREOVFL =    (KCLOSURES + KCLOSURESLOTS)
val CCLOSURES =       (KCLOSUREOVFL + 1)
val CCLOSURESLOTS =   11
val CCLOSUREOVFL =    (CCLOSURES + CCLOSURESLOTS)
val LINKS =           (CCLOSUREOVFL + 1)
val LINKSLOTS =       11
val LINKOVFL =        (LINKS + LINKSLOTS)
val SPLINKS =         (LINKOVFL + 1)
val SPLINKSLOTS =     11
val SPLINKOVFL =      (SPLINKS + SPLINKSLOTS)
val RECORDS =         (SPLINKOVFL + 1)
val RECORDSLOTS =     11
val RECORDOVFL =      (RECORDS + RECORDSLOTS)
val SPILLS =          (RECORDOVFL + 1)
val SPILLSLOTS =      21
val SPILLOVFL =       (SPILLS + SPILLSLOTS)
val KNOWNCALLS =      (SPILLOVFL + 1)
val STDKCALLS =       (KNOWNCALLS + 1)
val STDCALLS =        (STDKCALLS + 1)
val CNTCALLS =        (STDCALLS + 1)
val CNTKCALLS =       (CNTCALLS + 1)
val CSCNTCALLS =      (CNTKCALLS + 1)
val CSCNTKCALLS =     (CSCNTCALLS + 1)
val TLIMITCHECK =     (CSCNTKCALLS+1)
val ALIMITCHECK =     (TLIMITCHECK+1)
val ARITHOVH =        (ALIMITCHECK+1)
val ARITHSLOTS =      5
(* Make sure the array assigned to varptr in the runtime system is at
   least this big!!  Test how big by doing an allocReset from batch. *)
val PROFSIZE =        (ARITHOVH+ARITHSLOTS)

val PROFREG = 0  (* use pseudo register 0 *)

(* integer types/values *)
local
  val tt = {sz = Target.defaultIntSz, tag = true}
in
val tagIntTy = NUMt tt
fun tagInt n = NUM{ival = IntInf.fromInt n, ty = tt}
end

in

local
fun prof(s,i) = (* Header to increment slot s by i *)
 (fn ce => let val a1 = mkLvar() and a2 = mkLvar()
	       and x = mkLvar() and n = mkLvar()
	   in  LOOKER(P.getpseudo,[tagInt PROFREG],a1,BOGt,
	       LOOKER(P.subscript,[VAR a1,tagInt s],x,tagIntTy,
	       ARITH(P.iadd,[VAR x,tagInt i],n,tagIntTy,
	       LOOKER(P.getpseudo,[tagInt PROFREG],a2,BOGt,
	       SETTER(P.unboxedupdate,[VAR a2,tagInt s,VAR n],ce)))))
	   end)

fun profSlots(base,slots,ovfl) cost =
  if cost < slots
  then prof(base+cost,1)
  else prof(base,1) o prof(ovfl,cost)

val id = (fn x => x)
in

local val profLinks0 = profSlots(LINKS,LINKSLOTS,LINKOVFL)
in
fun profLinks(cost) = if cost=0 then id else profLinks0 cost
end

fun profRecLinks(l) = foldr (fn (cost,h) => profLinks(cost) o h) id l

local val profRecord0 = profSlots(RECORDS,RECORDSLOTS,RECORDOVFL)
in
fun profRecord(cost) = if cost=0 then id else profRecord0 cost
end

val profClosure = profSlots(CLOSURES,CLOSURESLOTS,CLOSUREOVFL)

val profKClosure = profSlots(KCLOSURES,KCLOSURESLOTS,KCLOSUREOVFL)

val profCClosure = profSlots(CCLOSURES,CCLOSURESLOTS,CCLOSUREOVFL)

val profSpill = profSlots(SPILLS,SPILLSLOTS,SPILLOVFL)

val profStdCall = prof(STDCALLS,1)

val profStdkCall = prof(STDKCALLS,1)

val profCntCall = prof(CNTCALLS,1)

val profCntkCall = prof(CNTKCALLS,1)

val profCSCntCall = prof(CSCNTCALLS,1)

val profCSCntkCall = prof(CSCNTKCALLS,1)

val profKnownCall = prof(KNOWNCALLS,1)

fun profRefCell k = prof(REFCELLS,k)

val profRefList = prof(REFLISTS,1)

val profTLCHECK = prof(TLIMITCHECK,1)

val profALCHECK = prof(ALIMITCHECK,1)

end (* local *)


fun print_profile_info(outstrm) =
  let val im = Int.toString
      fun pr x = TextIO.output(outstrm,x)
      val printf = app pr
      (* Right justify st in a string of length w. *)
      fun field (st,w) =
        if w <= String.size st then st
        else let val s = "                              " ^ st
              in substring(s,String.size s - w, w)
             end

      fun ifield(i,w) = field(im i,w)
      (* Put a decimal point at position w in string st. *)
      fun decimal(st,w) =
        let val l = String.size st - w
            val a = if (l <= 0) then "0" else substring(st,0,l)
            val st' = "0000000000" ^ st
         in  a ^ "." ^ substring(st',String.size st' - w,w)
        end
      fun muldiv(i,j,k) =
        (i*j div k) handle Overflow => muldiv(i,j div 2, k div 2)
      fun decfield(n,j,k,w1,w2) =
        field(decimal(im (muldiv(n,j,k)),w1)
	    handle Div => "",w2)
      (* Returns the percentage i/j to 1 decimal place in a field of width k *)
      fun percent(i,j,k) = decfield(1000,i,j,1,k)
      (* Returns the percentage i/j to 2 decimal places in a field of width k*)
      fun percent2(i,j,k) = decfield(10000,i,j,2,k)

      fun for(start,upto,f) =
        let fun iter(i,cum:int) =
              if i < upto then iter(i+1,cum + f(i)) else cum
         in  iter(start,0)
        end
      fun for'(start,upto,f) =
        let fun iter(i) = if i < upto then (f(i); iter(i+1)) else ()
         in  iter(start)
        end


      val profvec : int array = Unsafe.getPseudo(PROFREG)
      fun getprof(x) = Array.sub(profvec,x)
      fun links(i) = getprof(LINKS+i)
      fun closures(i) = getprof(CLOSURES+i)
      fun kclosures(i) = getprof(KCLOSURES+i)
      fun cclosures(i) = getprof(CCLOSURES+i)
      fun records(i) = getprof(RECORDS+i)
      fun spills(i) = getprof(SPILLS+i)

      val num_calls = getprof(KNOWNCALLS)
	                + getprof(STDKCALLS) + getprof(STDCALLS)
			+ getprof(CNTKCALLS) + getprof(CNTCALLS)
			+ getprof(CSCNTKCALLS) + getprof(CSCNTCALLS)

      val num_closures = for(0, CLOSURESLOTS,fn i => closures(i))
      val space_closures = for(1, CLOSURESLOTS, fn i => closures(i) * (i+1))
      val space_closures = space_closures + getprof(CLOSUREOVFL) + closures(0)

      val num_kclosures = for(0, KCLOSURESLOTS,fn i => kclosures(i))
      val space_kclosures = for(1, KCLOSURESLOTS, fn i => kclosures(i) * (i+1))
      val space_kclosures =
               space_kclosures + getprof(KCLOSUREOVFL) + kclosures(0)

      val num_cclosures = for(0, CCLOSURESLOTS,fn i => cclosures(i))
      val space_cclosures = for(1, CCLOSURESLOTS, fn i => cclosures(i) * (i+1))
      val space_cclosures =
               space_cclosures + getprof(CCLOSUREOVFL) + cclosures(0)

      val num_closure_accesses = for(0, LINKSLOTS, fn i => links(i))
      val num_links_traced = for(1, LINKSLOTS, fn i => links(i) * i)
      val num_links_traced = num_links_traced + getprof(LINKOVFL)

      val num_records = for(0, RECORDSLOTS, fn i => records(i))
      val space_records = for(1, RECORDSLOTS, fn i => records(i) * (i+1))
      val space_records = space_records + getprof(RECORDOVFL) + records(0)

      val num_spills = for(0, SPILLSLOTS, fn i => spills(i))
      val space_spills = for(1, SPILLSLOTS, fn i => spills(i) * (i+1))
      val space_spills = space_spills + getprof(SPILLOVFL) + spills(0)
      val total = space_closures + space_kclosures + space_cclosures
	        + space_records + space_spills
		+ getprof(ARRAYSIZE) + getprof(ARRAYS)
		+ getprof(STRINGSIZE) + getprof(STRINGS)
		+ getprof(REFCELLS) * 2
		+ getprof(REFLISTS) * 2

      val descriptors = num_closures + num_kclosures + num_cclosures
	         + num_records + num_spills
		 + getprof(ARRAYS) + getprof(STRINGS)+ getprof(REFCELLS)

      val sgetprof = im o getprof

      fun printLinks() =
        if num_closure_accesses>0 then
	(for'(1, LINKSLOTS,
	      fn k =>
		 if links(k) > 0 then
		 printf[ifield(k,4),
			ifield(links(k),13),
			percent(links(k),num_closure_accesses,12),
			"%",
			ifield(links(k) * k,12),
			percent(links(k) * k, num_links_traced, 9),
			"%\n"]
		 else ());
	 if links(0) > 0 then
		 printf[">",
			ifield(LINKSLOTS - 1,5),
			ifield(links(0),9),
			percent(links(0),num_closure_accesses,10),
			"%",
			ifield(getprof(LINKOVFL),13),
			percent(getprof(LINKOVFL),num_links_traced,10),
			"%\n"]
	 else ();

	 printf[decfield(100,num_links_traced,num_closure_accesses,2,0),
		" links were traced per access on average.\n\n"]
	 ) else printf["\n"] (* end function printLinks *)

      fun print1(num,name,slots,getstat,ovfl,space) =
        if num>0 then
	(printf[name,":\n"];
	 for'(1, slots,
	      fn k =>
		 if getstat(k) > 0 then
		 printf[ifield(k,6),
			ifield(getstat(k),9),
			percent(getstat(k),num,9),
			"%",
			ifield(getstat(k) * (k+1),13),
			percent(getstat(k) * (k+1), total, 10),
			"%\n"]
		 else ());
	 if getstat(0) > 0 then
		 printf[">",
			ifield(slots - 1,5),
			ifield(getstat(0),9),
			percent(getstat(0),num,9),
			"%",
			ifield(getprof(ovfl)+getstat(0),13),
			percent(getprof(ovfl)+getstat(0),total,10),
			"%\n"]
	 else ();

	 printf["total:",
		ifield(num,9),
		ifield(space,23),
		percent(space,total,10),
		"%  Average size ",
		decfield(100,space-num,num,2,0),
		"\n\n"]
	 ) else if (String.size(name) > 12)
	        then printf[name,": 0\n\n"]
		else printf[name,": ",
			    ifield(0,13 - String.size(name)),"\n\n"]
      (* end function print1 *)

      fun print2(stat,size,name) =
	if getprof(stat) <> 0 then
	printf[name,
	       ifield(getprof(stat),6),
	       ifield(getprof(size) + getprof(stat), 23),
	       percent(getprof(size) + getprof(stat),total,10),
	       "%  Average size ",
	       decfield(100,getprof(size),getprof(stat),2,0),
	       "\n"]
	else printf[name,ifield(0,6),"\n"]

      fun print3(stat,name) =
        if getprof(stat) <> 0 then
	printf[name,
	       ifield(getprof(stat),6),
	       ifield(getprof(stat) * 2, 23),
	       percent(getprof(stat) * 2,total,10),
	       "%\n"]
	else printf[name,ifield(0,6),"\n"]

      fun print4(stat,name) =
        if getprof(stat) <> 0 then
          printf[name, ifield(getprof(stat),10), "\n"]
        else printf[name,ifield(0,12),"\n"]

  in  pr "\n-------------------- ALLOCATION PROFILE --------------------\n\n";

      pr "\n                 ----- FUNCTION CALLS -----\n";
      if (num_calls > 0) then
      printf["Known functions:                 ",
	     ifield(getprof(KNOWNCALLS),10),
	     " (",
	     percent(getprof(KNOWNCALLS),num_calls,4),
	     "%)\n",

	     "Escaping functions:              ",
	     ifield(getprof(STDCALLS),10),
	     " (",
	     percent(getprof(STDCALLS),num_calls,4),
	     "%)\n",


	     "Known escaping functions:        ",
	     ifield(getprof(STDKCALLS),10),
	     " (",
	     percent(getprof(STDKCALLS),num_calls,4),
	     "%)\n",

	     "Continuations:                   ",
             ifield(getprof(CNTCALLS),10),
	     " (",
	     percent(getprof(CNTCALLS),num_calls,4),
	     "%)\n",

	     "Known continuations:             ",
             ifield(getprof(CNTKCALLS),10),
	     " (",
	     percent(getprof(CNTKCALLS),num_calls,4),
	     "%)\n",

	     "Callee-save continuations:       ",
             ifield(getprof(CSCNTCALLS),10),
	     " (",
	     percent(getprof(CSCNTCALLS),num_calls,4),
	     "%)\n",

	     "Known callee-save continuations: ",
             ifield(getprof(CSCNTKCALLS),10),
	     " (",
	     percent(getprof(CSCNTKCALLS),num_calls,4),
	     "%)\n"]
      else ();
      printf["\nTotal function calls:            ",
	     ifield(num_calls,10),"\n\n"];


      pr "\n                ----- CLOSURE ACCESSES -----\n";
      printf["Closure elements were accessed ",
	     im num_closure_accesses,
	     " times through ",
	     im num_links_traced,
	     " links:\n",
	     "Size     Accesses   % accesses       Links   % links\n"];
      printLinks();

      pr "\n                ----- HEAP ALLOCATIONS -----\n";
      pr "             (only total sizes include descriptors)\n\n";
      printf["TOTAL size ", im total];
      if (total > 0) then (
      printf["; ",
	     im descriptors, " descriptors accounted for ",
	     percent(descriptors,total,0), "%.\n\n"])
      else printf[".\n\n"];

      printf["  Size   Number   % total   Total size    % TOTAL\n\n"];

      print1(num_closures,"Closures for escaping functions",
	     CLOSURESLOTS,closures,CLOSUREOVFL,space_closures);
      print1(num_kclosures,"Closures for known functions",
	     KCLOSURESLOTS,kclosures,KCLOSUREOVFL,space_kclosures);
      print1(num_cclosures,"Closures for callee-save continuations",
	     CCLOSURESLOTS,cclosures,CCLOSUREOVFL,space_cclosures);

      print1(num_records,"Records",RECORDSLOTS,records,
	     RECORDOVFL,space_records);
      print1(num_spills,"Spills",SPILLSLOTS,spills,
	     SPILLOVFL,space_spills);

      print2(ARRAYS,ARRAYSIZE,"Arrays:  ");
      print2(STRINGS,STRINGSIZE,"Strings: ");

      print3(REFCELLS,"Refs:    ");
      print3(REFLISTS,"Ref\n list:   ");

      print4(TLIMITCHECK,"Limit Checks for Continuations Only: ");
      print4(ALIMITCHECK,"Limit Checks for Other allocations: ")

  end (* fun print_profile_info *)


fun reset() = (print "New  alloc profvec, size ";
	       print (Int.toString PROFSIZE); print "\n";
	       Unsafe.setPseudo(Array.array(PROFSIZE,0),PROFREG))

end (* toplevel local *)
end (* structure AllocProf *)

