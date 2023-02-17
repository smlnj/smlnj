(* stats.sml
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *)
signature STATS = 
  sig
    type stat
    type counter

    (* The counters (argument) will be add'd whenever the new counter is *)
    val newCounter : counter list -> counter
    val getCounter : counter -> int
    val addCounter : counter -> int -> unit

    (* A stat contains the sum of the argument counters. *)
    val newStat : string * counter list -> stat
    val getStat : stat -> int
    (* Add the stat to the summary *)
    val registerStat : stat -> unit

    (* old interface, deprecated. *)
    val makeStat : string -> stat
    val addStat : stat -> int -> unit

    type phase
    val makePhase : string -> phase
    val doPhase : phase -> ('a -> 'b) -> ('a -> 'b)
    val keepTime : bool ref
    val approxTime : bool ref  (* doesn't do anything right now *)
    val sayBegin : bool ref
    val sayEnd : bool ref
    val summary : unit -> unit
    val summarySp : unit -> unit
    val reset : unit -> unit
  end;
                
structure Stats :> STATS = 
  struct

    structure T = Time

    val timeToStr = T.fmt 2

    datatype counter = C of {c:int ref, cs:counter list}

    datatype stat = STAT of {name:string, tot: counter list}
    val allStats = ref (nil : stat list)

    fun lookSt(name,nil) = NONE
      | lookSt(name,(p as STAT{name=n,...})::rest) = 
            if name=n then SOME p else lookSt(name,rest)

    fun insert(p as STAT{name=pn,...}, (q as STAT{name=qn,...})::rest) =
          if pn<qn then p::q::rest else q::insert(p,rest)
      | insert(p,nil) = p::nil

    fun newCounter cs = C{c=ref 0, cs=cs}
    fun addCounter (C{c,cs}) n = (c := !c + n; app (fn c => addCounter c n) cs)
    fun getCounter (C{c=ref c,...}) = c

    fun newStat (name,cs) = STAT{name=name,tot=cs}
    fun registerStat (p as STAT{name,tot}) = 
	(case lookSt (name,!allStats)
	  of SOME p => ()
	   | NONE => allStats := insert(p,!allStats))

    fun makeStat name = (case lookSt (name,!allStats)
	   of SOME p => p
	    | NONE => let
		val p = newStat(name, [newCounter[]])
		in
		  allStats := insert(p,!allStats); p
		end
	  (* end case *))
    fun addStat(STAT{tot = (c :: _),...}) n = addCounter c n
      | addStat(STAT{tot = [], ... }) _ = ()

    val say = Control_Print.say
    val flush = Control_Print.flush

(** NOTE: we should be able to rewrite this using the Timer structure **)
    type times = {usr:T.time, sys:T.time, gc:T.time}
    val zeros = {usr=T.zeroTime, sys=T.zeroTime, gc=T.zeroTime}

    datatype phase = PHASE of {name : string, accum : times ref, this: times ref}

    fun lookPh(name,nil) = NONE
      | lookPh(name,(p as PHASE{name=n,...})::rest) = 
          if name=n then SOME p else lookPh(name,rest)

    fun insert(p as PHASE{name=pn,...}, (q as PHASE{name=qn,...})::rest) =
          if pn<qn then p::q::rest else q::insert(p,rest)
      | insert(p,nil) = p::nil


    val allPhases = ref (nil : phase list)

    fun makePhase name = (case lookPh(name,!allPhases)
	   of SOME p => p
	    | NONE => let
		val p = PHASE{name=name,accum=ref zeros, this=ref zeros}
		in
		  allPhases := insert(p,!allPhases); p
		end
	  (* end case *))
 
    val current = ref (makePhase "Other")

    val keepTime = ref true
    val approxTime = ref true
    val sayBegin = ref false
    val sayEnd = ref false

    infix 6 ++ val op ++ = Time.+
    infix 6 -- val op -- = Time.-

    infix 6 +++
    fun {usr,sys,gc}+++{usr=u,sys=s,gc=g} = {usr=usr++u,sys=sys++s,gc=gc++g}
    infix 6 ---
    fun {usr,sys,gc}---{usr=u,sys=s,gc=g} = 
          if (Time.<(usr, u))
	      then zeros 
              else {usr=usr--u,sys=sys--s,gc=gc--g}

    local
      fun gettime () = let
	  val { nongc, gc } =
	      Timer.checkCPUTimes(Timer.totalCPUTimer())
      in
	  (* This is a hack.
	   * (This module deserves a complete rewrite!!) *)
	  { usr = #usr nongc, sys = Time.+ (#sys nongc, #sys gc),
	    gc = #usr gc }
      end
      val last = ref (gettime())
    in 
    fun reset() = (
	  last := gettime();
	  app (fn PHASE{this,accum,...} => (this := zeros; accum := zeros)) 
            (!allPhases);
	  app (fn STAT{tot,...} => app (fn C{c,...} => c:=0) tot) (!allStats))

    structure CU = SMLofNJ.Internals.CleanUp
    val _ = CU.addCleaner (
	  "CompilerStats",
	  [CU.AtExportML, CU.AtExportFn, CU.AtInit],
	  fn CU.AtInit => reset() | _ => last := zeros)

    fun since() = let
(***
          val x = if !approxTime
	            then let
		      val t1 = !lastcollect
		      val u1 = !System.Runtime.minorcollections
		      in lastcollect := u1; u1<>t1 end
		    else true
***)
	  val x = true
          in
	    if x
	      then let
		val t = !last
		val u = gettime()
		in last := u; (u --- t) end
             else zeros
	  end
    end (* local *)

    fun repeat 0 f x = () | repeat n f x= (f x; repeat (n-1) f x)
    fun sayfield(n,s) = (say s; repeat (Int.max(0,n-size s)) say " ")

    fun doPhase (p as PHASE{name,this,accum}) f x = let
	  val prev as PHASE{this=t',...} = !current
	  fun endTime() = let
		val x as {usr,sys,gc} = since() +++ !this
	        in
		  this := zeros;
		  accum := !accum +++ x;
		  usr++sys++gc
	        end
	  fun finish() = (
		current := prev;
		if !sayEnd
		  then (
		    say "End   ";
		    sayfield(40,name);
		    if !keepTime
		      then app say ["    ", timeToStr(endTime()), " sec\n"]
		      else  say "\n";
		    flush())
	          else (endTime(); ()))
          in
	    if !keepTime
	      then t' := since() +++ !t'
	      else ();
	    current := p;
	    if !sayBegin then (app say ["Begin ",name,"\n"]; flush()) else ();
	    (f x handle e => (finish(); raise e))
              before finish()
          end
  
    fun getStat (STAT{tot,...}) = foldl (fn (c,s) => getCounter c + s) 0 tot
    fun showStat(s as STAT{name,tot}) = (
	  sayfield(40, name);
	  say(Int.toString(getStat s));
	  say "\n")

    fun showPhase(PHASE{name,this,accum}) = let
	  val {usr,sys,gc} = !this +++ !accum
          in sayfield(40,name); 
	    say(timeToStr usr); say "u  ";
	    say(timeToStr sys); say "s  ";
	    say(timeToStr gc); say "g  ";
            say "\n"
          end

    fun summary() = let
	  val sum = foldr (fn(PHASE{accum,...},t)=> !accum+++t) zeros (!allPhases)
          in
	    app showStat (!allStats); 
	    app showPhase
	      (!allPhases @ [PHASE{name="TOTAL",this=ref zeros, accum=ref sum}])
	  end

    fun showPhaseSp(PHASE{name,this,accum}) = let
	  val {usr,sys,gc} = !this +++ !accum
          in case T.compare(usr++sys++gc, T.zeroTime)
	     of EQUAL => ()
	      | _ => 
               (sayfield(40,name); 
    	        say(timeToStr (usr++sys)); say "u  ";
(*	        say(timeToStr sys); say "s  "; *)
	        say(timeToStr gc); say "g  ";
                say "\n")
          end

    fun summarySp() = let
	  val sum = foldr (fn(PHASE{accum,...},t)=> !accum+++t) zeros (!allPhases)
          in
	    app showStat (!allStats); 
	    app showPhaseSp
	      (!allPhases @ [PHASE{name="TOTAL",this=ref zeros, accum=ref sum}])
	  end

  end (* structure Stats *)
