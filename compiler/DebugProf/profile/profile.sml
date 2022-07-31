(* profile.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

structure Profile : sig

    val reset : unit -> unit          (* reset profiling counts to zero *)

    val report : TextIO.outstream -> unit
	  (* print profiling report to stream *)
    val reportAll : TextIO.outstream -> unit
	  (* print profiling report to stream; DON'T suppress zero entries*)
    val reportData: unit -> {name: string, count: int, time: Time.time} list
	  (* Return the unformatted data for a report *)

    val runTimeIndex : int
    val minorGCIndex : int
    val majorGCIndex : int
    val otherIndex : int
    val compileIndex : int
    val numPredefIndices : int

  end = struct

    structure A = Array
    structure PC = SMLofNJ.Internals.ProfControl

    val runTimeIndex = PC.runTimeIndex
    val minorGCIndex = PC.minorGCIndex
    val majorGCIndex = PC.majorGCIndex
    val otherIndex = PC.otherIndex
    val compileIndex = PC.compileIndex
    val numPredefIndices = PC.numPredefIndices

    val reset = PC.reset
 
    datatype entry = ENTRY of {name: string, count: int, time: int}

    val splitlines = String.tokens (fn #"\n" => true | _ => false)

    fun join (entries, base, _, counts, times, nil) = entries
      | join (entries, base, n, counts, times, line::lines) =
          join (ENTRY{
	      name = line, count= A.sub(counts, n), time = A.sub(times, base+n)
	    }::entries, base, n+1, counts, times, lines)

    fun batch (PC.UNIT{base,size,counts,names}) =
	  join(nil, base, 0, counts, PC.getTimeArray(), splitlines names)

    fun log10 0 = 0
      | log10 i = 1 + log10(i div 10)

    fun field (st, w) = StringCvt.padLeft #" " w st

  (* take a string of digits and a number of decimal places, and return the
   * digits with the decimal point added in the right place.
   *)
    fun decimal (st, w) = let
	  val len = size st
	  in
	    if (len <= w)
	      then String.concat [".", StringCvt.padLeft #"0" w st]
	      else String.concat [
		  substring(st, 0, len-w), ".", substring(st, len-w, w)
		]
	  end

    fun muldiv(i,j,k) =
	  (i*j div k) 
	     handle Overflow => muldiv(i,j div 2, k div 2)

    (* This convolution is required because the PPC cannot distinguish 
     * between div-by-zero and overflow -- Lal.
     *)
    fun muldiv(i, j, 0) = raise General.Div
      | muldiv(i, j, k) = 
         (i * j div k) handle Overflow => muldiv(i, j div 2, k div 2)
 
    fun decfield(n,j,k,w1,w2) = 
	  field(decimal(Int.toString (muldiv(n,j,k)),w1)
		  handle Div => "",w2)

    fun getBigList() = let
	  val biglist = List.concat (List.map batch (! PC.units))
          fun compare (
		ENTRY{time=a,count=ca,name=na,...},
		ENTRY{time=b,count=cb,name=nb,...}
	      ) = a<b orelse a=b andalso (ca<cb orelse ca=cb andalso na>nb)
	  in
	    ListMergeSort.sort compare biglist
	  end

    fun reportData() = let
	  val usPerSample = Int.toLarge(PC.getQuantum())
	  fun f (ENTRY{name,count,time}) = {
		  name=name, count=count,
		  time=Time.fromMicroseconds(Int.toLarge time * usPerSample)
		}
	  in
	    map f (getBigList ())
	  end

    fun reportx suppress outstream = let
	  val biglist' = getBigList()
	  val tot = List.foldr (fn (ENTRY{time=a,...},b)=> a+b) 0 biglist'
	  val maxc = List.foldr (fn (ENTRY{count=a,...},b)=> Int.max(a,b)) 0 biglist'
	  val digits_cum = log10 tot
          val digits_cnt = Int.max(6,1+log10 maxc)
	  fun pr s = TextIO.output(outstream, s)
	  fun printlines (ENTRY{time,name,count}::rest, cum) =
		if suppress andalso count=0 andalso time=0 then ()
                else (
		  pr(decfield(time,10000,tot,2,6));
		  if (digits_cum > 4)
		    then pr(field(Int.toString(cum+time+50 div 100),7))
		    else pr(decfield(cum+time,1,1,2,7));
		  pr(field(Int.toString count,digits_cnt));
(*		  pr(decfield(time,50000,count,4,10)); *)
		  pr "  "; pr name; pr "\n";
		  printlines(rest,cum+time))
	    | printlines (nil, _) = ()

	 in pr(field("%time",6));
	    pr(field("cumsec",7));
	    pr(field("#call",digits_cnt));
(*	    pr(field("ms/call",10)); *)
	    pr("  name\n");
	    printlines(biglist',0);
	    TextIO.flushOut outstream
        end

   val report = reportx true
   val reportAll = reportx false

end;

