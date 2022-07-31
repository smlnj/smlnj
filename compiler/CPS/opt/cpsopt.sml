(* cpsopt.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature CPSOPT =
  sig

    val reduce : (CPS.function * Unsafe.Object.object option * bool) -> CPS.function

  end (* signature CPSOPT *)

functor CPSopt (MachSpec: MACH_SPEC) : CPSOPT =
  struct

    structure CG = Control.CG
    structure Flatten = Flatten(MachSpec)
    structure Uncurry = Uncurry(MachSpec)

    fun say msg = Control.Print.say(String.concat msg)

  (** the main optimization function *)
(* NOTE: The third argument to reduce is currently ignored.
   It used to be used for reopening closures. *)
    fun reduce (function, _, afterClosure) = let
	  val debug = !CG.debugcps (* false *)
	  fun debugprint s = if debug then say s else ()
	  fun debugflush() = if debug then Control.Print.flush() else ()
	  val clicked = ref 0
	  fun click (s:string) = (debugprint[s]; clicked := !clicked+1)

	  val cpssize = ref 0

	  val prC = let
		fun prGen (flag, printE) s e = if !flag
		      then (say["\n\n[After ", s, " ...]\n\n"]; printE e; e)
		      else e
		in
		  prGen (Control.CG.printit, PPCps.printcps0)
		end

	  fun contract last f = let
		val f' = (
		      clicked := 0;
		      Contract.contract {
			  function=f, click=click, last=last, size=cpssize
			})
		in
		  debugprint [
		      "Contract stats: CPS Size = ", Int.toString (!cpssize),
		      " , clicks = ", Int.toString (!clicked), "\n"
		    ];
		  f'
		end

	(* dropargs are turned off in first_contract to ban unsafe eta reduction *)
	  fun first_contract f = let
		val dpargs = !CG.dropargs
		val f' = (
		      clicked := 0; CG.dropargs := false;
		      Contract.contract {
			  function=f, click=click, last=false, size=cpssize
			})
		in
		  debugprint [
		      "First contract stats: CPS Size = ", Int.toString (!cpssize),
		      " , clicks = ", Int.toString (!clicked), "\n"
		    ];
		  CG.dropargs := dpargs;
		  f'
		end

	(* in the last contract phase, certain contractions are prohibited *)
	  fun last_contract f = let
		val f' = (
		      clicked := 0;
		      Contract.contract {
			  function=f, click=click, last=true, size=cpssize
			})
		in
		  debugprint [
		      "Last contract stats: CPS Size = ", Int.toString (!cpssize),
		      " , clicks = ", Int.toString (!clicked), "\n"
		    ];
		  f'
		end

	  fun expand (f, n, unroll) = (
		clicked := 0;
		if not(!CG.betaexpand)
		  then f
		  else let
		    val f' = Expand.expand{
			    function=f, click=click, bodysize=n,
			    afterClosure=afterClosure,
			    unroll=unroll, do_headers=true
			  }
		    in
		      debugprint["Expand stats: clicks = ", Int.toString (!clicked), "\n"];
		      f'
		    end)

	  fun zeroexpand f = let
		val _ = clicked := 0;
		val f' = Expand.expand{
			function=f, click=click, bodysize=0,
			afterClosure=afterClosure,
			unroll=false, do_headers=false
		      }
		in
		  debugprint["ZeroExpand stats: clicks = ", Int.toString (!clicked), "\n"];
		  f'
		end

	  fun flatten f = (
		clicked := 0;
		if not(!CG.flattenargs)
		  then f
		  else let
		    val f' = Flatten.flatten{function=f, click=click}
		    in
		      debugprint["Flatten stats: clicks = ", Int.toString (!clicked), "\n"];
		      f'
		    end)

	  fun unroll_contract (f, n) = let
		val f' = expand(f,n,true)
		val c = !clicked
		in
		  if c > 0 then (c, contract true f') else (c, f')
		end

	  fun expand_flatten_contract (f, n) = let
		val f1 = expand(f, n, false)
		val c1 = !clicked
		val f2 = flatten f1
		val c2 = !clicked
		val c = c1+c2
		in
		  if c > 0 then (c, contract false f2) else (c, f2)
		end

	  fun eta f = (
		clicked := 0;
		if not(!CG.eta)
		  then f
		  else let
		    val f' = Eta.eta{function=f, click=click}
		    in
		      debugprint["Eta stats: clicks = ", Int.toString (!clicked), "\n"];
		      f'
		    end)

	  fun uncurry f = if afterClosure
		then f
		else (
		  clicked := 0;
		  if not(!CG.uncurry)
		    then f
		    else let
		      val f' = Uncurry.etasplit{function=f, click=click}
		      in
			debugprint["Uncurry stats: clicks = ", Int.toString (!clicked), "\n"];
			f'
		      end)

	  fun etasplit f = (
		clicked := 0;
		if not(!CG.etasplit)
		  then f
		  else let
		    val f' = EtaSplit.etasplit{function=f, click=click}
		    in
		      debugprint["Etasplit stats: clicks = ", Int.toString (!clicked), "\n"];
		      f'
		    end)

	  fun lambdaprop x = x
              (* if !CG.lambdaprop then (debugprint "\nLambdaprop:"; CfUse.hoist x) else x *)

	  val bodysize = !CG.bodysize
	  val rounds = !CG.rounds
	  val reducemore = !CG.reducemore

	(* Note the parameter k starts at rounds..0 *)
	  fun linear_decrease k = (bodysize * k) div rounds
(*** NOT USED ***
	  fun double_linear k = (bodysize*2*k div rounds) - bodysize
	  fun cosine_decrease k =
		 Real.trunc(real bodysize * (Math.cos(1.5708*(1.0 - real k / real rounds))))
***)

	(* This function is just hacked up and should be tuned someday *)
	  fun cycle (0, true, func) = func
	    | cycle (0, false, func) = unroll func
	    | cycle (k, unrolled, func) = let
		val func = lambdaprop func
		val (c, func) = if !CG.betaexpand orelse !CG.flattenargs
		      then expand_flatten_contract(func, linear_decrease k)
		      else (0, func)
		(* val _ = prC "cycle_contract" func *)
		in
		  if c * 1000 > !cpssize * reducemore
		    then cycle(k-1, unrolled, func)
		  else if unrolled
		    then func
		    else unroll func
		end
	  and unroll func = let
		val (c,func') = unroll_contract(func,bodysize)
		in
		  if c > 0 then cycle(rounds, true, func') else func'
		end
	  fun apply ("first_contract",f) = first_contract f
	    | apply ("eta",f)            = eta f
	    | apply ("uncurry",f)	 = uncurry f
	    | apply ("etasplit",f)       = etasplit f
	    | apply ("last_contract",f)  = last_contract f
	    | apply ("cycle_expand",f)   = cycle(rounds, not(!CG.unroll), f)
	    | apply ("contract",f)       = contract false f
	    | apply ("flatten",f)        = flatten f
	    | apply ("zeroexpand",f)     = zeroexpand f
	    | apply ("expand",f)         = expand(f, bodysize, false)
	    | apply ("print",f)          = (PPCps.printcps0 f; f)
	    | apply (p,f)                = (say["\n!! Unknown cps phase '", p, "' !!\n"]; f)
	  in
	    (if rounds < 0
	      then function
	      else let
		val optimized = foldl apply function (!CG.cpsopt)
(*              val function1 = first_contract function *)
(*              val function2 = eta function1 *)
(*              val function3 = uncurry function2 *)
(*              val function4 = etasplit function3 *)
(*              val function5 = cycle(rounds, not(!CG.unroll), function4) *)
(*              val function6 = eta function5 (* ZSH added this new phase *) *)
(*              val function7 = last_contract function6 *)
(*              val optimized function7 *)
		in
		(* expand out the 64-bit and IntInf operations and then do one last
		 * contraction pass.
		 *)
		  if !CG.printit
		    then (
		      say ["\n\n[Before lowering]\n\n"];
		      PPCps.printcps0 optimized)
		    else ();
		  last_contract (LowerCPS.transform optimized)
		end)
	    before (debugprint["\n"]; debugflush())
	  end (* fun reduce *)

  end (* functor CPSopt *)
