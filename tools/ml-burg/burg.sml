(* burg.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * $Log$
 * Revision 1.2  2000/06/01 18:33:42  monnier
 * bring revisions from the vendor branch to the trunk
 *
 * Revision 1.1.1.8  1999/04/17 18:56:04  monnier
 * version 110.16
 *
 * Revision 1.2  1997/10/28 15:02:45  george
 *    Made compatible with new basis
 *
# Revision 1.1.1.1  1997/01/14  01:37:59  george
#   Version 109.24
#
 * Revision 1.1.1.2  1997/01/11  18:52:29  george
 *   ml-burg Version 109.24
 *
 * Revision 1.3  1996/06/03  17:48:15  jhr
 * Changes to bring ML-Burg upto new SML/NJ library.
 *
 * Revision 1.2  1996/02/26  15:02:05  george
 *    print no longer overloaded.
 *    use of makestring has been removed and replaced with Int.toString ..
 *    use of IO replaced with TextIO
 *
 * Revision 1.1.1.1  1996/01/31  16:01:24  george
 * Version 109
 * 
 *)


signature BURGEMIT = sig
  exception BurgError
  val emit : TextIO.instream * (unit -> TextIO.outstream) -> unit
end



structure BurgEmit : BURGEMIT =
  struct

    structure HashStringKey : HASH_KEY = struct
      type hash_key = string
      val hashVal = HashString.hashString
      val sameKey = (op =) : string * string -> bool
    end
    structure BurgHash = HashTableFn (HashStringKey)
    exception NotThere;				  (* raised by BurgHash.find *)
	
    exception BurgError				      (* for error reporting *)

    val inf = 16383

    open BurgAST

    (* debugging *)
    fun debug s = (TextIO.output (TextIO.stdErr, s); 
		   TextIO.flushOut TextIO.stdErr)


    (* Output functions *)
    val s_out = ref TextIO.stdOut	   (* changed into the output stream *)
    fun say s = TextIO.output (!s_out, s)
    fun saynl s = say (s^"\n")
    fun sayi s = say ("\t"^s)
    fun sayinl s = say ("\t"^s^"\n")


    fun arrayapp (function, array) =
      let
	val len = Array.length array
	fun loop pos =
	  if pos=len then ()
	  else
	    (function (Array.sub (array, pos)); loop (pos+1))
      in
	loop 0
      end
    
    fun arrayiter (function, array) =
      let
	val len = Array.length array
	fun loop pos =
	  if pos=len then ()
	  else
	    (function (pos, Array.sub (array, pos)); loop (pos+1))
      in
	loop 0
      end

    fun iter (function, n) =
      let
	fun loop pos =
	  if pos=n then () else (function pos; loop (pos+1))
      in
	loop 0
      end

    fun listiter (function, lis) =
      let
	fun loop (pos, li) =
	  case li of
	    [] => ()
	  | (l::ll) => (function (pos, l); loop ((pos+1), ll))
      in
	loop (0, lis)
      end

    exception NotSameSize

    fun exists2 (function, list1, list2) =
      let
	exception Found
	fun loop ([],[]) = ()
	  | loop (e1::l1,e2::l2) =
	      if function (e1,e2) then raise Found else loop (l1,l2)
	  | loop _ = raise NotSameSize
      in
	(loop (list1,list2); false) handle Found => true
      end

    fun forall2 (f,l1,l2) = not (exists2 (not o f, l1, l2))

    fun map2 (function, list1, list2) =
      let
	fun loop ([],[],acc) = rev acc
	  | loop (e1::l1,e2::l2,acc) = loop (l1,l2,(function(e1,e2))::acc)
	  | loop _ = raise NotSameSize
      in
	loop (list1,list2,[])
      end

    fun tofirstupper s = (case String.explode s
	   of [] => ""
	     | (c::r) => implode(Char.toUpper c :: (map Char.toLower r))
	  (* end case *))

    fun emit (s_in, oustreamgen) =
      let

	(*
	 * Error reporting
	 *)
	val error_encountered = ref false
	fun warning s = (error_encountered := true;
			 TextIO.output (TextIO.stdErr, "Error: "^s^"\n");
			 TextIO.flushOut TextIO.stdErr)
	fun error s = (TextIO.output (TextIO.stdErr, "Error: "^s^"\n");
		       TextIO.flushOut TextIO.stdErr;
		       raise BurgError)
	fun stop_if_error () = if !error_encountered then raise BurgError else ()

	(*
	 * ids (for hashing) :
	 * TERMINAL (internal terminal number, external terminal string/number)
	 * NONTERMINAL (internal nonterminal number)
	 *)
	datatype ids = TERMINAL of int * string
		     | NONTERMINAL of int
	  
	(* hash table type *)
	type htt = ids BurgHash.hash_table
	  
	(*
	 * rule_pat :
	 * NT (nonterminal)
	 * T (terminal, sons)
	 *)
	datatype rule_pat = NT of int | T of int * rule_pat list
	  
	(*
	 * rule
	 *)
	type ern = string		      (* type for external rule name *)
	type rule = {nt:int, pat:rule_pat, ern:ern, cost: int, num:int}



	(* hash table symbols *)
	val HT = BurgHash.mkTable (60, NotThere) : htt

	(* hash table for rule names and the arity of the pattern *)
	val HR = BurgHash.mkTable (60, NotThere)
	  : int BurgHash.hash_table


	val start_sym = ref (NONE : string option)	    (* %start symbol *)
	val start = ref 0		       (* nonterminal where to start *)


	val term_prefix = ref ""		     (* prefix for terminals *)
	val rule_prefix = ref ""			 (* prefix for rules *)
	val sig_name = ref ""				  (* BURM by default *)
	val struct_name = ref ""	   (* Burm (first upper, rest lower) *)

	val nb_t = ref 0		 (* current internal terminal number *)
	val nb_nt = ref 0	      (* current internal nonterminal number *)

	(* Return a new internal terminal number *)
	fun gen_tnum () = !nb_t before (nb_t := !nb_t+1)

	(* Return a new internal nonterminal number *)
	fun gen_ntnum () = !nb_nt before (nb_nt := !nb_nt+1)


	(*
	 * Emit the header
	 *)
	fun emit_header (SPEC {head, ...}) = app say head


	(*
	 * Emit the tail
	 *)
	fun emit_tail (SPEC {tail, ...}) = app say tail


	(*
	 * Give each terminal an internal terminal number,
	 * and remember the external terminal number.
	 * Also, find start symbol.
	 *)
	fun reparse_decls (SPEC {decls=decls, ...}) =
	  let
	    val t_prefix = ref (NONE : string option)
	    val r_prefix = ref (NONE : string option)
	    val s_name = ref (NONE : string option)

	    fun newt (sym, etn') =
	      let
		val etn = case etn' of
		  SOME str => str
		| NONE => sym
	      in
		case (BurgHash.find HT sym) : ids option of
		  NONE => BurgHash.insert HT (sym, TERMINAL (gen_tnum(), etn))
		| SOME _ => warning ("term "^sym^" redefined")
	      end

	    fun newdecl (START s) =
	          (case !start_sym of
		     NONE => start_sym := (SOME s)
		   | (SOME _) => warning "%start redefined")
	      | newdecl (TERM l) = app newt l
	      | newdecl (TERMPREFIX tp) =
		  (case (!t_prefix) of
		     NONE => t_prefix := (SOME tp)
		   | _ => warning "%termprefix redefined")
	      | newdecl (RULEPREFIX rp) =
		  (case (!r_prefix) of
		     NONE => r_prefix := (SOME rp)
		   | _ => warning "%ruleprefix redefined")
	      | newdecl (SIG s) =
		  (case (!s_name) of
		     NONE => s_name := (SOME s)
		   | _ => warning "%sig redefined")
	  in
	    app newdecl decls;
	    if !nb_t=0 then error "no terminals !" else ();
	    term_prefix :=
	      (case (!t_prefix) of
		 NONE => ""
	       | SOME tp => tp);
	    rule_prefix :=
	      (case (!r_prefix) of
		 NONE => ""
	       | SOME rp => rp);
	    sig_name :=
	      (case (!s_name) of
		 NONE => "BURM"
	       | SOME s => String.translate (String.str o Char.toUpper) s);
	    struct_name := tofirstupper (!sig_name)
	  end (* fun reparse_decls *)


	fun get_id sym =
	  case (BurgHash.find HT sym) : ids option of
	    NONE => error ("symbol "^sym^" not declared")
	  | SOME id => id


	(*
	 * Arrays that contain for each t or nt its external symbol.
	 *)
	val sym_terminals = ref (Array.array (0,("","")))
	val sym_nonterminals = ref (Array.array (0,""))


	fun build_num_to_sym_arrays () =
	  let
	    fun store (sym, TERMINAL (t, etn)) =
	          Array.update (!sym_terminals, t, (sym, etn))
	      | store (sym, NONTERMINAL nt) =
		  Array.update (!sym_nonterminals, nt, sym)
	  in
	    sym_terminals := Array.array (!nb_t, ("",""));
	    sym_nonterminals := Array.array (!nb_nt, (""));
	    BurgHash.appi store HT
	  end

	fun get_ntsym nt = Array.sub (!sym_nonterminals, nt)
	fun get_tsym t = #1 (Array.sub (!sym_terminals, t))


	fun reparse_rules (SPEC {rules=spec_rules, ...}) =
	  let
	    (* Arity for terminals. *)
	    val t_arity = Array.array (!nb_t, NONE : int option)

	    fun newnt (RULE (ntsym, _, _, _)) =
	      case (BurgHash.find HT ntsym) : ids option of
		NONE => BurgHash.insert HT (ntsym, NONTERMINAL (gen_ntnum ()))
	      | SOME (TERMINAL _) =>
		  warning (ntsym^" redefined as a nonterminal")
	      | SOME (NONTERMINAL _) => ()


	    val rule_num = ref 0		     (* first rule is rule 1 *)

	    fun newrule (RULE (ntsym, pattern, ern, costlist)) =
	      let
		val num = (rule_num := !rule_num+1; !rule_num)

		val nt =
		  case BurgHash.find HT ntsym of
		    SOME (NONTERMINAL nt) => nt
		  | _ => error "internal : get nt"

		val cost = case costlist of [] => 0 | (c::_) => c

		val pat =
		  let
		    fun makepat (PAT (sym, sons)) =
		      case get_id sym of
			NONTERMINAL nt =>
			  (NT nt) before
			  (if (null sons) then () else
			     warning ("nonterminal "^sym^" is not a tree"))
		      | TERMINAL (t, _) =>
			  let
			    val len = List.length sons
			  in
			    case Array.sub (t_arity, t) of
			      NONE => Array.update (t_arity, t, SOME len)
			    | SOME len' => if len=len' then () else
				warning ("bad arity for terminal "^sym);
				T (t, map makepat sons)
			  end
		  in
		    makepat pattern
		  end (* val pat *)
		val patarity =
		  let
		    fun cnt (NT _, n) = n+1
		      | cnt (T (_, pat), n) =
			  List.foldl cnt n pat
		  in
		    cnt (pat, 0)
		  end
	      in
		case (BurgHash.find HR ern) of
		  NONE => BurgHash.insert HR (ern, patarity)
		| SOME ar => if ar = patarity then () else
		    warning ("rulename "^ern^" is used with patterns of different arity");
		{nt=nt, pat=pat, ern=ern, cost=cost, num=num}
	      end (* fun newrule *)

	    val _ = app newnt spec_rules
	    val _ = stop_if_error ()
	    val _ = if !nb_nt=0 then error "no rules !" else ()
	    val rules = Array.fromList (map newrule spec_rules)
	    val _ = stop_if_error ()
	    val _ = build_num_to_sym_arrays ()
	    val arity = Array.tabulate (!nb_t,     (* terminals numbers begin at 0 *)
			fn i => case Array.sub (t_arity, i) of
			    NONE => 0 before
				(warning ("terminal "^(get_tsym i)^" unused"))
			  | SOME len => len)
	    val _ = stop_if_error ()
	  in
	    (rules, arity)
	  end (* fun reparse_rules *)


	fun print_intarray array =
	  let
	    fun printit (pos, n) =
	      (if pos>0 then say "," else ();
	       say (Int.toString n)
	      )
	  in
	    arrayiter (printit, array)
	  end

	(*
	 * Print a rule.
	 *)
	fun print_rule ({nt, pat, ern, cost, ...} : rule) =
	  let
	    fun print_sons [] = ()
	      | print_sons [p] = print_pat p
	      | print_sons (p::pl) =
		  (print_pat p; say ","; print_sons pl)
	    and print_pat (NT nt) = say (get_ntsym nt)
	      | print_pat (T (t, sons)) =
		(say (get_tsym t);
		 case (List.length sons) of
		   0 => ()
		 | len => (say "("; print_sons sons; say ")")
		 )
	  in
	    say ((get_ntsym nt)^":\t");
	    print_pat pat;
	    say ("\t= "^ern^" ("^(Int.toString cost)^");\n")
	  end


	fun prep_rule_cons ({ern=ern, ...} : rule) = (!rule_prefix)^ern


	fun prep_node_cons t =
	  let
	    val (sym, _) = Array.sub (!sym_terminals, t)
	  in
	    "N_"^sym
	  end


	fun prep_term_cons t = (!term_prefix)^(#2 (Array.sub (!sym_terminals, t)))


	(*
	 * rules_for_lhs : array with the rules for a given lhs nt
	 * chains_for_rhs : array with the chain rules for a given rhs nt
	 * rule_groups :
	 *      (rl,ntl,str_for_match,uniqstr,iscst,iswot) list list array
	 * array of, for each terminal that begin a pattern
	 *   list of, for each different "case of"
	 *     list of, for each pattern in "case of"
	 *       (rule list * ntl) list
	 *	 string for the match expression printing
	 *	 unique string for constant patterns
	 *       is_cst (bool: is the pattern without nonterminals)
	 *       is_wot (bool: is the pattern without terminals : A(x,y,z,t))
	 *)

	fun build_rules_tables (rules : rule array) =
	  let
	    val rules_for_lhs = Array.array (!nb_nt, []:rule list)
	    val chains_for_rhs = Array.array (!nb_nt, []:rule list)
	
	    fun add_lhs_rhs (rule as {nt,pat,...} : rule) =
	      (Array.update (rules_for_lhs, nt,
		       rule::(Array.sub (rules_for_lhs, nt)));
	       case pat of
		 NT rhs => Array.update (chains_for_rhs, rhs,
				   rule::(Array.sub (chains_for_rhs, rhs)))
	       | _ => ()
	      )


	    fun findntl (rule as {pat,...} : rule) =
	      let
		fun flat (NT nt, ntl) = nt::ntl
		  | flat (T (_,sons), ntl) = List.foldr flat ntl sons
	      in
		(rule, flat (pat,[]))
	      end


	    local
	      exception NotSamePat;
	      fun samepattern (NT _, NT _) = true
		| samepattern (T (t1,spat1), T (t2, spat2)) =
		    if t1=t2
		      then samepatternsons (spat1,spat2)
		      else raise NotSamePat
		| samepattern _ = raise NotSamePat
	      and samepatternsons (l1,l2) =
		if ((forall2 (fn (p1,p2) => samepattern (p1,p2), l1, l2))
		    handle NotSameSize => raise NotSamePat)
		  then true
		  else raise NotSamePat
	    in
	      fun samepat (p1,p2) =
		samepattern (p1,p2) handle NotSamePat => false
	    end

	    fun clustersamepat (zap as ({pat,...}:rule, _), rg) =
	      let
		fun loop ([],_) = (pat,[zap])::rg
		  | loop ((e as (p,zapl))::rest, acc) =
		      if samepat (p,pat)
			then acc@((p,zap::zapl)::rest)	 (* don't keep order *)
			else loop (rest,e::acc)
	      in
		loop (rg, [])
	      end


	    fun minmaxcostlhss (pat,zapl) =
	      let
		fun min (({cost,...}:rule,_), b) = if cost<=b then cost else b
		fun max (({cost,...}:rule,_), b) = if cost>=b then cost else b
		val mincost = List.foldl min inf zapl
		val maxcost = List.foldl max ~1 zapl
		fun addlhs (({nt=lhs,...}:rule,_), lhss) =
		  let
		    fun loop ([],_) = lhs::lhss
		      | loop (e as (i::il), acc) =
			  if lhs=i then lhss
			  else if lhs<i then (rev acc)@(lhs::e)
			  else loop (il,i::acc)
		  in
		    loop (lhss, [])
		  end
		val lhss = List.foldl addlhs [] zapl
	      in
		(pat,zapl,mincost,maxcost,lhss)
	      end
	    

	    (* zapl is (rule,ntl) list *)
	    fun clustersamentl (pat,zapl,min,max,lhss) =
	      let
		fun scan ((r,ntl),clusters) =
		  let
		    fun loop ([],_) = ([r],ntl)::clusters
		      | loop ((e as (rl,ntl'))::rest, acc) =
			if ntl=ntl'
			  then acc@((r::rl,ntl)::rest)	 (* don't keep order *)
			  else loop (rest,e::acc)
		  in
		    loop (clusters ,[])
		  end
		val rlntll = List.foldl scan [] zapl
	      in
		(* rlntll is (rule list,ntl) list *)
		(pat,rlntll,min,max,lhss)
	      end



	    datatype utype = NotUnif | NoMG | SameG | FirstMG | SecondMG

	    local
	      exception Forced of utype
	      fun uniftype (NT _, NT _) = SameG
		| uniftype (NT _, T _) = FirstMG
		| uniftype (T _, NT _) = SecondMG
		| uniftype (T (t1,spat1), T (t2,spat2)) =
		    if t1<>t2 then raise (Forced NotUnif) else
		      (let
			 val sonsg = map2 (uniftype, spat1, spat2)
			 fun addson (NotUnif,_) = raise (Forced NotUnif)
			   | addson (_,NotUnif) = raise (Forced NotUnif)
			   | addson (NoMG,_) = NoMG
			   | addson (_,NoMG) = NoMG
			   | addson (SameG,x) = x
			   | addson (x,SameG) = x
			   | addson (FirstMG, FirstMG) = FirstMG
			   | addson (SecondMG, SecondMG) = SecondMG
			   | addson _ = NoMG
		       in
			 List.foldl addson SameG sonsg
		       end
		       handle NotSameSize => error "bug : uniftype")
	    in
	      fun unify (p1,p2) = (uniftype (p1,p2)) handle (Forced x) => x
	    end


	    (* "matches" is a list.  Each elem is a list of (pat,...)
	     * in increasing order of minimum cost for the rl, and with
	     * either non-unifiable patterns, or with a pattern more general
	     * than another -- but only if the more general one is second, and
	     * it has a strictly higher cost, and all lhs of rules in the more
	     * general pattern are also lhs of some rules in the less general
	     * one (that is, if the less general rule matches, we lose
	     * nothing in not seeing the more general one).
	     * That's all.
	     *)
	    fun clustermatches (elem as (pat,_,mincost,maxcost,lhss),
				matches) =
	      let
		(* works on already (increasing,unique) ordered lists *)
		fun subset ([],_) = true
		  | subset (_,[]) = false
		  | subset (a1 as (e1::l1),e2::l2) =
		      if e1=e2 then subset (l1,l2)
		      else if e1>(e2:int) then subset (a1,l2)
		      else false
		datatype sowhat = ANOTHER | NOTU | AFTER | BEFORE of int
		fun loop (prev, i, []) = prev
		  | loop (prev, i, (p,_,min,max,lh)::rest) =
		      case unify (pat,p) of
			NotUnif => loop (prev,i+1,rest)
		      | NoMG => ANOTHER
		      | SameG => error "bug : clustermatches.SameG"
		      | FirstMG =>
			  if mincost>(max:int) andalso subset (lhss,lh)
			    then
			      case prev of
				NOTU => loop (AFTER,i+1,rest)
			      | AFTER => loop (AFTER,i+1,rest)
			      | BEFORE k => ANOTHER
			      | _ => error "bug : clustermatches.FirstMG"
			    else ANOTHER
		      | SecondMG =>
			  if min>(maxcost:int) andalso subset (lh,lhss)
			    then
			      case prev of
				NOTU => loop (BEFORE i,i+1,rest)
			      | AFTER => loop (BEFORE i,i+1,rest)
			      | BEFORE k => ANOTHER
			      | _ => error "bug : clustermatches.SecondMG"
			    else ANOTHER
		fun insertat (0,prev,next,e) = (rev prev)@(e::next)
		  | insertat (n,prev,x::next,e) = insertat (n-1,x::prev,next,e)
		  | insertat (_,prev,[],e) = rev (e::prev)
		fun try ([],_) = [elem]::matches
		  | try (l::ll,acc) =
		      case loop (NOTU,0,l) of
			ANOTHER => try (ll,l::acc)
		      | NOTU => acc@((elem::l)::ll)	 (* don't keep order *)
		      | AFTER => acc@((l@[elem])::ll)
		      | BEFORE i => acc@((insertat (i,[],l,elem))::ll)
	      in
		try (matches,[])
	      end


	    val uniq_cnt = ref 0

	    fun compute (pat, rlntll, _, _, _) =
	      let
		fun do_pat (NT nt, cnt, iswot) =
		      let val s = Int.toString cnt in
			("(s"^s^"_c,s"^s^"_r,_,_)", cnt+1, iswot)
		      end
		  | do_pat (T (t,sons), cnt, _) =
		      let
			val (s,cnt',_) = do_sons (sons, cnt)
		      in
			("(_,_,"^(prep_node_cons t)
			 ^(if null sons then "" else
			     if null (tl sons) then s else
			       "("^s^")")
			 ^",_)"
			 , cnt', false)
		      end
		and do_sons (sons,cnt) =
		  let
		    val (s,cnt,_,iswot) =
		      List.foldl (fn (pat,(s,cnt,first,iswot)) =>
			       let
				 val (s',cnt',iswot') =
				   do_pat (pat,cnt,iswot)
			       in
				 (if first then s' else s^","^s', cnt', false,
				  iswot')
			       end
			       ) ("",cnt,true,true) sons
		  in (s,cnt,iswot) end

		val (string_for_match, iscst, iswot) =
		  case pat of
		    T (_,sons) =>
		      let val (s,c,iswot) = do_sons (sons,0)
		      in (s,c=0,iswot) end
		  | NT _ => error "bug : string_for_match"
		val uniqstr = Int.toString(!uniq_cnt) before (uniq_cnt := !uniq_cnt+1)
		  
	      in
		(rlntll, string_for_match, uniqstr, iscst, iswot)
	      end
		  
	    val tgroup = Array.array (!nb_t, []:rule list)

	    fun addt (rule as {pat,...} : rule) =
	      case pat of
		T (t,_) => Array.update (tgroup, t, rule::(Array.sub (tgroup, t)))
	      | NT _ => ()
	    val _ = arrayapp (addt,rules)

	    fun eacht t =
	      let
		val v1 = Array.sub (tgroup, t)
		(* v1 : rule list *)
		val v2 = map findntl v1
		(* v2 : (rule * ntl) list  (= zap list) *)
		val v3 = List.foldl clustersamepat [] v2
		(* v3 : (pattern * zap list) list *)
		val v4 = map minmaxcostlhss v3
		(* v4 : (pattern * zap list * mincost * maxcost * lhss) list*)
		val v5 = map clustersamentl v4
	        (* v5 : same thing with (rule list * ntl) list  (= rlntll)
		         instead of zap list *)
		val v6 = List.foldl clustermatches [] v5
		(* v6 : (pattern * rlntll * min * max * lhss) list list *)
	      in
		(* now, inside each subgroup, compute the elements *)
		map (map compute) v6
		(* : (rlntll*str_for_match*uniqstr*iscst*iswot) list list *)
	      end

	    val rule_groups = Array.tabulate (!nb_t, eacht)
	  in
	    arrayapp (add_lhs_rhs, rules);
	    (rules_for_lhs, chains_for_rhs, rule_groups)
	  end


	(*
	 * Check that each nonterminal is reachable from start.
	 *)
	fun check_reachable (start, rules_for_lhs : rule list array) =
	  let
	    val notseen = Array.array (!nb_nt, true)
	    fun explore_nt nt =
	      (Array.update (notseen, nt, false);
	       app (fn ({pat,...}:rule) => reach pat)
	           (Array.sub (rules_for_lhs, nt))
	      )
	    and reach (NT nt) =
	          if Array.sub (notseen, nt) then explore_nt nt else ()
	      | reach (T (t, sons)) = app reach sons
	    fun test (nt, b) =
	      if b then
		warning ("nonterminal "^(get_ntsym nt)^" is unreachable")
	      else ()
	  in
	    explore_nt start;
	    arrayiter (test, notseen);
	    stop_if_error ()
	  end
	      

	(**
	 ** Emit the code
	 **)

	fun emit_type_rule rules =
	  let
	    (* I just want a map, really, not a hashtable. *)
	    val H = BurgHash.mkTable (32, NotThere) : unit BurgHash.hash_table
	    val first = ref true
	    fun onerule (rule as {ern=ern, ...} : rule) =
	      let
		val name = prep_rule_cons rule
	      in
                case (BurgHash.find H name) of
                  NONE =>
		    let
		      val patarity =
			case (BurgHash.find HR ern) of
			  NONE => error "emit_type_rule, no rule name ?"
			| SOME ar => ar
		      fun pr 0 = ""
			| pr 1 = " of (rule * tree)"
			| pr n = ((pr (n-1))^" * (rule * tree)")
		      val constructor = name^(pr patarity)
		    in
		      BurgHash.insert H (name, ());
		      if !first then first := false else say "\t\t| ";
		      saynl constructor
		    end
		| SOME _ => ()
	      end
	  in
	    say "  datatype rule = ";
	    arrayapp (onerule, rules)
	  end



        fun emit_ruleToString rules = let
	    val H : unit BurgHash.hash_table = BurgHash.mkTable(32,NotThere) 
	    val first = ref true
	    fun onerule (rule as {ern,...}:rule) = let
		val name = prep_rule_cons rule
	      in
		case (BurgHash.find H name)
		of NONE => let 
		     val patarity = 
			   case BurgHash.find HR ern 
			   of NONE    => error "emit_ruleToString.onerule"
			    | SOME ar => ar
		     fun pr 0 = ""
		       | pr _ = " _"
		     val constructor = "("^ name ^ (pr patarity) ^ ")"
		   in
		      BurgHash.insert H (name,());
		      if !first then first:=false 
                      else say "      | ruleToString";
		      say constructor;
		      saynl (" = " ^ "\"" ^ name ^ "\"")
		   end
	        | SOME _ => ()
	      end
          in
	     say "    fun ruleToString ";
	     arrayapp (onerule,rules)
          end



	fun emit_debug rules =
	  let
	    fun p_nterm (i, sym) =
	      saynl ("nonterm "^(Int.toString i)^" : "^sym)
	    fun p_rule (i, rule as {num, ...} : rule) =
	      (say ("rule "^(Int.toString num)^" : ");
	       print_rule rule
	      )
	  in
	    saynl "(***** debug info *****";
	    arrayiter (p_nterm, !sym_nonterminals);
	    say "\n";
	    arrayiter (p_rule, rules);
	    saynl "**********************)\n\n"
	  end


	fun emit_struct_burmterm () =
	  let
	     fun loop t =
	       (if t=0 then () else say "\t       | ";
		saynl (prep_term_cons t)
	       )
	  in
	    saynl ("structure "^(!struct_name)^"Ops = struct");
	    say "  datatype ops = ";
	    iter (loop, !nb_t);
	    saynl "end\n\n"
	  end

	fun emit_sig_burmgen () =
	  (saynl ("signature "^(!sig_name)^"_INPUT_SPEC = sig");
	   saynl "  type tree";
	   saynl ("  val opchildren : tree -> "^(!struct_name)
		  ^"Ops.ops * (tree list)");
	   saynl "end\n\n"
	  )
	  
	fun emit_sig_burm rules =
	  (saynl ("signature "^(!sig_name)^" = sig");
	   saynl "  exception NoMatch";
	   saynl "  type tree";
	   emit_type_rule rules;
	   saynl "  val reduce : tree -> rule * tree";
	   saynl "  val ruleToString : rule -> string";
	   saynl "end\n\n"
	  )

	fun emit_beg_functor (rules, arity) =
	  let
	    fun loop_node t =
	      let
		val ar = Array.sub (arity, t)
		fun loop_sons i =
		  (say "s_tree";
		   if i=ar then () else
		     (say " * "; loop_sons (i+1))
		  )
	      in
		say (if t=0 then "      " else "    | ");
		say (prep_node_cons t);
		if ar>0 then
		  (say "\t\tof ";
		   loop_sons 1
		  )
		else ();
		say "\n"
	      end
	  in
	    saynl ("functor "^(!struct_name)^"Gen (In : "
		   ^(!sig_name)^"_INPUT_SPEC) : "^(!sig_name)^" =");
	    saynl "  struct\n";
	    saynl "    type tree = In.tree\n";
	    saynl "    exception NoMatch";
	    emit_type_rule rules;
	    say "\n\n";
	    emit_ruleToString rules; say "\n\n";
	    saynl "    type s_cost = int Array.array";
	    saynl "    type s_rule = int Array.array";
	    saynl "    datatype s_node =";
	    iter (loop_node, !nb_t);
	    saynl "    withtype s_tree = s_cost * s_rule * s_node * tree\n\n";
	    saynl "    val sub = Array.sub";
	    saynl "    val update = Array.update"
	  end


	fun emit_val_cst (rules, arity, chains_for_rhs, rule_groups) =
	  let
	    fun do_cstrule (t, rlntll: (rule list * int list) list,
			    uniqstr, iscst) =
	      if iscst then
		let
		  val ar = Array.sub (arity, t)
		  val a_cost = Array.array (!nb_nt, inf);
		  val a_rule = Array.array (!nb_nt, 0);
		    
		  fun record ({nt=lhs, cost, num, ...} : rule, c) =
		    let
		      val cc = c + cost
		    in
		      if cc < (Array.sub (a_cost, lhs)) then
			(Array.update (a_cost, lhs, cc);
			 Array.update (a_rule, lhs, num);
			 app (fn rule => record (rule, cc))
			     (Array.sub (chains_for_rhs, lhs))
		        )
		      else ()
		    end
		in
		  app ((app (fn rule => record (rule, 0))) o #1) rlntll;
		  if ar=0 then
		    (saynl ("    val leaf_"^(prep_node_cons t)^" =");
		     say "      (Array.fromList [";
		     print_intarray a_cost;
		     say "],\n       Array.fromList [";
		     print_intarray a_rule;
		     saynl ("],\n       "^(prep_node_cons t)^")")
		    )
		  else
		    (say ("    val cst_cost_"^uniqstr^" = Array.fromList [");
		     print_intarray a_cost;
		     saynl "]";
		     say ("    val cst_rule_"^uniqstr^" = Array.fromList [");
		     print_intarray a_rule;
		     saynl "]"
		    )
		end
	      else ()

	    fun do_cstrules (t, ll) =
	      app (app (fn (rlntll,_,uniqstr,iscst,_) =>
		            do_cstrule (t, rlntll, uniqstr, iscst)))  ll
	    val n = Int.toString (!nb_nt)
	    val sinf = Int.toString inf
	  in
	    arrayiter (do_cstrules, rule_groups);
	    saynl ("    val s_c_nothing = Array.array ("^n^","^sinf^")");
	    saynl ("    val s_r_nothing = Array.array ("^n^",0)");
	    say "\n\n"
	  end


	fun emit_label_function (rules, arity, chains_for_rhs, rule_groups) =
	  let
	    val firstcl = ref true
	    fun emit_closure (nt, rl : rule list) =
	      let
		val firstrule = ref true
		fun emit_cl ({nt=lhs, cost, num, ...} : rule) =
		  let
		    val c = Int.toString cost
		    val slhs = Int.toString lhs;
		  in
		    if !firstrule
		      then firstrule := false
		      else say ";\n\t   ";
		    saynl ("if c + "^c^" < sub (s_c,"^slhs^") then");
		    sayinl ("     (update (s_c,"^slhs^",c + "^c^");");
		    sayi ("      update (s_r,"^slhs^","^(Int.toString num)
			  ^")");
		    if null (Array.sub (chains_for_rhs, lhs)) then () else
		      say (";\n\t      closure_"^(get_ntsym lhs)
			   ^" (s_c, s_r, c + "^c^")");
		      saynl "\n\t     )";
		      sayinl "   else";
		      sayi "     ()"
		  end
	      in
		if null rl then () else
		  (if !firstcl then
		     (firstcl := false; say "\tfun") else say "\tand";
		   saynl (" closure_"^(get_ntsym nt)^" (s_c, s_r, c) =");
		   sayi "  (";
		   List.app emit_cl rl;
		   saynl "\n\t  )"
		  ) 
	      end


	    val nbnt = Int.toString (!nb_nt)
	    val sinf = Int.toString inf
	    val firstmatch = ref true

	    fun emit_match t =
	      let (* "(" *)
		val ar = Array.sub (arity, t)

		fun inlistofsons i = (say ("t"^(Int.toString i));
				      if i=(ar-1) then () else say ",")

		fun listofsons () =
		  (say " ("; iter (inlistofsons, ar); say ")")

		val firstcst = ref true
		fun emit_match_cst (_,str,uniq,iscst,_) =
		  if iscst then
		    (if !firstcst
		       then (say "\t    "; firstcst := false)
		       else say "\t  | ";
		     saynl ("("^str^") =>");
		     sayinl ("\t      (cst_cost_"^uniq^", cst_rule_"^uniq^")")
		    )
		  else ()



		val firstcase = ref true
		val firstcaseelem = ref true
		fun emit_match_case (rlntll,str,uniq,iscst,iswot) =
		  if iscst then () else
		    (if !firstcase then
		       (firstcase := false;
			saynl "z =>";
			sayinl "\tlet";
			sayinl ("\t  val s_c = Array.array ("
			      ^nbnt^","^sinf^")");
			sayinl ("\t  val s_r = Array.array ("
				^nbnt^",0)");
			sayinl "\tin")
		     else ();
		     if !firstcaseelem then
		       (firstcaseelem := false;
			sayinl "\tcase z of";
			sayi "\t    ")
		     else sayi "\t  | ";
		     saynl ("("^str^") =>");
		     sayinl "\t      (";
		     let
		       fun dorules (rl : rule list, ntl) =
			 let
			   fun dorule ({nt=lhs, num, cost, ...} : rule) =
			     let
			       val slhs = Int.toString lhs
			       val c = Int.toString cost
			     in
			       sayinl ("\t\t   if c + "^c^" < sub (s_c,"^slhs
				       ^") then");
			       sayinl ("\t\t     (update (s_c, "^slhs
				       ^", c + "^c^");");
			       sayinl ("\t\t      update (s_r, "^slhs
				       ^", "^(Int.toString num)^");");
			       if null (Array.sub (chains_for_rhs, lhs)) then ()
			       else sayinl ("\t\t      closure_"
					    ^(get_ntsym lhs)
					    ^" (s_c, s_r, c + "^c^");");
			       sayinl "\t\t     ())";
			       sayinl "\t\t   else ();"
			     end
			 in
			   sayi "\t       if ";
			   listiter ((fn (i, nt) =>
				      (if i=0 then () else say "andalso ";
					 say ("sub (s"^(Int.toString i)^"_r,"
					      ^(Int.toString (nt:int))
					      ^")<>0 "))),
				     ntl);
			   saynl "then";
			   sayinl "\t\t let";
			   sayi ("\t\t   val c = ");
			   listiter ((fn (i, nt) =>
				      (if i=0 then () else say " + ";
					 say ("sub (s"^(Int.toString i)^"_c,"
					      ^(Int.toString (nt:int))^")"))),
				     ntl);
			   saynl "\n\t\t\t in";
			   app dorule rl;
			   sayinl "\t\t   ()";
			   sayinl "\t\t end";
			   sayinl "\t       else ();"
			 end
		     in
		       app dorules rlntll
		     end;
		     sayinl "\t       ()";
		     sayinl "\t      )"
		    ) (* fun emit_match_case *)

	      in (* ")(" fun emit_match *)

		if !firstmatch
		  then (sayi "  "; firstmatch := false)
		  else sayi "| ";
		say ((!struct_name)^"Ops.");
		saynl ((prep_term_cons t)^" =>");

		if ar=0 then					(* leaf term *)
		  if null (Array.sub (rule_groups, t))
		    then sayinl ("    (s_c_nothing, s_r_nothing, "
				 ^(prep_node_cons t)^")")
		    else sayinl ("    leaf_"^(prep_node_cons t))
		else						    (* ar<>0 *)
		  let
		    val group = Array.sub (rule_groups, t)
		    fun dosamecase eleml =
		      (firstcaseelem := true;
		       app emit_match_case eleml;
		       if (not (!firstcaseelem) andalso
			   not (List.exists (fn (_,_,_,_,iswot) => iswot) eleml))
			 then sayinl "\t  | _ => ()" else ();
		       if (not (!firstcaseelem)) then sayinl "\t  ;" else ()
		       )
		  in
		    sayinl "    let";
		    sayi "      val [";
		    iter (inlistofsons, ar);
		    saynl "] = map rec_label children";
		    sayinl "    in";
		    if null group then		   (* transfert rule *)
		      (sayi "      (s_c_nothing, s_r_nothing, ";
		       say (prep_node_cons t);
		       listofsons ();
		       saynl ")"
		      )
		    else
		      (sayi "      let val (s_c, s_r) = case";
		       listofsons ();
		       saynl " of";
		       app (app emit_match_cst) group;
		       sayi (if !firstcst then "\t    " else "\t  | ");
		       app dosamecase group;
		       if !firstcase then
			 saynl "_ => (s_c_nothing, s_r_nothing)"
		       else
			 (sayinl "\t  (s_c, s_r)";
			  sayinl "\tend"
			 );
		       sayi "      in (s_c, s_r, ";
		       say (prep_node_cons t);
		       listofsons ();
		       saynl ") end"
		      );
		    sayinl "    end"
		  end

	      end (* ")" fun emit_match *)


	  in
	    saynl "    fun rec_label (tree : In.tree) =";
	    saynl "      let";
	    arrayiter (emit_closure, chains_for_rhs);
	    sayinl "val (term, children) = In.opchildren tree";
	    sayinl "val (s_c, s_r, t) = case term of";
	    iter (emit_match, !nb_t);
	    saynl "      in";
	    saynl "        (s_c, s_r, t, tree)";
	    saynl "      end\n"
	  end


	fun emit_reduce_function (rules) =
	  let
	    val firstmatch = ref true

	    fun domatch (rule as {num, pat, ...} : rule) =
	      let
		fun flatsons (the_sons, cnt, ntl) =
		  List.foldl
		    (fn (patson, (b, c, l, ss)) =>
		       let
			 val (c', l', ss') = flat (patson, c, l)
		       in
			 (false, c', l', (if b then ss' else ss^","^ss'))
		       end)
		    (true, cnt, ntl, "")
		    the_sons
		and flat (pat, cnt, ntl) =
		  case pat of
		    NT nt => (cnt+1, nt::ntl, "t"^(Int.toString cnt))
		  | T (t, sons) =>
		      let
			val len = List.length sons
			val (_, cnt', ntl', s') = flatsons (sons, cnt, ntl)
			val nexts =
			  "(_,_,"^(prep_node_cons t)
			  ^(if len=0 then "" else
			      (if len=1 then " "^s' else " ("^s'^")"))
			  ^",_)"
		      in
			(cnt', ntl', nexts)
		      end

		val (cnt, ntl, s) = flat (pat, 0, [])
		val ntl = rev ntl
	      in
		if !firstmatch then (firstmatch := false; say "\t\t(") else
		  say "\t      | (";
		saynl ((Int.toString num)^", "^s^") =>");
		sayi ("\t  ("^(prep_rule_cons rule));
		case pat of
		  NT nt => say (" (doreduce (t0,"^(Int.toString nt)^"))")
		| T (t, _) =>
		    (case List.length ntl of
		       0 => ()
		     | _ =>
			 (say " (";
			  listiter ((fn (i,nt) =>
				     (if i=0 then () else say ", ";
					say ("doreduce (t"^(Int.toString i)^","
					     ^(Int.toString nt)^")"))),
				    ntl);
			  say ")")
		    );
		saynl ")"
	      end
	  in
	    saynl "    fun doreduce (stree : s_tree, nt) =";
	    saynl "      let";
	    sayinl "val (s_c, s_r, _, tree) = stree";
	    sayinl "val cost = sub (s_c, nt)";
	    saynl "      in";

sayinl ("if cost="^(Int.toString inf)^" then");
sayinl ("  (print (\"No Match on nonterminal \"^(Int.toString nt)^\"\\n\");");
sayinl ("   print \"Possibilities were :\\n\";");
sayinl ("   let");
sayinl ("     fun loop n =");
sayinl ("       let");
sayinl ("         val c = Array.sub (s_c, n);");
sayinl ("         val r = Array.sub (s_r, n);");
sayinl ("       in");
sayinl ("         if c=16383 then () else");
sayinl ("           print (\"rule \"^(Int.toString r)^\" with cost \"");
sayinl ("                  ^(Int.toString c)^\"\\n\");");
sayinl ("         loop (n+1)");
sayinl ("       end");
sayinl ("   in");
sayinl ("     (loop 0) handle General.Subscript => ()");
sayinl ("   end;");
sayinl ("   raise NoMatch)");
sayinl ("else");


	    sayinl "  let";
	    sayinl "    val rulensons =";
	    sayinl "      case (sub (s_r, nt), stree) of";
	    arrayapp (domatch, rules);
	    sayinl "      | _ => raise NoMatch (* bug in iburg *)";
	    sayinl "  in";
	    sayinl "    (rulensons, tree)";
	    sayinl "  end";
	    saynl "      end\n"
	  end
	

	fun emit_end_functor (start : int) =
	  (saynl "    fun reduce (tree) =";
	   saynl ("      doreduce (rec_label (tree), "^(Int.toString start)^")");
	   saynl "  end\n\n"
	  )

      in
	let
	  val spec = #1 (Parse.parse s_in) before TextIO.closeIn s_in
	  val _ = reparse_decls spec
	  val (rules, arity) = reparse_rules spec
	  val start =
 	    case !start_sym of
	      NONE => 0
	    | SOME sym =>
		case get_id sym of
		  TERMINAL _ => error ("cannot start on a terminal")
		| NONTERMINAL n => n
	  (* rule numbers for each nonterminal (array) *)
	  val (rules_for_lhs, chains_for_rhs, rule_groups)
	    = build_rules_tables rules
	in
	  check_reachable (start, rules_for_lhs);
	  s_out := (oustreamgen ());
	  emit_header (spec);
	  emit_debug (rules);
	  emit_struct_burmterm ();
	  emit_sig_burmgen ();
	  emit_sig_burm (rules);
	  emit_beg_functor (rules, arity);
	  emit_val_cst (rules, arity, chains_for_rhs, rule_groups);
	  emit_label_function (rules, arity, chains_for_rhs, rule_groups);
	  emit_reduce_function (rules);
	  emit_end_functor (start);
	  emit_tail (spec);
	  TextIO.closeOut (!s_out)
	end
      end (* fun emit *)

  end

