(* bt-engine.sml
 *
 * COPYRIGHT (c) 2008 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Implements a regular expressions matcher based on a backtracking search.
 *)

structure BackTrackEngine : REGEXP_ENGINE =
  struct

    exception Error

    structure S = RegExpSyntax
    structure M = MatchTree

    type regexp = S.syntax

  (* a match specifies the position (as a stream) and the length of the match *)
    type 'a match = {pos : 'a, len : int} MatchTree.match_tree

    fun compile r = r

    fun scan (regexp,getc,pos,stream) =
	let fun getc' (s) = (case (getc (s))
			       of SOME v => v
				| NONE => raise Subscript)
	    (* This function gets an empty match structure, for when the appropriate
	     * alternative is not followed at all
	     *)
	    fun getMatchStructure (S.Group e) = (*[M.Match (NONE,getMatchStructure e)]*) getMatchStructure e
	      | getMatchStructure (S.Alt l) = List.concat (map getMatchStructure l)
	      | getMatchStructure (S.Concat l) = List.concat (map getMatchStructure l)
	      | getMatchStructure (S.Interval (e,_,_)) = getMatchStructure e
	      | getMatchStructure (_) = []
	    (* Walk a regular expression in continuation-passing style
	     * The continuation is simply a list of all that is left to do
	     * Continuations only seem to arise when concatenation is considered
	     *
	     * Walk returns the boolean status of the beast, and a match_tree
	     * containing the match information.
	     * Also: the last position scanned and the remainder stream
	     * MODIFICATION: walk returns a list of matches
	     * (because we need to extract the longest match)
	     *)
	    fun max [] sel = raise Error
	      | max (x::xs) sel =
		let fun max' [] curr currSel = curr
		      | max' (x::xs) curr currSel = let val xSel = sel(x)
						    in if (xSel>currSel)
							   then max' xs x xSel
						       else max' xs curr currSel
						    end
		in
		    max' xs x (sel x)
		end
	    fun longest l = max l (#3: 'a * 'b * int * 'c -> int)
	    fun optMinus1 (SOME i) = SOME (i-1)
	      | optMinus1 NONE = NONE
	    fun walk (S.Group e,cont,p,inits) =
		(case walk (e,[],p,inits)
		   of [] => [(false,[],p,inits)]
		    | ((b,matches,last,s)::ls) =>
		       let fun loop [] cLast 1 cCont cList =
			   let val [(b,matches,last,s)] = cList
			       val [(b',matches',last',s')] = cCont
			   in
			       [(b', (M.Match ({pos=inits, len=last-p},
					       matches))::matches',last',s')]
			   end
			     | loop [] cLast n cCont cList = raise Error
			     | loop ((b,matches,last,s)::es) cLen cNum cCont cList =
			   let val v as (_,_,last',_) = longest (walk (S.Concat [], cont,last,s))
			   in
			       if (last' > cLen)
				   then loop es last' 1 [v] [(b,matches,last,s)]
			       else if (last' = cLen)
					then loop es cLen (cNum+1) (v::cCont)
						  ((b,matches,last,s)::cList)
				    else loop es cLen cNum cCont cList
			   end
		       in
			   loop ls last 1 [longest(walk (S.Concat [],cont,last,s))]
				[(b,matches,last,s)]
		       end)
	      | walk (S.Alt [],[],p,inits) = [(true,[],p,inits)]
	      | walk (S.Alt [], (c::cs),p,inits) = walk (c,cs,p,inits)
	      | walk (S.Alt l, cont,p,inits) =
		     let fun loop [] = []
			   | loop (e::es) = let val g = longest (walk (e,cont,p,inits))
					    in
						if (#1 g)
						    then g::(loop es)
						else loop es
					    end
		     in
			 loop l
		     end
	      | walk (S.Concat [],[],p,inits) = [(true,[],p,inits)]
	      | walk (S.Concat [], (c::cs),p,inits) = walk (c,cs,p,inits)
	      | walk (S.Concat (e::es),cont,p,inits) = walk (e,(es@cont),p,inits)
	      | walk (S.Interval (e,0,SOME 0),[],p,inits) = [(true,[],p,inits)]
	      | walk (S.Interval (e,0,SOME 0),(c::cs),p,inits) = walk (c,cs,p,inits)
	      | walk (S.Interval(e,0,k), cont, p, inits) =
		     let val (b',matches',last',s') = longest (walk (S.Concat [],cont,p,inits))
			 val (b,matches,last,s) = longest (walk (S.Interval (e,1,k),cont,p,inits))
		     in
			 if ((b andalso b' andalso last >= last') orelse (b andalso (not b')))
			     then [(b,matches,last,s)]
			 else if ((b' andalso b andalso last' > last) orelse (b' andalso (not b)))
(* FIXME: getMatchStructure isn't really doing anything so we need to fix this code.  But how? *)
				  then [(b',(getMatchStructure e)@matches',last',s')]
			      else [(false,[],p,inits)]
		end
	      | walk (S.Interval (e,1,SOME 1),cont,p,inits) = walk (e,cont,p,inits)
	      | walk (S.Interval (e,1,k),cont,p,inits) =
		let val (b',matches',last',s') = longest (walk (e,[],p,inits)) (* need to match 1 *)
		in
		    if (not b')
			then [(false, [], p, inits)]
		    else let val (b,matches,last,s) = longest (walk (S.Interval (e,1,optMinus1 k),
								     cont,last',s'))
			     val (b'',matches'',last'',s'') = longest (walk (S.Concat [],
									     cont,last',s'))
			 in
			     if (b andalso b'' andalso last'' >= last)
				 then [(b'',matches'@matches'',last'',s'')]
			     else if (b)
				      then [(b,matches,last,s)]
				  else [(b'',matches'@matches'',last'',s'')]
			 end
		end
	      | walk (S.Interval(e,n1,k), cont, p, inits) =
		walk (S.Concat [e,S.Interval (e,n1-1,optMinus1 k)],cont,p,inits)
	      | walk (S.MatchSet set,[],p,inits) =
		if (S.CharSet.isEmpty set)
		    then [(true,[],p,inits)]
		else
		    (case (getc (inits))
		       of SOME (chr,s) =>
			   let val b = S.CharSet.member (set,chr)
			   in
			       [(b,[],p+(if b then 1 else 0),(if b then s else inits))]
			   end
			| NONE => [(false,[],p,inits)])
	      | walk (S.MatchSet set,(c::cs),p,inits) =
		if (S.CharSet.isEmpty set)
		    then walk (c,cs,p,inits)
		else (case (getc (inits))
			of SOME (chr,s) =>
			    if (S.CharSet.member (set,chr))
				then walk (c,cs,(p+1),s)
			    else [(false,[],p,inits)]
			 | NONE => [(false,[],p,inits)])
	      | walk (S.NonmatchSet set,[],p,inits) =
		(case (getc (inits))
		   of SOME (chr,s) =>
		       let val b = not (S.CharSet.member (set,chr))
		       in
			   [(b, [], p+(if b then 1 else 0),(if b then s else inits))]
		       end
		    | NONE => [(false,[],p,inits)])
	      | walk (S.NonmatchSet set,(c::cs),p,inits) =
		(case (getc (inits))
		   of SOME (chr,s) => if (S.CharSet.member (set,chr))
					  then [(false,[],p,inits)]
				      else walk (c,cs,(p+1),s)
		    | NONE => [(false,[],p,inits)])
	      | walk (S.Char ch,[],p,inits) =
		(case (getc (inits))
		   of SOME (chr,s) =>
		       let val b = (chr = ch)
		       in
			   [(b, [],p+(if b then 1 else 0),(if b then s else inits))]
		       end
		    | NONE => [(false,[],p,inits)])
	      | walk (S.Char ch,(c::cs),p,inits) =
		(case (getc (inits))
		   of SOME (chr,s) => if (chr = ch)
					  then walk (c,cs,(p+1),s)
				      else [(false,[],p,inits)]
		    | NONE => [(false,[],p,inits)])
	      | walk (S.Begin,[],p,inits) = [(p=0,[],p,inits)]
	      | walk (S.Begin,(c::cs),p,inits) = if (p=0)
						     then walk (c,cs,p,inits)
						 else [(false,[],p,inits)]
	      | walk (S.End,[],p,inits) = [(not (Option.isSome (getc (inits))),[],p,inits)]
	      | walk (S.End,(c::cs),p,inits) = if (Option.isSome (getc (inits)))
						   then [(false,[],p,inits)]
					       else walk (c,cs,p,inits)
	    val l = walk (regexp,[],pos,stream) handle Subscript => [(false,[],pos,stream)]
	    val v as (result,matches,last,s') = longest l handle _ => (false,[],pos,stream)
	in
	    if result then SOME(M.Match ({pos=stream,len=last-pos}, matches),s')
	    else NONE
	end

    fun prefix regexp getc stream = scan (regexp,getc,0,stream)

    fun find regexp getc stream =
	let fun loop (p,s) = (case (scan (regexp,getc,p,s))
				of NONE => (case (getc (s))
					      of SOME (_,s') => loop (p+1,s')
					       | NONE => NONE)
				 | SOME v => SOME v)
	in
	    loop (0,stream)
	end

    fun match [] getc stream = NONE
      | match l getc stream =
	let val m = map (fn (r,f) => (scan (r, getc,0,stream), f)) l
	    (* find the longest SOME *)
	    fun loop ([],max,len) = max
	      | loop ((NONE,_)::xs,max,maxlen) = loop (xs,max,maxlen)
	      | loop ((SOME(m,cs),f)::xs,max,maxlen) =
		let val {len, pos} = MatchTree.root m
		in
		    if (len>maxlen)
			then loop (xs,(SOME(m,cs),f),len)
		    else loop (xs,max,maxlen)
		end
	    val (max,f) = loop (tl(m),hd(m),~1)
	in
	    case max
	      of NONE => NONE
	       | SOME (m,cs) => SOME (f m,cs)
	end

    end
