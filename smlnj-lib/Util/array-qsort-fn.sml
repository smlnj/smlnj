(* array-qsort-fn.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * Functor for in-place sorting of abstract arrays.
 * Uses an engineered version of quicksort due to 
 * Bentley and McIlroy.
 *
 *)

functor ArrayQSortFn (A : MONO_ARRAY) : MONO_ARRAY_SORT =
  struct

    structure A = A

    fun isort (array, start, n, cmp) = let
          fun item i = A.sub(array,i)
          fun swap (i,j) = let 
                val tmp = A.sub(array,i)
                in A.update(array,i,A.sub(array,j)); A.update(array,j,tmp) end
          fun vecswap (i,j,0) = ()
            | vecswap (i,j,n) = (swap(i,j);vecswap(i+1,j+1,n-1))
          fun insertSort (start, n) = let
                val limit = start+n
                fun outer i =
                      if i >= limit then ()
                      else let
                        fun inner j =
                              if j = start then outer(i+1)
                              else let
                                val j' = j - 1
                                in
                                  if cmp(item j',item j) = GREATER
                                    then (swap(j,j'); inner j')
                                    else outer(i+1)
                                end
                        in inner i end
                in
                  outer (start+1)
                end
          in insertSort (start, n); array end

    fun sortRange (array, start, n, cmp) = let
          fun item i = A.sub(array,i)
          fun swap (i,j) = let 
                val tmp = A.sub(array,i)
                in A.update(array,i,A.sub(array,j)); A.update(array,j,tmp) end
          fun vecswap (i,j,0) = ()
            | vecswap (i,j,n) = (swap(i,j);vecswap(i+1,j+1,n-1))
          fun insertSort (start, n) = let
                val limit = start+n
                fun outer i =
                      if i >= limit then ()
                      else let
                        fun inner j =
                              if j = start then outer(i+1)
                              else let
                                val j' = j - 1
                                in
                                  if cmp(item j',item j) = GREATER
                                    then (swap(j,j'); inner j')
                                    else outer(i+1)
                                end
                        in inner i end
                in
                  outer (start+1)
                end

          fun med3(a,b,c) = let
		val a' = item a and b' = item b and c' = item c
		in
		  case (cmp(a', b'),cmp(b', c'))
		   of (LESS, LESS) => b
		    | (LESS, _) => (
			case cmp(a', c') of LESS => c | _ => a)
		    | (_, GREATER) => b
                    | _ => (case cmp(a', c') of LESS => a | _ => c)
		  (* end case *)
		end

          fun getPivot (a,n) = 
                if n <= 7 then a + n div 2
                else let
                  val p1 = a
                  val pm = a + n div 2
                  val pn = a + n - 1
                  in
                    if n <= 40 then med3(p1,pm,pn)
                    else let
                      val d = n div 8
                      val p1 = med3(p1,p1+d,p1+2*d)
                      val pm = med3(pm-d,pm,pm+d)
                      val pn = med3(pn-2*d,pn-d,pn)
                      in
                        med3(p1,pm,pn)
                      end
                  end
          
          fun quickSort (arg as (a, n)) = let
                fun bottom limit = let
                      fun loop (arg as (pa,pb)) =
                            if pb > limit then arg
                            else case cmp(item pb,item a) of
                              GREATER => arg
                            | LESS => loop (pa,pb+1)
                            | _ => (swap arg; loop (pa+1,pb+1))
                      in loop end
      
                fun top limit = let
                      fun loop (arg as (pc,pd)) =
                            if limit > pc then arg
                            else case cmp(item pc,item a) of
                              LESS => arg
                            | GREATER => loop (pc-1,pd)
                            | _ => (swap arg; loop (pc-1,pd-1))
                      in loop end

                fun split (pa,pb,pc,pd) = let
                      val (pa,pb) = bottom pc (pa,pb)
                      val (pc,pd) = top pb (pc,pd)
                      in
                        if pb > pc then (pa,pb,pc,pd)
                        else (swap(pb,pc); split(pa,pb+1,pc-1,pd))
                      end

                val pm = getPivot arg
                val _ = swap(a,pm)
                val pa = a + 1
                val pc = a + (n-1)
                val (pa,pb,pc,pd) = split(pa,pa,pc,pc)
                val pn = a + n
                val r = Int.min(pa - a, pb - pa)
                val _ = vecswap(a, pb-r, r)
                val r = Int.min(pd - pc, pn - pd - 1)
                val _ = vecswap(pb, pn-r, r)
                val n' = pb - pa
                val _ = if n' > 1 then sort(a,n') else ()
                val n' = pd - pc
                val _ = if n' > 1 then sort(pn-n',n') else ()
                in () end

          and sort (arg as (_, n)) = if n < 7 then insertSort arg 
                                     else quickSort arg
          in sort (start,n) end

    fun sort cmp array = sortRange(array,0,A.length array, cmp)

    fun sorted cmp array = let
          val len = A.length array
          fun s (v,i) = let
                val v' = A.sub(array,i)
                in
                  case cmp(v,v') of
                    GREATER => false
                  | _ => if i+1 = len then true else s(v',i+1)
                end
          in
            if len = 0 orelse len = 1 then true
            else s(A.sub(array,0),1)
          end

  end (* ArraySortFn *)

