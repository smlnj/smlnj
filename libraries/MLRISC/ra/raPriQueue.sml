  (*
   * Priority Queue.  Let's hope the compiler will inline it for performance
   *)
functor RaPriQueue(type elem val less : elem * elem -> bool) : RA_PRIORITY_QUEUE = struct

 (* A leftist tree is a binary tree with priority ordering
  * with the invariant that the left branch is always the taller one         
  *)
 type elem = elem
 datatype pri_queue = TREE of elem * int * pri_queue * pri_queue | EMPTY

 fun merge'(EMPTY, EMPTY) = (EMPTY, 0)
   | merge'(EMPTY, a as TREE(_, d, _, _)) = (a, d)
   | merge'(a as TREE(_, d, _, _), EMPTY) = (a, d)
   | merge'(a as TREE(x, d, l, r), b as TREE(y, d', l', r')) =
     let val (root, l, r1, r2) = 
	     if less(x, y) then (x, l, r, b) else (y, l', r', a) 
	 val (r, d_r) = merge'(r1, r2)
	 val d_l = case l of EMPTY => 0 | TREE(_, d, _, _) => d 
	 val (l, r, d_t) = if d_l >= d_r then (l, r, d_l+1) else (r, l, d_r+1)
     in  (TREE(root, d_t, l, r), d_t) end

 fun merge(a, b) = #1(merge'(a, b))

 fun add (x, EMPTY) =  TREE(x, 1, EMPTY, EMPTY)
   | add (x, b as TREE(y, d', l', r')) = 
     if less(x,y) then TREE(x, d'+1, b, EMPTY)
     else #1(merge'(TREE(x, 1, EMPTY, EMPTY), b))
end

