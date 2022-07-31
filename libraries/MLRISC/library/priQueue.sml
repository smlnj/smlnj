(*
 * Priority queues implemented as leftist trees
 * 
 * -- Allen
 *)

structure PriorityQueue :> PRIORITY_QUEUE =
struct

                       
   (* A leftist tree is a binary tree with priority ordering
    * with the invariant that the left branch is always the taller one         
    *)
   datatype 'a leftist = NODE of 'a * int * 'a leftist * 'a leftist
                       | EMPTY

   datatype 'a priority_queue = PQ of { less : 'a * 'a -> bool, 
                                        root : 'a leftist ref 
                                      }

   exception EmptyPriorityQueue

       (* assume a is smaller than b *)
   fun mergeTrees less (a,b) =
   let fun dist EMPTY           = 0
         | dist (NODE(_,d,_,_)) = d

       fun m (EMPTY,a)  = a
         | m (a, EMPTY) = a
         | m (a as NODE(x,d,l,r), b as NODE(y,d',l',r')) =
           let val (root,l,r) = 
                  if less(x,y) then (x,l,m(r,b)) else (y,l',m(r',a)) 
               val d_l   = dist l
               val d_r   = dist r
               val (l,r) = if d_l >= d_r then (l,r) else (r,l)
           in  
               NODE(root,1+Int.max(d_l,d_r),l,r) 
           end
   in  m (a, b) 
   end

   fun create less = PQ { less = less, root = ref EMPTY }
   fun createN (less,_,_) = create less

   fun min (PQ { root = ref(NODE(x,_,_,_)), ... }) = x
     | min _ = raise EmptyPriorityQueue

   fun isEmpty (PQ { root = ref EMPTY, ... }) = true
     | isEmpty _                              = false

   fun clear (PQ { root, ... }) = root := EMPTY

   fun deleteMin (PQ { root = root as ref(NODE(x,_,l,r)), less }) =
         (root := mergeTrees less (l,r); x)   
     | deleteMin _ = raise EmptyPriorityQueue

   fun merge (PQ { root = r1, less }, PQ { root = r2, ...}) =
      PQ { root = ref(mergeTrees less (!r1,!r2)), less = less }

   fun mergeInto { src = PQ { root = ref t1, less }, 
                   dst = PQ { root = r as ref t2, ...} } =
      r := mergeTrees less (t1,t2)

   fun mergeElems (less, q, elements) =
   let fun m (q,[])    = q
         | m (q,e::es) = m(mergeTrees less (q, NODE(e,1,EMPTY,EMPTY)), es)
   in  m(q, elements)
   end

   fun insert (PQ { root = r as ref t1, less}) x =
      r := mergeTrees less (t1,NODE(x,1,EMPTY,EMPTY)) 

   fun fromList less list =
       PQ { root = ref(mergeElems(less, EMPTY, list)), less = less }

   fun collect (EMPTY, e) = e
     | collect (NODE(x,_,l,r),e) = collect(l,collect(r,x::e))

   fun toList (PQ { root = ref t, ... }) = collect (t, [])

end

