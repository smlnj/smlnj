(*
 * This implements a priority queue for nodes in a graph
 * 
 * -- Allen
 *)

functor NodePriorityQueue(A : ARRAY) : NODE_PRIORITY_QUEUE =
struct
   structure G = Graph

   exception EmptyPriorityQueue

   datatype node_priority_queue = 
       Q of { less : G.node_id * G.node_id -> bool,
              heap : G.node_id A.array, 
              pos  : int A.array, 
              size : int ref
            }
   fun create N less = Q { less = less, 
                           heap = A.array(N,0),
                           pos  = A.array(N,0),
                           size = ref 0
                         }
  
   fun isEmpty (Q{ size = ref 0, ... }) = true
     | isEmpty _ = false

   fun clear (Q{ size, ... }) = size := 0

   fun min(Q{ size = ref 0, ... }) = raise EmptyPriorityQueue
     | min(Q{ heap, ... }) = A.sub(heap, 0)

   fun decreaseWeight(Q{ size, heap, pos, less }, x) =
   let fun siftup 0 = 0
         | siftup i =
       let val j = (i-1) div 2
           val y = A.sub(heap,j)
       in  if less(x,y) then (A.update(heap,i,y); A.update(pos,y,i); siftup j)
           else i
       end 
       val x_pos = siftup(A.sub(pos,x))
   in  A.update(heap,x_pos,x); A.update(pos,x,x_pos)
   end

   fun insert(q as Q{ size, heap, pos, ...}, x) =
   let val N = !size
   in  A.update(heap,N,x); A.update(pos,x,N); size := N + 1;
       decreaseWeight(q,x)
   end

   fun deleteMin(Q{ size = ref 0, ...}) = raise EmptyPriorityQueue
     | deleteMin(Q{ size, heap, pos, less}) =
   let val N = !size - 1
       fun siftdown (i,x) = 
       let val j = i + i + 1
           val k = j + 1
       in  if j >= N then i
           else let val y = A.sub(heap,j)
                in  if k >= N then
                       if less(y,x) then go(i,x,j,y) else i 
                    else 
                       let val z = A.sub(heap,k)
                       in  if less(y,x) then
                              if less(z,y) then go(i,x,k,z) 
                              else go(i,x,j,y)
                           else if less(z,x) then go(i,x,k,z)
                           else i
                       end
                end
       end
       and go(i,x,j,y) = (A.update(heap,i,y); A.update(pos,y,i); siftdown(j,x))
       val min   = A.sub(heap,0)
       val x     = A.sub(heap,N)
       val x_pos = siftdown(0, x)
   in  A.update(heap,x_pos,x); A.update(pos,x,x_pos); 
       size := N;
       min
   end

   fun fromGraph less (G.GRAPH G) =
   let val N    = #order G ()
       val heap = A.array(N,0) 
       val pos  = A.array(#capacity G (),0) 
       fun siftdown (i,x) = 
       let val j = i*2 + 1
           val k = j + 1
       in  if j >= N then A.update(heap,i,x)
           else if k >= N then
              let val y = A.sub(heap,j)
              in  if less(y,x) then go(i,x,j,y) else A.update(heap,i,x)
              end
           else 
              let val y = A.sub(heap,j)
                  val z = A.sub(heap,k)
              in  if less(y,x) then
                     if less(z,y) then go(i,x,k,z) 
                     else go(i,x,j,y)
                  else if less(z,x) then go(i,x,k,z)
                  else A.update(heap,i,x)
              end
       end
       and go(i,x,j,y) = (A.update(heap,i,y); siftdown(j,x))
 
       fun make_heap ~1 = ()
         | make_heap i = (siftdown(i,A.sub(heap,i)); make_heap(i-1))

       val i = ref 0 
       val _ = #forall_nodes G (fn (u,_) => 
                 let val i' = !i in A.update(heap,i',u); i := i'+1 end)

       val _ = make_heap((N+1) div 2)

       val _ = A.appi (fn (i,x) => A.update(pos,x,i)) heap

   in  Q{ less = less, heap = heap, pos = pos, size = ref N } 
   end
end
