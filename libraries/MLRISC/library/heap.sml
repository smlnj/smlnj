(*
 * This implements a priority queue using a heap
 * 
 * -- Allen
 *)

structure PriorityHeap :> PRIORITY_QUEUE =
struct
   structure A = Array
   exception EmptyPriorityQueue
   exception Unimplemented

   datatype 'a priority_queue = 
       HEAP of { less : 'a * 'a -> bool,
                 heap : 'a A.array,
                 size : int ref
               }

   fun createN(less,N,dummy) = 
       HEAP{less=less, heap = A.array(N,dummy), size = ref 0}

   fun unimplemented() = raise Unimplemented
   
   fun create _ = unimplemented()
   fun merge _ = unimplemented()
   fun mergeInto _ = unimplemented()
   fun toList _ = unimplemented()
  
   fun isEmpty (HEAP{ size = ref 0, ... }) = true
     | isEmpty _ = false

   fun clear (HEAP{ size, ... }) = size := 0

   fun min(HEAP{ size = ref 0, ... }) = raise EmptyPriorityQueue
     | min(HEAP{ heap, ... }) = A.sub(heap, 0)


   fun insert(HEAP{ size, heap, less, ...}) x =
   let val N = !size
       fun siftup 0 = 0
         | siftup i =
       let val j = (i-1) div 2
           val y = A.sub(heap,j)
       in  if less(x,y) then (A.update(heap,i,y); siftup j)
           else i
       end 
   in  size := N + 1;
       A.update(heap,siftup N,x)
   end

   fun siftDown(heap, less, N, i, x) =
   let fun siftdown (i, x) = 
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
       and go(i,x,j,y) = (A.update(heap,i,y); siftdown(j,x))
       val pos_x = siftdown(i, x) 
   in  A.update(heap, pos_x, x); 
       pos_x 
   end
 
   fun deleteMin(HEAP{ size = ref 0, ...}) = raise EmptyPriorityQueue
     | deleteMin(HEAP{ size, heap, less, ...}) =
   let val N = !size - 1
       val min   = A.sub(heap,0)
       val x     = A.sub(heap,N)
       val x_pos = siftDown(heap, less, N, 0, x)
   in  size := N;
       min
   end

   fun fromList less data =
   let val heap = A.fromList data
       val N    = A.length heap
       fun make_heap ~1 = ()
         | make_heap i = 
           (siftDown(heap,less,N,i,A.sub(heap,i)); make_heap(i-1))
   in  if N >= 2 then make_heap((N+1) div 2) else ();
       HEAP{ less = less, heap = heap, size = ref N } 
   end
end
