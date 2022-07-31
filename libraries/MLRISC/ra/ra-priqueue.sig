(*
 * Interface of a fast (applicative) 
 * version of priority queue just for the register allocator
 * 
 * -- Allen
 *)
signature RA_PRIORITY_QUEUE =
sig

   type elem  
   datatype pri_queue = 
       EMPTY | TREE of elem * int * pri_queue * pri_queue

   val add   : elem * pri_queue -> pri_queue
   val merge : pri_queue * pri_queue -> pri_queue
end
