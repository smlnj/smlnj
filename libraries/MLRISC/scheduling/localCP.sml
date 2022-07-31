functor LocalCriticalPath
   (DDG : SCHEDULER_DDG) : SCHEDULING_RANKS where type edge = DDG.latency =
struct

   structure DDG = DDG
   structure I   = DDG.I
   structure G   = Graph
   structure A   = Array

   type edge = DDG.latency

   fun rank(DDG as G.GRAPH ddg) =
   let val N        = #capacity ddg ()
       val len      = A.array(N,0)
       val parents = A.tabulate(N,fn i => length(#in_edges ddg i))
       fun process i =
       let fun g((i,j,l)::es,n) = g(es,Int.max(A.sub(len,j) + l + 1,n))
             | g([],n) = n
       in  A.update(len,i,g(#out_edges ddg i,0))
       end 
       fun order((i,_),(j,_)) = 
           case Int.compare(A.sub(len,i),A.sub(len,j)) of
              EQUAL => A.sub(parents,i) > A.sub(parents,j)
           |  LESS  => false
           |  GREATER => true
   in  app process (rev (GraphTopsort.topsort DDG (map #1 (#nodes ddg ()))));
       order
   end

end
