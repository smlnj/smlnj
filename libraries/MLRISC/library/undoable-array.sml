(*
 *  Create a version of arrays that keeps track of its versions.
 *
 *  -- Allen
 *)

functor UndoableArray
      (structure Array : ARRAY
       structure Log : TRANSACTION_LOG) : ARRAY =
struct

   structure A = Array

   type 'a vector = 'a A.vector 
   type 'a array =  'a A.array * Log.version ref

   infix 9 sub

   val maxLen = A.maxLen

   fun array (n,d) = (A.array (n,d),ref(!Log.version))

   fun get (a,_) = a

   fun commit (a,v) = fn ver => v := ver
   fun rollback (a,v) = 
   let val N = A.length a
       val a' = A.array(N,A.sub(a,0))
   in  A.copy{src=a, si=0, len=NONE, dst = a', di = 0};
       fn ver => (A.copy{src=a',si=0,len=NONE,dst=a,di=0}; v := ver)
   end

   fun get' (A as (a,v)) =
   let val ver = !Log.version
   in  if !v <> ver then 
	 (Log.add_object {commit   = commit A, 
	                  rollback = rollback A};
          v := ver
         )
       else ();
       a
   end
	   
   fun length a = A.length(get a)
   fun a sub i = A.sub(get a,i) 
   fun update (a, i, e) = A.update(get' a, i, e)
   fun extract (a, i, j) = A.extract(get a, i, j)
   fun copy {src, si, len, dst, di } =
       A.copy{src=get src, si=si, len=len, dst=get' dst, di=di}
   fun copyVec { src, si, len, dst, di } = 
       A.copyVec { src = src, si = si, len = len, dst = get' dst, di = di }
   fun tabulate (n, f) = (A.tabulate(n,f),ref(!Log.version))
   fun fromList l = (A.fromList l,ref(!Log.version))
   fun app f a = A.app f (get a)
   fun foldl f u a = A.foldl f u (get a)
   fun foldr f u a = A.foldr f u (get a)
   fun modify f a = A.modify f (get' a)
   fun appi f (a,i,j) = A.appi f (get a, i, j)
   fun foldli f u (a, i, j) = A.foldli f u (get a, i, j)
   fun foldri f u (a, i, j) = A.foldri f u (get a, i, j)
   fun modifyi f (a, i, j) = A.modifyi f (get' a, i, j)

end

