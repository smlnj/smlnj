(*
 * Union-find
 *
 * -- Allen
 *)

signature UNION_FIND =
sig

    type 'a union_find

    val union_find : int * (int -> 'a) -> 'a union_find
    val find       : 'a union_find -> int -> 'a
    val union'     : 'a union_find -> int * int -> bool
    val union      : 'a union_find -> ('a * 'a -> 'a) -> int * int -> bool
    val ==         : 'a union_find -> int * int -> bool
end

structure Unionfind :> UNION_FIND =
struct

   structure A = Array 
   structure U = UnionFindRef

   type 'a union_find = 'a U.uref A.array 
 
   fun union_find (n,f) = A.tabulate(n,(fn i => U.uref(f i)))
 
   fun find U x = U.!!(A.sub(U,x))

   fun union' U (x,y) = U.union' (A.sub(U,x),A.sub(U,y))

   fun union  U f (x,y) = U.union f (A.sub(U,x),A.sub(U,y))

   fun == U (x,y) = U.==(A.sub(U,x),A.sub(U,y)) 

end
