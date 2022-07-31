(*
 * This implements a functional map 
 *
 * -- Allen
 *)

signature TREE_MAP =
sig
    type key
    type 'a map 
    exception NotFound
    val empty    : 'a map
    val insert   : 'a map * key * 'a -> 'a map
    val remove   : 'a map * key -> 'a map
    val lookup   : 'a map * key -> 'a
    val lookup'  : 'a map * key -> key * 'a
    val toList   : 'a map -> (key * 'a) list
    val fromList : (key * 'a) list -> 'a map
    val foldl    : (key * 'a * 'b -> 'b) -> 'b -> 'a map -> 'b
    val foldr    : (key * 'a * 'b -> 'b) -> 'b -> 'a map -> 'b 
end

functor TreeMap
   (type key
    exception NotFound
    val compare : key * key -> order
   ) : TREE_MAP = 
struct
   type key = key
   datatype 'a map = NODE of key * 'a * 'a map * 'a map
                   | EMPTY

   exception NotFound = NotFound
   val empty = EMPTY
   fun insert(EMPTY,k',v') = NODE(k',v',EMPTY,EMPTY)
     | insert(NODE(k,v,l,r),k',v') =  
          case compare(k',k) of
             EQUAL   => NODE(k,v',l,r)
          |  LESS    => NODE(k,v,insert(l,k',v'),r)
          |  GREATER => NODE(k,v,l,insert(r,k',v'))
   fun lookup'(EMPTY,k) = raise NotFound
     | lookup'(NODE(k,v,l,r),k') =
         case compare(k',k) of
            EQUAL   => (k,v)
         |  LESS    => lookup'(l,k')
         |  GREATER => lookup'(r,k')
   fun lookup(t,k) = #2(lookup'(t,k))
   fun remove(EMPTY,k) = EMPTY
     | remove(NODE(k,v,l,r),k') =
       case compare(k',k) of 
          EQUAL =>
          (case (l,r) of
              (EMPTY,r) => r
           |  (l,EMPTY) => l
           |  (_,_)   => let fun remove_succ EMPTY = EMPTY
                               | remove_succ(NODE(_,_,EMPTY,r)) = r
                               | remove_succ(NODE(k,v,l,r)) =
                                     NODE(k,v,remove_succ l,r)
                         in  NODE(k,v,l,remove_succ r)
                         end
          )
       |  LESS    => NODE(k,v,remove(l,k'),r)
       |  GREATER => NODE(k,v,l,remove(r,k'))

    fun foldl f x =
    let fun g(EMPTY,x) = x
          | g(NODE(k,v,l,r),x) = g(l,f(k,v,g(r,x)))
    in  fn t => g(t,x) end

    fun foldr f x = 
    let fun g(EMPTY,x) = x
          | g(NODE(k,v,l,r),x) = g(r,f(k,v,g(l,x)))
    in  fn t => g(t,x) end

    fun toList m = 
    let fun collect(EMPTY,L) = L
          | collect(NODE(k,v,l,r),L) = collect(l,collect(r,(k,v)::L))
    in  collect(m,[]) end

    fun fromList l = 
    let fun f([],m) = m
          | f((k,v)::l,m) = f(l,insert(m,k,v))
    in  f(l,EMPTY) end
     
end

