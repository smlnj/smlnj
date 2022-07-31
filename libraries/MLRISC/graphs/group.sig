(*
 * Commutative groups.
 * 
 * -- Allen
 *)

signature ABELIAN_GROUP =
sig
   type elem 
   val +    : elem * elem -> elem
   val -    : elem * elem -> elem
   val ~    : elem -> elem
   val zero : elem
   val <    : elem * elem -> bool
   val ==   : elem * elem -> bool
end

signature ABELIAN_GROUP_WITH_INF =
sig

   include ABELIAN_GROUP
   val inf : elem

end
