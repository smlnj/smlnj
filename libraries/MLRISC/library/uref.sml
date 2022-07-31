(*
 * References that can be merged
 *
 * -- Allen
 *)

signature UNION_FIND_REF =
sig

   type 'a uref

   val uref   : 'a -> 'a uref
   val !!     : 'a uref -> 'a
   val ::=    : 'a uref * 'a -> unit
   val ==     : 'a uref * 'a uref -> bool
   val eq     : 'a uref * 'a uref -> bool
   val find   : 'a uref -> 'a uref
   val union  : ('a * 'a -> 'a) -> 'a uref * 'a uref -> bool
   val union' : 'a uref * 'a uref -> bool
end

structure UnionFindRef :> UNION_FIND_REF =
struct

   datatype 'a uptree = ROOT of 'a * int
                      | LINK of 'a uref
   withtype 'a uref = 'a uptree ref

   fun uref x = ref(ROOT(x,1))
   fun eq (x : 'a uref,y : 'a uref) = x = y
   fun find r =
   let fun look (r as ref(ROOT _))  = r
         | look (r' as ref(LINK r)) = 
           let val r'' = look r
           in  if r <> r'' then r' := LINK r'' else ();
               r''
           end
   in  look r end

   fun == (x,y) = find x = find y

   fun !! r     = let val ROOT(x,_) = !(find r) in x end
   fun ::=(r,x) = let val r as ref(ROOT(_,w)) = find r 
                  in  r := ROOT(x,w) end
   fun union f (x,y) =
   let val r  as ref(x as ROOT(i,w))  = find x
       val r' as ref(y as ROOT(j,w')) = find y
   in  if r = r' then false
       else if w > w' then
               (r  := ROOT(f(i,j),w+w'); r' := LINK r; true)
            else
               (r' := ROOT(f(i,j),w+w'); r := LINK r'; true)
   end
   fun union' (x,y) =
   let val r  as ref(x as ROOT(i,w))  = find x
       val r' as ref(y as ROOT(j,w')) = find y
   in  if r = r' then false
       else if w > w' then
               (r  := ROOT(i,w+w'); r' := LINK r; true)
            else
               (r' := ROOT(j,w+w'); r := LINK r'; true)
   end
end

