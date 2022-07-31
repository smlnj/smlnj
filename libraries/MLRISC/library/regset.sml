(*
 * Register set datatype. Implemented as sorted lists.
 *
 * -- Allen
 *) 

structure RegSet :> REGISTER_SET =
struct

   type reg = int

   type regset = reg list

   val empty = [] 

   fun sort [] = []
     | sort (l as [_]) = l
     | sort (l as [x,y]) = if Int.<(x,y) then l else 
                           if x = y then [x] else [y,x]
     | sort l =
       let val (a,b) = split (l,[],[])
       in  mergeUniq(sort a, sort b)
       end

   and split ([],a,b)    = (a,b)
     | split (r::rs,a,b) = split(rs,r::b,a)

   and mergeUniq(l as u::us, l' as v::vs) =
         if u = v then mergeUniq(l,vs)
         else if Int.<(u,v) then u::mergeUniq(us,l')
         else v::mergeUniq(l,vs)
     | mergeUniq(l,[]) = l
     | mergeUniq([],l) = l

   fun union []      = []
     | union (r::rs) = mergeUniq(r,union rs)

   fun difference ([],_) = []
     | difference (set,[]) = set
     | difference (set as r::rs,set' as r'::rs') =
        if r = r' then difference(rs,set')
        else if r < r' then r::difference(rs,set')
        else (* r > r' *) difference(set,rs')

   fun intersect (set,[]) = []
     | intersect ([],set) = []
     | intersect (set as r::rs,set' as r'::rs') =
         if r = r' then r::intersect(rs,rs')
         else if r < r' then intersect(rs,set')
         else intersect(set,rs')

   fun intersects []  = []
     | intersects [a] = a
     | intersects (a::b) = intersect(a,intersects b)
   
   fun ==([],[]) = true
     | ==(r::rs,r'::rs') = (r : int) = r' andalso ==(rs,rs')
     | ==(_,_)   = false

   fun isEmpty [] = true
     | isEmpty _  = false

   val app = List.app

   fun contains ([], r)    = false
     | contains (r'::rs,r) = r' = r orelse (r > r' andalso contains(rs,r))

   fun exists (set, [])    = false
     | exists (set, r::rs) = contains(set,r) orelse exists(set,rs)

   fun insert([],r) = [r]
     | insert(set as r'::rs,r) =
         if r = r' then set
         else if r' < r then r'::insert(rs,r)
         else r::set

   fun insertChanged (set,r) = 
   let fun ins [] = ([r],true)
         | ins (set as r'::rs) =
             if r = r' then (set,false)
             else if r > r' then
                let val (rs,changed) = ins rs
                in  if changed then (r'::rs,true)
                               else (set,false)
                end
             else (r::set,true)
   in  ins set
   end

   fun remove ([],r) = []
     | remove (set as r'::rs,r) =
         if r' = r then rs
         else if r' < r then r'::remove(rs,r)
         else set
     
   fun removeChanged (set,r) =
   let fun rmv [] = ([],false)
         | rmv (set as r'::rs) =
              if r = r' then (rs,true)
              else if r > r' then
                   let val (rs,changed) = rmv rs
                   in  if changed then (r'::rs,true)
                                  else (set,false)
                   end
              else (set,false)
   in
       rmv set
   end

   fun fromList l       = sort l
   fun fromSortedList l = l
   fun toList set       = set

   fun toString set =
   let fun collect([],l) = l
         | collect(r::rs,l) = Int.toString r::collect'(rs,l)
       and collect'(rs,l) = 
           let val l = collect(rs,l)
           in  case l of [_] => l 
                       | l  => ","::l
           end
   in  String.concat("{"::collect(set,["}"]))
   end

   val op + = mergeUniq
   val op - = difference
   val op * = intersect

end

