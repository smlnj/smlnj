(*
 * This module performs low-level flow insensitive points-to 
 * analysis for type-safe languages.
 *)
structure PointsTo : POINTS_TO =
struct

   datatype edgekind = PI | DOM | RAN | RECORD | MARK

   structure C = CellsBasis

   datatype cell = 
     LINK  of region
   | SREF  of C.cell * edges ref
   | WREF  of C.cell * edges ref
   | SCELL of C.cell * edges ref
   | WCELL of C.cell * edges ref
   | TOP   of {mutable:bool, id:C.cell, name:string}
      (* a collapsed node *)

   withtype region = cell ref
   and      edges  = (edgekind * int * region) list

   fun error msg = MLRiscErrorMsg.error("PointsTo",msg)

   (* PI > DOM > RAN > RECORD *)
   fun greaterKind(PI,_) = false   
     | greaterKind(DOM,PI) = false
     | greaterKind(RAN,(PI | DOM)) = false
     | greaterKind(RECORD,(PI | DOM | RAN)) = false
     | greaterKind(MARK,(PI | DOM | RAN | RECORD)) = false
     | greaterKind _ = true

   fun less(k,i,k',i') = k=k' andalso i > i' orelse greaterKind(k,k')

   val sort : (edgekind * int * region) list -> 
              (edgekind * int * region) list = 
      ListMergeSort.sort (fn ((k,i,_),(k',i',_)) => less(k,i,k',i'))

   val newMem = ref(fn _ => error "newMem") : (unit -> C.cell) ref
   fun reset f = newMem := f

   fun newSRef() = ref(SREF(!newMem(),ref []))
   fun newWRef() = ref(WREF(!newMem(),ref []))
   fun newSCell() = ref(SCELL(!newMem(),ref []))
   fun newWCell() = ref(WCELL(!newMem(),ref []))
   fun newTop{name,mutable} = 
     ref(TOP{mutable=mutable, id= !newMem(), name=name})

   fun find(ref(LINK x)) = find x
     | find x = x

   fun mut(ref(LINK x)) = mut x
     | mut(r as ref(TOP{mutable=false, id, name})) = 
       (r := TOP{mutable=true, id=id, name=name})
     | mut(r as ref(SCELL x)) = r := SREF x
     | mut(r as ref(WCELL x)) = r := WREF x
     | mut _ = ()

   and weak(ref(LINK x)) = weak x
     | weak(ref(TOP _)) = ()
     | weak(r as ref(SCELL x)) = (r := WCELL x; mergePis x)
     | weak(r as ref(SREF x)) = (r := WREF x; mergePis x)
     | weak _ = ()

   and mergePis(_,edges) = 
       let val x = newSCell()
           fun merge([],es') = es'
             | merge((PI,_,y)::es,es') = (unify(x,y); merge(es, es'))
             | merge(e::es,es') = merge(es,e::es')
       in  edges := (PI,0,x)::merge(!edges, []) end

   and getIth(k,i,ref(LINK x)) = getIth(k,i,x)
     | getIth(k,i,r as ref(TOP _)) = r
     | getIth(k,i,ref(SREF(_,edges))) = getIth'(k,i,edges)
     | getIth(k,i,ref(WREF(_,edges))) = getIth'(k,i,edges)
     | getIth(k,i,ref(SCELL(_,edges))) = getIth'(k,i,edges)
     | getIth(k,i,ref(WCELL(_,edges))) = getIth'(k,i,edges)

   and getIth'(k,i,edges) =
       let fun search((k',i',x)::es) = 
                 if k = k' andalso i = i' then find x else search es
             | search [] = 
               let val x = newSCell() 
               in edges := (k,i,x) :: !edges; x end
       in  search(!edges) end

   and unify(x,y) =
   let val x = find x
       val y = find y
       fun linkImmut(edges,x,y) = (x := LINK y; collapseAll(!edges,y))
       fun linkMut(edges,x,y) = (x := LINK y; mut y; collapseAll(!edges,y))
       fun linky(ex,ey,x,y) = (x := LINK y; ey := unifyList(!ex,!ey))
       fun linkx(ex,ey,x,y) = (y := LINK x; ex := unifyList(!ex,!ey))
       fun linkWREF(ex,ey,id,x,y) = 
       let val ey = unifyList(!ex,!ey)
           val n  = WREF(id,ref ey)
       in  x := LINK y; y := n end

   in  if x = y then () else
       case (!x,!y) of
         (TOP{mutable=false,...},TOP{mutable=false, ...}) => (x := LINK y)
       | (TOP _, TOP _)           => (x := LINK y; mut y)

       | (SREF(_,edges),  TOP _)  => linkMut(edges,x,y)
       | (WREF(_,edges),  TOP _)  => linkMut(edges,x,y)
       | (SCELL(_,edges), TOP _)  => linkImmut(edges,x,y)
       | (WCELL(_,edges), TOP _)  => linkImmut(edges,x,y)

       | (TOP _, SREF(_,edges))   => linkMut(edges,y,x)
       | (TOP _, WREF(_,edges))   => linkMut(edges,y,x)
       | (TOP _, SCELL(_,edges))  => linkImmut(edges,y,x)
       | (TOP _, WCELL(_,edges))  => linkImmut(edges,y,x)

       | (WREF(_,e1), WREF(_,e2)) => linky(e1,e2,x,y)
       | (SREF(_,e1), WREF(_,e2)) => linky(e1,e2,x,y)
       | (WCELL(_,e1),WREF(_,e2)) => linky(e1,e2,x,y)
       | (SCELL(_,e1),WREF(_,e2)) => linky(e1,e2,x,y)

       | (WREF(_,e1), SREF(_,e2))  => linkx(e1,e2,x,y)
       | (SREF(_,e1), SREF(_,e2))  => linkx(e1,e2,x,y)
       | (WCELL(_,e1),SREF(id,e2)) => linkWREF(e1,e2,id,x,y)
       | (SCELL(_,e1),SREF(_,e2))  => linky(e1,e2,x,y)

       | (WREF(_,e1), WCELL(_,e2)) => linkx(e1,e2,x,y)
       | (SREF(_,e1), WCELL(id,e2)) => linkWREF(e1,e2,id,x,y)
       | (WCELL(_,e1),WCELL(_,e2)) => linkx(e1,e2,x,y)
       | (SCELL(_,e1),WCELL(_,e2)) => linky(e1,e2,x,y)

       | (WREF(_,e1), SCELL(_,e2)) => linkx(e1,e2,x,y)
       | (SREF(_,e1), SCELL(_,e2)) => linkx(e1,e2,x,y)
       | (WCELL(_,e1),SCELL(_,e2)) => linkx(e1,e2,x,y)
       | (SCELL(_,e1),SCELL(_,e2)) => linkx(e1,e2,x,y)
       | _ => error "unify"
   end

   and collapseAll([],_)    = ()
     | collapseAll((_,_,x)::xs,y) = (unify(x,y); collapseAll(xs,y))
   
   and unifyList(l1,l2) =
       let fun merge([],l) = l
             | merge(l,[]) = l
             | merge(a as (c as (k,i,x))::u,b as (d as (k',i',y))::v) =
                if k=k' andalso i=i' then (unify(x,y); c::merge(u,v)) 
                else if less(k,i,k',i') then d::merge(a,v) else c::merge(u,b)
       in merge(sort l1,sort l2) end

   fun pi(x,i)  = getIth(PI,i,x)
   fun dom(x,i) = getIth(DOM,i,x)
   fun ran(x,i) = getIth(RAN,i,x)
   fun sub(x,i) = let val m = getIth(PI,i,x) in mut m; m end

   fun offset(x,i) = (unify(x,newTop{mutable=false,name=""}); find x)
   
   and unifyAll(x,[]) = ()
     | unifyAll(x,(_,_,y)::l) = (unify(x,y); unifyAll(x,l)) 

   fun mkHeader(NONE,es) = es
     | mkHeader(SOME h,es) = (PI,~1,h)::es

   fun mkAlloc(header, xs) = 
   let fun collect(_,[],l) = l
         | collect(i,x::xs,l) = collect(i+1,xs,(PI,i,x)::l)
   in  (!newMem(), ref(mkHeader(header,collect(0,xs,[])))) end

   fun mkRecord(header,xs) = ref(SCELL(mkAlloc(header, xs)))
   fun mkRef(header,x)     = ref(SREF(mkAlloc(header, [x])))
   fun mkArray(header,xs)  = ref(SREF(mkAlloc(header, xs)))
   fun mkVector(header,xs) = ref(SCELL(mkAlloc(header, xs)))
   fun mkLambda(xs) = 
   let fun collect(_,[],l) = l
         | collect(i,x::xs,l) = collect(i+1,xs,(DOM,i,x)::l)
   in  ref(SCELL(!newMem(), ref(collect(0,xs,[])))) end

   fun app(f,xs) =
   let fun loop(_,[]) = ()
         | loop(i,x::xs) = (unify(dom(f,i),x); loop(i+1,xs))
   in  loop(0,xs) end

   fun ret(f,xs) =
   let fun loop(_,[]) = ()
         | loop(i,x::xs) = (unify(ran(f,i),x); loop(i+1,xs))
   in  loop(0,xs) end

   fun strongUpdate(a,i,x) = unify(sub(a,i),x)
   fun strongSubscript(a,i) = sub(a,i)
   fun weakUpdate(a,x) = 
       let val elem = sub(a, 0)
       in  weak elem; unify(elem, x) end
   fun weakSubscript(a) = 
       let val elem = sub(a, 0)
       in  weak elem; elem end

   fun interfere(x,y) = find x = find y

   val maxLevels = MLRiscControl.mkInt ("points-to-show-max-levels",
					"max # of level to show in pointsTo")
   val _ = maxLevels := 3

   fun toString r = show(!r, !maxLevels)

   and show(LINK x, lvl) = show(!x, lvl)
     | show(SREF(id,es), lvl) = "sref"^C.toString id^edges(es, lvl)
     | show(WREF(id,es), lvl) = "wref"^C.toString id^edges(es, lvl) 
     | show(SCELL(id,es), lvl) = "s"^C.toString id^edges(es, lvl) 
     | show(WCELL(id,es), lvl) = "w"^C.toString id^edges(es, lvl) 
     | show(TOP{name="",mutable=true,id,...}, _) = "var"^C.toString id
     | show(TOP{name="",mutable=false,id,...}, _) = "const"^C.toString id
     | show(TOP{name,...}, _) = name

   and edges(es, ~1) = ""
     | edges(es, lvl) = 
       let fun prInt i = if i < 0 then "-"^Int.toString(~i) else Int.toString i
           fun add(a,"") = a
             | add(a,b)  = a^","^b
           fun cnv((PI,i,x),s) = add(prInt i^"->"^show(!x, lvl-1),s)
             | cnv(_,s) = s
       in  case foldr cnv "" (!es) of 
             "" => "" 
           | t  => if lvl = 0 then "..." else "["^t^"]" 
       end

end
