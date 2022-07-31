(* staticprof.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

signature STATICPROF = sig
  val initfk : unit -> unit
  val incfk :  CPS.fun_kind * int -> unit
  val incln : int -> unit 
  val reportfk : unit -> unit
end

functor StaticProf (MachSpec : MACH_SPEC) : STATICPROF = struct

local open CPS
      structure CGoptions = Control.CG
in

val pr = Control.Print.say
val lenlimit = 40
val esize = Array.array(lenlimit+1,0)
val ksize = Array.array(lenlimit+1,0)
val csize = Array.array(lenlimit+1,0)
val links = Array.array(11,0)
val numvars = ref 0 
val printf = app pr

fun zeroArray(a) = 
  let val len = Array.length a
      fun h(n) = if n >= len then ()
                 else (Array.update(a,n,0); h(n+1))
   in h(0)
  end

fun initfk() = (numvars := 0;
                app zeroArray [esize,ksize,csize,links])

fun incfk(fk,sz) = 
  let val a = case fk of ESCAPE => esize
                       | CONT => csize
                       | _ => ksize
      val i = if (sz >= lenlimit) then lenlimit-1 else sz      
      val c = Array.sub(a,i)
      val s = Array.sub(a,lenlimit)
   in Array.update(a,i,c+1);
      Array.update(a,lenlimit,s+(sz+1))
  end

fun incln(sz) = 
  let val i = if (sz >= 10) then 10 else sz
      val n = !numvars
      val c = Array.sub(links,i)
   in numvars := n+sz;
      Array.update(links,i,c+1)
  end

val im = Int.toString
fun field (st,w) =
        if w <= String.size st then st
        else let val s = "                              " ^ st
              in substring(s,String.size s - w, w)
             end

fun ifield(i,w) = if i=0 then field(" ",w)
                  else (field(im i,w))

fun fromto(m,n) = if m>n then [] else m::(fromto(m+1,n))

fun reportsz(fk) = 
  let val (a,s) = case fk of ESCAPE => (esize,"ESCAPE")
                           | CONT => (csize, "CALLEE")
                           | _ => (ksize,"KNOWN")
      fun loop(n,k,j) = 
        if (k >= j) then (printf ["\n"])
        else (printf [" | ",ifield(Array.sub(a,n+k),4)];
              loop(n,k+1,j))
      fun loop2(n) =
        if n >= lenlimit then ()
        else (let val k = Int.min(10,lenlimit-n)
               in printf [ifield(n div 10,2)];
                  loop(n,0,k);
                  loop2(n+k)
              end)
       val totalsize = Array.sub(a,lenlimit)
   in printf ["CSregs = ",im(MachSpec.numCalleeSaves),
              " Total Size = ",im(totalsize),
              " for ",s," functions: \n"];
      printf ["  "];
      app (fn n => printf [" | ",ifield(n,4)]) [0,1,2,3,4,5,6,7,8,9];
      pr "\n";
      printf ["--"];
      app (fn n => printf ["---","----"]) [0,1,2,3,4,5,6,7,8,9];
      pr "\n";
      loop2(0)
  end

fun reportfk() = 
  let val s = Array.sub(esize,lenlimit) +
              Array.sub(csize,lenlimit) +
              Array.sub(ksize,lenlimit)
   in if s <> 0 then
       (printf ["**"];
        app (fn n => printf ["*******"]) [0,1,2,3,4,5,6,7,8,9];
        pr "\n";
        printf ["CSregs = ",im(MachSpec.numCalleeSaves),
                " Total Links = ",im(!numvars),
                " for all variables: \n"];
        printf ["  "];
        app (fn n => printf [" | ",ifield(Array.sub(links,n),4)]) 
                                                       (fromto(1,10));
        pr "\n\n";
        reportsz(ESCAPE); pr "\n\n";
        reportsz(KNOWN); pr "\n\n";
        reportsz(CONT); pr "\n\n";
        printf ["**"];
        app (fn n => printf ["*******"]) [0,1,2,3,4,5,6,7,8,9];
        printf ["\n\n"])
      else ()
  end

end (* local *)
end (* structure ReportSZ *)

