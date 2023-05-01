(* sourceutil.sml *)

structure SourceUtil =
struct

(* stringToLines : string * int => (string * int list) *)

fun stringToLines (s, initpos) =
    let val lines = String.fields (fn c => c = #"\n") s
        val sizes = map size lines
     in case lines
          of [x] => [(x,initpos)]
           | x::rest => 
	     rev(foldl (fn (s,c as (s0,p)::_) => (s, p+size s0+1)::c) [(x,initpos)] rest)
    end

fun splitAt (s, p) = 
    let val pre = substring(s,0,p)
        val post = substring(s,p,size s - p)
     in (pre,post)
    end

fun hlctrl (s,p,hlstr) = 
    let val (pre,post) = splitAt (s,p)
     in concat [pre,hlstr,post]
    end

(* highlight region (p1,p2) within one line, represented by string s *)
fun hl (s, p1, p2, hlOn, hlOff) =
    let val (s1,s2) = splitAt(s,p1)
        val (s2,s3) = splitAt(s2,p2-p1)
     in concat [s1,hlOn,s2,hlOff,s3]
    end

val redOn = "\027[1;31m"
val hlOff = "\027[0m"

end (* structure SourceUtil *)
