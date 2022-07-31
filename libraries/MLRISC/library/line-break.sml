signature LINE_BREAK =
sig
    val lineBreak : int -> string -> string
end

structure LineBreak : LINE_BREAK =
struct
   fun lineBreak maxChars text =
   let fun loop([],_,text) = String.concat(rev text)
         | loop(s::ss,n,text) = 
           let val m = String.size s + 1
               val n' = m+n
           in  if n' > maxChars 
               then loop(ss, m, s::" "::"\n"::text)
               else loop(ss, n', s::" "::text)
           end
       val toks = String.fields (fn c => c = #" ") text
   in  loop(toks, 0, []) end
end
