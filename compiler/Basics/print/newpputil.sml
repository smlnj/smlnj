(* Basics/print/newpputil.sml *)

structure NewPPUtil : NEW_PP_UTIL =
struct

local
  structure PP = NewPP
in    

(* We vertically align the element formats, taking account of the possibly
 * that the two header strings have different sizes by left-padding the 
 * shorter header, using the leftpad function to equalize the sizes of the headers. *)

(* padleft : string * string -> string * string *)
(* pad the shorter string with spaces on the left to make them the same size.
 * ASSERT: (s1', s2') = pad (s1, s2) => size s1' = size s2' = max (size s1, size s2).
 *)
fun padHeaders (s1, s2) =
    let val maxsize = Int.max (size s1, size s2)
     in (StringCvt.padLeft #" " maxsize s1,
	 StringCvt.padLeft #" " maxsize s2)
    end

(* vHeaders : {header1 : string, header2 : string, formatter: 'a -> PP.format}
           -> 'a list -> PP.format} *)
fun vHeaders {header1: string, header2: string, formatter: 'a -> PP.format}
		    (elems: 'a list) =
    let val (header1, header2) = padHeaders (header1, header2)
     in case elems
	  of nil => PP.empty
	   | elem :: rest =>
	       PP.vcat
		 [PP.hcat [PP.text header1, formatter elem] ::
		  map (fn e => PP.hcat [PP.text header2, formatter e]) rest]
    end

(* vHeaderFormats : {header1 : string, header2 : string, formatter: 'a -> PP.format}
                 -> PP.format list -> PP.format} *)
fun vHeaderFormats {header1: string, header2: string} (elems: PP.format list) =
    let val (header1, header2) = padHeaders (header1, header2)
     in case elems
	  of nil => PP.empty
	   | elem :: rest =>
	       PP.vcat
		 [PP.hcat [PP.text header1, elem] ::
		  map (fn fmt => PP.hcat [PP.text header2, fmt]) rest]
    end

end (* top local *)
end (* structure NewPPUtil *)
