(* Copyright 1989 by AT&T Bell Laboratories *)
(* printutil.sml *)

structure PrintUtil : PRINTUTIL =
struct

  (* printing functions -- print to stdOut *)
  val say = Control_Print.say

  fun newline () = say "\n"
  fun tab 0 = () | tab n = (say " "; tab(n-1))
  fun nlindent n = (newline(); tab n)

  fun printSequence (separator: string) pr elems =
      let fun prElems [el] = pr el
	    | prElems (el::rest) = (pr el; say separator; prElems rest)
	    | prElems [] = ()
       in prElems elems
      end

  fun printvseq ind (sep:string) pr elems =
      let fun prElems [el] = pr el
	    | prElems (el::rest) = (pr el; nlindent ind; say sep; prElems rest)
	    | prElems [] = ()
       in prElems elems
      end

  fun printClosedSequence (front: string, sep, back:string) pr elems =
      (say front; printSequence sep pr elems; say back)

  fun printSym(s: Symbol.symbol) = say (Symbol.name s)

  (* formatting functions -- translate to "formatted" string *)

  fun trimmed (s, maxsz) =
      if size s <= maxsz then s
      else String.substring (s, 0, maxsz) ^ "#"

  fun quoteString s = concat ["\"", String.toString s, "\""]
  fun formatString s = quoteString (trimmed (s, !Control_Print.stringDepth))
  fun formatIntInf i = trimmed (IntInf.toString i, !Control_Print.intinfDepth)

  (* listToString : (string * string * string) -> ('a -> string) -> 'a list -> string *)
  fun listToString (front,sep,back) (toStr : 'a -> string) (l: 'a list) =
      let fun prElems [] = [back]
	    | prElems [x] = [toStr x, back]
	    | prElems (x::xs) = toStr x :: sep :: prElems xs
       in concat ( front :: prElems l )
      end

  (* interpws : string list -> string
   *  interpolate white space (" ") between the elements of a list of strings, then concat *)
  fun interpws (strs : string list) = String.concatWith " " strs

end (* structure PrintUtil *)

