(* printutil.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

structure PrintUtil : PRINTUTIL =
  struct

    (* printing functions -- print to stdOut *)
    val say = Control_Print.say

    fun newline () = say "\n"
    fun tab 0 = () | tab n = (say " "; tab(n-1))
    fun nlindent n = (newline(); tab n)

    fun printDepth () = Int.max(!Control_Print.printDepth, 1)
    fun printLength () = Int.max(!Control_Print.printLength, 1)
    fun stringDepth () = Int.max(!Control_Print.stringDepth, 3)
    fun intinfDepth () = Int.max(!Control_Print.intinfDepth, 3)
    fun lineWidth () = Int.max(!Control_Print.lineWidth, 40)

    fun printSequence (separator: string) pr elems = let
        fun prElems [el] = pr el
          | prElems (el::rest) = (pr el; say separator; prElems rest)
          | prElems [] = ()
        in
          prElems elems
        end

    fun printvseq ind (sep:string) pr elems = let
        fun prElems [el] = pr el
          | prElems (el::rest) = (pr el; nlindent ind; say sep; prElems rest)
          | prElems [] = ()
        in
          prElems elems
        end

    fun printClosedSequence (front: string, sep, back:string) pr elems =
        (say front; printSequence sep pr elems; say back)

    (* formatting functions -- translate to "formatted" string *)

    fun trimmed (s, maxsz) = if size s <= maxsz
          then s
          else String.substring (s, 0, maxsz) ^ "#"

    fun quoteString s = concat ["\"", String.toString s, "\""]
    fun formatString s = quoteString (trimmed (s, stringDepth()))
    fun formatIntInf i = trimmed (IntInf.toString i, intinfDepth())

    (* listToString : (string * string * string) -> ('a -> string) -> 'a list -> string *)
    fun listToString (front,sep,back) (toStr : 'a -> string) (l: 'a list) = let
        fun prElems [] = [back]
          | prElems [x] = [toStr x, back]
          | prElems (x::xs) = toStr x :: sep :: prElems xs
        in
          concat ( front :: prElems l )
        end

    fun interpws (strs : string list) = String.concatWith " " strs

  end (* structure PrintUtil *)

