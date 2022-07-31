(*
 * An ``intelligent'' pretty printer
 *)
signature PP =
sig

   type pp (* a piece of pretty printed text *)

   val ++       : pp * pp -> pp      (* combine two pieces of text *)
   val !        : string -> pp       (* an identifier *)
   val !!       : string -> pp       (* a symbol *)
   val int      : int -> pp          (* an integer *)
   val int32    : Int32.int -> pp    (* an integer *)
   val intinf   : IntInf.int -> pp   (* an integer *)
   val char     : char -> pp         (* an ML character *)
   val word     : word -> pp         (* an ML word constant *)
   val word32   : Word32.word -> pp  (* an ML word constant *)
   val real     : real -> pp         (* an ML real constant *)
   val string   : string -> pp       (* an ML string constant *)
   val bool     : bool -> pp         (* a boolean *)
   val nop      : pp                 (* empty text *)
   val indent   : pp                 (* add indentation *)
   val settab   : pp                 (* set the current position as tab *)
   val unindent : pp                 (* remove indentation *)
   val tab      : pp                 (* move to tab position *)
   val tab'     : int -> pp
   val setmode  : string -> pp
   val unsetmode : pp
   val select   : (string -> pp) -> pp
   val sp       : pp
   val nl       : pp
   val nl'      : int * int -> pp    
   val block    : pp -> pp
   val line     : pp -> pp
   val seq      : (pp * pp * pp) -> pp list -> pp
   val paren    : pp -> pp
   val group    : string * string -> pp -> pp
   val concat   : pp list -> pp
   val text     : pp -> string
   val textWidth : int -> pp

end
