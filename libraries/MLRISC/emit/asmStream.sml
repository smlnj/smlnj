(* asmStream.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

(* AsmStream - this structure is available to all codegenerators.
 *             Typically asmOutStream is rebound to a file.
 *)

signature ASM_STREAM = sig
  val asmOutStream : TextIO.outstream ref
  val withStream : TextIO.outstream -> ('a -> 'b) -> 'a -> 'b
end

structure AsmStream : ASM_STREAM = struct
  val asmOutStream = ref TextIO.stdOut
  fun withStream stream body x = let
     val s = !asmOutStream 
     val _ = asmOutStream := stream
  in
    (body x before asmOutStream := s)
       handle e => (asmOutStream := s; raise e)
  end   
end



