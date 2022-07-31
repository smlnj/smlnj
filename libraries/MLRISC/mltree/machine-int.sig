(*
 * This module implements 2's complement arithmetic of various widths.
 *)
signature MACHINE_INT =
sig

   type machine_int = IntInf.int
   type sz = int (* width in bits *)

   datatype div_rounding_mode = DIV_TO_ZERO | DIV_TO_NEGINF

   val hash : machine_int -> word

   (* machine_int <-> other types *)
   val fromInt     : sz * int -> machine_int
   val fromInt32   : sz * Int32.int -> machine_int
   val fromInt64   : sz * Int64.int -> machine_int
   val fromWord    : sz * word -> machine_int
   val fromWord32  : sz * Word32.word -> machine_int
   val fromWord64  : sz * Word64.word -> machine_int

   val toInt       : sz * machine_int -> int
   val toInt32     : sz * machine_int -> Int32.int
   val toInt64     : sz * machine_int -> Int64.int
   val toWord      : sz * machine_int -> word
   val toWord32    : sz * machine_int -> Word32.word
   val toWord64    : sz * machine_int -> Word64.word

   val fromString  : sz * string -> machine_int option
   val toString    : sz * machine_int -> string
   val toHexString : sz * machine_int -> string
   val toBinString : sz * machine_int -> string


    (* when in doubt, use this to narrow to a given width! *)
   val narrow : sz * IntInf.int -> machine_int

    (* convert to signed/unsigned representation *)
   val signed   : sz * machine_int -> IntInf.int
   val unsigned : sz * machine_int -> IntInf.int

    (* Split a machine_int of length sz into words of word sizes.
     * The least significant word is at the front of the list
     *)
   val split : {sz:sz, wordSize:sz, i:machine_int} -> machine_int list

   (* two's complement operators *)
   val NEG   : sz * machine_int -> machine_int
   val ABS   : sz * machine_int -> machine_int
   val ADD   : sz * machine_int * machine_int -> machine_int
   val SUB   : sz * machine_int * machine_int -> machine_int
   val MULS  : sz * machine_int * machine_int -> machine_int
   val DIVS  : div_rounding_mode *
	       sz * machine_int * machine_int -> machine_int
   val REMS  : div_rounding_mode *
	       sz * machine_int * machine_int -> machine_int

   (* unsigned operators *)
   val MULU  : sz * machine_int * machine_int -> machine_int
   val DIVU  : sz * machine_int * machine_int -> machine_int
(*
   val QUOTU : sz * machine_int * machine_int -> machine_int
*)
   val REMU  : sz * machine_int * machine_int -> machine_int

   (* Signed, trapping operators, may raise Overflow *)
   val ABST  : sz * machine_int -> machine_int
   val NEGT  : sz * machine_int -> machine_int
   val ADDT  : sz * machine_int * machine_int -> machine_int
   val SUBT  : sz * machine_int * machine_int -> machine_int
   val MULT  : sz * machine_int * machine_int -> machine_int
   val DIVT  : div_rounding_mode *
	       sz * machine_int * machine_int -> machine_int

   (* bit operators *)
   val NOTB  : sz * machine_int -> machine_int
   val ANDB  : sz * machine_int * machine_int -> machine_int
   val ORB   : sz * machine_int * machine_int -> machine_int
   val XORB  : sz * machine_int * machine_int -> machine_int
   val EQVB  : sz * machine_int * machine_int -> machine_int
   val SLL   : sz * machine_int * machine_int -> machine_int
   val SRL   : sz * machine_int * machine_int -> machine_int
   val SRA   : sz * machine_int * machine_int -> machine_int
   val BITSLICE : sz * (int * int) list * machine_int -> machine_int

   (* Other useful operators *)
   val Sll       : sz * machine_int * word -> machine_int
   val Srl       : sz * machine_int * word -> machine_int
   val Sra       : sz * machine_int * word -> machine_int
   val pow2      : int -> machine_int
   val maxOfSize : sz -> machine_int
   val minOfSize : sz -> machine_int
   val isInRange : sz * machine_int -> bool

   (* Indexing *)
   val bitOf     : sz * machine_int * int -> word        (* 0w0 or 0w1 *)
   val byteOf    : sz * machine_int * int -> word        (* 8 bits *)
   val halfOf    : sz * machine_int * int -> word        (* 16 bits *)
   val wordOf    : sz * machine_int * int -> Word32.word (* 32 bits *)

   (* type promotion *)
   val SX    : sz (* to *) * sz (* from *) * machine_int -> machine_int
   val ZX    : sz (* to *) * sz (* from *) * machine_int -> machine_int

   (* comparisions *)
   val EQ  : sz * machine_int * machine_int -> bool
   val NE  : sz * machine_int * machine_int -> bool
   val GT  : sz * machine_int * machine_int -> bool
   val GE  : sz * machine_int * machine_int -> bool
   val LT  : sz * machine_int * machine_int -> bool
   val LE  : sz * machine_int * machine_int -> bool
   val LTU : sz * machine_int * machine_int -> bool
   val GTU : sz * machine_int * machine_int -> bool
   val LEU : sz * machine_int * machine_int -> bool
   val GEU : sz * machine_int * machine_int -> bool
end
