(*
 * A table for storing operands for a compilation unit.
 * We give each distinct operand a unique (negative) value number.
 *)
signature OPERAND_TABLE =
sig

   structure I : INSTRUCTIONS

   type operandTable
   type valueNumber = I.C.cell

   datatype const =
     INT of int                        (* small integer operand *)
   | INTINF of MachineInt.machine_int  (* large integer operand *)
   | OPERAND of I.operand              (* other operand *)

   datatype valueNumberMethods =
      VALUE_NUMBERING of
      { int     : int -> valueNumber,
        word    : word -> valueNumber,
        word32  : Word32.word -> valueNumber,
        int32   : Int32.int -> valueNumber,
        intinf  : IntInf.int -> valueNumber,
        operand : I.operand -> valueNumber
      }

   exception NoOperand
   exception NoInt
   exception NoIntInf
   exception NoConst

   (* Special values *)
   val bot      : valueNumber
   val top      : valueNumber
   val volatile : valueNumber

   (* Create a new table *)
   val create  : int ref -> operandTable 

   (* Lookup methods *)

   (* Value number -> int/operand/label *)
   val const       : valueNumber -> const
   val int         : operandTable -> int -> valueNumber
   val word        : operandTable -> word -> valueNumber
   val int32       : operandTable -> Int32.int -> valueNumber
   val word32      : operandTable -> Word32.word -> valueNumber
   val intinf      : operandTable -> IntInf.int -> valueNumber
   val operand     : operandTable -> I.operand -> valueNumber

   (* Create new value numbers *)
   val makeNewValueNumbers : operandTable -> valueNumberMethods

   (* Lookup but don't create *)
   val lookupValueNumbers : operandTable -> valueNumberMethods


end
