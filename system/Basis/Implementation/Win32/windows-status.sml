(* windows-status.sml
 *
 * COPYRIGHT (c) 2008 Fellowship of SML/NJ
 * 
 * Structure with the windows status constants
 *
 *)
local
    structure SysWord = SysWordImp
in
structure Windows_STATUS : WINDOWS_STATUS =
  struct
    type status = SysWord.word
    val accessViolation     : status = 0wxC0000005
    val arrayBoundsExceeded : status = 0wxC000008C
    val breakpoint          : status = 0wx80000003
    val controlCExit        : status = 0wxC000013A
    val datatypeMisalignment : status = 0wx80000002
    val floatDenormalOperand  : status = 0wxC000008D
    val floatDivideByZero     : status = 0wxC000008E
    val floatInexactResult    : status = 0wxC000008F
    val floatInvalidOperation : status = 0wxC0000090
    val floatOverflow         : status = 0wxC0000091
    val floatStackCheck       : status = 0wxC0000092
    val floatUnderflow        : status = 0wxC0000093
    val guardPageViolation : status = 0wx80000001
    val integerDivideByZero : status = 0wxC0000094
    val integerOverflow     : status = 0wxC0000095
    val illegalInstruction : status = 0wxC000001D
    val invalidDisposition : status = 0wxC0000026
    val invalidHandle      : status = 0wxC0000008
    val inPageError             : status = 0wxC0000006
    val noncontinuableException : status = 0wxC0000025
    val pending                 : status = 0wx00000103
    val privilegedInstruction   : status = 0wxC0000096
    val singleStep              : status = 0wx80000004
    val stackOverflow           : status = 0wxC00000FD
    val timeout                 : status = 0wx00000102
    val userAPC                 : status = 0wx000000C0
  end 
end
