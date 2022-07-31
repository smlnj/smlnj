(* windows-status.sig
 *
 * COPYRIGHT (c) 2008 Fellowship of SML/NJ
 * 
 * Signature with the windows status constants
 *
 *)

signature WINDOWS_STATUS = sig
    type status
    val accessViolation     : status
    val arrayBoundsExceeded : status
    val breakpoint          : status
    val controlCExit        : status
    val datatypeMisalignment : status
    val floatDenormalOperand  : status
    val floatDivideByZero     : status
    val floatInexactResult    : status
    val floatInvalidOperation : status
    val floatOverflow         : status
    val floatStackCheck       : status
    val floatUnderflow        : status
    val guardPageViolation : status
    val integerDivideByZero : status
    val integerOverflow     : status
    val illegalInstruction : status
    val invalidDisposition : status
    val invalidHandle      : status
    val inPageError             : status
    val noncontinuableException : status
    val pending                 : status
    val privilegedInstruction   : status
    val singleStep              : status
    val stackOverflow           : status
    val timeout                 : status
    val userAPC                 : status
  end
