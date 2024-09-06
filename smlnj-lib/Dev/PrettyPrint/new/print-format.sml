(* print-format.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *
 * Version 10.2.
 * Use PrintFormatFn to define two "Print" structures: PrintTextIO and PrintANSI.
 *)

(* Print structures for TextIO and ANSI terminal devices *)

structure PrintTextIO = PrintFormatFn (SimpleTextIODev)

structure PrintANSI = PrintFormatFn (ANSITermDev)
