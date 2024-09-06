(* print-format-fn.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *
 * printing (i.e., rendering) formats
 *
 * Version 10.1
 * Imports: Formatting, Render, DeviceType
 * Exports: structure PrintFormat
 *
 * Version 10.2.
 * Functorized PrintFormat taking a DEVICE structure, yielding PrintFormatFn.
 * Use PrintFormatFn to define two "Print" structures: PrintPlain and PrintANSI.
 *)

functor PrintFormatFn (D : sig

    include PP_DEVICE

    val openDev : {dst : TextIO.outstream, wid : int} -> device

  end) : PRINT_FORMAT =
  struct

    structure Device = D

    structure Render = RenderFn (Device)

    val defaultLineWidth = 80

    fun renderStdout {styleMap, tokenMap, width} fmt =
          Render.render
            {styleMap = styleMap, tokenMap = tokenMap}
            (D.openDev {dst = TextIO.stdOut, wid = width})
            fmt

    val printFormat = renderStdout {
            styleMap = Device.defaultStyleMap,
            tokenMap = fn _ => NONE,
            width = defaultLineWidth
          }

    val printFormatNL = printFormat o Formatting.appendNewLine

  end (* functor PrintFormatFn *)
