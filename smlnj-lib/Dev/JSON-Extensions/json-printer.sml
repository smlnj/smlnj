(* json-printer.sml
 *
 * COPYRIGHT (c) 2008 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * A printer for JSON values.
 *)

structure JSONPrinter : sig

    val print : TextIO.outstream * JSON.value -> unit
    val print' : {strm : TextIO.outstream, pretty : bool} -> JSON.value -> unit

    (* return the size of the compact output (i.e., no whitespace) for the value *)
    val sizeOfValue : JSON.value -> int

  end = struct

    structure J = JSON
    structure JSP = JSONStreamPrinter

    fun printWith printer v = (JSP.value(printer, v); JSP.close printer)

    fun print (strm, v) = printWith (JSP.new strm) v

    fun print' {strm, pretty} = printWith (JSP.new' {strm=strm, pretty=pretty})

    val sizeOfValue = JSP.sizeOfValue

  end
