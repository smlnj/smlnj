(* json-stream-output.sig
 *
 * COPYRIGHT (c) 2021 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature JSON_STREAM_OUTPUT =
  sig

    type outstream

  (* a `printer` packages up the output state needed to match
   * braces and bracket, and indentation (when pretty printing).
   *)
    type printer

  (* print a `null` value *)
    val null : printer -> unit
  (* print a boolean value *)
    val boolean : printer * bool -> unit
  (* print an integer numeric value *)
    val int : printer * int -> unit
  (* print an integer numeric value *)
    val integer : printer * IntInf.int -> unit
  (* print a floating-point numeric value *)
    val float : printer * real -> unit
  (* print a UTF8 string value; any necessary escape sequences will be added *)
    val string : printer * string -> unit

  (* begin a JSON object; this function also prints "{" *)
    val beginObject : printer -> unit
  (* print the key field for a JSON object *)
    val objectKey : printer * string -> unit
  (* end a JSON object; this function also prints "}" *)
    val endObject : printer -> unit
  (* begin a JSON object; this function also prints "{" *)
    val beginArray : printer -> unit
  (* end a JSON object; this function also prints "]" *)
    val endArray : printer -> unit

  (* create a new printer; `new outS` is equivalent to the expression
   * `new' {strm = outS, pretty=false}`
   *)
    val new : outstream -> printer
  (* create a new printer with the pretty printing mode specified.  If set to `true`,
   * then newlines and indentation will be used to make the output human readable.
   * Otherwise, the output will have no excess whitespace.
   *)
    val new' : {strm : outstream, pretty : bool} -> printer
  (* close the printer; this function checks that there are no pending open object/
   * array values and raises `Fail` if there are.
   * Note that it **does not** close the underlying output stream.
   *)
    val close : printer -> unit

  (* embed the given value into the output *)
    val value : printer * JSON.value -> unit

  end
