(* json-buffer-printer.sml
 *
 * COPYRIGHT (c) 2021 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Print JSON to a `CharBuffer.buf`
 *)

structure JSONBufferPrinter
  : JSON_STREAM_OUTPUT where type outstream = CharBuffer.buf
  = JSONStreamOutputFn (
    struct
      type outstream = CharBuffer.buf
      val output1 = CharBuffer.add1
      val output = CharBuffer.addVec
      val outputSlice = CharBuffer.addSlice
    end)
