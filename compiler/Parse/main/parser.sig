(* parser.sig
 *
 * COPYRIGHT (c) 2016 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature SMLNJ_PARSER = sig

    val parse : Source.inputSource -> unit -> ParseResult.parseResult

  end
