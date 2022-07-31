(* parse-result.sml
 *
 * COPYRIGHT (c) 2016 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure ParseResult =
  struct

    datatype parseResult
      = EOF
      | ERROR
      | ABORT
      | PARSE of Ast.dec

  end
