(* mlton-templates.sml
 *
 * COPYRIGHT (c) 2009 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * In MLton, the templates live in files that we generate at compile time.
 *)

structure Templates =
  struct

    val lexTemplate  = LexTemplate.template
    val ulexTemplate = ULexTemplate.template

  end
