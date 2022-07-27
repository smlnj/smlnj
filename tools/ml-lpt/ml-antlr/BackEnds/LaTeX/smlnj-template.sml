(* smlnj-template.sml
 *
 * COPYRIGHT (c) 2009 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * In SML/NJ, we load the template files when we elaborate this module.
 *)

structure LaTeXTemplate =
  struct

    val template  = ExpandFile.mkTemplateFromFile "BackEnds/LaTeX/template.tex" 

  end
