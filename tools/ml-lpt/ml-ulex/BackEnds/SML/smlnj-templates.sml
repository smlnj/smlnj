(* smlnj-templates.sml
 *
 * COPYRIGHT (c) 2009 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * In SML/NJ, we load the template files when we elaborate this module.
 *)

structure Templates =
  struct

    val lexTemplate  = ExpandFile.mkTemplateFromFile "BackEnds/SML/template-ml-lex.sml" 
    val ulexTemplate = ExpandFile.mkTemplateFromFile "BackEnds/SML/template-ml-ulex.sml" 

  end
