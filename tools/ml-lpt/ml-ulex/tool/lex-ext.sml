(* ext.sml
 *
 * COPYRIGHT (c) 2022 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Classifier plugin that maps the ".lex" extension to the ml-ulex tool.
 *
 * Author: Matthias Blume (blume@tti-c.org)
 *)

structure ULexLexExt =
  struct

    local
      val suffixes = ["lex"]
      val class = "ml-ulex"
      fun sfx s =
            Tools.registerClassifier
              (Tools.stdSfxClassifier { sfx = s, class = class })
    in
    val _ = List.app sfx suffixes
    end

  end
