(* mllex-ext.sml
 *
 * COPYRIGHT (c) 2022 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Classifier plugin that maps the ".lex" extension to the ml-ulex tool
 * in compatibility mode.
 *
 * Author: Matthias Blume (blume@tti-c.org)
 *)

structure ULexMLLexExt =
  struct

    local
      val suffixes = ["lex", "l"]
      val class = "mllex"
      fun sfx s = Tools.registerClassifier (Tools.stdSfxClassifier { sfx = s, class = class })
    in
    val _ = app sfx suffixes
    end

  end
