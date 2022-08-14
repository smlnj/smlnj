(* mllex-tool.sml
 *
 * COPYRIGHT (c) 2022 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Plugin for the "mllex" CM tool class that causes "legacy" ml-lex
 * input to be processed by "ml-ulex --ml-lex-mode".
 *
 * Author: Matthias Blume (blume@tti-c.org)
 *)

structure ULexMLLexTool =
  struct

    val _ = Tools.registerStdShellCmdTool {
            tool = "ULex-ML-Lex",
            class = "mllex",
            cmdStdPath = fn () => ("ml-ulex", ["--ml-lex-mode"]),
            template = NONE,
            extensionStyle = Tools.EXTEND [("sml", SOME "sml", fn too => too)],
            dflopts = []
          }

  end
