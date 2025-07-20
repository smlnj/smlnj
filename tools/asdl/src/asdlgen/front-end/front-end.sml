(* front-end.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

structure FrontEnd : sig

    val doFile : string -> AST.file option

  end = struct

  (* check for and report any errors *)
    fun anyErrors {includes, file} = let
          val errFlg = ref false
          fun anyErrors' (file : Parser.file) = if Error.anyErrors (#errStrm file)
                then (
                  errFlg := true;
                  Error.report (TextIO.stdErr, #errStrm file))
                else ()
          in
            List.app anyErrors' includes;
            anyErrors' file;
            !errFlg
          end

    fun doFile file = let
          val parseTree = Parser.parse file
          in
            if anyErrors parseTree
              then NONE
              else let
                val file = Typecheck.check parseTree
                in
                  if anyErrors parseTree
                    then NONE
                    else SOME file
                end
          end

  end
