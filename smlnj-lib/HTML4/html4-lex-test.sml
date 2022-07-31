(* html4-lex-test.sml
 *
 * COPYRIGHT (c) 2014 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Test = struct

open HTML4Tokens

fun handle_tok tok = print ((tokToString tok) ^ "\n")

fun handle_toks (source_map, lex_stream) =
    let val (tok, span, lex_stream') = HTML4Lexer.lex source_map lex_stream
            handle ex =>
                   (print ("Exception at " ^ (Int.toString (HTML4Lexer.getPos
                                                                lex_stream)) ^
                           "\n"); raise ex)
    in
        handle_tok tok;
        (* XXX Getting some weird equality type complaint if I use the
        equality operator here... *)
        case tok of EOF => () | _ => handle_toks(source_map, lex_stream')
    end

fun handle_file file_name =
    let
        val source_map = AntlrStreamPos.mkSourcemap ()
        val in_strm = TextIO.openIn file_name
        val lex_strm = HTML4Lexer.streamifyInstream in_strm
        val _ = handle_toks(source_map, lex_strm)
            handle ex => (TextIO.closeIn in_strm;
                          raise ex)
    in
        TextIO.closeIn in_strm
    end

fun main (_, args) = (List.app handle_file args; OS.Process.success)

end

(* ______________________________________________________________________
   End of html4-lex-test.sml
   ______________________________________________________________________ *)
