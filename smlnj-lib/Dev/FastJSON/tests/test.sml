(*
use "../json.sml";
use "../fast-parser.sml";
*)

datatype strm = S of string * int;

fun openIn file = let
      val inS = TextIO.openIn file
      val contents = TextIO.inputAll inS
      in
        TextIO.closeIn inS;
        S(contents, 0)
      end;

fun getc (S(contents, n)) = if (n < size contents)
      then SOME(String.sub(contents, n), S(contents, n+1))
      else NONE;

fun close _ = ()

fun error (ec, S(contents, n)) = let
      val i = Int.max(0, n-10)
      val j = Int.min(n+10, size contents)
      val msg = concat [
            "Error (", FastJSONParser.errorMessage ec, ") at ",
            Int.toString n, ": ...",
            String.toString(String.substring(contents, i, j-i)), "..."]
      in
        raise Fail msg
      end;

fun test file = let
      val inS = openIn file
      val options = {comments = true, maxDigits=SOME 16, error = error}
      in
        #1(FastJSONParser.parseWithOpts options getc inS) before close inS
      end;
