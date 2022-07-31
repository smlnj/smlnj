(* sexp-string-util.sml
 *
 * COPYRIGHT (c) 2022 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Common code for rendering string values according to the Scheme syntax.
 * See https://www.scheme.com/tspl4/grammar.html#./grammar:strings.
 *)

structure SExpStringUtil : sig

    (* map a S-Expression STRING value to its printable representation *)
    val toString : string -> string

  end = struct

    (* map a character to its hexadecimal escape sequence *)
    local
      val i2x = Int.fmt StringCvt.HEX
      val zeroPad = StringCvt.padLeft #"0" 2
    in
    fun hexEscape c = String.concat ["\\x", zeroPad (i2x (Char.ord c)), ";"]
    end (* local *)

    (* translate a character to its representation in a string. *)
    fun trChar #"\"" = "\\\""
      | trChar #"\\" = "\\\\"
      | trChar c = if Char.isGraph c
            then String.str c
          else if (c < #" ")
            then (case c
               of #"\a" => "\\a"
                | #"\b" => "\\b"
                | #"\f" => "\\f"
                | #"\n" => "\\n"
                | #"\r" => "\\r"
                | #"\t" => "\\t"
                | #"\v" => "\\v"
                | _ => hexEscape c
              (* end case *))
            else hexEscape c

    val trString = String.translate trChar

    fun toString s = String.concat["\"", trString s, "\""]

  end
