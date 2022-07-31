(* read-csv-fn.sml
 *
 * COPYRIGHT (c) 2022 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Functor for reading CSV files; includes support for multiline entries.
 *)

signature CSV_ROW =
  sig

  (* the representation of a row in a CSV file; simple examples include lists
   * of strings and vectors of strings.
   *)
    type row

    val fromList : string list -> row
    val toList : row -> string list

  end

functor CSVReadFn (R : CSV_ROW) : sig

    exception BadRow of {
        msg : string,           (* a message explaining the error *)
        ln : string,            (* the complete input line; note that for a multiline
                                 * input, this is the line where the input occurred.
                                 *)
        pos : int               (* the character index where the error occurred *)
      }

    val getRow : TextIO.instream -> R.row option

  end = struct

    structure S = String

    fun getc (s, i) = if (i < size s) then S.sub(s, i) else #"\n"

    exception BadRow of {
        msg : string,           (* a message explaining the error *)
        ln : string,            (* the complete input line; note that for a multiline
                                 * input, this is the line where the input occurred.
                                 *)
        pos : int               (* the character index where the error occurred *)
      }

    fun getRow inS = let
          fun getLine () = TextIO.inputLine inS
          fun mkField (chrs, flds) = String.implodeRev chrs :: flds
          fun finish flds = SOME(R.fromList (List.rev flds))
          (* raise the BadRow exception *)
          fun badRow (ln, i, msg) = raise BadRow{msg = msg, ln = ln, pos = i}
          (* scan a field in the line; the arguments are:
           *   ln       -- the line being scanned
           *   i        -- index of next character to process
           *   inQ      -- true when we are inside a quoted field
           *   chrs     -- list of the field characters so far (in reverse order)
           *   flds     -- list of the fields so far (in reverse order)
           *)
          fun scanField (ln, i, inQ, chrs, flds) = (case getc (ln, i)
                 of #"\n" => if inQ
                      then (case getLine() (* multi-line field *)
                         of SOME ln => scanField (ln, 0, inQ, #"\n"::chrs, flds)
                          | NONE => badRow ("", 0, "end-of-file in myltiline row")
                        (* end case *))
                      else finish (mkField (chrs, flds))
                  | #"," => if inQ
                      then scanField (ln, i+1, inQ, #","::chrs, flds)
                      else nextField (ln, i+1, mkField(chrs, flds))
                  | #"\"" => if inQ
                      then (case getc (ln, i+1)
                         of #"\"" => (* escaped double quote *)
                              scanField (ln, i+2, inQ, #"\""::chrs, flds)
                          | #"," => nextField (ln, i+2, mkField(chrs, flds))
                          | #"\n" => finish (mkField (chrs, flds))
                          | _ => badRow (ln, i, "bad end of quoted field")
                        (* end case  *))
                      else badRow (ln, i, "unescaped double quote in field")
                  | c => scanField (ln, i+1, inQ, c::chrs, flds)
                (* end case *))
          and nextField (ln, i, flds) = (case getc (ln, i)
                 of #"\n" => finish flds
                  | #"\"" => scanField (ln, i+1, true, [], flds)
                  | #"," => nextField (ln, i+1, ""::flds)
                  | c => scanField (ln, i, false, [], flds)
                (* end case *))
          in
            case getLine ()
             of NONE => NONE
              | SOME ln => scanField (ln, 0, false, [], [])
            (* end case *)
          end

  end
