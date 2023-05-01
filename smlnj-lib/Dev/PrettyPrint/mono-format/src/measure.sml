(* measure.sml
 *
 * COPYRIGHT (c) 2023 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Measure : sig

    (* return the width of a flat-render of the format *)
    val measure : Format.format -> int

    (* return the width of a flat-render of the element list *)
    val measureElements : Format.element list -> int

    (* return the width of a flat-render of the format list, where the first
     * argument is the width of the breaks that separate the list items.
     *)
    val measureFormats : int * Format.format list -> int

  end = struct

    structure F = Format

    (* ------------------------------------------------------------------------
     *  Measuring
     * ------------------------------------------------------------------------ *)

    (* measure: pre-rendering conservative estimate of how much horizontal
     * line space a format takes. We use the most conservative measure, assuming
     * that a format will be rendered "flat", with no line breaks (we call this
     * "flat measure").
     * Measuring a format takes place before rendering, so it must be a
     * conservative estimate of the line space required by a format. This estimate
     * is the length of its rendering on a single unbounded line, with F.NewLine
     * separators being treated as a single space and indenting blocks treated as
     * non-indenting for the sake of measurement.
     *)

    (* the flat measure of a format (length of line span if rendered on a
     * single, unbounded line), using memoization of the measure in the block
     * measure fields.
     *)
    fun measure (format : F.format) = (case format
           of F.EMPTY => 0
            | F.TEXT s => size s
            | F.TOKEN(F.TOK{sz, ...}) => sz
            | F.STYLE(_, fmt) => measure fmt
              (* atomic formats *)
            | F.BLOCK{sz, ...} => sz
              (* basic blocks *)
            | F.ABLOCK{sz, ...} => sz
              (* aligned blocks *)
            | F.INDENT(_, fmt) => measure fmt
            | F.FLAT format => measure format
            | F.ALT(format1, format2) => measure format1
             (* measure the first format, which will normally be the wider one,
              * alternatively, measure both arguments and return the max (min?)
              * of the two measures. *)
        (* end case *))

    fun measureElement (F.BRK break) = (case break
           of F.Newline => 1
            | (F.Break n | F.Space n) => n
                  (* measured as n spaces, since flat rendered as n spaces *)
            | F.NullBreak => 0
          (* end case *))
      | measureElement (F.FMT format) = measure format

    fun measureElements elements =
        let fun mElements (nil, n) = n
              | mElements (element :: elements, n) =
                  mElements (elements, n + measureElement element)
         in mElements (elements, 0)
        end

    fun measureFormats (breaksize : int, formats : F.format list) =
        let fun mFormats (nil, acc) = acc
              | mFormats ([format], acc) = measure format + acc
              | mFormats (format :: rest, acc) =  (* rest not null *)
                (* add breaksize for virtual separator, if any *)
                  mFormats (rest, measure format + breaksize + acc)
         in mFormats (formats, 0)
        end

  end (* structure Measure *)
