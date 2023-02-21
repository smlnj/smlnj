(* pretty-print.sml
 *
 * COPYRIGHT (c) 2023 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure PrettyPrint : PRETTY_PRINT =
  struct

    structure M = Measure

    type style = Format.style
    datatype token = datatype Format.token

    datatype alignment = datatype Format.alignment
    datatype break = datatype Format.break
    datatype format = datatype Format.format
    datatype element = datatype Format.element

    type elements = element list
    type formats = format list

    (* eliminate EMPTY format elements *)
    fun reduceFormats (formats : formats) : formats =
        let fun notEmpty EMPTY = false
              | notEmpty _ = true
         in List.filter notEmpty formats
        end

    (* eliminate EMPTY format elements *)
    fun reduceElements (elements : elements) : elements =
        let fun notEmpty (FMT EMPTY) = false
              | notEmpty _ = true
         in List.filter notEmpty elements
        end

    fun block (elements : elements) : format = (
        case reduceElements elements
         of [] => EMPTY
            (* blocks consisting of a single (FMT fmt) element reduce to fmt *)
          | [FMT fmt] => fmt
          | _ => BLOCK{content = elements, sz = M.measureElements elements}
        (* end case *))

    fun alignedBlock (align : alignment, formats : formats) : format = let
          val breaksize = (case align of C => 0 | _ => 1)
          in
            case reduceFormats formats
              of [] => EMPTY
(* watch out for this probably obsolete (since bindent is gone) bug:
 * BUG! this is wrong if block is indented, since this loses the ABLOCK
 * that should carry the indentation (in its bindent field)!
 * E.g., iblock (HI 3) [text "aa"] ==> text "aa", indentation lost!
 *)
               | [fmt] => fmt
               | formats' => ABLOCK{
                    content = formats',
                    align = align,
                    sz = M.measureFormats (breaksize, formats')
                  }
            (* end case *)
          end


    (*** block building functions for non-indenting blocks ***)

    (* constructing aligned blocks: common abbreviations *)
    (* xblock : format list -> format, for x = h, v, p, c *)
    fun hBlock fmts = alignedBlock (H, fmts)
    fun vBlock fmts = alignedBlock (V, fmts)
    fun pBlock fmts = alignedBlock (P, fmts)
    fun cBlock fmts = alignedBlock (C, fmts)

    (* "conditional" formats *)

    fun tryFlat (fmt : format) : format = ALT(FLAT fmt, fmt)

    (* alt : format * format -> format *)
    val alt = ALT

    fun hvBlock (fmts : formats) : format = tryFlat (vBlock fmts)

    (*** format-building utility functions for some primitive types ***)

    val empty = EMPTY
    val text = TEXT
    fun style sty fmt = STYLE(sty, fmt)
    val token = TOKEN
    fun lift toString v = TEXT(toString v)


    (*** wrapping or closing formats, e.g., parenthesizing a format ***)

    (* tight -- no space between left, right, and fmt *)
    fun enclose {left : format, right : format} fmt =
          alignedBlock (C, [left, fmt, right])

    (*** functions for formatting sequences of formats (format lists) ***)

    (* alignmentToBreak : alignment -> break
     * The virtual break associated with each alignment.
     * This is a utility function used in functions sequence and sequenceWithMap *)
    fun alignmentToBreak H = Space 1
      | alignmentToBreak V = Newline
      | alignmentToBreak P = Break 1
      | alignmentToBreak C = NullBreak

    (* sequence : alignement -> format -> format list -> format
     *  The second argument (sep: format) is normally a symbol (TEXT) such as comma or semicolon *)
    fun sequence {align : alignment, sep : format} (formats : formats) =
        let val separate =
                (case align
                   of C => (fn elems => FMT sep :: elems)  (* alignment = C *)
                    | _ =>
                      let val break = alignmentToBreak align
                       in (fn elems => FMT sep :: BRK break :: elems)
                      end)
            fun addBreaks nil = nil
              | addBreaks fmts =  (* fmts non-null *)
                  let fun inter [fmt] = [FMT fmt]
                        | inter (fmt :: rest) =  (* rest non-null *)
                            FMT fmt :: (separate (inter rest))
                        | inter nil = nil (* won't happen *)
                   in inter fmts
                  end
          in block (addBreaks formats)
         end

    (* xsequence : [sep:]format -> format list -> format, x = h, v, p, c *)
    fun hSequence sep fmts = sequence {align=H, sep=sep} fmts
    fun pSequence sep fmts = sequence {align=P, sep=sep} fmts
    fun vSequence sep fmts = sequence {align=V, sep=sep} fmts
    fun cSequence sep fmts = sequence {align=C, sep=sep} fmts

    (*** functions for formatting sequences of values (of homogeneous types, i.e. 'a lists) ***)

    fun 'a sequenceWithMap {
              align : alignment, sep : format, fmt: 'a -> format
            }
            (xs: 'a list) = let
          val separate = (case align
                 of C => (fn elems => FMT sep :: elems)  (* alignment = C *)
                  | _ => let
                      val break = alignmentToBreak align
                      in
                        (fn elems => FMT sep :: BRK break :: elems)
                      end
                (* end case *))
          val formats = map fmt xs
          fun addBreaks nil = nil
            | addBreaks fmts = let (* fmts non-null *)
                fun inter [fmt] = [FMT fmt]
                  | inter (fmt :: rest) =  (* rest non-null *)
                      FMT fmt :: (separate (inter rest))
                  | inter nil = nil (* won't happen *)
                in
                  inter fmts
                end
          in
            block (addBreaks formats)
          end

    fun closedSequenceWithMap {
            align : alignment,
            left : format, sep : format, right : format,
            fmt: 'a -> format
          }
          (xs: 'a list) =
            enclose {left=left, right=right}
              (sequenceWithMap {align=align, sep=sep, fmt=fmt} xs)


    (*** "indenting" formats ***)

    (* indent : int -> format -> format *)
    (* When applied to EMPTY, produces EMPTY
     * The resulting format soft-indents n spaces (iff following a line break) *)
    fun indent (n: int) (fmt : format) = (case fmt
           of EMPTY => EMPTY
            | _ => INDENT(n, fmt)
          (* end case *))

  end (* structure PrettyPrint *)
