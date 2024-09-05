(* formatting.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Formatting : FORMATTING =
  struct

    structure M = Measure

    datatype style = datatype Format.style
    datatype token = datatype Format.token

    datatype alignment = datatype Format.alignment
    datatype break = datatype Format.break
    datatype format = datatype Format.format
    datatype element = datatype Format.element

    type elements = element list
    type formats = format list

    (* eliminate EMPTY format elements *)
    fun reduceFormats (formats : formats) : formats = let
          fun notEmpty EMPTY = false
            | notEmpty _ = true
          in
            List.filter notEmpty formats
          end

    (* eliminate EMPTY format elements *)
    fun reduceElements (elements : elements) : elements = let
          fun notEmpty (FMT EMPTY) = false
            | notEmpty _ = true
          in
            List.filter notEmpty elements
          end

    fun block (elements : elements) : format = (case reduceElements elements
           of [] => EMPTY
              (* blocks consisting of a single (FMT fmt) element reduce to fmt *)
            | [FMT fmt] => fmt
            | _ => BLOCK{content = elements, sz = M.measureElements elements}
          (* end case *))

    fun aBlock (align : alignment, formats : formats) : format = let
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
    fun hBlock fmts = aBlock (H, fmts)
    fun vBlock fmts = aBlock (V, fmts)
    fun pBlock fmts = aBlock (P, fmts)
    fun cBlock fmts = aBlock (C, fmts)

    (* "conditional" formats *)

    fun tryFlat (fmt : format) : format = ALT(FLAT fmt, fmt)

    (* alt : format * format -> format *)
    val alt = ALT

    fun hvBlock (fmts : formats) : format = tryFlat (vBlock fmts)

    (*** format-building utility functions for some primitive types ***)

    val empty = EMPTY
    fun text "" = EMPTY
      | text s = TEXT s
    fun style _ EMPTY = EMPTY
      | style sty fmt = STYLE(sty, fmt)
    val token = TOKEN
    fun lift toString v = TEXT(toString v)


    (*** wrapping or closing formats, e.g., parenthesizing a format ***)

    (* tight -- no space between left, right, and fmt *)
    fun enclose {left : format, right : format} fmt =
          aBlock (C, [left, fmt, right])

    (*** functions for formatting sequences of formats (format lists) ***)

    (* alignmentToBreak : alignment -> break
     * The virtual break associated with each alignment.
     * This is a utility function used in functions sequence and sequenceWithMap *)
    fun alignmentToBreak H = Space 1
      | alignmentToBreak V = Hard
      | alignmentToBreak P = Soft 1
      | alignmentToBreak C = Null

    (* sequence : alignement -> format -> format list -> format
     *  The second argument (sep: format) is normally a symbol (TEXT) such as comma or semicolon *)
    fun sequence (align : alignment) (sep : format) (formats : formats) =
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
    fun hSequence sep = sequence H sep
    fun pSequence sep = sequence P sep
    fun vSequence sep = sequence V sep
    fun cSequence sep = sequence C sep

    (*** functions for formatting sequences of values (of homogeneous types, i.e. 'a lists) ***)

    fun 'a sequenceWithMap {align : alignment, sep : format, fmt: 'a -> format}
            (xs: 'a list) =
          let
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

    (*** labeled lists *)

    fun fmtLabeledList (_, _, []) = empty
      | fmtLabeledList (first, rest, x::xs) =
          vBlock (hBlock [first, x] :: map (fn x => hBlock [rest, x]) xs)

    fun vLabeledList {first : string, rest : string} = let
          val first = text first
          val rest = text rest
          in
            fn xs => fmtLabeledList (first, rest, xs)
          end

    fun vLabeledListLAlign {first : string, rest : string} = let
          val n1 = size first and n2 = size rest
          val lab1 = text first and lab2 = text rest
          fun padRight (n, txt) = block[FMT txt, BRK(Space n)]
          val (first, rest) = (case Int.compare(n1, n2)
                 of LESS => (padRight(n2 - n1, lab1), lab2)
                  | EQUAL => (lab1, lab2)
                  | GREATER => (lab1, padRight (n1 - n2, lab2))
                (* end case *))
          in
            fn xs => fmtLabeledList (first, rest, xs)
          end

    fun vLabeledListRAlign {first : string, rest : string} = let
          val n1 = size first and n2 = size rest
          val lab1 = text first and lab2 = text rest
          fun padLeft (n, txt) = block[BRK(Space n), FMT txt]
          val (first, rest) = (case Int.compare(n1, n2)
                 of LESS => (padLeft(n2 - n1, lab1), lab2)
                  | EQUAL => (lab1, lab2)
                  | GREATER => (lab1, padLeft (n1 - n2, lab2))
                (* end case *))
          in
            fn xs => fmtLabeledList (first, rest, xs)
          end

    (*** "indenting" formats ***)

    (* indent : int -> format -> format *)
    (* When applied to EMPTY, produces EMPTY
     * The resulting format soft-indents n spaces (iff following a line break) *)
    fun indent (n: int) (fmt : format) = (case fmt
           of EMPTY => EMPTY
            | _ => INDENT(n, fmt)
          (* end case *))

  end (* structure PrettyPrint *)
