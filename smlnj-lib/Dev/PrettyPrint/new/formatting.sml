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

    (* styles *)
    fun style s = STY s
    val defaultStyle = style ""

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

    fun block (elements : elements) = (case reduceElements elements
           of [] => EMPTY
              (* blocks consisting of a single (FMT fmt) element reduce to fmt *)
            | [FMT fmt] => fmt
            | _ => BLOCK{elements = elements, measure = M.measureElements elements}
          (* end case *))

    fun aBlock (align : alignment, formats : formats) = let
          val breaksize = (case align of C => 0 | _ => 1)
          in
            case reduceFormats formats
              of [] => EMPTY
               | [fmt] => fmt
               | formats' => ABLOCK{
                    formats = formats',
                    align = align,
                    measure = M.measureFormats (breaksize, formats')
                  }
            (* end case *)
          end

    (*** block building functions for non-indenting blocks ***)

    (* constructing aligned blocks: common abbreviations *)
    (* xblock list -> format, for x = h, v, p, c *)
    fun hBlock fmts = aBlock (H, fmts)
    fun vBlock fmts = aBlock (V, fmts)
    fun pBlock fmts = aBlock (P, fmts)
    fun cBlock fmts = aBlock (C, fmts)

    (* "conditional" formats *)

    fun tryFlat (fmt) = ALT(FLAT fmt, fmt)

    (* alt * format -> format *)
    val alt = ALT

    fun hvBlock (fmts : formats) = tryFlat (vBlock fmts)

    (*** format-building utility functions for some primitive types ***)

    val empty = EMPTY
    fun text "" = EMPTY
      | text s = TEXT s
    fun styled _ EMPTY = EMPTY
      | styled sty fmt = STYLE(sty, fmt)
    val token = TOKEN
    fun lift toString v = TEXT(toString v)
    val int = lift Int.toString
    fun string s = TEXT(String.concat["\"", String.toString s, "\""])
    fun char c = TEXT(String.concat["#\"", Char.toString c, "\""])
    val bool = lift Bool.toString
    val atom = lift Atom.toString

    (*** wrapping or closing formats, e.g., parenthesizing a format ***)

    (* tight -- no space between front, back, and fmt *)
    fun enclose {front, back} fmt =
          aBlock (C, [front, fmt, back])

    val parens = enclose {front=text "(", back=text ")"}
    val brackets = enclose {front=text "[", back=text "]"}
    val braces = enclose {front=text "{", back=text "}"}
    val angleBrackets = enclose {front=text "<", back=text ">"}

    fun appendNewLine fmt = block [FMT fmt, BRK Hard]

    (*** functions for formatting sequences of formats (format lists) ***)

    (* alignmentToBreak : alignment -> break
     * The virtual break associated with each alignment.
     * This is a utility function used in functions sequence and sequenceWithMap
     *)
    fun alignmentToBreak H = Space 1
      | alignmentToBreak V = Hard
      | alignmentToBreak P = Soft 1
      | alignmentToBreak C = Null

    (* sequence : alignement -> format -> format list -> format
     * The second argument (sep: format) is normally a symbol (TEXT) such as
     * comma or semicolon
     *)
    fun sequence (align : alignment) (sep) (formats : formats) =
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

    fun smlTuple (formats: format list) = parens (pSequence (TEXT ",") formats)

    fun smlList (formats: format list) = brackets (pSequence (TEXT ",") formats)

    fun smlOption NONE = TEXT "NONE"
      | smlOption (SOME fmt) = cBlock [TEXT "SOME(", fmt, TEXT ")"]

    (*** functions for formatting sequences of values (of homogeneous types, i.e. 'a lists) ***)

    fun 'a sequenceWithMap {align : alignment, sep, fmt: 'a -> format}
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
            front, sep, back,
            fmt: 'a -> format
          }
          (xs: 'a list) =
            enclose {front=front, back=back}
              (sequenceWithMap {align=align, sep=sep, fmt=fmt} xs)

    (*** labeled lists *)

    fun label' (lab, fmt) = hBlock [lab, fmt]

    fun label lab fmt = label' (TEXT lab, fmt)

    fun fmtLabeledList (_, _, []) = empty
      | fmtLabeledList (first, rest, x::xs) =
          vBlock (label'(first, x) :: List.map (fn x => label'(rest, x)) xs)

    fun vLabeledList {first : string, rest : string} xs =
          fmtLabeledList (TEXT first, TEXT rest, xs)

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
    fun indent (n: int) (fmt) = (case fmt
           of EMPTY => EMPTY
            | _ => INDENT(n, fmt)
          (* end case *))

    (*** "punctuation" characters and related symbols ***)
    val comma     = TEXT ","
    val colon     = TEXT ":"
    val semicolon = TEXT ";"
    val period    = TEXT "."
    val lparen    = TEXT "("
    val rparen    = TEXT ")"
    val lbracket  = TEXT "["
    val rbracket  = TEXT "]"
    val lbrace    = TEXT "{"
    val rbrace    = TEXT "}"
    val langle    = TEXT "<"
    val rangle    = TEXT ">"
    val equal     = TEXT "="

  end (* structure PrettyPrint *)
