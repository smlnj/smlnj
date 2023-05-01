(* render-fn.sml
 *
 * COPYRIGHT (c) 2023 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * TODO: text width (vs. line width)
 *       max indent
 *)

functor RenderFn (Dev : PP_DEVICE) :> sig

    type device = Dev.device

    (* style/token map that always returns `NONE` *)
    val nullMap : Atom.atom -> 'a option

    (* render a pretty-print format to the device *)
    val render : {
            dev : device,
            styleMap : Atom.atom -> Dev.style option,
            tokenMap : Atom.atom -> Dev.token option
          } -> PrettyPrint.format -> unit

(** TODO
    (* compute the dimensions of the result of rendering a format *)
    val dimensions : PrettyPrint.format -> {
            nr : int,           (* number of rows (i.e., lines) in the render *)
            nc : int,           (* number of columns, including indentation,
                                 * in the render
                                 *)
            maxTextWid : int    (* width of longest line of text (not including
                                 * indentation) in the render
                                 *)
          }
**)

  end = struct

    structure F = Format
    structure M = Measure

    type device = Dev.device

    (* style/token map that always returns `NONE` *)
    fun nullMap (_ : Atom.atom) = NONE

    fun error msg = raise Fail("Render Error: " ^ msg)

    val maxInt = (case Int.maxInt of SOME n => n | NONE => 1000000)

    (* Block Left Margin (blm: int)
     * The blm is the "left margin" of a block assigned to it by the renderer.
     * No non-blank character in the block should be printed to the left of this
     * margin.
     *
     * The blm of a nonindenting block is defined as the cc at the point where
     * that block is rendered. The blm of an indenting block is the parent block's
     * blm + the specified incremental indentation. The blm only changes when
     * entering an indented block.
     * Note that if the blm is (column) 3, then the indentation is 3, because
     * the column counts from zero.
     *)

    (* render : format * (string -> unit) * int -> unit
     *   format: format  -- the format to be rendered and printed
     *   output: string -> unit  -- the output function
     *   lw: int  -- the line width, assumed fixed during the rendering
     * The top-level render function decides where to conditionally break lines,
     * and how much indentation should follow each line break, based on the
     * line space available (the difference between the currend column and the
     * line width).
     * In this version (Version 8), the render function also prints the content
     * and formatting, using the given device.
     *)
    fun render {dev, styleMap, tokenMap} format = let
          fun sp n = Dev.space (dev, n)
          (* lineBreak : int -> unit  -- perform a newline and indentation of n spaces *)
          fun lineBreak n = (Dev.newline dev; Dev.indent(dev, n))
          (* get the current line width from the device *)
          val lw = Option.getOpt(Dev.lineWidth dev, maxInt)
          (* render0: format * int * int * bool -> int * bool
           * the main recursive rendering of the format
           * Inputs:
           *   cc -- current column, incremented or reset after any output
           *         actions (TEXT, sp, lineBreak)
           *   outerBlm -- block left margin of "parent" block containing this
           *               format; defaults to 0 for top-level format
           *               blm = the cumulative inherited indentation from containing
           *               formats (incremented by surrounding nested INDENT constrs)
           *               rebound at the entrance to each block to that block's
           *               left margin (initially cc, which may = blm)
           *   newlinep -- flag indicating whether the immediately previously
           *               rendered format or break resulted in a newline+indent; the
           *               top-level call of render0 is treated as though it followed
           *               a newline with 0 indent.
           *
           *   ASSERT: blm <= cc  (When is blm < cc?)
           *
           * Outputs:
           *   cc' -- the current column when the render is completed (= position
           *          where next character will be printed)
           *   newlinep' -- reports whether this render0 call _ended_ with a
           *                newline+indent
           *
           * -- INVARIANT: outerBlm <= cc
           * -- INVARIANT: we will never print to the left of the outer
           *               block's blm (outerBlm)
           * -- ASSERT: if newlinep is true, then cc = outerBlm
           *)
          fun render0  (format, outerBlm, cc, newlinep) = (case format
                 of F.EMPTY =>
                      (* nothing printed, nothing changed; outerBlm not relevant *)
                      (cc, newlinep)
                    (* print the string unconditionally; move cc accordingly;
                     * outerBlm not relevant
                     *)
                  | F.TEXT s => (Dev.string(dev, s); (cc + size s, false))
                    (* establishes a new local blm = cc; outerBlm not relevant *)
                  | F.TOKEN(F.TOK{txt, sz}) => (case tokenMap txt
                       of NONE => (Dev.string (dev, "<?token?>"); (cc + sz, false))
                        | SOME tok => (Dev.token (dev, tok); (cc + sz, false))
                      (* end case *))
                  | F.STYLE(sty, fmt) => (case (styleMap sty)
                         of NONE => render0 (fmt, outerBlm, cc, newlinep)
                          | SOME sty' => (
                              Dev.pushStyle (dev, sty');
                              render0 (fmt, outerBlm, cc, newlinep) before
                              Dev.popStyle dev)
                        (* end case *))
                  | F.BLOCK{content, ...} => renderBLOCK (content, cc, newlinep)
                    (* establishes a new local blm = cc; outerBlm not relevant *)
                  | F.ABLOCK{content, align, ...} =>
                      renderABLOCK (content, align, cc, newlinep)
                    (* soft indented block; depends on outerBlm *)
                  | F.INDENT(n, fmt) => if newlinep
                      (* ASSERT: at outerBlm after newline+indent (i.e. cc = outerBlm) *)
                      then (
                        Dev.indent(dev, n);
                        (* increase outerBlm indentation to cc' = outerBlm + n *)
                        render0 (fmt, outerBlm, outerBlm + n, true))
                      (* else not on new line, proceed at cc without line break *)
                      else render0 (fmt, outerBlm, cc, false)
                    (* unconditionally render the format as flat; outerBlm not relevant *)
                  | F.FLAT format => (
                      flatRender format;
                      (cc + M.measure format, false))
                  | F.ALT(format1, format2) => if M.measure format1 <= lw - cc
                     then render0 (format1, outerBlm, cc, newlinep) (* format1 fits flat *)
                     else render0 (format2, outerBlm, cc, newlinep)
                (* end case *))
          (* renderBLOCK : element list * int * int * bool -> int * bool
           *  rendering the elements of an BLOCK
           *)
          and renderBLOCK (elements, cc, newlinep) = let
                val blm = cc (* the new block's blm is the entry cc *)
                fun render (fmt, blm, cc, newlinep) =
                      render0 (fmt, blm, cc, newlinep)
                fun re (nil, cc, newlinep) = (cc, newlinep)
                  | re (element::rest, cc, newlinep) = (case element
                       of F.FMT format => let
                            val (cc', newlinep') = render (format, blm, cc, newlinep)
                            in
                              re (rest, cc', newlinep')
                            end
                        | F.BRK break => ( (* rest should start with a FMT! *)
                            case break
                             of F.NullBreak => re (rest, cc, false)
                              | F.Newline => (lineBreak blm; re (rest, blm, true))
                              | F.Space n => (sp n; re (rest, cc + n, newlinep))
                              | F.Break n => (
                                  (* ASSERT: rest = FMT _ :: _; a BRK should be
                                   * followed by a FMT
                                   *)
                                  case rest
                                   of F.FMT format' :: rest' =>
                                        if M.measure format' <= (lw - cc) - n  (* lw - (cc + n) *)
                                          then let
                                            val (cc', newlinep') = (
                                                  sp n;
                                                  render (format', blm, cc + n, false))
                                            in
                                              re (rest', cc', newlinep')
                                            end
                                          else ( (* trigger newline+indent *)
                                            lineBreak blm;
                                            re (rest, blm, true))
                                    | _ => error "renderBLOCK 1: adjacent breaks"
                                  (* end case *))
                            (* end case *))
                    (* end case *))
                in
                  re (elements, cc, newlinep)
                end (* end renderBLOCK *)
          (* Render the contents of an aligned block with the effects of the virtual
           * break and bindent.  The first three elements are the components of the
           * block being rendered, the last two arguments are:
           *   cc       -- current column at block entry, which becomes the new
           *               block's blm unless it is indented,
           *   newlinep -- flag indicating whether this block follows a newline+indent
           * Rendering the new block does not need to use the partent's blm,
           * so no blm argument is passed.
           *)
          and renderABLOCK (nil, _, cc, newlinep) =
                (* Special case of "empty" block, containing no formats, renders
                 * as the empty format, producing no output.  But this case should
                 * not occur, because (alignedBlock _ nil) should yield EMPTY,
                 * not an empty ABLOCK.
                 *)
                (cc, newlinep)
            | renderABLOCK (formats, align, cc, newlinep) = let
                (* val _ = print ">>> renderABLOCK[not nil]\n" *)
                val blm = cc  (* the blm of _this_ block is defined as the entry cc *)
                fun render (fmt, blm, cc, newlinep) =
                      render0 (fmt, blm, cc, newlinep)
                (* renderFormats : format list * int * bool -> int * bool
                 * Arguments:
                 *  format :: rest  -- the formats constituting the body
                 *                     (children) of the block
                 *  cc              -- the current column where the block starts;
                 *                     used to define the block's blm
                 *  newlinep        -- flag indicating whether following
                 *                     immediately after a newline+indent
                 *)
                fun renderFormats (format::rest, cc, newlinep) = let
                      (* renderBreak : [cc:]int * [m:]int -> [cc:]int * [newlinep:]bool *)
                      val renderBreak : (int * int) -> (int * bool) = (case align
                             of F.C => (fn (cc, m) => (cc, false))
                              | F.H => (fn (cc, m) => (sp 1; (cc+1, false)))
                              | F.V => (fn (cc, m) => (lineBreak blm; (blm, true)))
                              | F.P =>  (* virtual break is `Break 1` *)
                                  (fn (cc, m) => if m <= (lw - cc) - 1
                                      then (sp 1; (cc+1, false))
                                      else (lineBreak blm; (blm, true)))
                            (* end case *))
                      fun renderRest (nil, cc, newlinep) = (cc, newlinep)
                        | renderRest (format :: rest, cc, newlinep) = let
                            val (cc0, newlinep0) = renderBreak (cc, M.measure format)
                            (* render the next format *)
                            val (cc1, newlinep1) = render (format, blm, cc0, newlinep0)
                            in
                              (* then render the rest *)
                              renderRest (rest, cc1, newlinep1)
                            end
                      (* render the 1st format *)
                      val (cc', newlinep') = render (format, blm, cc, newlinep)
                      in
                        (* then render the rest *)
                        renderRest (rest, cc', newlinep')
                      end
                  | renderFormats (nil, _, _) = error "renderFormats: no formats"
                in
                  renderFormats (formats, cc, newlinep)
                end (* fun renderABLOCK *)
        (* Render as though on an unbounded line (lw = "infinity"), thus "flat" (i.e.,
         * no line space pressure).  _No_ newlines are triggered, not even `Newline`
         * breaks and `INDENT` formats, which are rendered as single spaces, like
         * `Break` breaks. `flatRender` is called once when rendering a FLAT format
         * when the format fits.
         *)
        and flatRender (format : F.format) = let
              (* flatRender0: format -> unit
               *   -- recurses over the format structure
               *)
              fun flatRender0 (format : F.format) = (case format
                     of F.EMPTY => ()
                      | F.TEXT s => Dev.string (dev, s)
                      | F.TOKEN(F.TOK{txt, ...}) => (case tokenMap txt
                           of NONE => Dev.string (dev, "<?token?>")
                            | SOME tok => Dev.token (dev, tok)
                          (* end case *))
                      | F.STYLE(sty, fmt) => (case (styleMap sty)
                             of NONE => flatRender0 fmt
                              | SOME sty' => (
                                  Dev.pushStyle (dev, sty');
                                  flatRender0 fmt before
                                  Dev.popStyle dev)
                            (* end case *))
                      | F.BLOCK{content, ...} => flatRenderBLOCK content
                      | F.ABLOCK{content, ...} => flatRenderABLOCK content
                      | F.INDENT(_, fmt) => flatRender0 fmt
                      | F.FLAT fmt => flatRender0 fmt
                      | F.ALT(fmt, _) => flatRender0 fmt   (* any format fits *)
                    (* end case *))
                (* flatRenderBLOCK : element list -> unit *)
                and flatRenderBLOCK [] = sp 1
                  | flatRenderBLOCK elements = let
                      fun rend [] = ()
                        | rend (element::rest) = (case element
                             of F.FMT format => (flatRender0 format; rend rest)
                              | F.BRK break => (case break
                                   of F.Newline => (sp 1; rend rest)
                                    | F.Break n => (sp n; rend rest)
                                    | F.Space n => (sp n; rend rest)
                                    | F.NullBreak => rend rest
                                  (* end case *))
                            (* end case *))
                      in
                        rend elements
                      end
                (* flatRenderABLOCK : format list -> unit *)
                and flatRenderABLOCK [] = sp 1
                  | flatRenderABLOCK formats = let (* formats not empty *)
                      fun rf [] = ()
                        | rf [format] = flatRender0 format (* no break after last format *)
                        | rf (format::rest) = (flatRender0 format; sp 1; rf rest)
                      in
                        rf formats
                      end
                in
                  flatRender0 format
                end (* fun flatRender *)
        in
          (* the initial "context" of a render is a vitrual newline + 0 indentation *)
          ignore (render0 (PrettyPrint.block [F.FMT format, F.BRK F.Newline], 0, 0, true))
        end (* fun render *)

(** TODO
    fun dimensions (fmt : F.format) = let
          val nLines = ref 1
          val maxCol = ref 0
          val maxTextWid = ref 0
          fun measure (format, outerBlm, cc, nlp) = (case format
                 of F.EMPTY => (cc, nlp)
                  | F.TEXT s => (cc + size s, false)
                  | F.TOKEN tok => raise Fail "FIXME"
                  | F.STYLE(sty, fmt) => measure (format, outerBlm, cc, nlp)
                  | F.BLOCK{content, ...} => measureBLOCK (content, cc, nlp)
                  | F.ABLOCK{content, align, ...} =>
                      measureABLOCK (content, align, cc, nlp)
                  | F.INDENT(n, fmt) => if nlp
                      then measure (fmt, outerBlm, outerBlm + n, true)
                      else measure (fmt, outerBlm, cc, false)
                  | F.FLAT format => (cc + M.measure format, false)
                  | F.ALT(format1, format2) => if M.measure format1 <= lw - cc
                     then measure (format1, outerBlm, cc, nlp)
                     else measure (format2, outerBlm, cc, nlp)
                (* end case *))
          and measureBLOCK (elements, cc, nlp) = let
                val blm = cc (* the new block's blm is the entry cc *)
                fun re (nil, cc, nlp) = (cc, nlp)
                  | re (element::rest, cc, nlp) = (case element
                       of FMT format => let
                            val (cc', nlp') = measure (format, blm, cc, nlp)
                            in
                              re (rest, cc', nlp')
                            end
                        | BRK break => ( (* rest should start with a FMT! *)
                            case break
                             of NullBreak => re (rest, cc, false)
                              | Newline => (lineBreak blm; re (rest, blm, true))
                              | Space n => re (rest, cc + n, nlp)
                              | Break n => (
                                  (* ASSERT: rest = FMT _ :: _; a BRK should be
                                   * followed by a FMT
                                   *)
                                  case rest
                                   of FMT format' :: rest' =>
                                        if M.measure format' <= (lw - cc) - n  (* lw - (cc + n) *)
                                          then let
                                            val (cc', nlp') =
                                                  measure (format', blm, cc + n, false)
                                            in
                                              re (rest', cc', nlp')
                                            end
                                          else ( (* trigger newline+indent *)
                                            lineBreak blm;
                                            re (rest, blm, true))
                                    | _ => error "measureBLOCK 1: adjacent breaks"
                                  (* end case *))
                            (* end case *))
                    (* end case *))
                in
                  re (elements, cc, nlp)
                end (* end measureBLOCK *)
          and measureABLOCK (nil, _, cc, nlp) = (cc, nlp)
            | measureABLOCK (formats, align, cc, nlp) = let
                (* val _ = print ">>> measureABLOCK[not nil]\n" *)
                val blm = cc  (* the blm of _this_ block is defined as the entry cc *)
                fun renderFormats (format::rest, cc, nlp) = let
                      val renderBreak : (int * int) -> (int * bool) = (case align
                             of C => (fn (cc, m) => (cc, false))
                              | H => (fn (cc, m) => (cc+1, false))
                              | V => (fn (cc, m) => (lineBreak blm; (blm, true)))
                              | P =>  (* virtual break is `Break 1` *)
                                  (fn (cc, m) => if m <= (lw - cc) - 1
                                      then (cc+1, false)
                                      else (lineBreak blm; (blm, true)))
                            (* end case *))
                      fun renderRest (nil, cc, nlp) = (cc, nlp)
                        | renderRest (format :: rest, cc, nlp) = let
                            val (cc0, newlinep0) = renderBreak (cc, M.measure format)
                            (* render the next format *)
                            val (cc1, newlinep1) = measure (format, blm, cc0, newlinep0)
                            in
                              (* then render the rest *)
                              renderRest (rest, cc1, newlinep1)
                            end
                      (* render the 1st format *)
                      val (cc', nlp') = measure (format, blm, cc, nlp)
                      in
                        (* then render the rest *)
                        renderRest (rest, cc', nlp')
                      end
                  | renderFormats (nil, _, _) = error "renderFormats: no formats"
                in
                  renderFormats (formats, cc, nlp)
                end (* fun measureABLOCK *)
          in
            {nr = !nLines, nc = !maxCol, maxTextWid = !maxTextWid}
          end
**)

  end (* functor RenderFn *)
