(* render-fn.sml
 *
 * COPYRIGHT (c) 2023 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

functor RenderFn (Dev : PP_DEVICE) : RENDER =
  struct

    structure F = Format
    structure M = Measure

    structure Device = Dev

    (* internal types *)

    (* the state maintained during rendering.
     *   int: the current indentation or starting column
     *   bool: whether the render immediately follows a newline + indentation
     *)
    type render_state = int * bool

    (* the initial "context" of a render is a virtual newline + 0 indentation *)
    val renderState0 : render_state = (0, true)

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
    fun render {styleMap, tokenMap} dev = let
          (* get the current line width from the device *)
          val lw = Option.getOpt(Dev.lineWidth dev, maxInt)
          (* output n spaces *)
          fun sp n = Dev.space (dev, n)
          (* render a token while returning its size.  If the token is not defined in
           * the token map, then we render its name as a string.
           *)
          fun token (tok as F.TOK{name, measure}) = (case tokenMap tok
                 of NONE => (Dev.string (dev, name); size name)
                  | SOME tok => (Dev.token (dev, tok); measure)
                (* end case *))
          (* render styled text returning the resulting render state *)
          fun renderStyled (style : Format.style, thunk : unit -> render_state) : render_state =
                Dev.withStyle (dev, styleMap style, thunk)
          (* lineBreak : int -> unit  -- output a newline and indentation of n spaces *)
          fun lineBreak n = (Dev.newline dev; Dev.indent(dev, n))
          (* flatRender : format -> unit
           * Render as though on an unbounded line (lw = "infinity"), thus "flat" (i.e.,
           * no line space pressure).  _No_ newlines are triggered, not even `Hard`
           * breaks and `INDENT` formats, which are rendered as single spaces, like
           * `Soft` breaks. `flatRender` is called once when rendering a FLAT format
           * when the format fits.
           *)
          fun flatRender (format : F.format) = let
                (* flat0: format -> unit
                 *   -- recurses over the format structure
                 *)
                fun flat0 (format : F.format) = (case format
                       of F.EMPTY => ()
                        | F.TEXT s => Dev.string (dev, s)
                        | F.TOKEN tok => ignore(token tok)
                        | F.STYLE(sty, fmt) =>
                            ignore (renderStyled (sty, fn () => (flat0 fmt; renderState0)))
                        | F.BLOCK{elements, ...} => flatBLOCK elements
                        | F.ABLOCK{formats, ...} => flatABLOCK formats
                        | F.INDENT(_, fmt) => flat0 fmt
                        | F.FLAT fmt => flat0 fmt
                        | F.ALT(fmt, _) => flat0 fmt   (* any format fits *)
                      (* end case *))
                  (* flatBLOCK : element list -> unit *)
                  and flatBLOCK [] = sp 1
                    | flatBLOCK elements = let
                        fun rend [] = ()
                          | rend (element::rest) = (case element
                               of F.FMT format => (flat0 format; rend rest)
                                | F.BRK break => (case break
                                     of F.Hard => (sp 1; rend rest)
                                      | F.Soft n => (sp n; rend rest)
                                      | F.Space n => (sp n; rend rest)
                                      | F.Null => rend rest
                                    (* end case *))
                              (* end case *))
                        in
                          rend elements
                        end
                  (* flatABLOCK : format list -> unit *)
                  and flatABLOCK [] = sp 1
                    | flatABLOCK formats = let (* ASSERT: formats not empty *)
                        fun rend [] = ()
                          | rend [format] = flat0 format (* no break after last format *)
                          | rend (format::rest) = (flat0 format; sp 1; rend rest)
                        in
                          rend formats
                        end
                  in
                    flat0 format
                  end (* fun flatRender *)
          (* render1: F.format -> render_state -> render_state
           * the main recursive rendering of the format
           *
           * Input renderState (cc, newlinep):
           *   cc: current column, incremented or reset after any output actions
           *     (string, space, lineBreak).  If the format is the initial format
           *     of a block, or follows a newline+indent, then cc = the parent's blm
           *     (where blm ("block left margin") = the cumulative inherited
           *     indentation from containing formats, incremented by surrounding
           *     nested INDENT constrs).
           *   newlinep: bool indicating whether the immediately previously rendered
           *     format or break resulted in a newline+indent; the top-level call of
           *     render1 is treated as though it followed a newline with 0 indent.
           *
           * Output renderState:
           *   cc' : int -- the current column when the render is completed
           *      (= position where next character will be printed)
           *   newlinep' : bool -- reports whether this render1 call *ended* with a
           *      newline+indent
           *   -- INVARIANT: outerBlm <= cc
           *   -- INVARIANT: we will never print to the left of the outer block's blm (outerBlm)
           *   -- ASSERT: if newlinep is true, then cc = outerBlm
           *)
	  fun render1
                (fmt : F.format)
                (inputState : render_state as (cc, newlinep)) : render_state
              = (case fmt
                 of F.EMPTY =>
                      (* nothing printed, nothing changed; outerBlm not relevant *)
                      inputState
                  | F.TEXT s =>
                      (* output the string unconditionally; move cc accordingly *)
                      (Dev.string(dev, s); (cc + size s, false))
                  | F.TOKEN tok =>
                      (* output the token unconditionally; move cc accordingly *)
                      (cc + token tok, false)
                  | F.STYLE(sty, fmt) =>
		      renderStyled (sty, (fn () => render1 fmt inputState))
                  | F.BLOCK{elements, ...} =>
                      (* establishes a new local blm = cc for the BLOCK *)
		      renderBLOCK elements inputState
                  | F.ABLOCK{formats, align, ...} =>
                      (* establishes a new local blm = cc for the ABLOCK *)
		      renderABLOCK align formats inputState
                    (* soft indented block; depends on outerBlm *)
                  | F.INDENT(n, fmt) => if newlinep
                      (* ASSERT: at outerBlm after newline+indent (i.e. cc = outerBlm) *)
                      then (
                        Dev.indent(dev, n);
                        (* increase outerBlm indentation to cc' = outerBlm + n *)
                        render1 fmt (cc + n, true))
                      (* else not on new line, proceed at cc without line break *)
                      else render1 fmt (cc, false)
                    (* unconditionally render the format as flat; outerBlm not relevant *)
                  | F.FLAT fmt => (
                      flatRender fmt;
                      (cc + M.measure fmt, false))
                  | F.ALT(fmt1, fmt2) => if M.measure fmt1 <= lw - cc
                     then render1 fmt1 inputState (* fmt1 fits flat *)
                     else render1 fmt2 inputState
                (* end case *))
          (* renderBLOCK : element list -> renderState -> renderState
           *   rendering the elements of a BLOCK
           *   blm will be the caller's cc
           *)
          and renderBLOCK elements (inputState as (blm, newlinep)) = let
                fun re ([], rstate) = rstate
                  | re (element::rest, rstate as (cc, newlinep)) = (case element
                       of F.FMT fmt => re (rest, render1 fmt rstate)
                        | F.BRK break => ( (* rest should start with a FMT! *)
                            case break
                             of F.Null => re (rest, (cc, false))
                              | F.Hard => (lineBreak blm; re (rest, (blm, true)))
                              | F.Space n => (sp n; re (rest, (cc + n, newlinep)))
                              | F.Soft n => (
                                  (* ASSERT: rest = FMT _ :: _; a BRK should be
                                   * followed by a FMT
                                   *)
                                  case rest
                                   of F.FMT fmt' :: rest' =>
                                        if M.measure fmt' <= (lw - cc) - n  (* lw - (cc + n) *)
                                          then (
                                            (* rendering Soft n as n spaces without
                                             * a line break
                                             *)
                                            sp n;
                                            re (rest', render1 fmt' (cc + n, false)))
                                          else (
                                            (* rendering Soft n as newline + blm indent *)
                                            lineBreak blm;
                                            re (rest, (blm, true)))
                                    | _ => error "renderBLOCK 1: adjacent breaks"
                                  (* end case *))
                            (* end case *))
                    (* end case *))
                in
                  re (elements, inputState)
                end (* end renderBLOCK *)
          (* renderABLOCK : [formats]format list * alignment * [blm]int * [newlinep]bool -> int * bool
           * Render the contents of an aligned block with the effects of the virtual
           * break and bindent.
           * The first argument is the component formats of the block being rendered;
           * The second argument is the alignment, which determines the virtual breaks
           * separating the formats
           * The input renderState (blm, newlinep) is:
           *   blm: int -- the current column at block entry, which defines this
           *               new block's blm
           *   newlinep: bool -- flag indicating whether this block follows a newline+indent
           * Rendering the new block does not need to use the partent's blm, so no
           * blm argument is passed.
           * The special case of an "empty" ABLOCK, containing no formats, renders
           * as the empty format, producing no output; But this case should not occur,
           * because (alignedBlock _ []) should yield EMPTY, not an ABLOCK with null
           * formats list.
           *)
          and renderABLOCK align formats (inputState as (blm, _)) = let
                (* renderFormats : format list -> renderState -> renderState
                 * Arguments:
                 *   formats : format list -- the formats constituting the body
                *                             (components) of the block
                 *   (blm, newlinep) : renderstate, where
                 *     blm : int -- the current column when renderBLOCK was called,
                 *                  which becomes the block's blm
                 *     newlinep : bool -- flag indicating whether following
                 *                        immediately after a newline+indent
                 * ASSERT: not (null formats)
                 *)
                fun rend (fmt::rest) (cc, newlinep) = let
                      (* renderBreak : [cc:]int * [m:]int -> [cc:]int * [newlinep:]bool
                       * -- m is the measure of following format
                       *)
                      val renderBreak : (int * int) -> render_state = (case align
                             of F.C => (fn (cc, m) => (cc, false))
                              | F.H => (fn (cc, m) => (sp 1; (cc+1, false)))
                              | F.V => (fn (cc, m) => (lineBreak blm; (blm, true)))
                              | F.P =>  (* virtual break is `Soft 1` *)
                                  (fn (cc, m) => if m <= (lw - cc) - 1
                                      (* no line break, print 1 space *)
                                      then (sp 1; (cc+1, false))
                                      (* triggered line break *)
                                      else (lineBreak blm; (blm, true)))
                            (* end case *))
                      (* renderTail : format list -> renderState *)
                      fun renderTail [] rstate =
                            (* when we've rendered all the formats *)
                            rstate
                        | renderTail (fmt :: rest) (cc, _) = let
                            (* newlinep argument not used in this case! *)
                            val rstate1 = renderBreak (cc, M.measure fmt)
                            in
                              renderTail rest (render1 fmt rstate1)
                            end
                      in
                        renderTail rest (render1 fmt (cc, newlinep))
                      end
                  | rend [] inputState = inputState
                in
                  rend formats inputState
                end (* fun renderABLOCK *)
        in
          fn fmt => (
              ignore (render1 fmt renderState0);
              Dev.flush dev)
        end (* fun render *)

  end (* functor RenderFn *)
