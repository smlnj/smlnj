(* smlnj-lib/PrettyPrint/src/render.sml *)

(* Version 7.1
 *  the Render structure
 *  -- revised measure function with measures memoized in blocks
 *
 * Version 7.4
 *  -- SEP --> BRK (separator --> break)
 *
 * Version 8.1 [2023.1.1]
 *  -- HINDENT dropped, SINDENT --> INDENT
 *
 * Version 8.4 [2023.3.1]
 *  -- simplify Break constructor names: HardLine -> Hard, SoftLine -> Soft, NullBreak -> Null
 *)

structure Render : RENDER =
struct

local

  structure H = HTML (* smlnj-lib/HTML/html.sml (HTML 3 abstract syntax) *)
  structure F = Format
  structure M = Measure
  structure S = Style (* html/html-style.sml *)

  fun error (msg: string) = (print ("NewPrettyPrint Error: " ^ msg); raise Fail "Render")
in

(* utilities *)

(* empty : H.text *)
val empty = H.TextList nil

(* textCat : H.text * H.text -> H.text *)
fun textCat (text1, text2) = H.TextList [text1, text2]

(* listToText : H.text list -> H.text *)
fun listToText nil = empty
  | listToText [t] = t
  | listToText xs = H.TextList xs
			 
(* space : int -> H.text *)
(* a text string (pcdata) of n non-breakable spaces *)
fun space (n: int) =
    H.PCDATA (concat (List.tabulate (n, fn _ => "&nbsp;")))

(* lineBreak [blm]int * [texts]text list -> text list) *)
fun lineBreak (blm, texts) =
    space blm :: H.BR {clear=NONE} :: texts


(* --------------------------------------------------------------------------------
 *  Rendering
 * -------------------------------------------------------------------------------- *)

(* wrapStyle : S.style * H.text -> H.text *)
(* wrap a text with a style, e.g. B for bold *)
fun wrapStyle (style, text) =
    (case style
      of S.NOEMPH => text
       | S.TT => H.TT text
       | S.I => H.I text
       | S.B => H.B text
       | S.U => H.U text
       | S.STRIKE => H.STRIKE text
       | S.EM => H.EM text
       | S.STRONG => H.STRONG text
       | S.DFN => H.DFN text
       | S.CODE => H.CODE text
       | S.SAMP => H.SAMP text
       | S.KBD => H.KBD text
       | S.VAR => H.VAR text
       | S.CITE => H.CITE text
       | S.COLOR c => H.FONT{color=SOME c, size=NONE, content=text}
       | S.A{name, href} =>
	   H.A {name = name, href = href,
		rel = NONE, rev = NONE, title = NONE,
		content = text})

(* post-processing texts *)

(* consolidate: text -> text
 *  (1) concatenate adjacent strings (PCDATA texts).
 *  (2) flatten nested TextLists *)
fun consolidate text =
    (case text
      of H.TT text' => H.TT (consolidate text')
       | H.I text' => H.I (consolidate text')
       | H.B text' => H.B (consolidate text')
       | H.U text' => H.U (consolidate text')
       | H.STRIKE text' => H.STRIKE (consolidate text')
       | H.EM text' => H.EM (consolidate text')
       | H.STRONG text' => H.STRONG (consolidate text')
       | H.DFN text' => H.DFN (consolidate text')
       | H.CODE text' => H.CODE (consolidate text')
       | H.SAMP text' => H.SAMP (consolidate text')
       | H.KBD text' => H.KBD (consolidate text')
       | H.VAR text' => H.VAR (consolidate text')
       | H.CITE text' => H.CITE (consolidate text')
       | H.FONT {color, size, content} => H.FONT{color=color, size=size, content=(consolidate content)}
       | H.A{name, href, rel, rev, title, content} =>
	   H.A {name = name, href = href,
		rel = rel, rev = rev, title = title,
		content = consolidate content}
       | H.PCDATA s => text
       | H.TextList texts => H.TextList (flat (texts, nil, nil)))

(* flat : text list * string list * text list *)
and flat (nil, nil, segs) = (rev segs)
  | flat (nil, strings, segs) = H.PCDATA (concat (rev strings)) :: segs
  | flat (H.PCDATA s :: rest, strings, segs) = flat (rest, s::strings, segs)
  | flat (H.TextList texts :: rest, strings, segs) = flat (texts @ rest, strings, segs)
  | flat (text :: rest, strings, segs) =
      flat (rest, nil, (consolidate text) :: H.PCDATA (String.concat (rev strings)) :: segs)

(* lineBreak : int * H.text list -> H.text list *)
fun lineBreak (ind: int, texts: H.text list) = space ind :: H.BR{clear=NONE} :: texts

(* There is one exported rendering function: render. *)

(* render : format * [lw]int -> H.text
 *   format: format  -- the format to be rendered and printed
 *   lw: int  -- the line width, assumed fixed during the rendering
 * The top-level render function decides where to conditionally break lines, and how much indentation
 * should follow each line break, based on the line space available (the difference between the current
 * column and the line width).
 * In this version (Version 9.1 - HTML), the render function also prints the content and formatting,
 * using the output function passed as the second argument. So in this version, rendering and printing
 * are unified and there is no intermediate "layout" structure.
 * Internal rendering functions (render0, renderBLOCK, renderABLOCK) : state -> state,
 * where state = int * bool (state = (cc, newlinep)).
 * cc represents the "print cursor", while newlinep indicates whether we are starting
 * immediately after a line break (a newline + indentation).
 *)
fun render (format: F.format, lineWidth: int) : H.text =
    let (* flatRender : format -> H.text
	 *   render as though on an unbounded line (lw = "infinity"), thus "flat" (i.e. no line space pressure).
	 *   _No_ newlines are triggered, not even Hard breaks and INDENT formats, which are
	 *   rendered as single spaces, like Softline breaks. Thus we do not need to keep track of cc and 
	 *   newlinep (post line break status).
	 *   flatRender is called once when rendering a FLAT format when the format fits.
	 *   NOTE: flat formatting is independent of order in blocks, so we can use foldr *)
	fun flatRender format =
	    let (* render1: format -> H.text
		 * recurses over the format structure, translating into HTML.text while suppressing all
		 * line breaks. *)
		fun render1  (format: F.format) =
		      (case format
			of F.EMPTY => empty
			 | F.TEXT s => H.PCDATA s
			 | F.BLOCK {elements, ...} => renderBLOCK elements
			 | F.ABLOCK {alignment, formats, ...} => renderABLOCK alignment formats
			 | F.INDENT (_, fmt) => render1 fmt  (* ignoring INDENT *)
			 | F.FLAT fmt => render1 fmt         (* already flat, so FLAT does nothing *)
			 | F.ALT (fmt, _) => render1 fmt     (* any format fits, so render first *)
			 | F.STYLE (style, fmt) => wrapStyle (style, render1 fmt))

		(* renderBLOCK : element list -> H.text *)
		and renderBLOCK nil = empty  (* render an empty BLOCK as empty; should not happen! *)
		  | renderBLOCK elements =
		    let (* folder : element * H.text list -> H.text list *)
			fun folder (element, texts) =
			      (case element
				 of F.FMT format => render1 format :: texts
				  | F.BRK break =>
				     (case break
				       of F.Hard    => space 1 :: texts
					| F.Soft n  => space n :: texts
					| F.Space n => space n :: texts
					| F.Null    => texts))
		     in listToText (foldr folder nil elements)
		    end

		(* renderABLOCK : format list -> H.text *)
		and renderABLOCK alignment formats = (* ASSERT: not (null formats) *)
		    let val space1 = space 1  (* 1 nonbreakable space *)
			val separator : H.text * H.text list -> H.text list =
			    (case alignment
			      of F.C => (op ::)
			       | _ => (fn (text, texts) => (text :: space1 :: texts)))
			fun folder (format: F.format, texts: H.text list) = 
			    separator (render1 format, texts)
		       in listToText (foldr folder nil formats)
		      end

	    in render1 format
	   end (* fun flatRender *)

        (* render1: format * int * bool -> text * int * bool
	 * the main recursive rendering of the format
	 * Inputs:
         *   cc: current column, incremented or reset after any output actions (TEXT, sp, lineBreak)
         *   outerBlm: block left margin of the "parent" block that contains this format;
         *             defaults to 0 for top-level format
         *        blm = the cumulative inherited indentation from surrounding (ancestor) formats
         *              (incremented by surrounding nested INDENT constrs)
	 *     blm is rebound at the entrance to each block to that block's block left margin.
	 *     (blm will be initialized to cc, which may be the parent block's blm if this block is
	 *     the first element of the parent block or if it follows a newline+outerBLM and is not
	 *     (further) indented)
	 *   newlinep: bool indicating whether the immediately previously rendered format or break
         *     resulted in a newline+indent; the top-level call of render1 is treated as though it
         *     followed a newline with 0 indent (a "fresh line context")
         *   ASSERT: blm <= cc  (When is blm < cc?)
         * Outputs:
	 *   cc' : int -- the current column when the render is completed
         *                (= position where next character will be printed)
	 *   newlinep' : bool -- reports whether this render1 call _ended_ with a newline+indent
	 *   -- INVARIANT: outerBlm <= cc
	 *   -- INVARIANT: we will never print to the left of the outer block's blm (outerBlm)
	 *   -- ASSERT: if newlinep is true, then cc = outerBlm *)
	fun render1  (format: F.format, cc: int, newlinep: bool) =
	      (case format
	         of F.EMPTY =>  (* produces empty text, cc unchanged (no "progress"), no line break *)
		      (empty, cc, newlinep)

		  | F.TEXT s =>  (* print the string unconditionally; move cc its size *)
		      (H.PCDATA s, cc + size s, false)

 		  | F.BLOCK {elements, ...} => (* establishes a new local blm = cc for the BLOCK *)
		      renderBLOCK (elements, cc, newlinep)

		  | F.ABLOCK {formats, alignment, ...} =>
		      (* establishes a new local blm = cc for the ABLOCK *)
		      renderABLOCK (alignment, formats, cc, newlinep)

		  | F.INDENT (n, format) =>
		      (* soft indented block; depends on outerBlm *)
		      if newlinep  (* ASSERT: cc = parent's blm after newline+indent *)
		      then let val (text, cc', newlinep') = render1 (format, cc + n, true)
			    in (textCat (space n, text), cc', newlinep')
			   end
		      else (* not following a line break (+ indent); proceed at cc *)
			   render1 (format, cc, false)

		  | F.FLAT format =>
		      (* unconditionally render the format as flat; outerBlm not relevant *)
		      (flatRender format, cc + M.measure format, false)

		  | F.ALT (format1, format2) =>
		      if M.measure format1 <= lineWidth - cc  (* format1 fits flat *)
		      then render1 (format1, cc, newlinep)
		      else render1 (format2, cc, newlinep)

                  | F.STYLE (style, format) =>
		      let val (text, cc', newlinep') = render1 (format, cc, newlinep)
		       in (wrapStyle (style, text), cc', newlinep')
		      end)

        (* How to deal with adjacent breaks in BLOCKs:
         *  (1) Require that any break in a block separate two formats, so adjacent breaks are an error.
         *  (2) Allow adjacent breaks, but when adjacent breaks are found, skip breaks until the last break
	 *      before a format.
         *  (3) What about a BLOCK that ends with a break? Declare that this is illegal? This would eliminate
         *      BLOCKS that only contain breaks.
         *  (4x) What about a rule that says that a BLOCK cannot end with a break, but may _begin_ with a break.
	 *      Would this entail the invariant that a BLOCK never ends with a line-break+indent?
	 * How is it possible to create multiple line breaks, i.e. introduce blank lines?
	 * How about a vertically aligned block with blank lines between items?
         * It may be possible/necessary to follow PPML and associate a number with Hard breaks (e.g. Hard 2).
	 * Alternatively, we may allow EMPTY formats as elements of BLOCKs to notionally "separate" breaks
	 *   (while outlawing successive breaks).
	 *   E.g. BLOCK{ elements = [FMT EMPTY, BRK HARD, FMT EMPTY, BRK HARD, FMT EMPTY], ... }, which would
         *   produce a blank line when rendered.
         *)

        (* renderBLOCK : element list * int * bool -> H.text * int * bool
         *  rendering the elements of an BLOCK *)
        and renderBLOCK (elements, cc, newlinep) =
            let val blm = cc (* set this block's blm to the cc on entry *)
		(* fold doesn't work, need "lookahead" to next format for Soft breaks,
		   so need an ad hoc iteration over elements *)
		(* iter: element list * text list * int * bool -> text list * int * bool *)
		fun iter (nil, texts, cc, newlinep) = (texts, cc, newlinep)
		  | iter (element :: rest, texts, cc, newlinep) =
		    case element
		      of F.FMT format =>
			   let val (text, cc', newlinep') = render1 (format, cc, newlinep)
			    in (text::texts, cc', newlinep')
			   end
		       | F.BRK break =>  (* rest should start with a FMT! (or keep skipping breaks until this is true) *)
			   (case break
			      of F.Null    => (texts, cc, false)
			       | F.Hard    => (lineBreak (blm, texts), blm, true)
			       | F.Space n => (space n :: texts, cc + n, false)
			       | F.Soft n  =>  (* oops -- only have the current (head) element, don't have the "rest" *)
				   (case rest  
				      of F.FMT format' :: rest' =>  (* lookahead one format element *)
					   if M.measure format' <= (lineWidth - cc) - n  (* format' fits *)
					   then iter (rest, space n :: texts, cc+n, false) (* "emit" n spaces *)
					   else iter (rest, lineBreak (blm, texts), blm, true)
			                        (* trigger newline+indent *)
				       | _ :: rest' => iter (rest', texts, cc, newlinep)))
			                  (* skip a break that is followed by a break *)

		val (texts, cc', newlinep') = iter (elements, nil, cc, newlinep)
	     in (listToText (rev texts), cc', newlinep')
	    end (* end renderBLOCK *)

        (* renderABLOCK : alignment * [formats]format list * [cc]int * [newlinep]bool -> H.text * int * bool
	 * Render the contents of an aligned block with the effects of the virtual break and bindent.
         * The first three elements are the components of the block being rendered,
         * the last three arguments are:
	 *   cc: int -- current column at block entry, which becomes the new block's blm unless it is indented,
         *   newlinep: bool -- flag indicating whether this block follows a newline+indent
	 * Rendering the new block does not need to use the partent's blm, so no blm argument is passed.
         * ASSERT: not (null formats)
	 *)
	and renderABLOCK (_, nil, cc, newlinep) = (empty, cc, newlinep)
	      (* Special case of "empty" block, containing no formats, renders as the empty format, producing no output;
               * This case should not occur, because (alignedBlock _ nil) should yield EMPTY, not an empty ABLOCK. *)
          | renderABLOCK (alignment, formats, cc, newlinep) =
	      let (* val _ = print ">>> renderABLOCK[not nil]\n" *)
		  val blm = cc  (* this ABLOCK's blm is set to the entry cc, just like for BLOCK *)
		  (* renderBreak : [cc:]int * [measure:]m -> [cc:]int * [newlinep:]bool *)
		  val renderBreak : (H.text list * int * int) -> (H.text list * int * bool) =
		      (case alignment
			of F.C => (fn (texts, cc, m) => (texts, cc, false))
			 | F.H => (fn (texts, cc, m) => (space 1 :: texts, cc+1, false))
			 | F.V => (fn (texts, cc, m) => (lineBreak (blm, texts), blm, true))
			 | F.P =>  (* virtual break is Soft 1 *)
			     (fn (texts, cc, m) =>
				 if m <= (lineWidth - cc) - 1  (* conditional on m *)
				 then (space 1  :: texts, cc+1, false) (* fits -- no line break; "emit" a space *)
				 else (lineBreak (blm, texts), blm, true)))  (* triggered soft line break *)

		  (* iter : format list * H.text list * [cc]int * [newlinep]bool -> T.text * int * bool
		     iter operats on tails of the ABLOCK format list
		   * Arguments:
		   *   format list -- a tail of the formats of the block  (tl^n formats, n=1, ...)
		   *   cc : int -- the current column after previous renderings of formats in the block
		   *   newlinep : bool -- flag indicating whether following immediately after a newline+indent
		   * ASSERT: not (null formats) *)
		 fun iter (nil, texts, cc, newlinep) = (listToText (rev texts), cc, newlinep)
	                (* formats are all done -- wrap up *)
		   | iter (format::rest, texts, cc, newlinep) =  (* ASSERT: not (null rest) *)
		        (* format is the first of a tail of the block formats; preceeded by a notional break *)
		       let val (texts1, cc1, newlinep1) = renderBreak (texts, cc, M.measure format)
			   val (text2, cc2, newlinep2) = render1 (format, cc1, newlinep1)
			in iter (rest, text2 :: texts1, cc2, newlinep2)  (* then render the "next tail" of the block *)
		       end
	      in case formats
		  of nil => error "empty ABLOCK"
		   | [format] => render1 (format, cc, newlinep)
		   | format1::rest => (* ASSERT not (null rest) *)
		       let val (text1, cc1, newlinep1) = render1 (format1, cc, newlinep)
		        in iter (rest, [text1], cc1, newlinep1)
		       end
	      end

	  (* the initial "context" of a render is a virtual newline + 0 indentation, hence cc=0 and newlinep=true *)
	  val (text, _, _) = render1 (format, 0, true)

    in consolidate text
   end (* fun render *)

end (* top local *)
end (* structure Render *)

(* NOTES:

1. All newlines are followed by the cumulative block indentation, which may be 0, produced by the lineBreak
   output function.

2. blm (block left margin) values represent the cummulative effect of the indentations of containing blocks.
   -- the blm of an "in-line" (or non-indented) block is set to the current column (cc) at the entry to the block
   -- the blm of an indented block is set to the parent block's blm incremented by the block's indentation
      (if triggered)

3. [Q: Edge case] Should INDENT (n, EMPTY) produce nothing (since the format has no content) or should
   it produce a line break (newline+indent) and nothing else?

4. [Q1] Does rendering a format ever end with a final newline+indent?
   [Q2] Is BLOCK {elements = [BRK Hard], ...} a valid block? If so, it "ends with a newline".
   [Q3] Is vblock [empty, BRK Hard, empty] (or similar BLOCK formats) treated as equavalent to a newline?

A1: Yes?
We only emit a newline+indent at a Hard or triggered Soft break,
but a normal block will not end with a (virtual) break so "normal" blocks do not end with a newline+indent.

Another possibility is at an indented, but empty, block (e.g. INDENT (3, emtpy)), which could
appear on its own or as the last format in a block.  But we can have the reduction

   INDENT (n,EMPTY) --> EMPTY

in which case an indented empty format turns into the empty format and the indentation is ignored
(does not occur).

Also, a basic block whose last element is a BRK (Hard) is possible. Such a block would end with
a newline+indent (to its blm?).

Should this be disallowed?  Probably not, until we find that it is causing problems or confusion for users.


5. Block Left Margin (blm: int)
    The blm is the "left margin" of a block assigned to it by the renderer.  No non-blank character
    in the block should be printed to the left of this margin.

    The blm of a nonindenting block is defined as the cc at the point where that block is rendered.
    The blm of an indenting block is the parent block's blm + the specified incremental indentation.
    The blm only changes when entering an indented block.
    Note that if the blm is (column) 3, then the indentation is 3, because the column counts from zero.

QUESTION: under what circumstances will a rendered format _end_ with a newline+indent?
    If this doesn't, or can't, happen, then the returned newlinep value will always be false, and is
    therefore redundant. 
    Possible general principle: line breaks don't come at the end of a (rendered) format.
    Are there arguments or examples that contradict this principle?

*)
