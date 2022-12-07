(* sml/Dev/pp/new/new7/render.sml *)

(* Version 7.1
 *  the Render structure 
 *  -- revised measure function with measures memoized in blocks
 *
 * Version 7.4
 *  -- SEP --> BRK (separator --> break)
 *)

structure Render : RENDER =
struct

local
  open Format
  structure M = Measure

  fun error (msg: string) = (print ("NewPrettyPrint Error: " ^ msg); raise Fail "Render")
in

(* newlines and spaces *)

val newlineChar : string = "\n"

(* spaces : int -> string *)
fun spaces n = implode (List.tabulate (n, (fn _ => #" ")))

(* --------------------------------------------------------------------------------
 *  Rendering
 * -------------------------------------------------------------------------------- *)

(* There are two rendering functions: flatRender and render. *)

(* flatRender : format * (string -> unit) -> unit
 *   render as though on an unbounded line (lw = "infinity"), thus "flat"
 *   result is a reversed layout.
 *   flatRender is called once when rendering a FLAT format when the format fits. *)
fun flatRender (format, output) =
    let fun sp n = output (spaces n)
        (* render0: format -> unit
         *   -- recurses over the format structure *)
	fun render0  (format: format) =
	    (case format
	      of EMPTY => ()
	       | TEXT s => output s
	       | BLOCK {elements, ...} => renderElements elements
	       | ABLOCK {formats, ...} => renderABLOCK formats
	       | FLAT format => render0 format
	       | ALT (format, _) => render0 format)   (* any format fits *)

        (* renderElements : element list -> unit *)
        and renderElements nil = sp 1  (* render an empty BLOCK as a single space *)
          | renderElements elements =
            let fun re nil = ()
		  | re (element::rest) = 
		    (case element
		       of FMT format => (render0 format; re rest)
		        | BRK break =>
			   (case break
			     of HardLine   => (sp 1; re rest)
			      | SoftLine n => (sp n; re rest)
			      | Space n    => (sp n; re rest)
			      | NullBreak  => re rest))
	     in re elements
	    end

        (* renderABLOCK : format list -> unit *)
        and renderABLOCK nil = sp 1
          | renderABLOCK formats =  (* formats not empty *)
            let fun rf nil = ()
		  | rf [format] = render0 format (* no break after last format *)
		  | rf (format::rest) = (render0 format; sp 1; rf rest)
	     in rf formats
	    end

    in render0 format
   end (* fun flatRender *)

(* Block Left Margin (blm: int)
 * The blm is the "left margin" of a block assigned to it by the renderer.  No non-blank character
 * in the block should be printed to the left of this margin.
 * 
 * The blm of a nonindenting block is defined as the cc at the point where that block is rendered.
 * The blm of an indenting block is the parent block's blm + the specified incremental indentation.
 * The blm only changes when entering an indented block.
 * Note that if the blm is (column) 3, then the indentation is 3, because the column counts from zero. *)

(* render : format * (string -> unit) * int -> unit
 *   format: format  -- the format to be rendered and printed
 *   output: string -> unit  -- the output function 
 *   lw: int  -- the line width, assumed fixed during the rendering
 * The render function decides where to conditionally break lines, and how much indentation should follow
 * a line break, based on the line space available (the difference between the currend column and the line width).
 * In this version (Version 6), the render function also prints the content and formatting, using the output
 * function passed as the second argument. So in this version, rendering and printing are unified and there
 * is no intermediate "layout" structure. *)
fun render (format: format, output: string -> unit, lw: int) : unit =
    let fun sp n = output (spaces n)
	fun nlIndent n = (output newlineChar; sp n)

        (* render0: format * int * int * bool -> int * bool
	 * the main recursive rendering of the format
	 * Inputs:
         *   cc: current column, incremented or reset after any output actions (TEXT, sp, nlIndent)
         *   blm: block left margin of parent block containing this format, or 0 for top-level format
         *        i.e., the inherited cumulative indentation
	 *     rebound at the entrance to each block to that block's block left margin (initially cc, which may = blm)
	 *   newlinep: bool indicating whether the immediately previously rendered format or break resulted in a
	 *     newline+indent; the top-level call of render0 is treated as though it followed a newline with 0 indent.
         * Outputs:
	 *   cc' : int -- the current column when the render is completed (= position where next character will be printed)
	 *   newlinep' : bool -- reports whether this render0 call _ended_ with a newline+indent
	 *   -- INVARIANT: blm <= cc -- we will never print to the left of the current block's blm *)
	fun render0  (format: format, blm: int, cc: int, newlinep: bool) =
	      (case format
	         of EMPTY => (cc, newlinep)
		  | TEXT s => (output s; (cc + size s, false))
 		  | BLOCK {elements, bindent, ...} => 
		      (case bindent
			 of NI => (* basic block, with blm = cc, which may be greater than parent's blm *)
			      renderElements (elements, cc, newlinep)
			  | HI n =>  (* hard indented block *)
			      let val blm' = blm + n  (* increased (if n > 0) indentation for indented block *)
			       in if newlinep  (* already at blm *)
				  then sp n    (* add the additional n spaces of indentation *)
				  else nlIndent blm';  (* produce the newline+indent associated with the indented block *)
				  renderElements (elements, blm', true)
			      end
			  | SI n =>  (* soft indented block *)
			      let val blm' = blm + n  (* increased indentation for indented block *)
			       in if newlinep  (* at blm after newline+indent *)
				  then (sp n;  (* bumping already existing blm indentation up to blm' = blm + n *)
					renderElements (elements, blm', true))
				  else renderElements (elements, cc, false) (* there is no newline+indent, proceed without *)
			      end)
		  | ABLOCK {formats, alignment, bindent, ...} =>
		      renderABLOCK (formats, alignment, bindent, blm, cc, newlinep)
		  | FLAT format => (flatRender (format, output); (cc + M.measure format, false))
		  | ALT (format1, format2) =>
		      if M.measure format1 <= lw - cc  (* format1 fits flat *)
		      then render0 (format1, blm, cc, newlinep)
		      else render0 (format2, blm, cc, newlinep))

        (* renderElements : element list * int * bool -> int * bool
         *  rendering the elements of an BLOCK *)
        and renderElements (elements, blm, newlinep) =
            let fun re (nil, cc, newlinep) = (cc, newlinep)
		  | re (element::rest, cc, newlinep) =
		      (case element
			 of FMT format =>
			      let val (cc', newlinep') = render0 (format, blm, cc, newlinep)
			       in re (rest, cc', newlinep')
			      end
			  | BRK break =>  (* rest should start with a FMT! *)
			      (case break
				 of NullBreak  => re (rest, cc, false)
				  | HardLine   => (nlIndent blm; re (rest, blm, true))
				  | Space n    => (sp n; re (rest, cc + n, newlinep))
				  | SoftLine n =>
				      (case rest
					 of FMT format' :: rest' =>    (* ASSERT: rest = FMT _ :: _ *)
					      if M.measure format' <= (lw - cc) - n
					      then let val (cc', newlinep') =
							   (sp n; render0 (format', blm, cc + n, false))
						   in re (rest', cc', newlinep')
						   end
					      else (nlIndent blm; re (rest, blm, true))
					  | _ => error "renderElements 1: adjacent breaks")))
	     in re (elements, blm, newlinep)
	    end (* end renderElements *)

        (* renderABLOCK : format list * alignment * bindent * int * int * bool -> int * bool
	 * Render the contents of an aligned block with the effects of the virtual break and bindent.
         * The first three elements are the components of the block being rendered,
         * the last three arguments are:
	 *   blm: int -- parent block's blm (or 0 at "top-level", with no parent block),
	 *   cc: int -- current column at block entry, which becomes the new block's blm unless it is indented,
         *   newlinep: bool -- flag indicating whether this block follows a newline+indent *)
	and renderABLOCK (nil, _, _, _, cc, newlinep) =
	      (* Special case of "empty" block, containing no formats, renders as the empty format, producing no output;
               * but should bindent, if not NI, take effect? See Notes at the bottom of this file. *)
	      (cc, newlinep)
          | renderABLOCK (formats, alignment, bindent, parentBlm, cc, newlinep) =
	      let (* val _ = print ">>> renderABLOCK[not nil]\n" *)
		  (* renderFormats : format list * int * int * bool -> int * bool 
		   * Arguments:
		   *   format :: rest : format list -- the formats constituting the body (children) of the block
		   *   cc : int -- the current column where the block starts; used to define the block's blm
		   *   newlinep : bool -- flag indicating whether following immediately after a newline+indent
		   * ASSERT: not (null formats) *)
		  fun renderFormats (format::rest, cc, newlinep) =
		      (* new blm values are bound at calls of renderFormats, when there are block indentations *)
		      let val blm = cc  (* the blm of the this ABLOCK = cc on entry *)

			  val renderBreak : (int * int) -> (int * bool) =
				(case alignment
				   of C => (fn (cc, m) => (cc, false))
				    | H => (fn (cc, m) => (sp 1; (cc+1, false)))
				    | V => (fn (cc, m) => (nlIndent blm; (blm, true)))
				    | P =>  (* virtual break is SoftLine 1 *)
				        (fn (cc, m) =>
					    if m <= (lw - cc) - 1  (* conditional on m *)
				            then (sp 1; (cc+1, false)) (* no line break, print 1 space *)
				            else (nlIndent blm; (blm, true))))  (* triggered line break *)

			  fun renderRest (nil, cc, newlinep) = (cc, newlinep) (* when we've rendered all the formats *)
			    | renderRest (format :: rest, cc, newlinep) =  (* newlinep argument not used in this case! *)
				let val (cc0, newlinep0) = renderBreak (cc, M.measure format)
				    val (cc1, newlinep1) = render0 (format, blm, cc0, newlinep0)  (* render the next format *)
				 in renderRest (rest, cc1, newlinep1)  (* then render the rest *)
				end

			  val (cc', newlinep') = render0 (format, blm, cc, newlinep)  (* render the 1st format *)

		       in renderRest (rest, cc', newlinep') (* then render the rest *)
		      end
		    | renderFormats (nil, _, _) = error "renderFormats: no formats"

	      in case bindent  (* interpreted at new block entry *)
		   of NI => (* non-indented block, cc becomes the new blm, which is >= parent's blm *)
		      ((* print "renderABLOCK:NI\n"; *) renderFormats (formats, cc, newlinep))
		    | HI n =>  (* hard indented block *)
			let val blm' = parentBlm + n  (* increased indentation for indented block *)
                         (* val _ = print (concat ["renderABLOCK:HI: ", Int.toString n, " ", Bool.toString newlinep, "\n"]) *)
			 in if newlinep  (* already at blm following a newline+indent, don't need another newline *)
			    then sp n  (* but need to increase the existing blm indentation to blm' = parentBlm + n *)
			    else nlIndent blm';  (* no preceding newline+indent, so we need to supply one *)
			    renderFormats (formats, blm', true)
			end
		    | SI n =>  (* soft indented block *)
			let val blm' = parentBlm + n  (* increased indentation if block indentation is "triggered" *)
			 in if newlinep  (* we should be at parentBlm after newline+indent, just adjust the indentation *)
			    then (sp n;  (* bumping the parent's blm indentation up to blm' *)
				  renderFormats (formats, blm', true))  (* render the formats starting from here (blm') *)
			    else renderFormats (formats, cc, false) (* there is no newline+indent, render the formats
								     * starting from here, i.e. cc *)
			end
	      end (* fun renderABLOCK *)

(*	val _ = sp 5 -- this worked, so sp is ok *)
    in ignore (render0 (format, 0, 0, true))  (* the initial context of the render treated like a newline + 0 indentation ??? *)
   end (* fun render *)

end (* top local *)
end (* structure Render *)

(* NOTES:

1. All newlines are followed by the cumulative block indentation, which may be 0, produced by the nlIndent output function.

2. blm (block left margin) values represent the cummulative effect of the indentations of containing blocks.
   -- the blm of an "in-line" (or non-indented) block is set to the current column (cc) at the entry to the block
   -- the blm of an indented block is set to the parent block's blm incremented by the block's indentation (if triggered)

3. [Edge case] Should (hiblock (HI 2) nil) produce nothing (since the block has no content) or should it produce a
   newline+indent (for the HI 2 bindent) and nothing else?

4. [Q1] Does rendering a format ever end with a final newline+indent?
   [Q2] Is BLOCK {elements = [BRK HardLine], ...} a valid block? If so, it "ends with a newline".
   [Q3] Is vblock [empty, BRK HardLine, empty] (or similar BLOCK formats) treated as equavalent to a newline?

A1: Yes?
We only emit a newline+indent at a HardLine or triggered SoftLine break,
but a normal block will not end with a (virtual) break so "normal" blocks do not end with a newline+indent.

Another possibility is at an indented, but empty, block (e.g. hiblock (HI 3) nil), which could
appear on its own or as the last format in a block.  But we can have indentX n empty --> empty, in which
case an indented empty block turns into an empty block and the indentation is ignored (does not occur).

Also, a basic block whose last element is a BRK (HardLine) is possible. Such a block would end with
a newline+indent (to its blm?).

Should this be disallowed?  Probably not, until we find that it is causing problems or confusion for users.

*)

    
