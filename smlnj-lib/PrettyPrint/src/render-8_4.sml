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

(* this is the old, non-functor version of the Render module. Replaced by the RenderFn module in 
 * Version 9.1 *)

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
 *   render as though on an unbounded line (lw = "infinity"), thus "flat" (i.e. no line space pressure).
 *   _No_ newlines are triggered, not even Hard breaks and INDENT formats, which are
 *   rendered as single spaces, like Softline breaks.
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
		 | INDENT (_, fmt) => render0 fmt
		 | FLAT fmt => render0 fmt
		 | ALT (fmt, _) => render0 fmt)   (* any format fits *)

        (* renderElements : element list -> unit *)
        and renderElements nil = sp 1  (* render an empty BLOCK as a single space *)
          | renderElements elements =
            let fun rend nil = ()
		  | rend (element::rest) =
		      (case element
			 of FMT format => (render0 format; rend rest)
			  | BRK break =>
			     (case break
			       of Hard   => (sp 1; rend rest)
				| Soft n => (sp n; rend rest)
				| Space n    => (sp n; rend rest)
				| Null  => rend rest))
	     in rend elements
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
 * The top-level render function decides where to conditionally break lines, and how much indentation should follow
 * each line break, based on the line space available (the difference between the currend column and the line width).
 * In this version (Version 8), the render function also prints the content and formatting, using the output
 * function passed as the second argument. So in this version, rendering and printing are unified and there
 * is no intermediate "layout" structure.
 * Internal rendering functions (render0, renderBLOCK, renderABLOCK) : state -> state, where state = int * bool
 * (state = (cc, newlinep)). cc represents the "print cursor", while newlinep indicates whether we are starting
 * immediately after a line break (a newline + indentation).
 *)
fun render (format: format, output: string -> unit, lw: int) : unit =
    let fun sp n = output (spaces n)

        (* lineBreak : int -> unit  -- perform a newline and indentation of n spaces *)
	fun lineBreak n = (output newlineChar; sp n)

        (* render0: format * int * int * bool -> int * bool
	 * the main recursive rendering of the format
	 * Inputs:
         *   cc: current column, incremented or reset after any output actions (TEXT, sp, lineBreak)
         *   outerBlm: block left margin of "parent" block containing this format; defaults to 0 for top-level format
         *        blm = the cumulative inherited indentation from containing formats
         *              (incremented by surrounding nested INDENT constrs)
	 *     rebound at the entrance to each block to that block's block left margin (initially cc, which may = blm)
	 *   newlinep: bool indicating whether the immediately previously rendered format or break resulted in a
	 *     newline+indent; the top-level call of render0 is treated as though it followed a newline with 0 indent.
         *   ASSERT: blm <= cc  (When is blm < cc?)
         * Outputs:
	 *   cc' : int -- the current column when the render is completed (= position where next character will be printed)
	 *   newlinep' : bool -- reports whether this render0 call _ended_ with a newline+indent
	 *   -- INVARIANT: outerBlm <= cc
	 *   -- INVARIANT: we will never print to the left of the outer block's blm (outerBlm)
	 *   -- ASSERT: if newlinep is true, then cc = outerBlm *)
	fun render0  (format: format, cc: int, newlinep: bool) =
	      (case format
	         of EMPTY =>  (* nothing printed, nothing changed; outerBlm not relevant *)
		      (cc, newlinep)

		  | TEXT s =>  (* print the string unconditionally; move cc accordingly; outerBlm not relevant *)
		      (output s; (cc + size s, false))

 		  | BLOCK {elements, ...} => (* establishes a new local blm = cc; outerBlm not relevant *)
		      renderBLOCK (elements, cc, newlinep)

		  | ABLOCK {formats, alignment, ...} => (* establishes a new local blm = cc; outerBlm not relevant *)
		      renderABLOCK (formats, alignment, cc, newlinep)

		  | INDENT (n, fmt) => (* soft indented block; depends on outerBlm *)
		      if newlinep  (* ASSERT: if newlinep then cc = blm of parent block *)
		      then (sp n;  (* increase cc = cc + n (indenting n relative to blm of parent block *)
			    render0 (fmt, cc + n, true))
		      else render0 (fmt, cc, false) (* not on new line, proceed at cc without line break *)

		  | FLAT format =>  (* unconditionally render the format as flat; outerBlm not relevant *)
		      (flatRender (format, output); (cc + M.measure format, false))

		  | ALT (format1, format2) =>
		      if M.measure format1 <= lw - cc  (* format1 fits flat *)
		      then render0 (format1, cc, newlinep)
		      else render0 (format2, cc, newlinep))

        (* renderBLOCK : element list * int * bool -> int * bool
         *  rendering the elements of an BLOCK *)
        and renderBLOCK (elements, cc, newlinep) =
            let val blm = cc (* the new block's blm is the entry cc *)
		fun re (nil, cc, newlinep) = (cc, newlinep)
		  | re (element::rest, cc, newlinep) =
		      (case element
			 of FMT format =>
			      let val (cc', newlinep') = render0 (format, cc, newlinep)
			       in re (rest, cc', newlinep')
			      end
			  | BRK break =>  (* rest should start with a FMT! *)
			      (case break
				 of Null    => re (rest, cc, false)
				  | Hard    => (lineBreak blm; re (rest, blm, true))
				  | Space n => (sp n; re (rest, cc + n, false))
				  | Soft n  =>
				      (case rest  (* ASSERT: rest = FMT _ :: _; a BRK should be followed by a FMT *)
					 of FMT format' :: rest' =>
					      if M.measure format' <= (lw - cc) - n  (* lw - (cc + n) *)
					      then let val (cc', newlinep') = (sp n; render0 (format', cc + n, false))
						    in re (rest', cc', newlinep')
						   end
					      else (lineBreak blm; re (rest, blm, true))  (* trigger newline+indent *)
					  | _ => error "renderBLOCK 1: adjacent breaks")))
	     in re (elements, cc, newlinep)
	    end (* end renderBLOCK *)

        (* renderABLOCK : [formats:]format list * alignment * [cc:]int * [newlinep:]bool -> int * bool
	 * Render the contents of an aligned block with the effects of the virtual break and bindent.
         * The first three elements are the components of the block being rendered,
         * the last three arguments are:
	 *   cc: int -- current column at block entry, which becomes the new block's blm unless it is indented,
         *   newlinep: bool -- flag indicating whether this block follows a newline+indent
	 * Rendering the new block does not need to use the partent's blm, so no blm argument is passed. *)
	and renderABLOCK (nil, _, cc, newlinep) = (cc, newlinep)
	      (* Special case of "empty" block, containing no formats, renders as the empty format, producing no output;
               * But this case should not occur, because (alignedBlock _ nil) should yield EMPTY, not an empty ABLOCK. *)
          | renderABLOCK (formats, alignment, cc, newlinep) =
	      let (* val _ = print ">>> renderABLOCK[not nil]\n" *)
		  val blm = cc  (* the blm of _this_ block is defined as the entrance cc *)
		  (* renderFormats : format list * int * bool -> int * bool
		   * Arguments:
		   *   format :: rest : format list -- the formats constituting the body (children) of the block
		   *   cc : int -- the current column where the block starts; used to define the block's blm
		   *   newlinep : bool -- flag indicating whether following immediately after a newline+indent
		   * ASSERT: not (null formats) *)
		  fun renderFormats (format::rest, cc, newlinep) =
			let (* renderBreak : [cc:]int * [measure:]m -> [cc:]int * [newlinep:]bool *)
			    val renderBreak : (int * int) -> (int * bool) =
				  (case alignment
				     of C => (fn (cc, m) => (cc, false))
				      | H => (fn (cc, m) => (sp 1; (cc+1, false)))
				      | V => (fn (cc, m) => (lineBreak blm; (blm, true)))
				      | P =>  (* virtual break is Soft 1 *)
					  (fn (cc, m) =>
					      if m <= (lw - cc) - 1  (* conditional on m *)
					      then (sp 1; (cc+1, false)) (* no line break, print 1 space *)
					      else (lineBreak blm; (blm, true))))  (* triggered line break *)

			    fun renderRest (nil, cc, newlinep) = (cc, newlinep) (* when we've rendered all the formats *)
			      | renderRest (format :: rest, cc, newlinep) =  (* newlinep argument not used in this case! *)
				  let val (cc0, newlinep0) = renderBreak (cc, M.measure format)
				      val (cc1, newlinep1) = render0 (format, cc0, newlinep0)  (* render the next format *)
				   in renderRest (rest, cc1, newlinep1)  (* then render the rest *)
				  end

			    val (cc', newlinep') = render0 (format, cc, newlinep)  (* render the 1st format *)

			 in renderRest (rest, cc', newlinep') (* then render the rest *)
			end
		    | renderFormats (nil, _, _) = error "renderFormats: no formats"

	       in renderFormats (formats, cc, newlinep)
	      end (* fun renderABLOCK *)

    in (* the initial "context" of a render is a virtual newline + 0 indentation *)
       ignore (render0 (format, 0, true))
   end (* fun render *)

end (* top local *)
end (* structure Render *)

(* NOTES:

1. All newlines are followed by the cumulative block indentation, which may be 0, produced by the lineBreak output function.

2. blm (block left margin) values represent the cummulative effect of the indentations of containing blocks.
   -- the blm of an "in-line" (or non-indented) block is set to the current column (cc) at the entry to the block
   -- the blm of an indented block is set to the parent block's blm incremented by the block's indentation (if triggered)

3. [Q: Edge case] Should INDENT (n, EMPTY) produce nothing (since the format has no content) or should it produce
   a line break (newline+indent) and nothing else?

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

*)
