(* pp-stream-fn.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * The implementation of PP streams, where all the action is.
 *)

functor PPStreamFn (
    structure Token : PP_TOKEN
    structure Device : PP_DEVICE
      sharing type Token.style = Device.style
(**
  ) : PP_STREAM =
**)
  ) : sig
    include PP_STREAM
    val dump : (TextIO.outstream * stream) -> unit
  end = struct

    structure D = Device
    structure T = Token
    structure Q = Queue

  (* imperative stacks *)
    structure Stk :> sig
	type 'a t
	val new : unit -> 'a t		(* create a new stack *)
	val clear : 'a t -> unit	(* reset the stack to empty *)
	val push : 'a t * 'a -> unit	(* push an item *)
	val pop : 'a t -> 'a option	(* pop an item (`NONE` on empty) *)
	val top : 'a t -> 'a option	(* top of stack or `NONE` *)
        val discard : 'a t -> unit	(* discard top element (or nop on empty) *)
	val toList : 'a t -> 'a list	(* list of items; top first *)
      end = struct
	type 'a t = 'a list ref
	fun new () : 'a t = ref[]
	fun clear (stk : 'a t) = stk := []
	fun push (stk : 'a t, x) = stk := x :: !stk
	fun pop (stk : 'a t) = (case !stk
	       of [] => NONE
		| x::r => (stk := r; SOME x)
	      (* end case *))
	fun top (stk : 'a t) = (case !stk of [] => NONE | x::_ => SOME x)
        fun discard (stk : 'a t) = (case !stk of [] => () | _::r => stk := r)
	fun toList (stk : 'a t) = !stk
      end

    type device = D.device
    type token = T.token
    type style = T.style

    datatype indent
      = Abs of int		(* indent relative to outer indentation *)
      | Rel of int		(* indent relative to start of box *)

  (**** DATA STRUCTURES ****)

  (* tokens represent pending pretty-printing operations in the queue *)
    datatype pp_token
      = TEXT of string		(* raw text.  This includes tokens.  The *)
				(* width and style information is taken *)
				(* care of when they are inserted in *)
				(* queue. *)
      | NBSP of int		(* some number of non-breakable spaces *)
      | BREAK of {              (* a potential line break *)
            nsp : int,          (* width of whitespace used if there is no break *)
            offset : int        (* indentation offset of next line if there is a break *)
          }
      | BEGIN of {		(* the beginning of a box *)
            indent : indent,	(* the box's indentation mode and width *)
	    ty : box_ty	(* the type of box *)
	  }
      | END			(* the end of a box *)
      | PUSH_STYLE of style	(* push a style on the style stack *)
      | POP_STYLE		(* pop a style off of the style stack *)
      | NL			(* hard newline *)
      | IF_NL			(* [unimplemented] *)
      | CTL of (device -> unit)	(* device control operation *)

  (* the types of boxes *)
   and box_ty
    = HBOX			(* horizontal box: breaks map to spaces *)
    | VBOX			(* vertical box: break map to newlines *)
    | HVBOX			(* horizontal/vertical box: like an HBOX if the stuff fits,
				 * otherwise like a VBOX.
				 *)
    | HOVBOX			(* packing box: breaks are converted to spaces when they
				 * fix; otherwise line breaks are introduced.
				 *)
    | BOX			(* structural box: like a packing box, but breaks are mapped
				 * to newlines when the result of doing so would move the
				 * indent to the left.
				 *)
    | FITS			(* internal marker for boxes that have been determined to
				 * fit on the current line.
				 *)

    type pp_queue_elem = {	(* elements of the PP queue *)
	tok : pp_token,		(* the element *)
	sz : int ref,		(* size of block (set when known) *)
	len : int		(* the display length of the token for strings and breaks;
				 * all other tokens have len = 0.
				 *)
      }

    datatype stream = PP of {
	dev : device,			(* the underlying device *)
	closed : bool ref,		(* set to true, when the stream is *)
					(* closed *)
	width : int,			(* the width of the device *)
	maxIndent : int,		(* the maximum indentation allowed *)
	maxDepth : int,			(* maximum nesting depth of open boxes *)
	spaceLeft : int ref,		(* space left on current line *)
	curIndent : int ref,		(* current indentation *)
	curDepth : int ref,		(* current nesting level of boxes. *)
	leftTot : int ref,		(* total width of tokens already printed *)
	rightTot : int ref,		(* total width of tokens ever inserted *)
					(* into the queue. *)
	newline : bool ref,		(* `true` when we are at the start of a new line *)
	queue : pp_queue_elem Q.queue,	(* the queue of pending tokens *)
	fmtStk : (box_ty * int) Stk.t,  (* active blocks.  The int is the indentation *)
					(* of the block *)
	scanStk
	  : (int * pp_queue_elem) Stk.t,
	styleStk : style Stk.t
      }

  (**** DEBUGGING FUNCTIONS ****)
    structure F = Format
    fun boxTypeToString HBOX = "HBOX"
      | boxTypeToString VBOX = "VBOX"
      | boxTypeToString HVBOX = "HVBOX"
      | boxTypeToString HOVBOX = "HOVBOX"
      | boxTypeToString BOX = "BOX"
      | boxTypeToString FITS = "FITS"
    fun indentToString (Abs n) = concat["Abs ", Int.toString n]
      | indentToString (Rel n) = concat["Rel ", Int.toString n]
    fun tokToString (TEXT s) = concat["TEXT \"", String.toString s, "\""]
      | tokToString (NBSP n) = concat["NBSP ", Int.toString n]
      | tokToString (BREAK{nsp, offset}) =
	  F.format "BREAK{nsp=%d, offset=%d}" [F.INT nsp, F.INT offset]
      | tokToString (BEGIN{indent, ty}) = F.format "BEGIN{indent=%s, ty=%s}" [
	    F.STR(indentToString indent), F.STR(boxTypeToString ty)
	  ]
      | tokToString END = "END"
      | tokToString (PUSH_STYLE _) = "PUSH_STYLE _"
      | tokToString POP_STYLE = "POP_STYLE"
      | tokToString NL = "NL"
      | tokToString IF_NL = "IF_NL"
      | tokToString (CTL f) = "CTL _"
    fun qelemToString {tok, sz, len} = F.format "{tok=%s, sz=%d, len=%d}" [
	    F.STR(tokToString tok), F.INT(!sz), F.INT len
	  ]
    fun scanElemToString (n, elem) =
	  F.format "(%d, %s)" [F.INT n, F.STR(qelemToString elem)]
    fun fmtElemToString (ty, n) =
	  F.format "(%s, %d)" [F.STR(boxTypeToString ty), F.INT n]
    fun dump (outStrm, PP pp) = let
	  fun pr s = TextIO.output(outStrm, s)
	  fun prf (fmt, items) = pr(F.format fmt items)
	  fun prl fmtElem [] = pr "[]"
	    | prl fmtElem l = pr(ListFormat.fmt {
		  init = "[\n    ", final = "]", sep = "\n    ", fmt = fmtElem
		} l)
	  in
	    pr  ("BEGIN\n");
	    prf ("  width     = %3d, spaceLeft = %3d\n", [
		F.INT(#width pp), F.INT(!(#spaceLeft pp))
	      ]);
	    prf ("  curIndent = %3d, curDepth  = %3d\n", [
		F.INT(!(#curIndent pp)), F.INT(!(#curDepth pp))
	      ]);
	    prf ("  leftTot   = %3d, rightTot  = %3d\n", [
		F.INT(!(#leftTot pp)), F.INT(!(#rightTot pp))
	      ]);
	    pr   "  queue = "; prl qelemToString (Q.contents(#queue pp)); pr "\n";
	    pr   "  fmtStk = "; prl fmtElemToString (Stk.toList(#fmtStk pp)); pr "\n";
	    pr   "  scanStk = "; prl scanElemToString (Stk.toList(#scanStk pp)); pr "\n";
	    pr  ("END\n")
	  end

  (**** UTILITY FUNCTIONS ****)

  (* use as a limit value for when the device does not specify a limit *)
    val infinity = (case Int.maxInt of SOME n => n-1 | _ => 1000000)

  (* output text to the device; note that the size is specified separately,
   * since it might be different from the actual string length (e.g., UTF8
   * multibyte characters)
   *)
    fun output (_, "", 0) = ()
      | output (PP{dev, spaceLeft, newline, ...}, s, sz) = (
	  spaceLeft := !spaceLeft - sz;
	  newline := false;
	  D.string(dev, s))

  (* output a newline to the device *)
    fun outputNL (PP{dev, newline, ...}) = (
	  newline := true;
	  D.newline dev)

(* TODO: add `indent` function to device API *)
  (* output indentation to the device *)
    fun outputIndent (_, 0) = ()
      | outputIndent (PP{dev, ...}, n) = D.space (dev, n)

  (* output non-indent spaces to the device *)
    fun blanks (_, 0) = ()
      | blanks (PP{dev, ...}, n) = D.space (dev, n)

  (* add a token to the pretty-printer queue *)
    fun enqueueTok (PP{rightTot, queue, ...}, tok) = (
	  rightTot := !rightTot + #len tok;
	  Q.enqueue(queue, tok))

  (* format a break as a newline; indenting the new line.
   *   strm	-- PP stream
   *   offset	-- the extra indent amount supplied by the break
   *   wid	-- the remaining line width at the opening of the
   *		   innermost enclosing box.
   *)
    fun breakNewLine (strm, offset, wid) = let
	  val PP{width, maxIndent, curIndent, spaceLeft, ...} = strm
	(* limit indentation to maximum amount *)
	  val indent = Int.min(maxIndent, (width - wid) + offset)
	  in
	    outputNL strm;
	    curIndent := indent;
	    spaceLeft := width - indent;
	    outputIndent (strm, indent)
	  end

  (* format a break as spaces.
   *   strm	-- PP stream
   *   nsp	-- number of spaces to output.
   *)
    fun breakSameLine (strm as PP{spaceLeft, ...}, nsp) = (
	  spaceLeft := !spaceLeft - nsp;
	  blanks (strm, nsp))

  (* force a line break when opening a box would make the indentation larger than
   * the limit.
   *)
    fun forceLineBreak (strm as PP{fmtStk, spaceLeft, ...}) = (case Stk.top fmtStk
	   of SOME(ty, wid) => if (wid > !spaceLeft)
		then (case ty
		   of (FITS | HBOX) => ()
		    | _ => breakNewLine (strm, 0, wid)
		  (* end case *))
		else ()
	    | NONE => outputNL strm
	  (* end case *))

  (* skip a token *)
    fun skip (PP{queue, leftTot, spaceLeft, ...}) = (case Q.next queue
	   of NONE => ()
	    | SOME{tok, sz, len} => (
		leftTot := !leftTot - len;
		spaceLeft := !spaceLeft + !sz)
	  (* end case *))

  (* return the current style of the PP stream *)
    fun currentStyle (PP{styleStk, dev, ...}) = (case Stk.top styleStk
	   of NONE => D.defaultStyle dev
	    | SOME sty => sty
	  (* end case *))

  (**** FORMATTING ****)

  (* `format (strm, sz, tok)` formats a PP token that has the specified size *)
    fun format (strm, sz, tok) = (
	  case tok
	   of (TEXT s) => output (strm, s, sz)
	    | (NBSP n) => let
		val PP{spaceLeft, ...} = strm
		in
		  spaceLeft := !spaceLeft - sz;
		  blanks (strm, n)
		end
	    | (BREAK{nsp, offset}) => let
		val PP{fmtStk, spaceLeft, width, curIndent, newline, ...} = strm
		in
		  case Stk.top fmtStk
		   of SOME(HBOX, wid) => breakSameLine (strm, nsp)
		    | SOME(VBOX, wid) => breakNewLine (strm, offset, wid)
		    | SOME(HVBOX, wid) => breakNewLine (strm, offset, wid)
		    | SOME(HOVBOX, wid) => if (sz > !spaceLeft)
			then breakNewLine (strm, offset, wid)
			else breakSameLine (strm, nsp)
		    | SOME(BOX, wid) =>
			if !newline
			  then breakSameLine (strm, nsp)
			else if (sz > !spaceLeft)
			  then breakNewLine (strm, offset, wid)
			else if (!curIndent > (width - wid) + offset)
			  then breakNewLine (strm, offset, wid)
			  else breakSameLine (strm, nsp)
		    | SOME(FITS, wid) => breakSameLine (strm, nsp)
		    | NONE => () (* no open box *)
		  (* end case *)
		end
	    | (BEGIN{indent, ty}) => let
		val PP{maxIndent, curIndent, spaceLeft, width, fmtStk, ...} = strm
		val _ = if (width - !spaceLeft) > maxIndent
		      then forceLineBreak strm
		      else ()
		val spaceLeft' = !spaceLeft
	      (* compute offset from right margin of this block's indent *)
		val offset = (case indent
		       of (Rel off) => spaceLeft' - off
			| (Abs off) => (case Stk.top fmtStk
			     of SOME(_, wid) => wid - off
			      | NONE => width - (!curIndent + off)
			    (* end case *))
		      (* end case *))
		val ty' = (case ty
		       of VBOX => VBOX
			| _ => if (sz > spaceLeft') then ty else FITS
		      (* end case *))
		in
		  Stk.push (fmtStk, (ty', offset))
		end
	    | END => let
		val PP{fmtStk, ...} = strm
		in
		  Stk.discard fmtStk
		end
	    | (PUSH_STYLE sty) => let
		val PP{dev, ...} = strm
		in
		  D.pushStyle (dev, sty)
		end
	    | POP_STYLE => let
		val PP{dev, ...} = strm
		in
		  D.popStyle dev
		end
	    | NL => let
		val PP{fmtStk, ...} = strm
		in
		  case Stk.top fmtStk
		   of SOME(_, wid) => breakNewLine (strm, 0, wid)
		    | NONE => outputNL strm
		  (* end case *)
		end
	    | IF_NL => let
		val PP{newline, ...} = strm
		in
(* NOTE: the Ocaml version tests if !curIndent = width - !spaceLeft, but the newline
 * flag should be true in that case.
 *)
		  if !newline then () else skip strm
		end
	    | (CTL ctlFn) => let
		val PP{dev, ...} = strm
		in
		  ctlFn dev
		end
	  (* end case *))

    fun advanceLeft strm = let
	  val PP{spaceLeft, leftTot, rightTot, queue, ...} = strm
	  fun advance () = (case Q.peek queue
		 of (SOME{tok, sz=ref sz, len}) =>
		      if ((sz >= 0) orelse (!rightTot - !leftTot >= !spaceLeft))
			then (
			  ignore(Q.dequeue queue);
			  format (strm, if sz < 0 then infinity else sz, tok);
			  leftTot := len + !leftTot;
			  advance())
			else ()
		  | NONE => ()
		(* end case *))
	  in
	    advance ()
	  end

    fun enqueueAndAdvance (strm, tok) = (
	  enqueueTok (strm, tok);
	  advanceLeft strm)

    fun enqueueTokenWithLen (strm, tok, len) =
	  enqueueAndAdvance (strm, {sz = ref len, len = len, tok = tok})

    fun enqueueStringWithLen (strm, s, len) =
	  enqueueTokenWithLen (strm, TEXT s, len)

    fun enqueueToken (strm, tok) = enqueueTokenWithLen (strm, tok, 0)

  (* the scan stack always has this element on its bottom *)
    val scanStkBot = (~1, {sz = ref ~1, tok = TEXT "", len = 0})

  (* clear the scan stack *)
    fun clearScanStk (PP{scanStk, ...}) = (
	  Stk.clear scanStk;
	  Stk.push(scanStk, scanStkBot))

  (* Set the size of the element on the top of the scan stack.  The isBreak
   * flag is set to true for breaks and false for boxes.
   *)
    fun setSize (strm as PP{leftTot, rightTot, scanStk, ...}, isBreak) = (
	  case Stk.top scanStk
	   of NONE => raise Fail "PPStreamFn:setSize: impossible: scanStk is empty"
	    | SOME(leftTot', elem) =>
	      (* check for obsolete elements *)
		if (leftTot' < !leftTot)
		  then clearScanStk strm
		  else (case (elem, isBreak)
		     of ({sz, tok=BREAK _, ...}, true) => (
			  sz := !sz + !rightTot;
			  Stk.discard scanStk)
		      | ({sz, tok=BEGIN _, ...}, false) => (
			  sz := !sz + !rightTot;
			  Stk.discard scanStk)
		      | _ => ()
		    (* end case *))
	  (* end case *))

    fun pushScanElem (strm as PP{scanStk, rightTot, ...}, setSz, tok) = (
	  enqueueTok (strm, tok);
	  if setSz then setSize (strm, true) else ();
	  Stk.push (scanStk, (!rightTot, tok)))

  (* Open a new box *)
    fun ppOpenBox (strm, indent, boxTy) = let
	  val PP{dev, rightTot, maxDepth, curDepth, ...} = strm
	  in
	    curDepth := !curDepth + 1;
	    if (!curDepth < maxDepth)
	      then pushScanElem (strm, false, {
		  sz = ref(~(!rightTot)),
		  tok = BEGIN{indent=indent, ty=boxTy},
		  len = 0
		})
	    else if (!curDepth = maxDepth)
	      then let
		val (s, len) = D.ellipses dev
		in
		  enqueueStringWithLen (strm, s, len)
		end
	      else ()
	  end

  (* the root box, which is always open *)
    fun openSysBox strm = ppOpenBox (strm, Rel 0, HOVBOX)

  (* close a box *)
    fun ppCloseBox (strm as PP{maxDepth, curDepth as ref depth, ...}) =
	  if (depth <= 1)
	    then raise Fail "unmatched close box"
	  else if (depth < maxDepth)
	    then (
	      enqueueTok (strm, {sz = ref 0, tok = END, len = 0});
	      setSize (strm, true);
	      setSize (strm, false);
	      curDepth := depth-1)
	    else curDepth := depth-1

    fun ppBreak (strm as PP{rightTot, ...}, arg) = (
	  pushScanElem (strm, true, {
	      sz = ref(~(!rightTot)), tok = BREAK arg, len = #nsp arg
	    }))

    fun ppInit (strm as PP pp) = (
	  Q.clear(#queue pp);
	  clearScanStk strm;
	  #spaceLeft pp := #width pp;
	  #curIndent pp := 0;
	  #curDepth pp := 0;
	  #leftTot pp := 1;
	  #rightTot pp := 1;
	  #newline pp := true;
	  Stk.clear (#fmtStk pp);
	  Stk.clear (#styleStk pp);
	  openSysBox strm)

    fun ppNewline strm =
	  enqueueAndAdvance (strm, {sz = ref 0, tok = NL, len = 0})

    fun ppFlush (strm as PP{dev, curDepth, rightTot, ...}, withNL) = let
	  fun closeBoxes () = if (!curDepth > 1)
		then (ppCloseBox strm; closeBoxes())
		else ()
	  in
	    closeBoxes ();
	    rightTot := infinity;
	    advanceLeft strm;
	    if withNL then outputNL strm else ();
	    D.flush dev;
	    ppInit strm
	  end

  (**** USER FUNCTIONS ****)
    fun openStream d = let
	  fun limit optInt = Option.getOpt(optInt, infinity)
	  val width = limit(D.lineWidth d)
	  val maxIndent = Int.min(limit(D.maxIndent d), width-1)
	  val maxDepth = Int.max(limit(D.maxIndent d), 2)
	  val strm = PP{
		  dev = d,
		  closed = ref false,
		  width = width,
		  maxIndent = maxIndent,
		  maxDepth = maxDepth,
		  spaceLeft = ref 0,
		  curIndent = ref 0,
		  curDepth = ref 0,
		  leftTot = ref 1,	(* why 1 ? *)
		  rightTot = ref 1,	(* why 1 ? *)
		  newline = ref true,
		  queue = Q.mkQueue(),
		  fmtStk = Stk.new(),
		  scanStk = Stk.new(),
		  styleStk = Stk.new()
		}
	  in
	    if (width < 0) orelse (maxIndent < 0) orelse (maxDepth < 0)
	    orelse (width < maxIndent)
	      then raise Size
	      else ();
	    ppInit strm;
	    strm
	  end

    fun flushStream strm = ppFlush(strm, false)
    fun closeStream (strm as PP{closed, ...}) = (flushStream strm; closed := true)
    fun getDevice (PP{dev, ...}) = dev

    fun openHBox strm = ppOpenBox (strm, Abs 0, HBOX)
    fun openVBox strm indent = ppOpenBox (strm, indent, VBOX)
    fun openHVBox strm indent = ppOpenBox (strm, indent, HVBOX)
    fun openHOVBox strm indent = ppOpenBox (strm, indent, HOVBOX)
    fun openBox strm indent = ppOpenBox (strm, indent, BOX)
    fun closeBox strm = ppCloseBox strm

    fun token (strm as PP{dev, ...}) t = let
	  val tokStyle = T.style t
	  in
	    if (D.sameStyle(currentStyle strm, tokStyle))
	      then enqueueStringWithLen (strm, T.string t, T.size t)
	      else (
		enqueueToken (strm, PUSH_STYLE tokStyle);
		enqueueStringWithLen (strm, T.string t, T.size t);
		enqueueToken (strm, POP_STYLE))
	  end
    fun string strm s = enqueueStringWithLen(strm, s, size s)

    fun pushStyle (strm as PP{styleStk, ...}, sty) = (
	  if (D.sameStyle(currentStyle strm, sty))
	    then ()
	    else enqueueToken (strm, PUSH_STYLE sty);
	  Stk.push (styleStk, sty))
    fun popStyle (strm as PP{styleStk, ...}) = (case Stk.pop styleStk
	   of NONE => raise Fail "PP: unmatched popStyle"
	    | SOME sty => if (D.sameStyle(currentStyle strm, sty))
		then ()
		else enqueueToken (strm, POP_STYLE)
	  (* end case *))

    fun break strm arg = ppBreak (strm, arg)
    fun space strm n = break strm {nsp=n, offset=0}
    fun cut strm = break strm {nsp=0, offset=0}
    fun newline strm = ppNewline strm
    fun nbSpace strm n = enqueueTokenWithLen (strm, NBSP n, n)

    fun control strm ctlFn = enqueueToken (strm, CTL ctlFn)

  end
