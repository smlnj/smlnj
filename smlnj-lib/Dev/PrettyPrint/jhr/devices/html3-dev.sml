(* html3-dev.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * A pretty printing device that uses HTML (Version 3.2) markup to control layout.
 *)

structure HTML3Dev
: sig
    include PP_DEVICE
      where type token = HTML.text

  (* combine two styles into one *)
    val combineStyle : (style * style) -> style

  (* unstyled text *)
    val styleNONE : style

  (* standard HTML text styles *)
    val styleTT : style
    val styleI : style
    val styleB : style
    val styleU : style
    val styleSTRIKE : style
    val styleEM : style
    val styleSTRONG : style
    val styleDFN : style
    val styleCODE : style
    val styleSAMP : style
    val styleKBD : style
    val styleVAR : style
    val styleCITE : style

  (* color text (using FONT element) *)
    val color : string -> style

  (* hyper-text links and anchors *)
    val link : string -> style
    val anchor : string -> style
    val linkAnchor : {name : string, href : string} -> style

    val openDev : {wid : int, textWid : int option} -> device
    val done : device -> HTML.text

  end =

struct

  structure H = HTML (* smlnj-lib/HTML/html.sml -- HTML 3.2 *)

    datatype style
      = NOEMPH
      | TT | I | B | U | STRIKE | EM
      | STRONG | DFN | CODE | SAMP | KBD
      | VAR | CITE
      | COLOR of string
      | A of {href : string option, name : string option}
      | STYS of style list

    type token = H.text

    datatype device = DEV of {
	lineWid : int option ref,
	textWid : int option ref,
	emphStk	: (H.text list * style) list ref,
	txt : H.text list ref
      }

  (* return the current emphasis *)
    fun curEmph (DEV{emphStk, ...}) = (case !emphStk
	   of [] => NOEMPH
	    | ((_, em)::r) => em
	  (* end case *))

  (* add PCDATA to the text list *)
    fun pcdata (DEV{txt, ...}, s) = txt := H.PCDATA s :: !txt

   (* concatTxt : device -> H.text list *)
   (* replace the sequence of PCDATA elements at the head of the
    * device's text list (!txt) with its concatenation.
    *)
    fun concatTxt (DEV{txt, ...}) =
	let fun f ([], []) = []
	      | f (H.PCDATA s :: r, l) = f (r, s::l)
	      | f (r, l) = H.PCDATA(String.concat l) :: r
	 in f (!txt, [])
	end

   (* concatTexts : H.text list -> H.text list *)
   (* replace the sequence of PCDATA elements at the head of the
    * device's text list (!txt) with its concatenation.
    *)
    fun concatTexts (texts: H.text list) =
	let fun f ([]: H.text list, []: string list) = []
	      | f (H.PCDATA s :: r, leads) = f (r, s::leads)
	      | f (r, leads) =
		(case leads
		  of nil => r
		   | _ => H.PCDATA (String.concat (rev leads)) :: r
	 in f (texts, [])
	end

    (* wrapStyle : style * H.text list * H.text list -> H.text list *)
    fun wrapStyle (sty, [], tl') = tl'
      | wrapStyle (sty, tl, tl') =
	  let fun wrap (NOEMPH, t) = t
		| wrap (TT, t) = H.TT t
		| wrap (I, t) = H.I t
		| wrap (B, t) = H.B t
		| wrap (U, t) = H.U t
		| wrap (STRIKE, t) = H.STRIKE t
		| wrap (EM, t) = H.EM t
		| wrap (STRONG, t) = H.STRONG t
		| wrap (DFN, t) = H.DFN t
		| wrap (CODE, t) = H.CODE t
		| wrap (SAMP, t) = H.SAMP t
		| wrap (KBD, t) = H.KBD t
		| wrap (VAR, t) = H.VAR t
		| wrap (CITE, t) = H.CITE t
		| wrap (COLOR c, t) = H.FONT{color=SOME c, size=NONE, content=t}
		| wrap (A{name, href}, t) =
		    H.A {name = name, href = href,
			 rel = NONE, rev = NONE, title = NONE,
			 content = t}
		| wrap (STYS l, t) = List.foldr wrap t l
	      val t = (case tl of [t] => t | _ => H.TextList(List.rev tl))

	   in wrap(sty, t) :: tl'
	  end

    (* push/pop a style from the devices style stack.  A pop on an
     * empty style stack is a nop. *)

    (* pushStyle : device * style -> unit *)
    fun pushStyle (dev as DEV{emphStk, txt, ...}, sty) =
	(emphStk := (concatTexts (!txt), sty) :: !emphStk;
	 txt := nil)

    (* popStyle : device -> unit *)
    fun popStyle (DEV{emphStk as ref[], ...}) = ()
      | popStyle (dev as DEV{emphStk as ref ((tl, sty) :: rest), txt, ...}) =
	(txt := wrapStyle (sty, concatTexts (!txt), tl);
	 emphStk := rest)

  (* the default style for the device (this is the current style,
   * if the style stack is empty).
   *)
    fun defaultStyle _ = NOEMPH

  (* the width of the device *)
    fun lineWidth (DEV{lineWid, ...}) = !lineWid
    fun setLineWidth (DEV{lineWid, ...}, w) = lineWid := w

  (* the suggested maximum width of indentation; `NONE` is interpreted as no limit. *)
    fun maxIndent _ = NONE
    fun setMaxIndent _ = ()

  (* the suggested maximum width of text on a line *)
    fun textWidth (DEV{textWid, ...}) = !textWid
    fun setTextWidth (DEV{textWid, ...}, w) = textWid := w

  (* output some number of spaces to the device *)
    fun space (dev, n) =
	  pcdata(dev, concat(List.tabulate (n, fn _ => "&nbsp;")))

  (* output an indentation of the given width to the device *)
    val indent = space

  (* output a new-line to the device *)
    fun newline (dev as DEV{txt, ...}) =
	  txt := H.BR{clear=NONE} :: (concatTxt dev)

  (* output a string in the current style to the device *)
    val string = pcdata

  (* output a token, which is just a HTML text element (e.g., <IMG>) *)
    fun token (DEV{txt, ...}, t) = txt := t :: !txt

  (* flush is a nop for us *)
    fun flush _ = ()

    fun combineStyle (NOEMPH, sty) = sty
      | combineStyle (sty, NOEMPH) = sty
      | combineStyle (STYS l1, STYS l2) = STYS(l1 @ l2)
      | combineStyle (sty, STYS l) = STYS(sty::l)
      | combineStyle (sty1, sty2) = STYS[sty1, sty2]

    val styleNONE = NOEMPH
    val styleTT = TT
    val styleI = I
    val styleB = B
    val styleU = U
    val styleSTRIKE = STRIKE
    val styleEM = EM
    val styleSTRONG = STRONG
    val styleDFN = DFN
    val styleCODE = CODE
    val styleSAMP = SAMP
    val styleKBD = KBD
    val styleVAR = VAR
    val styleCITE = CITE
    val color = COLOR
    fun link s = A{href=SOME s, name=NONE}
    fun anchor s = A{href=NONE, name=SOME s}
    fun linkAnchor {name, href} = A{href=SOME href, name = SOME name}

    fun openDev {wid, textWid} = DEV{
	    txt = ref [],
	    emphStk = ref [],
	    lineWid = ref (SOME wid),
	    textWid = ref textWid
	  }

    fun done (dev as DEV{emphStk = ref [], txt, ...}) = (case (concatTxt dev)
	   of [t] => (txt := []; t)
	    | l => (txt := []; H.TextList(List.rev l))
	  (* end case *))
      | done _ = raise Fail "device is not done yet"

  end; (* HTMLDev *)

