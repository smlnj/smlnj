(* html3-dev.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * A pretty printing device that uses HTML (Version 3.2) markup to control layout.
 *
 * TODO: switch to using the `MakePPDeviceFn` functor.
 *)

local
  structure DevOps =
    struct

      (* ===== Types ===== *)

      datatype style
        = NOEMPH
        | TT | I | B | U | STRIKE | EM
        | STRONG | DFN | CODE | SAMP | KBD
        | VAR | CITE
        | COLOR of string
        | A of {href : string option, name : string option}
        | STYS of style list

      (* the container for the state of the device *)
      type t = {
          emphStk : (HTML.text list * style) list ref,
          txt : HTML.text list ref
        }

      (* tokens are arbitrary HTML text elements *)
      type token = HTML.text

      (* ===== Internal Operations ===== *)

      (* return the current emphasis *)
      fun curEmph (dev : t) = (case !(#emphStk dev)
             of [] => NOEMPH
              | ((_, em)::r) => em
            (* end case *))

      (* add a text element to the text list *)
      fun addTextElement ({txt, ...} : t, e : HTML.text) = txt := e :: !txt

      (* add PCDATA to the text list *)
      fun addPCData (dev, s) = addTextElement(dev, HTML.PCDATA s)

      (* replace the sequence of PCDATA elements at the head of the
       * txt list with its concatenation.
       *)
      fun concatTxt ({txt, ...} : t) = let
            fun f ([], []) = []
              | f (HTML.PCDATA s :: r, l) = f (r, s::l)
              | f (r, l) = HTML.PCDATA(String.concat l) :: r
            in
              f (!txt, [])
            end

      fun wrapStyle (sty, [], tl') = tl'
        | wrapStyle (sty, tl, tl') = let
            fun wrap (NOEMPH, t) = t
              | wrap (TT, t) = HTML.TT t
              | wrap (I, t) = HTML.I t
              | wrap (B, t) = HTML.B t
              | wrap (U, t) = HTML.U t
              | wrap (STRIKE, t) = HTML.STRIKE t
              | wrap (EM, t) = HTML.EM t
              | wrap (STRONG, t) = HTML.STRONG t
              | wrap (DFN, t) = HTML.DFN t
              | wrap (CODE, t) = HTML.CODE t
              | wrap (SAMP, t) = HTML.SAMP t
              | wrap (KBD, t) = HTML.KBD t
              | wrap (VAR, t) = HTML.VAR t
              | wrap (CITE, t) = HTML.CITE t
              | wrap (COLOR c, t) = HTML.FONT{color=SOME c, size=NONE, content=t}
              | wrap (A{name, href}, t) = HTML.A{
                    name = name, href = href,
                    rel = NONE, rev = NONE, title = NONE,
                    content = t
                  }
              | wrap (STYS l, t) = List.foldr wrap t l
            val t = (case tl of [t] => t | _ => HTML.TextList(List.rev tl))
            in
              wrap(sty, t) :: tl'
            end

      (* ===== Style Operations ===== *)

      fun pushStyle (dev as {emphStk, txt}, sty) = (
            emphStk := (concatTxt dev, sty) :: !emphStk;
            txt := [])

      fun popStyle ({emphStk as ref[], txt}) = ()
        | popStyle (dev as {emphStk as ref ((tl, sty) :: r), txt}) = (
            txt := wrapStyle (sty, concatTxt dev, tl);
            emphStk := r)

      val defaultStyle = NOEMPH

      (* ===== Output Operations ===== *)

      fun space (dev, n) =
            addPCData(dev, concat(List.tabulate (n, fn _ => "&nbsp;")))

      val indent = space

      fun newline (dev as {txt, ...}) =
            txt := HTML.BR{clear=NONE} :: (concatTxt dev)

      val string = addPCData

      val token = addTextElement

      (* flush is a nop for us *)
      fun flush _ = ()

    end
in

structure HTML3Dev : sig

    include PP_DEVICE

    (* combine two styles into one *)
    val combineStyle : (style * style) -> style

    (* unstyled text *)
    val styleNONE : style (* == defaultStyle *)

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

    val openDev : {wid : int} -> device
    val done : device -> HTML.text

  end = struct

    structure Device = MakePPDeviceFn (DevOps)

    open Device

    fun combineStyle (DevOps.NOEMPH, sty) = sty
      | combineStyle (sty, DevOps.NOEMPH) = sty
      | combineStyle (DevOps.STYS l1, DevOps.STYS l2) = DevOps.STYS(l1 @ l2)
      | combineStyle (sty, DevOps.STYS l) = DevOps.STYS(sty::l)
      | combineStyle (sty1, sty2) = DevOps.STYS[sty1, sty2]

    val styleNONE = defaultStyle
    val styleTT = DevOps.TT
    val styleI = DevOps.I
    val styleB = DevOps.B
    val styleU = DevOps.U
    val styleSTRIKE = DevOps.STRIKE
    val styleEM = DevOps.EM
    val styleSTRONG = DevOps.STRONG
    val styleDFN = DevOps.DFN
    val styleCODE = DevOps.CODE
    val styleSAMP = DevOps.SAMP
    val styleKBD = DevOps.KBD
    val styleVAR = DevOps.VAR
    val styleCITE = DevOps.CITE
    val color = DevOps.COLOR
    fun link s = DevOps.A{href=SOME s, name=NONE}
    fun anchor s = DevOps.A{href=NONE, name=SOME s}
    fun linkAnchor {name, href} = DevOps.A{href=SOME href, name = SOME name}

    fun openDev {wid} = newWithWidth ({txt = ref[], emphStk = ref[]}, wid)

    fun done device = (case devOps device
           of (devOps as {emphStk = ref [], txt, ...}) => (
                case (DevOps.concatTxt devOps)
                 of [t] => (txt := []; t)
                  | l => (txt := []; HTML.TextList(List.rev l))
                (* end case *))
            | _ => raise Fail "HTML3 device is not done yet"
	  (* end case *))

  end

end (* local *)
