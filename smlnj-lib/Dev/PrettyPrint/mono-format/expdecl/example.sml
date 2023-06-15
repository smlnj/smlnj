(* example.sml
 *
 * COPYRIGHT (c) 2023 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This example demonstrates that one can use a single pretty-printer
 * specification to output to multiple devices (plain text, ansii
 * terminal, and html) from the same code.
 *
 * To test in the REPL, try commands like
 *
 *      ChoosePPDev.render (ChoosePPDev.Text, TextIO.stdOut, 20) Example.exp1;
 *      ChoosePPDev.render (ChoosePPDev.Term, TextIO.stdOut, 20) Example.exp1;
 *      ChoosePPDev.render (ChoosePPDev.HTML, TextIO.stdOut, 20) Example.exp3;
 *)

structure AST =
  struct

    datatype exp
      = Let of dcl list * exp list
      | Var of string
      | Num of int
      | Plus of exp * exp

    and dcl
      = Val of string * exp

  end

(* generic pretty-printer for the AST *)
structure PPAST : sig

    (* the styles that we use to pretty-print expressions *)
    val kwStyle : PrettyPrint.style      (* keywords *)
    val varStyle : PrettyPrint.style     (* variables *)
    val numStyle : PrettyPrint.style     (* numbers *)
    val opStyle : PrettyPrint.style      (* operators *)

    (* pretty print the expression using the given rendering function *)
    val pp : (PrettyPrint.format -> unit) -> AST.exp -> unit

  end = struct

    structure PP = PrettyPrint

    val kwStyle = Atom.atom "kw"
    val varStyle = Atom.atom "var"
    val numStyle = Atom.atom "num"
    val opStyle = Atom.atom "op"

    fun kw s = PP.style kwStyle (PP.text s)
    val letKW = kw "let"
    val valKW = kw "val"
    val inKW = kw "in"
    val endKW = kw "end"
    fun var x = PP.style varStyle (PP.text x)
    fun num n = PP.style numStyle (PP.text (Int.toString n))
    val plusOP = PP.style opStyle (PP.text "+")

    fun formatExp (AST.Var s) = var s
      | formatExp (AST.Num n) = num n
      | formatExp (AST.Plus(exp1, exp2)) = PP.pBlock [
            PP.hBlock [formatExp exp1, plusOP],
            PP.indent 2 (formatExp exp2)
          ]
      | formatExp (AST.Let (dcls, exps)) = let
          val body = formatExps exps
          in
            PP.tryFlat (PP.vBlock [
                PP.hBlock[letKW, fmtDcls dcls],
                PP.alt (
                  PP.hBlock [inKW, body, endKW],
                  PP.vBlock [inKW, PP.indent 2 body, endKW])
              ])
          end

    and formatExps (exps: AST.exp list) =
          PP.tryFlat (PP.sequenceWithMap {align=PP.V, sep=PP.text ";", fmt=formatExp} exps)

    and fmtDcl (AST.Val (name, exp)) = PP.pBlock [
            PP.hBlock [valKW, PP.text name, PP.text "="],
            PP.indent 4 (formatExp exp)
          ]

    and fmtDcls dcls = PP.vBlock (List.map fmtDcl dcls)

    fun pp render e = render(formatExp e)

  end

structure ChoosePPDev : sig

    datatype target = Text | Term | HTML

    (* `render (tgt, outS, lw) exp` renders the AST `exp` to the specified target
     * output using the line width `lw`.
     *)
    val render : target * TextIO.outstream * int -> AST.exp -> unit

  end = struct

    (* utility function to create a style map from a list of pretty-printer/device
     * style pairs.
     *)
    fun styleMap (stys : (PrettyPrint.style * 'a) list) : PrettyPrint.style -> 'a option = let
          val tbl = AtomTable.mkTable(8, Fail "styleMap")
          in
            List.app (AtomTable.insert tbl) stys;
            AtomTable.find tbl
          end

    (* the plain-text renderer *)
    fun renderText (outS, lw) fmt =
          TextIORenderer.render (fmt, outS, lw)

    (* the ANSI terminal renderer *)
    local
      structure R = RenderFn (ANSITermDev)
    in
    fun renderTerm (outS, lw) fmt =
          R.render {
              dev = ANSITermDev.openDev{dst = outS, wid = lw},
              styleMap = styleMap [
                  (PPAST.kwStyle, [ANSITerm.BF, ANSITerm.FG ANSITerm.Blue]),
                  (PPAST.varStyle, [ANSITerm.UL]),
                  (PPAST.numStyle, [ANSITerm.FG ANSITerm.Green]),
                  (PPAST.opStyle, [ANSITerm.BF])
                ],
              tokenMap = R.nullMap
            } fmt;
    end (* local structure R = ... *)

    (* the HTML renderer *)
    local
      structure H3D = HTML3Dev
      structure R = RenderFn (H3D)
      (* print an HTML text value by embedding it in a HTML document *)
      fun prHTML (outS, textElem) = let
            val doc = HTML.HTML{
                    version = NONE,
                    head = [],
                    body = HTML.BODY{
                        background = NONE, bgcolor = NONE, text = NONE,
                        link = NONE, vlink = NONE, alink = NONE,
                        content = HTML.TextBlock textElem
                      }
                  }
            in
              PrHTML.prHTML {
                putc = fn c => TextIO.output1(outS, c),
                puts = fn s => TextIO.output(outS, s)
              } doc
            end
    in
    fun renderHTML (outS, lw) fmt = let
          val dev = H3D.openDev{wid = lw, textWid=NONE}
          in
            R.render {
                dev = dev,
                styleMap = styleMap [
                    (PPAST.kwStyle, H3D.combineStyle(H3D.styleB, H3D.color "DarkBlue")),
                    (PPAST.varStyle, H3D.styleI),
                    (PPAST.numStyle, H3D.color "Maroon"),
                    (PPAST.opStyle, H3D.styleB)
                  ],
                tokenMap = R.nullMap
              } fmt;
            prHTML (outS, H3D.done dev)
          end
    end (* local structure R = ... *)

    datatype target = Text | Term | HTML

    (* `render (tgt, outS, lw) exp` renders the AST `exp` to the specified target
     * output using the line width `lw`.
     *)
    fun render (tgt, outS, lw) = (case tgt
           of Text => PPAST.pp (renderText (outS, lw))
            | Term => PPAST.pp (renderTerm (outS, lw))
            | HTML => PPAST.pp (renderHTML (outS, lw))
          (* end case *))

  end

(* some example expressions for testing *)
structure Example =
  struct

    local open AST in

    val exp1 = Num 42
    val exp2 = Var "foo"
    val exp3 = Plus (Plus(Num 1, Num 2), Num 3)
    val exp4 = Let ([Val ("x", Num 1), Val ("y", Num 2)], [Plus (Var "x", Num 3), Var "y"]);

    end (* local open *)

  end
