(* parsercontrol.sml
 *
 * COPYRIGHT (c) 2015 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature PARSER_CONTROL =
  sig

    val primaryPrompt : string ref
    val secondaryPrompt : string ref

  (* turn on lazy keywords and lazy declaration processing *)
    val lazysml : bool ref		(* default false *)

  (* controls "overload" as keyword *)
    val overloadKW : bool ref

  (* controls backquote quotation *)
    val quotation : bool ref

  (* controls printing of internal Ast info *)
    val astInternals : bool ref			 

  (* set/clear Successor ML mode *)
    val setSuccML : bool -> unit

  end

structure ParserControl : sig

    include PARSER_CONTROL

  (* the following components are not part of the PARSER_CONTROL
   * signature, because we do not want it to be visibile in the REPL.
   *)

  (* controls Successor-ML features. *)
    val succML : bool ref

  (* raised to force a parser switch (e.g., from SML'97 to Succ ML) *)
    exception RESET_PARSER

  end = struct

    val priority = [10, 10, 3]
    val obscurity = 3
    val prefix = "parser"

    val registry = ControlRegistry.new { help = "parser settings" }

    val _ = BasicControl.nest (prefix, registry, priority)

    val string_cvt = ControlUtil.Cvt.string
    val flag_cvt = ControlUtil.Cvt.bool

    val nextpri = ref 0

    fun new (c, n, h, d) = let
	  val r = ref d
	  val p = !nextpri
	  val ctl = Controls.control {
		  name = n,
		  pri = [p],
		  obscurity = obscurity,
		  help = h,
		  ctl = r
		}
	  in
	    nextpri := p + 1;
	    ControlRegistry.register registry {
		ctl = Controls.stringControl c ctl,
		envName = SOME (ControlUtil.EnvName.toUpper "PARSER_" n)
	      };
	    r
	  end


    val primaryPrompt =
	  new (string_cvt, "primary-prompt", "primary prompt", "- ")

    val secondaryPrompt =
	  new (string_cvt, "secondary-prompt", "secondary prompt","= ")

    val lazysml =
	  new (flag_cvt, "lazy-keyword", "whether `lazy' is considered a keyword", false)

    val overloadKW =
	  new (flag_cvt, "overload", "whether (_)overload keyword is enabled", false)

    val quotation =
	  new (flag_cvt, "quotations", "whether (anti-)quotations are recognized", false)

    val astInternals =
	  new (flag_cvt, "astInternals", "printing of ast internal info", false)

    val succML =
	  new (flag_cvt, "succ-ml", "whether Successor-ML extensions are recognized", false)

    exception RESET_PARSER

  (* set/clear Successor ML mode *)
    fun setSuccML flg =
	  if (!succML <> flg)
	    then (succML := flg; raise RESET_PARSER)
	    else ()

  end
