(* test-parser.sml
 *
 * COPYRIGHT (c) 1996 AT&T REsearch.
 *
 * This is a simple test driver for the HTML parser.
 *)

structure Main : sig

    val doit : string -> HTML.html option
    val main : (string * string list) -> OS.Process.status

  end = struct

    structure Err =
      struct
	type context = {file : string option, line : int}

	structure F = Format

	fun prf ({file, line}, fmt, args) = (
	      case file
	       of NONE => TextIO.output (
		    TextIO.stdErr,
		    F.format "line %3d: " [F.INT line])
		| (SOME fname) => TextIO.output (
		    TextIO.stdErr,
		    F.format "%s[%d]: " [F.STR fname, F.INT line])
	      (* end case *);
	      TextIO.output(TextIO.stdErr, F.format fmt args);
	      TextIO.output1(TextIO.stdErr, #"\n"))

	fun badStartTag ctx tagName =
	      prf (ctx, "unrecognized start tag \"%s\"",[F.STR tagName])

	fun badEndTag ctx tagName =
	      prf (ctx, "unrecognized end tag \"%s\"",[F.STR tagName])

	fun badAttrVal ctx (attrName, attrVal) =
	      prf (ctx, "bad value \"%s\" for attribute \"%s\"",
		[F.STR attrVal, F.STR attrName])

	fun lexError ctx msg = prf (ctx, "%s", [F.STR msg])

	fun syntaxError ctx msg = prf (ctx, "%s", [F.STR msg])

	fun missingAttrVal ctx attrName =
	      prf (ctx, "missing value for \"%s\" attribute", [F.STR attrName])

	fun missingAttr ctx attrName =
	      prf (ctx, "missing \"%s\" attribute", [F.STR attrName])

	fun unknownAttr ctx attrName =
	      prf (ctx, "unknown attribute \"%s\"", [F.STR attrName])

	fun unquotedAttrVal ctx attrName =
	      prf (ctx, "attribute value for \"%s\" should be quoted",
		[F.STR attrName])

      end

    structure P = HTMLParserFn(Err);

    fun doit fname = SOME(P.parseFile fname) (* handle _ => NONE *)

    fun main (_, files) = (List.app (ignore o doit) files; OS.Process.success)

  end;
