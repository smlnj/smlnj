(* versiontool.sml
 *
 * COPYRIGHT (c) 2021 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 *   A CM tool for automatically generating file version.sml
 *   from a template, incorporating current version and release.
 *
 * Author: Matthias Blume (blume@tti-c.org)
 *)

structure VersionTool : sig end = struct

    (* trim leading and trailing whitespace from a string *)
    fun trimWS s = Substring.string (
	  Substring.dropl Char.isSpace (Substring.dropr Char.isSpace (Substring.full s)))

    local
      val defaultVers = ([0, 0], "")
    in
    fun getVersion file = let
	  val inS = TextIO.openIn file
	  (* convert a string of the form `"n.m.p"` to an int list `[n, m, p]`. *)
	  fun getVersId s = let
		val fl = String.tokens (fn c => Char.isSpace c orelse c = #".") s
		in
		  List.map (fn f => getOpt (Int.fromString f, 0)) fl
		end
	  in
	    case TextIO.inputLine inS
	     of SOME ln => (
		  TextIO.closeIn inS;
		  (* first we split on a possible suffix *)
		  case String.tokens (fn #"-" => true | _ => false) (trimWS ln)
		   of [] => defaultVers
		    | [v] => (getVersId v, "")
		    | [v, suffix] => (getVersId v, suffix)
		    | (v::suffix) => (getVersId v, String.concatWith "-" suffix)
		  (* end case *))
	      | NONE => defaultVers
	    (* end case *)
	  end
	    handle _ => defaultVers
    end (* local *)

    fun getReleaseDate file = let
	  val s = TextIO.openIn file
	  in
	    case TextIO.inputLine s
	     of SOME l => (
		  TextIO.closeIn s;
		  trimWS l)
	      | NONE => (TextIO.closeIn s; "")
	    (* end case *)
	  end
	    handle _ => ""

  (* generate the target file from the template *)
    fun gen { template, target, vfile, rdfile } = let
	  val (version, suffix) = getVersion vfile
	  val vstring = String.concatWith ", " (map Int.toString version)
	  val releaseDate = getReleaseDate rdfile
	  val ss = TextIO.openIn template
	  val ts = TextIO.openOut target
	  fun loop () = (case TextIO.input1 ss
		 of NONE => ()
		  | SOME #"%" => (case TextIO.input1 ss
		       of SOME #"V" => (
			    TextIO.output (ts, vstring);
			    loop ())
			| SOME #"F" => (
			    TextIO.output (ts, OS.Path.file target);
			    loop ())
			| SOME #"D" => (
			    TextIO.output (ts, releaseDate);
			    loop ())
			| SOME #"S" => (
			    TextIO.output (ts, suffix);
			    loop ())
			| SOME c => (TextIO.output1 (ts, c); loop ())
			| NONE => TextIO.output1 (ts, #"%")
		      (* end case *))
		  | SOME c => (TextIO.output1 (ts, c); loop ())
		(* end case *))
	  in
	    loop ();
	    TextIO.closeIn ss;
	    TextIO.closeOut ts
	  end

    val tool = "versiontool"
    val class = "version"

    val kw_target = "target"
    val kw_versionfile = "versionfile"
    val kw_releasedate = "releasedate"
    val keywords = [kw_target, kw_versionfile, kw_releasedate]

    fun versiontoolrule {
	  spec: Tools.spec,
	  native2pathmaker,
	  context: Tools.rulecontext,
	  defaultClassOf,
	  sysinfo
	} : Tools.partial_expansion = let
	  fun dogen (targetpp, versionfilepp, releasedatepp) () = let
		val templatep = Tools.srcpath (#mkpath spec ())
		val targetp = Tools.srcpath targetpp
		val target = Tools.nativeSpec targetp
		val template = Tools.nativeSpec templatep
		val vfile = Tools.nativePreSpec versionfilepp
		val rdfile = Tools.nativePreSpec releasedatepp
		fun newerThanTarget f = Tools.outdated tool ([target], f)
		val () = if List.exists newerThanTarget [template, vfile, rdfile]
		      then gen {
			  template = template, target = target,
			  vfile = vfile, rdfile = rdfile
			}
		      else ()
		val res = {
			smlfiles = [
			    (targetp, { share = Sharing.DONTCARE,
					setup = (NONE, NONE),
					noguid = false,
					locl = false,
					controllers = [] })
			  ],
			cmfiles = [],
			sources = [(templatep, { class = class, derived = #derived spec })]
		      }
		in
		  (res, [])
		end
	    fun complain l = raise Tools.ToolError { tool = tool, msg = concat l }
	    in
	      case #opts spec
	       of NONE => complain ["missing parameters"]
		| SOME to => let
	            val { matches, restoptions } =
			  Tools.parseOptions { tool = tool, keywords = keywords, options = to }
		    fun match kw = (case matches kw
			   of NONE => complain ["missing parameter \"", kw, "\""]
			    | SOME [Tools.STRING { mkpath, ... }] => mkpath ()
			    | _ => complain ["invalid parameter \"", kw, "\""]
			  (* end case *))
	            in
		      context (
			dogen (match kw_target, match kw_versionfile, match kw_releasedate))
	            end
	      (* end case *)
	    end

    val _ = Tools.registerClass (class, versiontoolrule)

  end
