(* options.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * TODO:
 *	--line-width option
 *	include view-specific options in usage message
 *)

structure Options : sig

  (* raised if parsing command-line args hits an error (e.g., missing option, syntax, ...).
   * The string is an error message.
   *)
    exception Usage of string

    type generator = {
	src : string,			(* name of source file from command line *)
	dir : string,			(* output directory for generated files *)
	stem : string,			(* stem of output files *)
	modules : AST.module list	(* modules from source file *)
      } -> unit

    datatype command
      = HELP
      | VERSION
      | CHECK
      | GENERATE of {
	    gen : generator,
	    genBuild : unit -> unit
	  }

  (* register the generators *)
    val registerGen : {
	    names : string list,	(* name(s) of generator *)
	    opts : unit GetOpt.opt_descr list,
					(* generator-specific options *)
	    desc : string,		(* description of generator *)
	    gen : generator,		(* the actual generator *)
	    genBuild : unit -> unit	(* build-file generator *)
	  } -> unit

  (* parse the command-line args *)
    val parseCmdLine : string list -> {
	    command : command,		(* the first command-line argument, which
					 * specifies the operation to perform
					 *)
            files : string list         (* source file *)
          }

  (* return a usage message. *)
    val usage : unit -> string

  (* kinds of picklers *)
    datatype pickler
      = TYPES	(* type definitions *)
      | MEMORY	(* pickle to/from memory *)
      | FILE	(* pickle to/from binary file *)
      | SEXP	(* pickle to/from text file using S-Expression syntax *)

  (* get option values *)
    val noOutput : unit -> bool			(* set by the `-n` option *)
    val lineWidth : unit -> int			(* set by `--line-width` *)
    val outputDir : unit -> string option	(* set by `-d` / `--output-directory` *)

  (* is a given component enabled (either default or by `--gen`)? This function
   * will always return false if the `-n` option was given.
   *)
    val isEnabled : pickler -> bool

  end = struct

    structure G = GetOpt
    structure P = OS.Path

    type generator = {
	src : string,			(* name of source file from command line *)
	dir : string,			(* output directory for generated files *)
	stem : string,			(* stem of output files *)
	modules : AST.module list	(* modules from source file *)
      } -> unit

    datatype command
      = HELP
      | VERSION
      | CHECK
      | GENERATE of {
	    gen : generator,
	    genBuild : unit -> unit
	  }

    exception Usage of string

  (* kinds of picklers *)
    datatype pickler
      = TYPES	(* type definitions *)
      | MEMORY	(* pickle to/from memory *)
      | FILE	(* pickle to/from binary file *)
      | SEXP	(* pickle to/from text file using S-Expression syntax *)

    val defaultPicklers = [TYPES, MEMORY, FILE]

  (* option flags that are set by getOpt *)
    val noOutputFlg = ref false
    val viewOpt : string option ref = ref NONE
    val picklersOpt = ref [TYPES, MEMORY, FILE]
    val lineWidOpt : int ref = ref 90
    val outputDirOpt : string option ref = ref NONE

  (* process the argument to the `--gen` flag, which is a list of pickler
   * names.  The word "none" is allowed (by itself) to specify no code
   * generation
   *)
    fun picklersFromString s = (case String.map Char.toLower s
	   of "none" => []
	    | s => let
		fun process ([], picklers) = picklers
		  | process ("types"::sr, picklers) = process(sr, TYPES::picklers)
		  | process ("memory"::sr, picklers) = process(sr, MEMORY::picklers)
		  | process ("file"::sr, picklers) = process(sr, FILE::picklers)
		  | process ("sexp"::sr, picklers) = process(sr, SEXP::picklers)
		  | process _ = raise Usage "unkonwn pickler kind"
		in
		  process (String.fields (fn #"," => true | _ => false) s, [])
		end
	  (* end case *))

  (* the list of options, which does not include the view-specific controls *)
    val optionList = [
	    { short = "n", long = [],
	      desc = G.NoArg(fn () => noOutputFlg := true),
	      help = "write list of generated files to stdout without generating output"
	    },
(* TODO:
--line-width
*)
	    { short = "d", long = ["output-directory"],
	      desc = G.ReqArg(fn s => outputDirOpt := SOME s, "<dir>"),
	      help = "specify output directory"
	    },
	    { short = "", long = ["gen"],
	      desc = G.ReqArg(fn s => picklersOpt := picklersFromString s, "<picklers>"),
	      help = "specify what components to generate"
	    }
          ]

    fun parse (cmd, _, []) = {
	    command = cmd,
            files = []
          }
      | parse (cmd, cmdOpts, args) = let
	  val (opts, files) = G.getOpt {
		  argOrder = G.RequireOrder,
		  options = cmdOpts @ optionList,
		  errFn = fn s => raise Usage s
		} args
	(* figure out filename pieces *)
	  val srcFiles = (case (cmd, files)
		 of (HELP, _) => []
		  | (VERSION, _) => []
		  | (_, []) => raise Usage "no input files specified"
		  | _ => files
		(* end case *))
	  in
	    if !noOutputFlg
	      then picklersOpt := [] (* disable all output formats *)
	      else ();
	    {command = cmd, files = srcFiles}
	  end

    type cmd_info = {
	    names : string list,	(* name(s) of generator *)
	    opts : unit GetOpt.opt_descr list,
					(* generator-specific options *)
	    desc : string,		(* description of generator *)
	    cmd : command		(* the command *)
	  }

    val commands : cmd_info list ref = ref [
	    { names = ["help"],
	      opts = [],
	      desc = "Display available options",
	      cmd = HELP
	    },
	    { names = ["version"],
	      opts = [],
	      desc = "Display asdlgen version",
	      cmd = VERSION
	    },
	    { names = ["check"],
	      opts = [],
	      desc = "Parse and typecheck input without generating output",
	      cmd = CHECK
	    }
	  ]

    fun registerGen {names, opts, desc, gen, genBuild} =
	  commands := !commands @ [{
	      names=names, opts=opts, desc=desc,
	      cmd=GENERATE{gen=gen,genBuild=genBuild}
	    }]

    fun parseCmdLine (command::rest) = let
	  fun isCmd ({names, ...} : cmd_info) =
		List.exists (fn name => (name = command)) names
	  in
	    case List.find isCmd (!commands)
	     of SOME{names, opts, cmd, ...} => parse (cmd, opts, rest)
	      | NONE => raise Usage "unknown command"
	    (* end case *)
	  end
      | parseCmdLine _ = raise Usage "no command specified"

    fun usage () = let
	  val cmds = let
		fun doCmd ({names, desc, ...} : cmd_info, (n, items)) = let
		      val names = String.concatWith " " names
		      in
			(Int.max(size names + 2, n), (names, desc)::items)
		      end
	      (* preprocess the commands *)
		val (n, items) = List.foldl doCmd (0, []) (!commands)
	      (* add padding *)
		val items = let
		      fun mkItem (names, desc) = String.concat[
			      "  ",
			      StringCvt.padRight #" " n names,
			      desc, "\n"
			    ]
		      in
			List.map mkItem items
		      end
		in
		  items
		end
          val hdr = String.concat(
                  "Usage: asdlgen command [options] file ...\n" ::
                  "Commands:\n" ::
		  cmds @
		  ["Options:"])
          in
            G.usageInfo {header = hdr, options = optionList}
          end

  (* get option values *)
    fun noOutput () = !noOutputFlg

    fun lineWidth () = !lineWidOpt

    fun outputDir () = !outputDirOpt

  (* is a given component enabled (either default or by `--gen`)? *)
    fun isEnabled pkl = List.exists (fn p => (p = pkl)) (! picklersOpt)

  end
