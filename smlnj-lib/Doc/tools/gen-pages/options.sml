(* options.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Handle command-line options for the gen-page tool.  The options are
 *
 *	-h, --help
 *	-v, --verbose
 *	--release-date=<date>
 *	--version=<version>
 *	--base-url=<url>
 *	--basis-lib-url=<url>
 *	--index=<file>
 *)

structure Options : sig

    val process : string list -> unit

    val usage : OS.Process.status -> unit

  (* option flags *)
    val verbose : bool ref
    val releaseDate : string ref
    val version : string ref
    val baseURL : string option ref
    val basisLibURL : string ref
    val indexFile : string ref
    val imagesURL : string option ref

  end = struct

    structure G = GetOpt

    val verbose = ref false
    val releaseDate = ref Config.releaseDate
    val version = ref Config.version
    val baseURL : string option ref = ref NONE
    val basisLibURL = ref "https://standardml.org/Basis"
    val indexFile = ref "index.json"
    val imagesURL = ref(SOME "https://smlnj.org/images")

    val helpFlg = ref false

    fun setOpt (r, name) = G.ReqArg(fn s => r := s, name)

    val opts = [
	    { short = "h", long = ["help"],
	      desc = G.NoArg(fn () => helpFlg := true),
	      help = "Print this message"
	    },
	    { short = "v", long = ["verbose"],
	      desc = G.NoArg(fn () => verbose := true),
	      help = "Print progress messages"
	    },
	    { short = "", long = ["release-date"],
	      desc = setOpt (releaseDate, "<date>"),
	      help = "Specify the release date attribute"
	    },
	    { short = "", long = ["version"],
	      desc = setOpt (version, "<version>"),
	      help = "Specify the SML/NJ version"
	    },
	    { short = "", long = ["base-url"],
	      desc = G.ReqArg(fn s => baseURL := SOME s, "<url>"),
	      help = "Specify a base URL for the documentation"
	    },
	    { short = "", long = ["basis-lib-url"],
	      desc = setOpt (basisLibURL, "<url>"),
	      help = "Specify the URL for the SML Basis Library documentation"
	    },
	    { short = "", long = ["index"],
	      desc = setOpt (indexFile, "<file>"),
	      help = "Specify the name of the index JSON file"
	    }
	  ]

    fun usage sts = (
	  print (G.usageInfo{header="gen-pages [options]", options=opts});
	  OS.Process.exit sts)

    fun process args = let
	  val errFlg = ref false
	  val (_, excess) = G.getOpt {
		  argOrder = G.RequireOrder,
		  options = opts,
		  errFn = fn msg => (
		    TextIO.output(TextIO.stdErr, concat["gen-pages: ", msg, "\n"]);
		    errFlg := true)
		} args
	  in
	    if !helpFlg
	      then usage OS.Process.success
	    else if !errFlg orelse not(null excess)
	      then usage OS.Process.failure
	      else ();
	    case !baseURL
	     of NONE => imagesURL := NONE	(* use relative path to images *)
	      | _ => ()
	  end

  end
