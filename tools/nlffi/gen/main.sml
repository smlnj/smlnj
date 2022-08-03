(*
 * main.sml - Driver routine ("main") for ml-ffigen.
 *
 *  (C) 2004  The Fellowship of SML/NJ
 *
 * author: Matthias Blume (blume@tti-c.org)
 *)
structure Main = struct
  local

    structure RE =
        RegExpFn (structure P = AwkSyntax
		  structure E = DfaEngine)

    fun tgt (n, sz, sh) =
	{ name  = n, sizes = sz, shift = sh }

    val default_target =
	tgt (DefaultName.name, DefaultSizes.sizes, DefaultEndian.shift)

    val target_table =
	[tgt ("sparc-unix", SizesSparc.sizes, EndianBig.shift),
	 tgt ("x86-unix", SizesX86.sizes, EndianLittle.shift),
	 tgt ("x86-win32", SizesX86.sizes, EndianLittle.shift),
	 tgt ("ppc-unix", SizesPPC.sizes, EndianLittle.shift)
	 (* needs to be extended ... *)
	 ]

    fun find_target tg =
	case List.find (fn x => tg = #name x) target_table of
	    SOME t => t
	  | NONE => raise Fail (concat ["unknown target: " ^ tg])

    fun main0 (arg0, args) = let
	fun substitute (tmpl, opts, s, t) = let
	    fun loop ([], a) = String.implode (rev a)
	      | loop (#"%" :: #"s" :: l, a) = loop (l, push (s, a))
	      | loop (#"%" :: #"t" :: l, a) = loop (l, push (t, a))
	      | loop (#"%" :: #"o" :: l, a) = loop (l, push (opts, a))
	      | loop (c :: l, a) = loop (l, c :: a)
	    and push (x, a) = List.revAppend (String.explode x, a)
	in
	    loop (String.explode tmpl, [])
	end

	val dir = ref "NLFFI-Generated"
	val cmf = ref "nlffi-generated.cm"
	val prefix = ref ""
	val gstem = ref ""
	val ems = ref []
	val libh = ref "Library.libh"
	val asu = ref false
	val wid = ref NONE
	val smlopts = ref []
	val noguid = ref true
	val target = ref default_target
	val wrq = ref NONE
	val namedargs = ref false
	val collect_enums = ref true
	val enumcons = ref false
	val cppopts = ref ""
	val regexp = ref NONE

	fun finish cfiles = let
	    fun mkidlsource cfile = let
		val ifile = OS.FileSys.tmpName ()
		val cpp_tmpl = getOpt (OS.Process.getEnv "FFIGEN_CPP",
				       "gcc -E -U__GNUC__ %o %s > %t")
		val cpp = substitute (cpp_tmpl, !cppopts, cfile, ifile)
	    in
		if OS.Process.system cpp <> OS.Process.success then
		    raise Fail ("C-preprocessor failed: " ^ cpp)
		else ();
		ifile
	    end

	    val match = let
		fun matchString scanFn s = let
		    val n = size s
		    fun getc i =
			if (i < n) then SOME (String.sub (s, i), i + 1)
			else NONE
		in
		    case scanFn getc 0 of
			NONE => false
		      | SOME (x, k) => k = n
		end
	    in
		case !regexp of
		    NONE => (fn _ => false)
		  | SOME re => matchString (RE.prefix re)
	    end
	in
	    Gen.gen { cfiles = cfiles,
		      match = match,
		      mkidlsource = mkidlsource,
		      dirname = !dir,
		      cmfile = !cmf,
		      prefix = !prefix,
		      gensym_stem = !gstem,
		      extramembers = !ems,
		      libraryhandle = !libh,
		      allSU = !asu,
		      smloptions = rev (!smlopts),
		      noguid = !noguid,
		      weightreq = !wrq,
		      wid = getOpt (!wid, 75),
		      namedargs = !namedargs,
		      collect_enums = !collect_enums,
		      enumcons = !enumcons,
		      target = !target };
	    OS.Process.success
	end

	fun iscppopt opt =
	    size opt > 2 andalso
	    String.sub (opt, 0) = #"-" andalso
	    Char.contains "IDU" (String.sub (opt, 1))

	fun addcppopt opt =
	    cppopts := (case !cppopts of
			    "" => opt
			  | opts => concat [opts, " ", opt])

	fun proc ("-allSU" :: l) =
	    (asu := true; proc l)
	  | proc (("-width" | "-w") :: i :: l) =
	    (wid := Int.fromString i; proc l)
	  | proc (("-smloption" | "-opt") :: s :: l) =
	    (smlopts := s :: !smlopts; proc l)
	  | proc ("-guids" :: l) =
	    (noguid := false; proc l)
	  | proc (("-target" | "-t") :: tg :: l) =
	    (target := find_target tg; proc l)
	  | proc (("-light" | "-l") :: l) =
	    (wrq := SOME false; proc l)
	  | proc (("-heavy" | "-h") :: l) =
	    (wrq := SOME true; proc l)
	  | proc (("-namedargs" | "-na") :: l) =
	    (namedargs := true; proc l)
	  | proc (("-libhandle" | "-lh") :: lh :: l) =
	    (libh := lh; proc l)
	  | proc (("-include" | "-add") :: es :: l) =
	    (ems := es :: !ems; proc l)
	  | proc (("-prefix" | "-p") :: p :: l) =
	    (prefix := p; proc l)
	  | proc (("-gensym" | "-g") :: g :: l) =
	    (gstem := g; proc l)
	  | proc (("-dir" | "-d") :: d :: l) =
	    (dir := d; proc l)
	  | proc (("-cmfile" | "-cm") :: f :: l) =
	    (cmf := f; proc l)
	  | proc ("-cppopt" :: opt :: l) =
	    (addcppopt opt; proc l)
	  | proc ("-nocollect" :: l) =
	    (collect_enums := false; proc l)
	  | proc (("-ec" | "-enum-constructors") :: l) =
	    (enumcons := true; proc l)
	  | proc ("-version" :: _) =
	    (TextIO.output (TextIO.stdOut, Gen.version ^ "\n");
	     OS.Process.exit OS.Process.success)
	  | proc (("-match" | "-m") :: re :: l) =
	    (regexp := SOME (RE.compileString re); proc l)
	  | proc ("--" :: cfiles) = finish cfiles
	  | proc (l0 as (opt :: l)) =
	    if iscppopt opt then (addcppopt opt; proc l) else finish l0
	  | proc cfiles = finish cfiles
    in
	proc args
    end
  in
    fun hist [] = ()
      | hist (h :: hs) =
	  (TextIO.output (TextIO.stdErr, concat ["\t", h, "\n"]);
	   hist hs)
    fun main args = main0 args
	handle exn => (TextIO.output (TextIO.stdErr, General.exnMessage exn);
		       TextIO.output (TextIO.stdErr, "\n");
		       hist (SMLofNJ.exnHistory exn);
		       OS.Process.failure)
  end
end
