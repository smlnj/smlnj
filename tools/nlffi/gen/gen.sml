(*
 * gen.sml - Generating and pretty-printing ML code implementing a
 *           typed interface to a C program.
 *
 *  (C) 2004  The Fellowship of SML/NJ
 *
 * author: Matthias Blume (blume@tti-c.org)
 *)
local
    val program = "ml-nlffigen"
    val version = "0.9.1"
    val author = "Matthias Blume"
    val email = "blume@tti-c.org"
    structure S = Spec
in

structure Gen :> sig
    val gen : { cfiles: string list,
		match: string -> bool,
		mkidlsource: string -> string,
		dirname: string,
		cmfile: string,
		prefix: string,
		gensym_stem: string,
		extramembers: string list,
		libraryhandle: string,

		allSU: bool,
		smloptions: string list,
		noguid: bool,
		wid: int,
		weightreq: bool option,	(* true -> heavy, false -> light *)
		namedargs: bool,
		collect_enums: bool,
		enumcons: bool,
		target : { name  : string,
			   sizes : Sizes.sizes,
			   shift : int * int * word -> word } } -> unit
    val version : string
end = struct

    val version = version

    structure SS = StringSet
    structure SM = StringMap
    structure IM = IntRedBlackMap
    structure LIS = RedBlackSetFn (type ord_key = LargeInt.int
				   val compare = LargeInt.compare)


    structure P = PrettyPrint
    structure PP = P.PP
    val Tuple = P.TUPLE
    fun Record [] = P.Unit
      | Record l = P.RECORD l
    val Con = P.CON
    val Arrow = P.ARROW
    val Type = P.Type
    val Unit = P.Unit
    val ETuple = P.ETUPLE
    val EUnit = ETuple []
    fun ERecord [] = P.ETUPLE []
      | ERecord l = P.ERECORD l
    val EVar = P.EVAR
    val EApp = P.EAPP
    val EConstr = P.ECONSTR
    val ESeq = P.ESEQ
    fun EWord w = EVar ("0wx" ^ Word.toString w)
    fun EInt i = EVar (Int.toString i)
    fun ELInt i = EVar (LargeInt.toString i)
    fun EString s = EVar (concat ["\"", String.toString s, "\""])

    fun warn m = TextIO.output (TextIO.stdErr, "warning: " ^ m)
    fun err m = raise Fail (concat ("gen: " :: m))

    fun unimp what = raise Fail ("unimplemented type: " ^ what)
    fun unimp_arg what = raise Fail ("unimplemented argument type: " ^ what)
    fun unimp_res what = raise Fail ("unimplemented result type: " ^ what)

    val writeto = "write'to"

    val dontedit = "(* This file has been generated automatically. \
		   \DO NOT EDIT! *)"

    fun mkCredits archos =
	concat ["(* [by ", author, "'s ",
		program, " (version ", version, ") for ",
		archos, "] *)"]
    val commentsto = concat ["(* Send comments and suggestions to ",
			     email, ". Thanks! *)"]


    fun fptr_rtti_struct_id i = "FPtrRTTI_" ^ Int.toString i
    fun fptr_rtti_qid i = fptr_rtti_struct_id i ^ ".typ"
    fun fptr_mkcall_qid i = fptr_rtti_struct_id i ^ ".mkcall"

    fun SUE_Tstruct K t = concat [K, "T_", t]
    val STstruct = SUE_Tstruct "S"
    val UTstruct = SUE_Tstruct "U"

    fun SUE_tag K tag = Type (SUE_Tstruct K tag ^ ".tag")
		   
    fun fieldtype_id n = "t_f_" ^ n
    fun fieldrtti_id n = "typ_f_" ^ n
    fun field_id (n, p) = concat ["f_", n, p]

    fun arg_id s = "a_" ^ s
    fun enum_id n = "e_" ^ n

    val $? = SM.find

    val %? = IM.find

    fun thetag (t: S.tag) t' = t = t'

    fun gen args = let
	val { cfiles, match, mkidlsource, gensym_stem,
	      dirname,
	      cmfile, prefix, extramembers, libraryhandle,
	      allSU, smloptions, noguid,
	      wid,
	      weightreq,
	      namedargs = doargnames,
	      collect_enums, enumcons,
	      target = { name = archos, sizes, shift } } = args

	val St = SUE_tag "S"
	val Un = SUE_tag "U"
	fun En (tag, anon) =
	    if collect_enums andalso anon then SUE_tag "E" "'"
	    else SUE_tag "E" tag

	val hash_cft = Hash.mkFHasher ()
	val hash_mltype = Hash.mkTHasher ()

	val gensym_suffix = if gensym_stem = "" then "" else "_" ^ gensym_stem

	fun SUEstruct K t = concat [prefix, K, "_", t]
	val Sstruct = SUEstruct "S"
	val Ustruct = SUEstruct "U"
	val Estruct = SUEstruct "E"
	fun Tstruct n = concat [prefix, "T_", n]
	fun Gstruct n = concat [prefix, "G_", n]
	fun Fstruct n = concat [prefix, "F_", n]
	fun Estruct' (n, anon) =
	    Estruct (if anon andalso collect_enums then "'" else n)

	fun Styp t = STstruct t ^ ".typ"
	fun Utyp t = UTstruct t ^ ".typ"

	val (doheavy, dolight) =
	    case weightreq of
		NONE => (true, true)
	      | SOME true => (true, false)
	      | SOME false => (false, true)

	val credits = mkCredits archos

	fun getSpec (cfile, s) = let
	    val idlsource = mkidlsource cfile
	in
	    (let val astbundle = ParseToAst.fileToAst'
				     TextIO.stdErr
				     (sizes, State.INITIAL)
				     idlsource
		 val s' =
		     AstToSpec.build { bundle = astbundle,
				       sizes = sizes,
				       collect_enums = collect_enums,
				       cfiles = cfiles,
				       match = match,
				       allSU = allSU,
				       eshift = shift,
				       gensym_suffix = gensym_suffix }
	     in
		 S.join (s', s)
	     end handle e => (OS.FileSys.remove idlsource handle _ => ();
			      raise e))
	    before (OS.FileSys.remove idlsource handle _ => ())
	end

	val spec = foldl getSpec S.empty cfiles

	val { structs, unions, gvars, gfuns, gtys, enums } = spec

	val do_dir = let
	    val done = ref false
	    fun doit () =
		if !done then ()
		else (done := true;
		      if OS.FileSys.isDir dirname handle _ => false then ()
		      else OS.FileSys.mkDir dirname)
	in
	    doit
	end

	val files = ref extramembers	(* all files that should go
					 * into the .cm description *)
	val exports = ref []

	(* we don't want apostrophes in file names -> turn them into minuses *)
	fun noquotes x = String.translate(fn #"'" => "-" | c => String.str c) x

	fun smlfile x = let
	    val nqx = noquotes x
	    val file = OS.Path.joinBaseExt { base = nqx, ext = SOME "sml" }
	    val result = OS.Path.joinDirFile { dir = dirname, file = file }
	    val opts = if noguid then "noguid" :: smloptions else smloptions
	    val opt =
		case opts of
		    [] => ""
		  | h :: t => concat ("(" :: h :: foldr
						   (fn (x, l) => " " :: x :: l)
						   [")"] t)
	in
	    files := file ^ opt :: !files;
	    do_dir ();
	    result
	end

	fun descrfile file = let
	    val result = OS.Path.joinDirFile { dir = dirname, file = file }
	in
	    do_dir ();
	    result
	end

	val structs =
	    foldl (fn (s, m) => SM.insert (m, #tag s, s)) SM.empty structs

	val unions =
	    foldl (fn (u, m) => SM.insert (m, #tag u, u)) SM.empty unions

	val enums =
	    foldl (fn (e, m) => SM.insert (m, #tag e, e)) SM.empty enums

	val (structs, unions, enums) = let
	    val sdone = ref SS.empty
	    val udone = ref SS.empty
	    val edone = ref SS.empty
	    val smap = ref SM.empty
	    val umap = ref SM.empty
	    val emap = ref SM.empty
	    val tq = ref []
	    fun ty_sched t = tq := t :: !tq
	    fun fs_sched (S.OFIELD { spec = (_, t), ... }) = ty_sched t
	      | fs_sched _ = ()
	    fun f_sched { name, spec } = fs_sched spec

	    fun xenter (xdone, xall, xmap, xfields) t =
		if SS.member (!xdone, t) then ()
		else (xdone := SS.add (!xdone, t);
		      case $? (xall, t) of
			  SOME x => (xmap := SM.insert (!xmap, t, x);
				     app f_sched (xfields x))
			| NONE => ())

	    val senter = xenter (sdone, structs, smap, #fields)
	    val uenter = xenter (udone, unions, umap, #all)
	    val eenter = xenter (edone, enums, emap, fn _ => [])

	    fun sinclude (s: S.s) = if #exclude s then () else senter (#tag s)
	    fun uinclude (u: S.u) = if #exclude u then () else uenter (#tag u)
	    fun einclude (e: S.enum) =
		if #exclude e then () else eenter (#tag e)

	    fun gty { src, name, spec } = ty_sched spec
	    fun gvar { src, name, spec = (_, t) } = ty_sched t
	    fun gfun { src, name, spec, argnames } = ty_sched (S.FPTR spec)
	    fun loop [] = ()
	      | loop tl = let
		    fun ty (S.STRUCT t) = senter t
		      | ty (S.UNION t) = uenter t
		      | ty (S.ENUM (t, anon)) =
			if collect_enums andalso anon then eenter "'"
			else eenter t
		      | ty (S.PTR (_, S.STRUCT t)) = ()
		      | ty (S.PTR (_, S.UNION t)) = ()
		      | ty (S.PTR (_, t)) = ty t
		      | ty (S.FPTR { args, res }) =
			(app ty args; Option.app ty res)
		      | ty (S.ARR { t, ... }) = ty t
		      | ty (S.UNIMPLEMENTED _) = ()
		      | ty (S.SCHAR | S.UCHAR | S.SINT | S.UINT |
			    S.SSHORT | S.USHORT | S.SLONG | S.ULONG |
			    S.SLONGLONG | S.ULONGLONG |
			    S.FLOAT | S.DOUBLE | S.VOIDPTR) = ()
		    fun tloop [] = nextround ()
		      | tloop (t :: ts) = (ty t; tloop ts)
		in
		    tq := [];
		    tloop tl
		end
	    and nextround () = loop (!tq)
	in
	    SM.app sinclude structs;
	    SM.app uinclude unions;
	    SM.app einclude enums;
	    app gty gtys;
	    app gvar gvars;
	    app gfun gfuns;
	    nextround ();
	    (!smap, !umap, !emap)
	end

	fun stem S.SCHAR = "schar"
	  | stem S.UCHAR = "uchar"
	  | stem S.SINT = "sint"
	  | stem S.UINT = "uint"
	  | stem S.SSHORT = "sshort"
	  | stem S.USHORT = "ushort"
	  | stem S.SLONG = "slong"
	  | stem S.ULONG = "ulong"
	  | stem S.SLONGLONG = "slonglong"
	  | stem S.ULONGLONG = "ulonglong"
	  | stem S.FLOAT = "float"
	  | stem S.DOUBLE = "double"
	  | stem S.VOIDPTR = "voidptr"
	  | stem _ = raise Fail "bad stem"

	fun taginsert (t, ss) =
	    if SS.member (ss, t) then ss else SS.add (ss, t)

	(* We don't expect many different function pointer types or
	 * incomplete types in any given C interface, so using linear
	 * lists here is probably ok. *)
	val (fptr_types,
	     incomplete_structs, incomplete_unions, incomplete_enums) = let
	    fun ty ((S.SCHAR | S.UCHAR | S.SINT | S.UINT |
		     S.SSHORT | S.USHORT | S.SLONG | S.ULONG |
		     S.SLONGLONG | S.ULONGLONG | S.FLOAT | S.DOUBLE |
		     S.VOIDPTR), a) = a
	      | ty (S.STRUCT t, a as (f, s, u, e)) =
		(case $? (structs, t) of
		     SOME _ => a
		   | NONE => (f, taginsert (t, s), u, e))
	      | ty (S.UNION t, a as (f, s, u, e)) =
		(case $? (unions, t) of
		     SOME _ => a
		   | NONE => (f, s, taginsert (t, u), e))
	      | ty (S.ENUM (t, anon), a as (f, s, u, e)) =
		(if collect_enums andalso anon then a
		 else case $? (enums, t) of
			  SOME _ => a
			| NONE => (f, s, u, taginsert (t, e)))
	      | ty ((S.PTR (_, t) | S.ARR { t, ... }), a) = ty (t, a)
	      | ty (S.FPTR (cft as { args, res }), a) = let
		    val a' = foldl ty a args
		    val a'' = case res of NONE => a'
					| SOME t => ty (t, a')
		    val (f, s, u, e) = a''
		    val cfth = hash_cft cft
		    val i = IM.numItems f
		in
		    if IM.inDomain (f, cfth) then (f, s, u, e)
		    else (IM.insert (f, cfth, (cft, i)), s, u, e)
		end
	      | ty (S.UNIMPLEMENTED _, a) = a
	    fun fs (S.OFIELD { spec = (_, t), ... }, a) = ty (t, a)
	      | fs (_, a) = a
	    fun f ({ name, spec }, a) = fs (spec, a)
	    fun s ({ src, tag, size, anon, fields, exclude }, a) =
		foldl f a fields
	    fun u ({ src, tag, size, anon, all, exclude }, a) =
		foldl f a all
	    fun gty ({ src, name, spec }, a) = ty (spec, a)
	    fun gvar ({ src, name, spec = (_, t) }, a) = ty (t, a)
	    fun gfun ({ src, name, spec, argnames }, a) = ty (S.FPTR spec, a)
	in
	    foldl gfun
	          (foldl gvar
			 (foldl gty
				(SM.foldl
				     u (SM.foldl
					    s (IM.empty,
					       SS.empty, SS.empty, SS.empty)
					    structs)
				     unions)
				gtys)
			 gvars)
		  gfuns
	end

	fun s_inc t = SS.member (incomplete_structs, t)
	fun u_inc t = SS.member (incomplete_unions, t)

	fun rwro S.RW = Type "rw"
	  | rwro S.RO = Type "ro"

	fun dim_ty 0 = Type "dec"
	  | dim_ty n = Con ("dg" ^ Int.toString (n mod 10),
			    [dim_ty (n div 10)])

	val dim_ty =
	    fn n =>
	       if n < 0 then raise Fail "negative dimension"
	       else dim_ty n

	fun Suobj'rw p sut = Con ("su_obj" ^ p, [sut, Type "rw"])
	fun Suobj'ro sut = Con ("su_obj'", [sut, Type "ro"])

	fun wtn_fptr_p p { args, res } = let
	    fun topty (S.STRUCT t) = Suobj'ro (St t)
	      | topty (S.UNION t) = Suobj'ro (Un t)
	      | topty t = wtn_ty' t
	    val (res_t, extra_arg_t) =
		case res of
		    NONE => (Unit, [])
		  | SOME (S.STRUCT t) => let
			val ot = Suobj'rw "'" (St t)
		    in
			(ot, [ot])
		    end
		  | SOME (S.UNION t) => let
			val ot = Suobj'rw "'" (Un t)
		    in
			(ot, [ot])
		    end
		  | SOME t => (topty t, [])
	    val arg_tl = extra_arg_t @ map topty args
	    val dom_t = Tuple arg_tl
	    val fct_t = Arrow (dom_t, res_t)
	in
	    Con ("fptr" ^ p, [fct_t])
	end

	and wtn_ty_p p (t as (S.SCHAR | S.UCHAR | S.SINT | S.UINT |
			      S.SSHORT | S.USHORT | S.SLONG | S.ULONG |
			      S.SLONGLONG | S.ULONGLONG |
			      S.FLOAT | S.DOUBLE | S.VOIDPTR)) =
	    Type (stem t)
	  | wtn_ty_p p (S.STRUCT t) = Con ("su", [St t])
	  | wtn_ty_p p (S.UNION t) = Con ("su", [Un t])
	  | wtn_ty_p p (S.ENUM ta) = Con ("enum", [En ta])
	  | wtn_ty_p p (S.PTR (c, t)) =
	    Con ("ptr" ^ p, [Con ("obj", [wtn_ty t, rwro c])])
	  | wtn_ty_p p (S.ARR { t, d, ... }) =
	    Con ("arr", [wtn_ty t, dim_ty d])
	  | wtn_ty_p p (S.FPTR spec) = wtn_fptr_p p spec
	  | wtn_ty_p _ (S.UNIMPLEMENTED what) = unimp what

	and wtn_ty t = wtn_ty_p "" t

	and wtn_ty' t = wtn_ty_p "'" t

	fun topfunc_ty p ({ args, res }, argnames) = let
	    fun topty (S.SCHAR | S.SINT | S.SSHORT | S.SLONG) =
		  Type "MLRep.Signed.int"
	      | topty S.SLONGLONG = 
		  Type "MLRep.LongLongSigned.int"
	      | topty (S.UCHAR | S.UINT | S.USHORT | S.ULONG) =
		  Type "MLRep.Unsigned.word"
	      | topty S.ULONGLONG =
		  Type "MLRep.LongLongUnsigned.word"
	      | topty (S.FLOAT | S.DOUBLE) =
		  Type "MLRep.Real.real"
	      | topty (S.STRUCT t) = Con ("su_obj" ^ p, [St t, Type "'c"])
	      | topty (S.UNION t) = Con ("su_obj" ^ p, [Un t, Type "'c"])
	      | topty (S.ENUM _) = Type "MLRep.Signed.int"
	      | topty t = wtn_ty_p p t
	    val (res_t, extra_arg_t, extra_argname) =
		case res of
		    NONE => (Unit, [], [])
		  | SOME (S.STRUCT t) => let
			val ot = Suobj'rw p (St t)
		    in
			(ot, [ot], [writeto])
		    end
		  | SOME (S.UNION t) => let
			val ot = Suobj'rw p (Un t)
		    in
			(ot, [ot], [writeto])
		    end
		  | SOME t => (topty t, [], [])
	    val argtyl = map topty args
	    val aggreg_argty =
		case (doargnames, argnames) of
		    (true, SOME nl) =>
		    Record (ListPair.zip (map arg_id (extra_argname @ nl),
					  extra_arg_t @ argtyl))
		  | _ => Tuple (extra_arg_t @ argtyl)
	in
	    Arrow (aggreg_argty, res_t)
	end

	fun rtti_ty t = Con ("T.typ", [wtn_ty t])

	fun obj_ty p (t, c) = Con ("obj" ^ p, [wtn_ty t, c])

	fun cro S.RW = Type "'c"
	  | cro S.RO = Type "ro"

	fun dim_val n = let
	    fun build 0 = EVar "dec"
	      | build n = EApp (build (n div 10),
				EVar ("dg" ^ Int.toString (n mod 10)))
	in
	    EApp (build n, EVar "dim")
	end

	exception Incomplete

	local
	    fun simple v = EVar ("T." ^ v)
	in
	    fun rtti_val (t as (S.SCHAR | S.UCHAR | S.SINT | S.UINT |
				S.SSHORT | S.USHORT | S.SLONG | S.ULONG |
				S.SLONGLONG | S.ULONGLONG |
				S.FLOAT | S.DOUBLE | S.VOIDPTR)) =
		simple (stem t)
	      | rtti_val (S.STRUCT t) =
		if s_inc t then raise Incomplete else EVar (Styp t)
	      | rtti_val (S.UNION t) =
		if u_inc t then raise Incomplete else EVar (Utyp t)
	      | rtti_val (S.ENUM ta) =
		EConstr (EVar "T.enum",
			 Con ("T.typ", [Con ("enum", [En ta])]))
	      | rtti_val (S.FPTR cft) = let
		    val cfth = hash_cft cft
		in
		    case %? (fptr_types, cfth) of
			SOME (_, i) => EVar (fptr_rtti_qid i)
		      | NONE => raise Fail "fptr type missing"
		end
	      | rtti_val (S.PTR (S.RW, t)) =
		EApp (EVar "T.pointer", rtti_val t)
	      | rtti_val (S.PTR (S.RO, t)) =
		EApp (EVar "T.ro", EApp (EVar "T.pointer", rtti_val t))
	      | rtti_val (S.ARR { t, d, ... }) =
		EApp (EVar "T.arr", ETuple [rtti_val t, dim_val d])
	      | rtti_val (S.UNIMPLEMENTED what) = raise Incomplete
	end

	fun fptr_mkcall spec = let
	    val h = hash_cft spec
	in
	    case %? (fptr_types, h) of
		SOME (_, i) => fptr_mkcall_qid i
	      | NONE => raise Fail "missing fptr_type (mkcall)"
	end

	fun openPP (f, src) = let
	    val device = CPIFDev.openOut (f, wid)
	    val stream = PP.openStream device

	    fun nl () = PP.newline stream
	    fun str s = PP.string stream s
	    fun sp () = PP.space stream 1
	    fun nsp () = PP.nbSpace stream 1
	    fun Box a = PP.openBox stream (PP.Abs a)
	    fun HBox () = PP.openHBox stream
	    fun HVBox x = PP.openHVBox stream x
	    fun HOVBox a = PP.openHOVBox stream (PP.Abs a)
	    fun VBox a = PP.openVBox stream (PP.Abs a)
	    fun endBox () = PP.closeBox stream
	    fun ppty t = P.ppType stream t
	    fun ppExp e = P.ppExp stream e
	    fun ppFun x = P.ppFun stream x
	    fun line s = (nl (); str s)
	    fun pr_vdef (v, e) =
		(nl (); HOVBox 4; str "val"; nsp (); str v; nsp (); str "=";
		 sp (); ppExp e; endBox ())
	    fun pr_fdef (f, args, res) = (nl (); ppFun (f, args, res))

	    fun pr_decl (keyword, connector) (v, t) =
		(nl (); HOVBox 4; str keyword; nsp (); str v; nsp ();
		 str connector; sp (); ppty t; endBox ())
	    val pr_tdef = pr_decl ("type", "=")
	    val pr_vdecl = pr_decl ("val", ":")
	    fun closePP () = (PP.closeStream stream; CPIFDev.closeOut device)
	in
	    str dontedit;
	    case src of
		NONE => ()
	      | SOME s =>
		(nl (); str (concat ["(* [from code at ", s, "] *)"]));
	    line credits;
	    line commentsto;
	    nl ();
	    { stream = stream,
	      nl = nl, str = str, sp = sp, nsp = nsp, Box = Box, HVBox = HVBox,
	      HBox = HBox, HOVBox = HOVBox, VBox = VBox, endBox = endBox,
	      ppty = ppty, ppExp = ppExp, ppFun = ppFun, line = line,
	      pr_vdef = pr_vdef, pr_fdef = pr_fdef, pr_tdef = pr_tdef,
	      pr_vdecl = pr_vdecl,
	      closePP = closePP
	      }
	end

	val get_callop = let
	    val ncallops = ref 0
	    val callops = ref IM.empty
	    fun callop_sid i = "Callop_" ^ Int.toString i
	    fun callop_qid i = callop_sid i ^ ".callop"
	    fun get (ml_args_t, e_proto, ml_res_t) = let
		val e_proto_hash = hash_mltype e_proto
	    in
		case %? (!callops, e_proto_hash) of
		    SOME i => callop_qid i
		  | NONE => let
			val i = !ncallops
			val sn = callop_sid i
			val file = smlfile ("callop-" ^ Int.toString i)
			val { pr_vdef, closePP, str, nl, Box, endBox, ... } =
			    openPP (file, NONE)
		    in
			ncallops := i + 1;
			callops := IM.insert (!callops, e_proto_hash, i);
			str (concat ["structure ", sn, " = struct"]);
			Box 4;
			pr_vdef ("callop",
				 EConstr (EVar "RawMemInlineT.rawccall",
					  Arrow (Tuple [Type "Word32.word",
							ml_args_t,
							e_proto],
						 ml_res_t)));
			endBox ();
			nl (); str "end"; nl (); closePP ();
			callop_qid i
		    end
	    end
	in
	    get
	end

	fun pr_fptr_rtti ({ args, res }, i) = let

	    val structname = fptr_rtti_struct_id i
	    val file = smlfile ("fptr-rtti-" ^ Int.toString i)

	    val { closePP, str, Box, endBox, pr_fdef, pr_vdef, nl, ... } =
		openPP (file, NONE)

	    (* cproto encoding *)
	    fun List t = Con ("list", [t])
	    val Real = Type "real"
	    val Char = Type "char"
	    val Word8 = Type "Word8.word"
	    val Int31 = Type "Int31.int"
	    val Word31 = Type "Word31.word"
	    val Int32 = Type "Int32.int"
	    val Word32 = Type "Word32.word"
	    val String = Type "string"
	    val Exn = Type "exn"

	    (* see src/compiler/Semant/types/cproto.sml for these... *)
	    val E_double = Real
	    val E_float = List Real
	    val E_schar = Char
	    val E_uchar = Word8
	    val E_sint = Int31
	    val E_uint = Word31
	    val E_slong = Int32
	    val E_ulong = Word32
	    val E_sshort = List Char
	    val E_ushort = List Word8
	    val E_sllong = List Int32
	    val E_ullong = List Word32
	    val E_ptr = String
	    val E_nullstruct = Exn

	    fun encode S.DOUBLE = E_double
	      | encode S.FLOAT = E_float
	      | encode S.SCHAR = E_schar
	      | encode S.UCHAR = E_uchar
	      | encode S.SINT = E_sint
	      | encode S.UINT = E_uint
	      | encode S.SSHORT = E_sshort
	      | encode S.USHORT = E_ushort
	      | encode S.SLONG = E_slong
	      | encode S.ULONG = E_ulong
	      | encode S.SLONGLONG = E_sllong
	      | encode S.ULONGLONG = E_ullong
	      | encode (S.PTR _ | S.VOIDPTR | S.FPTR _) = E_ptr
	      | encode (S.UNIMPLEMENTED what) = unimp what
	      | encode (S.ARR _) = raise Fail "unexpected array"
	      | encode (S.ENUM _) = E_sint
	      | encode (S.STRUCT t) =
		  (case $? (structs, t) of
		       SOME s => encode_fields Unit (#fields s)
		     | NONE => err ["incomplete struct argument: struct ", t])
	      | encode (S.UNION t) =
		  (case $? (unions, t) of
		       SOME u => encode_fields E_sint (#all u)
		     | NONE => err ["incomplete union argument: union", t])

	    and encode_fields dummy fields = let
		fun f0 (S.ARR { t, d = 0, ... }, a) = a
		  | f0 (S.ARR { t, d = 1, ... }, a) = f0 (t, a)
		  | f0 (S.ARR { t, d, esz }, a) =
		      f0 (t, f0 (S.ARR { t = t, d = d - 1, esz = esz }, a))
		  | f0 (t, a) = encode t :: a
		fun f ({ spec = S.OFIELD { spec, ... }, name }, a) =
		      f0 (#2 spec, a)
		  | f (_, a) = a
		val fel = foldr f [] fields
	    in
		case fel of
		    [] => E_nullstruct
		  | fel => Tuple (dummy :: fel)
	    end

	    val e_arg = Tuple (Unit :: map encode args)
	    val e_res = case res of NONE => Unit | SOME t => encode t
	    val e_proto = Con ("list", [Arrow (e_arg, e_res)])

	    (* generating the call operation *)

	    (* low-level type used to communicate a value to the
	     * low-level call operation *)
	    fun mlty (t as (S.SCHAR | S.UCHAR | S.SINT | S.UINT |
			    S.SSHORT | S.USHORT | S.SLONG | S.ULONG |
			    S.SLONGLONG | S.ULONGLONG |
			    S.FLOAT | S.DOUBLE)) =
		  Type ("CMemory.cc_" ^ stem t)
	      | mlty (S.VOIDPTR | S.PTR _ | S.FPTR _ | S.STRUCT _ | S.UNION _) =
		  Type "CMemory.cc_addr"
	      | mlty (S.ENUM _) = Type "CMemory.cc_sint"
	      | mlty (S.UNIMPLEMENTED what) = unimp what
	      | mlty (S.ARR _) = raise Fail "unexpected type"

	    fun wrap (e, n) =
		EApp (EVar ("CMemory.wrap_" ^ n),
		      EApp (EVar ("Cvt.ml_" ^ n), e))

	    fun vwrap e = EApp (EVar "CMemory.wrap_addr",
				EApp (EVar "reveal", e))
	    fun fwrap e = EApp (EVar "CMemory.wrap_addr",
				EApp (EVar "freveal", e))
	    fun pwrap e = EApp (EVar "CMemory.wrap_addr",
				EApp (EVar "reveal",
				      EApp (EVar "Ptr.inject'", e)))

	    fun suwrap e = pwrap (EApp (EVar "Ptr.|&!", e))

	    fun ewrap e = EApp (EVar "CMemory.wrap_sint",
				EApp (EVar "Cvt.c2i_enum", e))

	    (* this code is for passing structures in pieces
	     * (member-by-member); we don't use this and rather
	     * provide a pointer to the beginning of the struct *)

	    fun arglist ([], _) = ([], [])
	      | arglist (h :: tl, i) = let
		    val p = EVar ("x" ^ Int.toString i)
		    val (ta, ea) = arglist (tl, i + 1)
		    fun sel e = (mlty h :: ta, e :: ea)
		in
		    case h of
			(S.STRUCT _ | S.UNION _) => sel (suwrap p)
		      | (S.ENUM _) => sel (ewrap p)
		      | (S.SCHAR | S.UCHAR | S.SINT | S.UINT |
			 S.SSHORT | S.USHORT | S.SLONG | S.ULONG |
			 S.SLONGLONG | S.ULONGLONG |
			 S.FLOAT | S.DOUBLE) => sel (wrap (p, stem h))
		      | S.VOIDPTR => sel (vwrap p)
		      | S.PTR _ => sel (pwrap p)
		      | S.FPTR _ => sel (fwrap p)
		      | S.UNIMPLEMENTED what => unimp_arg what
		      | S.ARR _ => raise Fail "unexpected array argument"
		end

	    val (ml_res_t,
		 extra_arg_v, extra_arg_e, extra_ml_arg_t,
		 res_wrap) =
		case res of
		    NONE => (Unit, [], [], [], fn r => r)
		  | SOME (S.STRUCT _ | S.UNION _) =>
		    (Unit,
		     [EVar "x0"],
		     [suwrap (EVar "x0")],
		     [Type "CMemory.cc_addr"],
		     fn r => ESeq (r, EVar "x0"))
		  | SOME t => let
			fun unwrap n r =
			    EApp (EVar ("Cvt.c_" ^ n),
				  EApp (EVar ("CMemory.unwrap_" ^ n), r))
			fun punwrap cast r =
			    EApp (EVar cast,
				  EApp (EVar "CMemory.unwrap_addr", r))
			fun eunwrap r =
			    EApp (EVar "Cvt.i2c_enum",
				  EApp (EVar "CMemory.unwrap_sint", r))
			val res_wrap =
			    case t of
				(S.SCHAR | S.UCHAR | S.SINT | S.UINT |
				 S.SSHORT | S.USHORT | S.SLONG | S.ULONG |
				 S.SLONGLONG | S.ULONGLONG |
				 S.FLOAT | S.DOUBLE) => unwrap (stem t)
			      | S.VOIDPTR => punwrap "vcast"
			      | S.FPTR _ => punwrap "fcast"
			      | S.PTR _ => punwrap "pcast"
			      | S.ENUM _ => eunwrap
			      | S.UNIMPLEMENTED what => unimp_res what
			      | (S.STRUCT _ | S.UNION _ | S.ARR _) =>
				raise Fail "unexpected result type"
		    in
			(mlty t, [], [], [], res_wrap)
		    end

	    val (ml_args_tl, args_el) = arglist (args, 1)

	    val ml_args_t = Tuple (extra_ml_arg_t @ ml_args_tl)

	    val arg_vl =
		    rev (#1 (foldl (fn (_, (a, i)) =>
				       (EVar ("x" ^ Int.toString i) :: a,
					i + 1)) ([], 1)
				   args))

	    val arg_e = ETuple (extra_arg_e @ args_el)
	    val callop_n = get_callop (ml_args_t, e_proto, ml_res_t)
	in
	    str "local open C.Dim C_Int in";
	    nl (); str (concat ["structure ", structname, " = struct"]);
	    Box 4;
	    pr_fdef ("mkcall",
		     [EVar "a", ETuple (extra_arg_v @ arg_vl)],
		     res_wrap (EApp (EVar callop_n,
				     ETuple [EVar "a", arg_e,
					     EVar "nil"])));
	    pr_vdef ("typ",
		     EConstr (EApp (EVar "mk_fptr_typ",
				    EVar "mkcall"),
			      rtti_ty (S.FPTR { args = args,
						res = res })));
	    endBox ();
	    nl (); str "end"; nl (); str "end"; nl (); closePP ()
	end

	datatype sue_szinfo =
	    T_INC			(* generate no RTTI *)
	  | T_SU of word		(* generate struct/union RTTI *)
	  | T_E				(* generate enum RTTI *)

	fun pr_sue_t_structure (src, tag, anon, tinfo, k, K) = let
	    val file = smlfile (concat [k, "t-", tag])
	    val { str, closePP, nl, Box, endBox, VBox, pr_tdef,
		  pr_vdef, ... } =
		openPP (file, src)
	    fun build [] = Type k
	      | build (h :: tl) = Con ("t_" ^ String.str h, [build tl])
	    val (utildef, tag_t) =
		if anon then
		    ("structure X :> sig type t end \
		     \= struct type t = unit end",
		     Type "X.t")
		else
		    ("open Tag",
		     build (rev (String.explode tag)))
	in
	    str "local";
	    Box 4;
	    nl (); str (concat ["structure ", SUEstruct K tag, " = struct"]);
	    Box 4;
	    nl (); str "local";
	    VBox 4;
	    nl (); str utildef;
	    endBox ();
	    nl (); str "in";
	    VBox 4;
	    pr_tdef ("tag", tag_t);
	    endBox ();
	    nl (); str "end";
	    case tinfo of
		T_INC => ()
	      | T_SU size =>
		(pr_vdef ("size",
			  EConstr (EApp (EVar "C_Int.mk_su_size", EWord size),
				   Con ("C.S.size",
					[Con ("C.su", [Type "tag"])])));
		 pr_vdef ("typ",
			  EApp (EVar "C_Int.mk_su_typ", EVar "size")))
	      | T_E => ();
	    endBox ();
	    nl (); str "end";
	    endBox ();
	    nl (); str "in";
	    Box 4;
	    nl (); str (concat ["structure ", SUE_Tstruct K tag,
				" = ", SUEstruct K tag]);
	    endBox ();
	    nl (); str "end"; nl ();
	    closePP ()
	end

	fun pr_st_structure { src, tag, anon, size, fields, exclude } =
	    pr_sue_t_structure (SOME src, tag, anon, T_SU size, "s", "S")
	fun pr_ut_structure { src, tag, anon, size, all, exclude } =
	    pr_sue_t_structure (SOME src, tag, anon, T_SU size, "u", "U")
	fun pr_et_structure { src, tag, anon, descr, spec, exclude } =
	    pr_sue_t_structure (SOME src, tag, anon, T_E, "e", "E")

	fun pr_sue_it_structure (tag, k, K) =
	    (pr_sue_t_structure (NONE, tag, false, T_INC, k, K);
	     exports := ("structure " ^ SUE_Tstruct K tag) :: !exports)

	fun pr_i_st_structure tag = pr_sue_it_structure (tag, "s", "S")
	fun pr_i_ut_structure tag = pr_sue_it_structure (tag, "u", "U")
	fun pr_i_et_structure tag = pr_sue_it_structure (tag, "e", "E")

	fun pr_su_structure (src, tag, fields, k, K) = let

	    val file = smlfile (concat [k, "-", tag])
	    val { closePP, Box, endBox, str, nl, line,
		  pr_tdef, pr_vdef, pr_fdef, ... } =
		openPP (file, SOME src)

	    fun rwro S.RW = "rw"
	      | rwro S.RO = "ro"

	    fun pr_field_typ { name, spec = S.OFIELD { spec = (c, t),
						       synthetic = false,
						       offset } } =
		pr_tdef (fieldtype_id name, wtn_ty t)
	      | pr_field_typ _ = ()

	    fun pr_field_rtti { name, spec = S.OFIELD { spec = (c, t),
							synthetic = false,
							offset } } =
		pr_vdef (fieldrtti_id name,
			 EConstr (rtti_val t,
				  Con ("T.typ", [Type (fieldtype_id name)])))
	      | pr_field_rtti _ = ()

	    fun arg_x p = EConstr (EVar "x",
				   Con ("su_obj" ^ p,
					[Type "tag", Type "'c"]))
		

	    fun pr_bf_acc (name, p, sign, { offset, constness, bits, shift }) =
		let val maker =
			concat ["mk_", rwro constness, "_", sign, "bf", p]
		in
		    pr_fdef (field_id (name, p),
			     [arg_x p],
			     EApp (EApp (EVar maker,
					 ETuple [EInt offset,
						 EWord bits,
						 EWord shift]),
				   EVar "x"))
		end

	    fun pr_field_acc' { name, spec = S.OFIELD x } =
		let val { synthetic, spec = (c, t), offset, ... } = x
		in
		    if synthetic then ()
		    else pr_fdef (field_id (name, "'"),
				  [arg_x "'"],
				  EConstr (EApp (EVar "mk_field'",
						 ETuple [EInt offset,
							 EVar "x"]),
					   Con ("obj'",
						[Type (fieldtype_id name),
						 cro c])))
		end
	      | pr_field_acc' { name, spec = S.SBF bf } =
		pr_bf_acc (name, "'", "s", bf)
	      | pr_field_acc' { name, spec = S.UBF bf } =
		pr_bf_acc (name, "'", "u", bf)

	    fun pr_field_acc { name, spec = S.OFIELD { offset,
						       spec = (c, t),
						       synthetic } } =
		if synthetic then ()
		else let
			val maker = concat ["mk_", rwro c, "_field"]
			val rttival = EVar (fieldrtti_id name)
		    in
			pr_fdef (field_id (name, ""),
				 [arg_x ""],
				 EApp (EVar maker,
				       ETuple [rttival,
					       EInt offset,
					       EVar "x"]))
		    end
	      | pr_field_acc { name, spec = S.SBF bf } =
		pr_bf_acc (name, "", "s", bf)
	      | pr_field_acc { name, spec = S.UBF bf } =
		pr_bf_acc (name, "", "u", bf)

	    val sustruct = "structure " ^ SUEstruct K tag

	    fun pr_one_field f = let
		val _ = pr_field_typ f
		val inc = (pr_field_rtti f; false) handle Incomplete => true
	    in
		if dolight orelse inc then pr_field_acc' f else ();
		if doheavy andalso not inc then pr_field_acc f else ()
	    end
	in
	    str "local open C.Dim C_Int in";
	    nl (); str (sustruct ^ " = struct");
	    Box 4;
	    nl (); str ("open " ^ SUE_Tstruct K tag);
	    app pr_one_field fields;
	    endBox ();
	    nl (); str "end";
	    nl (); str "end";
	    nl (); closePP ();
	    exports := sustruct :: (!exports)
	end

	fun pr_s_structure { src, tag, anon, size, fields, exclude } =
	    pr_su_structure (src, tag, fields, "s", "S")
	fun pr_u_structure { src, tag, anon, size, all, exclude } =
	    pr_su_structure (src, tag, all, "u", "U")

	fun pr_e_structure { src, tag, anon, descr, spec, exclude } = let
	    val file = smlfile ("e-" ^ tag)
	    val { closePP, str, Box, endBox, nl, line, sp,
		  pr_fdef, pr_vdef, pr_tdef, ... } =
		openPP (file, SOME src)
	    val estruct = "structure " ^ Estruct' (tag, anon)
	    fun no_duplicate_values () = let
		fun loop ([], _) = true
		  | loop ({ name, spec } :: l, s) =
		    if LIS.member (s, spec) then
			(warn (concat ["enum ", descr,
				       " has duplicate values;\
				       \ using sint,\
				       \ not generating constructors\n"]);
			 false)
		    else loop (l, LIS.add (s, spec))
	    in
		loop (spec, LIS.empty)
	    end
	    val dodt = enumcons andalso no_duplicate_values ()

	    fun dt_mlrep () = let
		fun pcl () = let
		    fun loop (_, []) = ()
		      | loop (c, { name, spec } :: l) =
			(str (c ^ enum_id name); nextround l)
		    and nextround [] = ()
		      | nextround l = (sp (); loop ("| ", l))
		in
		    Box 2; nl ();
		    loop ("  ", spec);
		    endBox ()
		end
		fun pfl (fname, arg, res, fini) = let
		    fun loop (_, []) = ()
		      | loop (pfx, v :: l) =
			(line (concat [pfx, " ", arg v, " => ", res v]);
			 loop ("  |", l))
		in
		    line (concat ["fun ", fname, " x ="]);
		    Box 4;
		    line ("case x of");
		    loop ("   ", spec);
		    fini ();
		    endBox ()
		end
		fun cstr { name, spec } = enum_id name
		fun vstr { name, spec } =
		    LargeInt.toString spec ^ " : MLRep.Signed.int"
	    in
		line "datatype mlrep =";
		pcl ();
		pfl ("m2i", cstr, vstr, fn () => ());
		pfl ("i2m", vstr, cstr,
		     fn () => line "  | _ => raise General.Domain")
	    end
	    fun int_mlrep () = let
		fun v { name, spec } =
		    pr_vdef (enum_id name, EConstr (ELInt spec, Type "mlrep"))
		val mlx = EConstr (EVar "x", Type "mlrep")
		val ix = EConstr (EVar "x", Type "MLRep.Signed.int")
	    in
		pr_tdef ("mlrep", Type "MLRep.Signed.int");
		app v spec;
		pr_fdef ("m2i", [mlx], ix);
		pr_fdef ("i2m", [ix], mlx)
	    end
	    fun getset p = let
		fun constr c = Con ("enum_obj" ^ p, [Type "tag", Type c])
	    in
		pr_fdef ("get" ^ p,
			 [EConstr (EVar "x", constr "'c")],
			 EApp (EVar "i2m",
			       EApp (EVar ("Get.enum" ^ p), EVar "x")));
		pr_fdef ("set" ^ p,
			 [ETuple [EConstr (EVar "x", constr "rw"), EVar "v"]],
			 EApp (EVar ("Set.enum" ^ p),
			       ETuple [EVar "x", EApp (EVar "m2i", EVar "v")]))
	    end

	in
	    str "local open C in";
	    line (estruct ^ " = struct");
	    Box 4;
	    line ("open " ^ SUE_Tstruct "E" tag);
	    if dodt then dt_mlrep () else int_mlrep ();
	    pr_fdef ("c", [EVar "x"],
		     EConstr (EApp (EVar "Cvt.i2c_enum",
				    EApp (EVar "m2i", EVar "x")),
			      Con ("enum", [Type "tag"])));
	    pr_fdef ("ml", [EConstr (EVar "x", Con ("enum", [Type "tag"]))],
		     EApp (EVar "i2m",
			   EApp (EVar "Cvt.c2i_enum", EVar "x")));
	    if dolight then getset "'" else ();
	    if doheavy then getset "" else ();
	    endBox ();
	    line "end"; line "end (* local *)";
	    nl ();
	    closePP ();
	    exports := estruct :: !exports
	end

	fun pr_t_structure { src, name, spec } = let
	    val rttiv_opt = SOME (rtti_val spec) handle Incomplete => NONE
	    val file = smlfile ("t-" ^ name)
	    val { closePP, Box, endBox, str, nl, pr_tdef,
		  pr_vdef, ... } =
		openPP (file, SOME src)
	    val tstruct = "structure " ^ Tstruct name
	in
	    str "local open C.Dim C in";
	    nl (); str (tstruct ^ " = struct");
	    Box 4;
	    pr_tdef ("t", wtn_ty spec);
	    Option.app (fn rttiv =>
			   pr_vdef ("typ",
				    EConstr (rttiv,
					     Con ("T.typ", [Type "t"]))))
		       rttiv_opt;
	    endBox ();
	    nl (); str "end";
	    nl (); str "end";
	    nl ();
	    closePP ();
	    exports := tstruct :: !exports
	end

	fun pr_gvar { src, name, spec = (c, t) } = let
	    val file = smlfile ("g-" ^ name)
	    val { closePP, str, nl, Box, VBox, endBox,
		  pr_fdef, pr_vdef, pr_tdef, ... } =
		openPP (file, SOME src)
	    fun doit () = let
		val rwo = Type (case c of S.RW => "rw" | S.RO => "ro")
		val _ = pr_tdef ("t", wtn_ty t)
		val inc = (pr_vdef ("typ",
				    EConstr (rtti_val t,
					     Con ("T.typ", [Type "t"])));
			   false)
			  handle Incomplete => true
		val obj' =
		    EConstr (EApp (EVar "mk_obj'", EApp (EVar "h", EUnit)),
			     Con ("obj'", [Type "t", rwo]))
		val dolight = dolight orelse inc
	    in
		if dolight then pr_fdef ("obj'", [EUnit], obj') else ();
		if doheavy andalso not inc then
		    pr_fdef ("obj", [EUnit],
			     EApp (EApp (EVar "Heavy.obj", EVar "typ"),
				   if dolight then
				       EApp (EVar "obj'", EUnit)
				   else obj'))
		else ()
	    end

	    val gstruct = "structure " ^ Gstruct name
	in
	    str (gstruct ^ " = struct");
	    Box 4;
	    nl (); str "local";
	    VBox 4;
	    nl (); str "open C.Dim C_Int";
	    pr_vdef ("h", EApp (EVar libraryhandle, EString name));
	    endBox ();
	    nl (); str "in";
	    VBox 4;
	    doit ();
	    endBox ();
	    nl (); str "end";
	    endBox ();
	    nl (); str "end"; nl ();
	    closePP ();
	    exports := gstruct :: !exports
	end

	fun pr_gfun x = let
	    val { src, name, spec = spec as { args, res }, argnames } = x

	    val file = smlfile ("f-" ^ name)
	    val { closePP, str, nl, pr_fdef, Box, endBox,
		  pr_vdef, pr_vdecl, ... } =
		openPP (file, SOME src)
	    fun mk_do_f is_light = let
		val ml_vars =
		    rev (#1 (foldl (fn (_, (l, i)) =>
				       (EVar ("x" ^ Int.toString i) :: l,
					i + 1))
				   ([], 1)
				   args))
		fun app0 (what, e) =
		    if is_light then e else EApp (EVar what, e)
		fun light (what, e) = app0 ("Light." ^ what, e)
		fun heavy (what, t, e) =
		    if is_light then e
		    else EApp (EApp (EVar ("Heavy." ^ what), rtti_val t), e)
			 
		fun oneArg (e, t as (S.SCHAR | S.UCHAR | S.SINT | S.UINT |
				     S.SSHORT | S.USHORT | S.SLONG | S.ULONG |
				     S.SLONGLONG | S.ULONGLONG |
				     S.FLOAT | S.DOUBLE)) =
		      EApp (EVar ("Cvt.c_" ^ stem t), e)
		  | oneArg (e, (S.STRUCT _ | S.UNION _)) =
		      EApp (EVar "ro'", light ("obj", e))
		  | oneArg (e, S.ENUM ta) = EApp (EVar "Cvt.i2c_enum", e)
		  | oneArg (e, S.PTR _) = light ("ptr", e)
		  | oneArg (e, S.FPTR _) = light ("fptr", e)
		  | oneArg (e, S.VOIDPTR) = e
		  | oneArg (e, S.UNIMPLEMENTED what) = unimp_arg what
		  | oneArg (e, S.ARR _) = raise Fail "array argument type"
		val c_exps = ListPair.map oneArg (ml_vars, args)
		val (ml_vars, c_exps, extra_argname) =
		    case res of
			SOME (S.STRUCT _ | S.UNION _) =>
			(EVar "x0" :: ml_vars,
			 light ("obj", EVar "x0") :: c_exps,
			 [writeto])
		      | _ => (ml_vars, c_exps, [])
		val call = EApp (EVar "call",
				 ETuple [EApp (EVar "fptr", EUnit),
					 ETuple c_exps])
		val ml_res =
		    case res of
			SOME (t as (S.SCHAR | S.UCHAR | S.SINT | S.UINT |
				    S.SSHORT | S.USHORT | S.SLONG | S.ULONG |
				    S.SLONGLONG | S.ULONGLONG |
				    S.FLOAT | S.DOUBLE)) =>
			  EApp (EVar ("Cvt.ml_" ^ stem t), call)
		      | SOME (t as (S.STRUCT _ | S.UNION _)) =>
			  heavy ("obj", t, call)
		      | SOME (S.ENUM ta) => EApp (EVar "Cvt.c2i_enum", call)
		      | SOME (t as S.PTR _) => heavy ("ptr", t, call)
		      | SOME (t as S.FPTR _) => heavy ("fptr", t, call)
		      | SOME (S.ARR _) => raise Fail "array result type"
		      | SOME (S.UNIMPLEMENTED what) => unimp_res what
		      | (NONE | SOME S.VOIDPTR) => call
		val argspat =
		    case (doargnames, argnames) of
			(true, SOME nl) =>
			ERecord (ListPair.zip (map arg_id (extra_argname @ nl),
					       ml_vars))
		      | _ => ETuple ml_vars
	    in
		fn () =>
		   pr_fdef (if is_light then "f'" else "f", [argspat], ml_res)
	    end
	    fun do_fsig is_light = let
		val p = if is_light then "'" else ""
	    in
		pr_vdecl ("f" ^ p, topfunc_ty p (spec, argnames))
	    end
	    val fstruct = "structure " ^ Fstruct name
	    val (do_f_heavy, inc) =
		(if doheavy then mk_do_f false else (fn () => ()), false)
		handle Incomplete => (fn () => (), true)
	in
	    str "local";
	    Box 4;
	    nl (); str "open C.Dim C_Int";
	    pr_vdef ("h", EApp (EVar libraryhandle, EString name));
	    endBox ();
	    nl (); str "in";
	    nl (); str (fstruct ^ " : sig");
	    Box 4;
	    pr_vdecl ("typ", rtti_ty (S.FPTR spec));
	    pr_vdecl ("fptr", Arrow (Unit, wtn_ty (S.FPTR spec)));
	    if doheavy andalso not inc then do_fsig false else ();
	    if dolight orelse inc then do_fsig true else ();
	    endBox ();
	    nl (); str "end = struct";
	    Box 4;
	    pr_vdef ("typ", rtti_val (S.FPTR spec));
	    pr_fdef ("fptr",
		     [EUnit],
		     EApp (EVar "mk_fptr",
			   ETuple [EVar (fptr_mkcall spec),
				   EApp (EVar "h", EUnit)]));
	    do_f_heavy ();
	    if dolight orelse inc then mk_do_f true () else ();
	    endBox ();
	    nl (); str "end"; nl (); str "end"; nl ();
	    closePP ();
	    exports := fstruct :: !exports
	end

	fun do_cmfile () = let
	    val file = descrfile cmfile
	    val { closePP, line, str, nl, VBox, endBox, ... } =
		openPP (file, NONE)
	in
	    str "(primitive c-int)";
	    line "library";
	    VBox 4;
	    app line (!exports);
	    endBox ();
	    nl (); str "is";
	    VBox 4;
	    app line ["$/basis.cm",
		      "$c/internals/c-int.cm",
		      "$smlnj/init/init.cmi : cm"];
	    app line (!files);
	    endBox ();
	    nl ();
	    closePP ()
	end
    in
	IM.app pr_fptr_rtti fptr_types;
	SM.app pr_st_structure structs;
	SM.app pr_ut_structure unions;
	SM.app pr_et_structure enums;
	SS.app pr_i_st_structure incomplete_structs;
	SS.app pr_i_ut_structure incomplete_unions;
	SS.app pr_i_et_structure incomplete_enums;
	SM.app pr_s_structure structs;
	SM.app pr_u_structure unions;
	SM.app pr_e_structure enums;
	app pr_t_structure gtys;
	app pr_gvar gvars;
	app pr_gfun gfuns;
	do_cmfile ()
    end
end
end
