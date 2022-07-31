(*
 * Reading and writing skeletons to skeleton files.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
signature SKELIO = sig
    val read : string * TStamp.t -> Skeleton.decl option
    val write : string * Skeleton.decl * TStamp.t -> unit
end

structure SkelIO :> SKELIO = struct

    structure SK = Skeleton
    structure SS = SymbolSet
    structure S = Symbol
    structure SP = SymPath
    structure PU = PickleUtil
    structure PSymPid = PickleSymPid
    structure UU = UnpickleUtil

    infix 3 $

    exception Format = UU.Format

    val s2b = Byte.stringToBytes
    val b2s = Byte.bytesToString
    val b2c = Byte.byteToChar

    val version = "Skeleton 5\n"

    fun makeset l = SS.addList (SS.empty, l)

    fun inputLine s = let
	fun finish acc = String.implode (rev acc)
	fun loop acc =
	    case Option.map b2c (BinIO.input1 s) of
		NONE => finish (#"\n" :: acc)
	      | SOME #"\n" => finish (#"\n" :: acc)
	      | SOME c => loop (c :: acc)
    in
	loop []
    end

    fun write_decl (s, d) = let

	val (P, D, M) = (1, 2, 3)
	val symbol = PSymPid.w_symbol
	val list = PU.w_list

	val op $ = PU.$ P
	fun path (SP.SPATH p) = "p" $ [list symbol p]

	fun decl arg = let
	    val op $ = PU.$ D
	    fun d (SK.Bind (name, def)) = "a" $ [symbol name, modExp def]
	      | d (SK.Local (x, y)) = "b" $ [decl x, decl y]
	      | d (SK.Par l) = "c" $ [list decl l]
	      | d (SK.Seq l) = "d" $ [list decl l]
	      | d (SK.Open d) = "e" $ [modExp d]
	      | d (SK.Ref s) = "f" $ [list symbol (SS.listItems s)]
	in
	    d arg
	end

	and modExp arg = let
	    val op $ = PU.$ M
	    fun m (SK.Var p) = "g" $ [path p]
	      | m (SK.Decl d) = "h" $ [list decl d]
	      | m (SK.Let (d, e)) = "i" $ [list decl d, modExp e]
	      | m (SK.Ign1 (e1, e2)) = "j" $ [modExp e1, modExp e2]
	in
	    m arg
	end

	val pickle = s2b (PU.pickle () (decl d))
    in
	BinIO.output (s, Byte.stringToBytes version);
	BinIO.output (s, pickle)
    end

    fun read_decl s = let

	val firstLine = inputLine s

	val session = UU.mkSession (UU.stringGetter (b2s (BinIO.inputAll s)))

	val string = UU.r_string session
	val symbol = UnpickleSymPid.r_symbol (session, string)
	fun list m r = UU.r_list session m r
	fun share m f = UU.share session m f

	val pathM = UU.mkMap ()
	val symbolListM = UU.mkMap ()
	val declM = UU.mkMap ()
	val declListM = UU.mkMap ()
	val modExpM = UU.mkMap ()

	val symbollist = list symbolListM symbol

	fun path () = let
	    fun p #"p" = SP.SPATH (symbollist ())
	      | p _ = raise Format
	in
	    share pathM p
	end

	fun decl () = let
	    fun d #"a" = SK.Bind (symbol (), modExp ())
	      | d #"b" = SK.Local (decl (), decl ())
	      | d #"c" = SK.Par (decllist ())
	      | d #"d" = SK.Seq (decllist ())
	      | d #"e" = SK.Open (modExp ())
	      | d #"f" = SK.Ref (makeset (symbollist ()))
	      | d _ = raise Format
	in
	    share declM d
	end

	and decllist () = list declListM decl ()

	and modExp () = let
	    fun m #"g" = SK.Var (path ())
	      | m #"h" = SK.Decl (decllist ())
	      | m #"i" = SK.Let (decllist (), modExp ())
	      | m #"j" = SK.Ign1 (modExp (), modExp ())
	      | m _ = raise Format
	in
	    share modExpM m
	end
    in
	if firstLine = version then decl () else raise Format
    end

    fun read (s, ts) =
	if TStamp.needsUpdate { target = TStamp.fmodTime s, source = ts } then
	    NONE
	else
	    SOME (SafeIO.perform { openIt = fn () => BinIO.openIn s,
				   closeIt = BinIO.closeIn,
				   work = read_decl,
				   cleanup = fn _ => () })
	    handle _ => NONE

    fun write (s, sk, ts) = let
	fun cleanup _ =
	    (OS.FileSys.remove s handle _ => ();
	     Say.say ["[writing ", s, " failed]\n"])
    in
	SafeIO.perform { openIt = fn () => AutoDir.openBinOut s,
			 closeIt = BinIO.closeOut,
			 work = fn s => write_decl (s, sk),
			 cleanup = cleanup };
	TStamp.setTime (s, ts)
    end
end
