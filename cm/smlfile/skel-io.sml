(* skel-io.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * Reading and writing skeleton pickles to skeleton files.
 *)

signature SKELIO =
  sig
    val read : string * TStamp.t -> Skeleton.decl option
    val write : string * Skeleton.decl * TStamp.t -> unit
  end

structure SkelIO :> SKELIO =
  struct

    structure SK = Skeleton

    val version = "Skeleton 6\n"

    fun readDecl inS = let
          val vers = Byte.bytesToString(BinIO.inputN(inS, size version))
          in
            if vers = version
              then SkeletonFilePickle.read_decl inS
(* FIXME: print an error message *)
              else raise Fail(concat[
                  "SkelIO: incorrect version string \"", String.toString vers,
                  "\", expected \"", String.toString version, "\""
                ])
          end

    fun read (s, ts) = if TStamp.needsUpdate { target = TStamp.fmodTime s, source = ts }
          then NONE
	  else SOME(SafeIO.perform {
              openIt = fn () => BinIO.openIn s,
              closeIt = BinIO.closeIn,
              work = readDecl,
              cleanup = fn _ => ()
            }) handle _ => NONE

    fun writeDecl dcl outS = (
          BinIO.output(outS, Byte.stringToBytes version);
          SkeletonFilePickle.write_decl(outS, dcl))

    fun write (s, sk, ts) = let
	  fun cleanup _ = (
	        OS.FileSys.remove s handle _ => ();
	        Say.say ["[writing ", s, " failed]\n"])
          in
            SafeIO.perform {
                openIt = fn () => AutoDir.openBinOut s,
                closeIt = BinIO.closeOut,
                work = writeDecl sk,
                cleanup = cleanup
              };
	    TStamp.setTime (s, ts)
          end

  end
