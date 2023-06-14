(* cm/main/dbm-filename-policy.sml
 *
 * A "policy" is a record of 5 path transforms that implement file naming conventions.
 *
 * (C) 2023 The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (matthias.blume@gmail.com)
 * Edited: DBM (2023.6)
 *)

(* a "policy" is a record of five path transformer functions *)

signature FILENAMEPOLICY =
sig

    type policy

    (* store the binfiles, etc. along with the SML source files *)
    val colocate : { arch: string, os: string } -> policy

    (* store the binfiles and bootfiles (stable library binfiles) in separate
     * directories:
     * normally these directories are system/sml.bin.arch-opsys and
     * system/sml.boot.arch-opsys *)
    val separate : { bindir: Path.path, bootdir: Path.path }
		   -> { arch: string, os: string } -> policy

    val mkBinName : policy -> Path.path -> Path.path
    val mkSkelName : policy -> Path.path -> Path.path
    val mkGUidName : policy -> Path.path -> Path.path
    val mkStableName : policy -> Path.path -> Path.path
    val mkIndexName : policy -> Path.path -> Path.path

    val cm_dir_arc : string

end (* signature FILENAMEPOLICY *)


(* Imports: Path ($srcpath-lib.cm) *)

structure FilenamePolicy :> FILENAMEPOLICY =
struct

local

  structure P = Path

  (* the cm directory name is ".cm" by default, unless otherwise specified by the
   * shell environment variable CM_DIR_ARC. *)
  val cmdir : string = Option.getOpt (OS.Process.getEnv "CM_DIR_ARC", ".cm")

in		       

  type policy = { bin: P.path -> P.path,
		  skel: P.path -> P.path
		  guid: P.path -> P.path,
		  stable: P.path -> P.path,
		  index: P.path -> P.path }

  (* mkPolicy : ([shiftbin:] P.path -> P.path) * ([shiftstable:] P.path -> P.path)
		-> { arch: string, os: string } -> policy *)
  fun mkPolicy (shiftbin: P.path -> P.path, shiftstable: P.path -> P.path)
	       { arch: string, os: string } : policy =
      let (* cmname : P.arc -> P.path -> P.path *)
	  fun cmname arc path =
	      (case (P.splitPath path)
		of SOME (fname, path') =>
		       P.addArc (fname, P.addArc (arc, P.addArc (cmdir, path')))
		 | NONE => CMErr.error "mkPolicy.cmname: no arcs")

	  val archos = concat [arch, "-", os]

       in { skel = cmname "SKEL",
	    guid = cmname "GUID",
	    index = cmname "INDEX",
	    bin = cmname archos o shiftbin,
	    stable = cmname archos o shiftstable }
      end

  (* ident : path -> path  -- path identity function *)
  fun ident (path: P.path) = path


  (* colocate : { arch: string, os: string } -> policy *)
  val colocate = mkPolicy (ident, ident)

  (* separate : {bindir: path, bootdir: path} -> policy *)
  fun separate { bindir: path, bootdir: path } =
      let fun shiftname (root: P.path) (p: P.path) =
	      let fun anchor_cvt (anchor: string) = addArc (anchor, root)
	       in P.reanchorPath anchor_cvt p
	      end
       in mkPolicy (shiftname bindir, shiftname bootdir)
      end

  fun mkBinName (policy: policy) = #bin policy
  fun mkSkelName (policy: policy) = #skel policy
  fun mkGUidName (policy: policy) = #guid policy
  fun mkStableName (policy: policy) = #stable policy
  fun mkIndexName (policy: policy) = #index policy

  val cm_dir_arc = cmdir

end (* top local *)
end (* structure FilenamePolicy *)
