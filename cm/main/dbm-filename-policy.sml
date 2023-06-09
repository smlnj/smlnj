(* cm/main/dbm-filename-policy.sml
 *
 * A type representing different choices for file naming conventions.
 *
 * (C) 2023 The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (matthias.blume@gmail.com)
 *)

signature FILENAMEPOLICY =
sig

    type policy
    type policyMaker = { arch: string, os: string} -> policy

    val colocate : { arch: string, os: string } -> policy
    val separate : { bindir: ScrPath.path, bootdir: SrcPath.path }
		   -> { arch: string, os: string } -> policy

    val mkBinName : policy -> SrcPath.path -> SrcPath.path
    val mkSkelName : policy -> SrcPath.path -> SrcPath.path
    val mkGUidName : policy -> SrcPath.path -> SrcPath.path
    val mkStableName : policy -> SrcPath.path * Version.t option -> SrcPath.path
    val mkIndexName : policy -> SrcPath.path -> SrcPath.path

    val cm_dir_arc : string

end (* signature FILENAMEPOLICY *)


(* Imports: SrcPath ($srcpath-lib.cm), Version *)

functor FilenamePolicyFn (val cmdir : string
			  val versiondir: Version.t -> string
			  val skeldir : string
			  val guiddir : string
			  val indexdir : string) :> FILENAMEPOLICY =
struct

local

  structure SP = SrcPath

in		       

    type policy = { bin: SP.path -> SP.path,
		    skel: SP.path -> SP.path
		    guid: SP.path -> SP.path,
		    stable: SP.path * Version.t option -> SP.path,
		    index: SP.path -> SP.path }

    type policyMaker = { arch: string, os: string } -> policy

(* move os_kindToString elsewhere -- no longer used here, since os is assumed to be a string
    (* os_kindToString : SMLofNJ.SysInfo.os_kind -> string *)
    fun os_kindToString SMLofNJ.SysInfo.UNIX = "unix"
      | os_kindToString SMLofNJ.SysInfo.WIN32 = "win"  (* "win", "win32", "win64"? *)
*)

    (* mkPolicy : ([shiftbin:] path -> path) * ([shiftboot:] path -> path) * ([ignoreversion:] bool)
                  -> { arch: string, os: string } -> policy *)
    fun mkPolicy (shiftbin, shiftboot, ignoreversion) { arch: string, os: string } =
	let fun splitPath (head, arc::arcs) = (arc, (head, arcs))
	    fun addArc (arc: SP.arc, (head, arcs): SP.path) = (head, arc::arcs)
	    fun addArcs (newarcs: SP.arc list, (head, arcs): SP.path) =
		(head, List.revAppend (newarcs, arcs))

	    (* cmname : SP.arc list -> SP.path -> SP.path *)
	    fun cmname arcs path =
		(case (SP.splitPath path)
		  of SOME (fname, path0) =>
			 SP.addArc (fname, SP.addArcs (arcs, SP.addArc (cmdir, path0)))
		   | NONE => raise Fail "mkPolicy.cmname: no arcs")

	    val archos = concat [arch, "-", os]

	    val stable0: SP.path -> SP.path = cmname [archos] o shiftboot

	    (* stable : SP.path * Version.t option -> path *)
	    fun stable (path, versionOp) =
	        if ignoreversion
		then stable0 path
		else (case versionOp
			of NONE => stable0 path
			 | SOME v =>
			     let val try = cmname [versiondir v, archos] (shiftboot path)
				 val exists = OS.FileSys.access (SP.pathToFpath try, [])
					      handle _ => false
			      in if exists then try else stable0 path
			     end)

	 in { skel = cmname [skeldir],
	      guid = cmname [guiddir],
	      bin = cmname [archos] o shiftbin,
	      stable = stable,
	      index = cmname [indexdir] }
	end

    (* ident : path -> path  -- path identity function *)
    fun ident (path: SP.path) = path

				    
    (* colocate : { arch: string, os: string } -> policy *)
    val colocate = mkPolicy (ident, ident, false)

    (* separate : {bindir: path, bootdir: path} -> policy *)
    fun separate { bindir: path, bootdir: path } =
	let fun shiftname (root: SP.path) (p: SP.path) =
		let fun anchor_cvt (a: string) = addArc (a, root)
		 in SP.reanchorPath anchor_cvt p
		end
	 in mkPolicy (shiftname bindir, shiftname bootdir, true)
	end

    fun mkBinName (policy: policy) (path: SP.path) = #bin policy path
    fun mkSkelName (policy: policy) (path: SP.path) = #skel policy path
    fun mkGUidName (policy: policy) (path: SP.path = #guid policy path
    fun mkStableName (policy: policy) (path: SP.path, v) = #stable policy (s, v)
    fun mkIndexName (policy: policy) s = #index policy s

    val cm_dir_arc = cmdir

end (* top local *)
end (* functor FilenamePolicyFn *)

structure FilenamePolicy =
    FilenamePolicyFn
      (val cmdir = Option.getOpt (OS.Process.getEnv "CM_DIR_ARC", ".cm")
       val skeldir = "SKEL"
       val guiddir = "GUID"
       val indexdir = "INDEX"
       val versiondir = Version.toString)
