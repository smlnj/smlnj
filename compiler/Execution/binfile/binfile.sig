(* binfile.sig
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *
 * author: Matthias Blume and John Reppy
 *
 * This revised version of signature BINFILE is now machine-independent.
 * Moreover, it deals with the file format only and does not know how to
 * create new binfile contents (aka "compile") or how to interpret the
 * pickles.  As a result, it does not statically depend on the compiler.
 * (Eventually we might want to support a light-weight binfile loader.)
 *
 * See dev-notes/binfile.adoc for a description of the binfile format.
 *)

signature BINFILE = sig

    (* the contents of a binfile *)
    type t

    (* sections of a binfile.
     * NOTE: eventually, we should generalize this type to cover the other
     * parts of the file.
     *)
    datatype section
      = Literals of Word8Vector.vector
      | CFGPickle of Word8Vector.vector
      | Code of CodeObj.t

    type version_info = {
        bfVersion : word,       (* the binfile version; this will be 0w0 for
                                 * old-format files, and the hexadecimal version
                                 * date 0wxYYYYMMDD for SML/NJ 2021.1 and later.
                                 *)
        arch : string,          (* the architecture string *)
        smlnjVersion : string   (* the SML/NJ version string *)
      }

  (* create the version info for the current binfile version *)
    val mkVersion : {arch : string, smlnjVersion : string} -> version_info

  (* get the version info for the binfile *)
    val version : bfContents -> version_info

    exception FormatError

    type pid = PersStamps.persstamp
    type stats = { env : int, data : int, code : int }
    type pickle = { pid : pid, pickle : Word8Vector.vector }

    val staticPidOf    : t -> pid
    val exportPidOf    : t -> pid option
    val cmDataOf       : t -> pid list

    val senvPickleOf   : t -> pickle

    val guidOf         : t -> string

    (* calculate the size in bytes occupied by some binfile contents *)
    val size : { contents : t, nopickle : bool } -> int

    (* create the abstract binfile contents *)
    val create : {
            version : version_info,
	    imports: ImportTree.import list,
	    exportPid: pid option,
	    cmData: pid list,
	    senv: pickle,
	    guid: string,
            sections : section list
	  } -> t

    (* read just the guid *)
    val readGUid : BinIO.instream -> string

    (* read binfile contents from an IO stream *)
    val read : {
            version : version_info,             (* expected binfile version *)
	    stream: BinIO.instream
	  } -> { contents : t, stats : stats }

    (* write binfile contents to an IO stream *)
    val write : {
	    stream : BinIO.outstream,
	    contents : t, nopickle : bool
	  } -> stats

    (* Given a dynamic environment, link the code object contained in
     * some given binfile contents. The result is the delta environment
     * containing the bindings (if any) resulting from this link operation.
     *)
    val exec : t * DynamicEnv.env * (exn -> exn) -> DynamicEnv.env

  end
