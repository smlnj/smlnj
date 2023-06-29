(* cm/semant/dbm/group-reg.sml
 *
 * The "group registry".  CM uses this to remember which groups it is
 * currently working on and what the corresponding input sources (Source.source) are.
 *
 * (C) 2023 The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (matthias.blume@gmail.com)
 * Edited: DBM, 2023.6
 *)

signature GROUPREG =
sig

  type groupreg

  val new : unit -> groupreg
  val register : groupreg -> File.file * Source.source -> unit
  val lookup : groupreg -> File.file -> Source.source
  val registered : groupreg -> File.file -> bool
  val error : groupreg -> File.file * SourceMap.region -> ErrorMsg.complainer
  val anyErrors : groupreg -> File.file -> bool

end (* signature GROUPREG *)

structure GroupReg :> GROUPREG =
struct

local

  (* compiler imports *)
  structure SR = Source
  structure SM = SourceMap

  (* CM imports *)
  structure F = File
  structure FM = FileMap

in

  (* groupreg: a reference to a file -> source mapping *)
  type groupreg = SR.source FM.map ref

  (* new : unit -> groupreg *)
  fun new () = ref FM.empty : groupreg

  (* register : groupreg -> F.file * SR.source -> unit *)
  fun register (gr: groupreg) (file: F.file, src: SR.source) = gr := FM.insert (!gr, file, s)

  (* lookup : groupref -> F.file -> SR.source *)
  fun lookup (gr: groupreg) (file: file) =
      case FM.find (!gr, file)
        of SOME source => source
	 | NONE => raise Fail ("GroupReg.lookup " ^ F.filePath file)

  (* registered : groupreg -> F.file -> bool *)
  fun registered (gr: groupreg) (file: F.file) = isSome (FM.find (!gr, file))

  (* error : groupreg -> F.file * SM.region -> ER.complainer *)
  fun error (gr: groupreg) (file: F.file, r: SM.region) = ErrorMsg.error (lookup gr file) r

  (* anyErrors : groupreg -> F.file -> bool *)
  fun anyErrors (gr: groupreg) (file: F.file) = !(#anyErrors (lookup gr file))

end (* top local *)
end (* structure GroupReg *)
