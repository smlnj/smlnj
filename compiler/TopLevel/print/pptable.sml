(* pptable.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature PPTABLE =
sig
  exception PP_NOT_INSTALLED
  val pp_object : PrettyPrint.stream -> Stamps.stamp -> Unsafe.Object.object
                  -> unit
  val install_pp : string list ->
                   (PrettyPrint.stream -> Unsafe.Object.object -> unit) -> unit
end

structure PPTable : PPTABLE =
struct

(* The following code implements automatic prettyprinting of values. *)
(* The user defines a datatype d, then defines a prettyprinter       *)
(*                                                                   *)
(*     dp : ppstream -> d -> unit                                    *)
(*                                                                   *)
(* over d, perhaps using the Oppen primitives. Then dp is installed  *)
(* in the "pp table" via install_pp. Subsequently, when a value of   *)
(* type d comes to be printed out, we look in the table, find dp and *)
(* apply it to the value. If it is not found, we print the value in  *)
(* the default manner.                                               *)

  type object = Unsafe.Object.object

  exception PP_NOT_INSTALLED

  fun error msg =
        (ErrorMsg.errorNoFile (ErrorMsg.defaultConsumer(),ref false) (0,0)
			      ErrorMsg.COMPLAIN
			      msg
			      ErrorMsg.nullErrorBody;
	 raise ErrorMsg.Error)

  local
      val global_pp_table = ref StampMap.empty
  in

  fun make_path([s],p) = SymPath.SPATH(rev(Symbol.tycSymbol(s)::p))
    | make_path(s::r,p) = make_path(r,Symbol.strSymbol(s)::p)
    | make_path _ = error "install_pp: empty path"

  fun install_pp (path_names: string list)
                 (p: PrettyPrint.stream -> object -> unit) =
      let val sym_path = make_path(path_names,[])
	  val tycon = Lookup.lookTyc ((#static(EnvRef.combined())),
		sym_path,
		ErrorMsg.errorNoFile(ErrorMsg.defaultConsumer(),ref false) (0,0))
       in case tycon
	    of Types.GENtyc { stamp, ... } =>
	       global_pp_table := StampMap.insert (!global_pp_table, stamp, p)
	     | _ => error "install_pp: nongenerative type constructor"
      end

  fun pp_object ppstrm (s: Stamps.stamp) (obj:object) =
      case StampMap.find (!global_pp_table, s) of
	  SOME p => p ppstrm obj
	| NONE => raise PP_NOT_INSTALLED

  end

end (* structure PPTABLE *)

