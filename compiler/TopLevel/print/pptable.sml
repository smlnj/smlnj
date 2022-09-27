(* pptable.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature PPTABLE =
sig
  exception FORMATTER_NOT_INSTALLED
  val format_object : Stamps.stamp -> Unsafe.Object.object -> NewPP.format
  val install_formatter : string list -> (Unsafe.Object.object -> NewPP.format) -> unit
end

structure PPTable : PPTABLE =
struct

(* The following code implements automatic prettyprinting of values.
 * The user defines a datatype d, then defines a prettyprint formatter
 *
 *     formatter : d -> PP.format
 *
 * for d, defined using the NewPP interface. Then formatter is        
 * installed in the "pp table" via install_pp. Subsequently, when a value of
 * type d comes to be printed out, we look in the table, find formatter and
 * apply it to the value. If it is not found, we print the value in
 * the default manner as "-"
 *)

  type object = Unsafe.Object.object

  exception FORMATTER_NOT_INSTALLED

  fun error msg =
        (ErrorMsg.errorNoFile (ErrorMsg.defaultConsumer(), ref false)
			      (0,0) ErrorMsg.COMPLAIN msg ErrorMsg.nullErrorBody;
	 raise ErrorMsg.Error)
  local
      val global_formatter_table = ref StampMap.empty
  in

  fun make_path([s],p) = SymPath.SPATH (rev (Symbol.tycSymbol(s) :: p))
    | make_path(s::r,p) = make_path (r, Symbol.strSymbol(s)::p)
    | make_path _ = error "install_pp: empty path"

  fun install_formatter (path_names: string list) (formatter: object -> NewPP.format) =
      let val sym_path = make_path (path_names, [])
	  val tycon = Lookup.lookTyc ((#static(EnvRef.combined())),
				      sym_path,
				      ErrorMsg.errorNoFile(ErrorMsg.defaultConsumer(),ref false) (0,0))
       in case tycon
	    of Types.GENtyc {stamp, ...} =>
	       global_formatter_table := StampMap.insert (!global_formatter_table, stamp, formatter)
	     | _ => error "install_formatter: nongenerative type constructor"
      end

  fun format_object (s: Stamps.stamp) (obj:object) =
      case StampMap.find (!global_formatter_table, s)
        of SOME formatter => formatter obj
	 | NONE => raise FORMATTER_NOT_INSTALLED

  end

end (* structure PPTABLE *)

