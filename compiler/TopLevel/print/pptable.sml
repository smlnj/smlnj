(* pptable.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature PPTABLE =
sig
  exception FORMATTER_NOT_INSTALLED
  val formatObject : Stamps.stamp -> Unsafe.Object.object -> NewPP.format
  val installFormatter : string list -> (Unsafe.Object.object -> NewPP.format) -> unit
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

local
  structure EM = ErrorMsg
  structure S = Symbol
  structure SM = StampMap
  structure SRM = SourceMap
  structure PP = NewPP

  type object = Unsafe.Object.object

  val global_formatter_table = ref (SM.empty: (object -> PP.format) SM.map)

  val nullRegion = SourceMap.nullRegion

  (* error : string -> 'a *)
  fun error (msg: string) =
       (EM.errorNoSource (ref false) nullRegion EM.COMPLAIN msg
           EM.nullErrorBody;
	raise EM.Error)

  (* makePath : string list * S.symbol list *)
  fun makePath ([s], p) = SymPath.SPATH (rev (S.tycSymbol(s) :: p))
    | makePath (s::r, p) = makePath (r, S.strSymbol(s)::p)
    | makePath _ = error "install_pp: empty path"

in

  exception FORMATTER_NOT_INSTALLED

  fun installFormatter (path_names: string list) (formatter: object -> NewPP.format) =
      let val sym_path = makePath (path_names, [])
	  val err = (fn severity => (fn msg => (fn body => (error msg))))
	  val tycon = Lookup.lookTyc (#static(EnvRef.combined()), sym_path, err)
       in case tycon
	    of Types.GENtyc {stamp, ...} =>
	         global_formatter_table :=
		   SM.insert (!global_formatter_table, stamp, formatter)
	     | _ => error "install_formatter: non-GENtyc type constructor"
      end

  fun formatObject (s: Stamps.stamp) (obj:object) =
      case SM.find (!global_formatter_table, s)
        of SOME formatter => formatter obj
	 | NONE => raise FORMATTER_NOT_INSTALLED

end (* top local *)
end (* structure PPTABLE *)
