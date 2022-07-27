(* view-base-fn.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

functor ViewBaseFn (V : sig

    val viewName : string
    val template : View.template

  end) : VIEW_BASE = struct

    structure PN = PropNames

    val view = View.new (V.viewName, V.template)

    val getHeaderValue = View.getOptValue PN.header
    val getNameValue = View.getValue PN.name

    fun getSuppress entity = let
	  fun return (t, p, u) = {types = t, pickler = p, unpickler = u}
	  fun default () = return (false, false, false)
	  in
	    case View.getNames PN.suppress (view, entity)
	     of ["none"] => default()
	      | ["all"] => return (true, true, true)
	      | toks => let
		  fun doToks ([], t, p, u) = return (t, p, u)
		    | doToks ("types"::r, _, p, u) = doToks (r, true, p, u)
		    | doToks ("pickler"::r, t, _, u) = doToks (r, t, true, u)
		    | doToks ("unpickler"::r, t, p, _) = doToks (r, t, p, true)
		    | doToks (tok::r, t, p, u) = raise Fail "cvtSuppress: bogus name"
		  in
		    doToks (toks, false, false, false)
		  end
	    (* end case *)
	  end

    structure File =
      struct
	fun getHeader () = (
	      case getHeaderValue (view, View.File)
	       of NONE => []
		| SOME text => text
	      (* end case *))

        local
          fun getCode prop = View.getValues prop (view, View.File)
        in
	fun getInterfaceCode () = {
		prologue = getCode PN.interface_prologue,
		epilogue = getCode PN.interface_epilogue
	      }
	fun getImplementationCode () = {
		prologue = getCode PN.implementation_prologue,
		epilogue = getCode PN.implementation_epilogue
	      }
	end (* local *)

	val getSuppress = fn () => getSuppress View.File
      end

    structure Module =
      struct
	fun getName modId = (
	      case getNameValue (view, View.Module modId, AST.ModuleId.nameOf modId)
	       of [name] => name
		| _ => raise Fail "Module.getName"
	      (* end case *))

        local
          fun getCode prop modId = View.getValues prop (view, View.Module modId)
        in
	fun getInterfaceCode modId = {
		prologue = getCode PN.interface_prologue modId,
		epilogue = getCode PN.interface_epilogue modId
	      }
	fun getImplementationCode modId = {
		prologue = getCode PN.implementation_prologue modId,
		epilogue = getCode PN.implementation_epilogue modId
	      }
	end (* local *)

	val getSuppress = fn modId => getSuppress (View.Module modId)
      end

    structure Type =
      struct
	fun getName id = (
	      case getNameValue (view, View.Type id, AST.TypeId.nameOf id)
	       of [name] => name
		| _ => raise Fail "Type.getName"
	      (* end case *))
	local
	  fun getFn prop = let
		val get = View.getOptValue prop
		in
		  fn id => (case get (view, View.Type id)
		       of SOME[name] => SOME name
			| _ => NONE
		      (* end case *))
		end
	in
	val getReader = getFn PN.reader
	val getWriter = getFn PN.writer
	end (* local *)

	fun getNaturalType tyId = (
	      case View.getOptValue PN.natural_type (view, View.Type tyId)
	       of SOME[name] => name
		| _ => getName tyId
	      (* end case *))
        local
	  fun getFn prop = let
		val get = View.getOptValue prop
		in
		  fn id => (case get (view, View.Type id)
		       of SOME[name] => SOME name
			| _ => NONE
		      (* end case *))
		end
	in
	val getNaturalTypeCon = getFn PN.natural_type_con
	val getWrapper = getFn PN.wrapper
	val getUnwrapper = getFn PN.unwrapper
	end (* local *)
      end

    structure Constr =
      struct
	fun getName id = (
	      case getNameValue (view, View.Constr id, AST.ConstrId.nameOf id)
	       of [name] => name
		| _ => raise Fail "Constr.getName"
	      (* end case *))
      end

  end

