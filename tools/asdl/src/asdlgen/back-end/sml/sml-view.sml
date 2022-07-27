(* sml-view.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * The "Sml" view.
 *)

structure SMLView : sig

    val view : View.t

    structure File : VIEW_FILE_BASE

    structure Module : sig
	include VIEW_MODULE_BASE
      (* name of pickler signature *)
        val getPickleSigName : AST.ModuleId.t -> string
      (* name of memory pickler module *)
	val getPickleName : AST.ModuleId.t -> string
      (* name of file pickler module *)
	val getIOName : AST.ModuleId.t -> string
      (* name of S-expression pickle module *)
        val getSExpName : AST.ModuleId.t -> string
      end

    structure Type : VIEW_TYPE_BASE

    structure Constr : VIEW_CONSTR_BASE

  end = struct

    structure CV = CommonView
    structure PN = PropNames
    structure PTy = PrimTypes

    structure ViewBase = ViewBaseFn (
      struct
	val viewName = "sml"
	val template =  {
		fileProps = #fileProps CV.template,
		moduleProps =
		  CV.prop(PN.memory_pickler_name, false) ::
		  CV.prop(PN.file_pickler_name, false) ::
		  CV.prop(PN.sexp_pickle_name, false) ::
		  #moduleProps CV.template,
		typeProps = #typeProps CV.template,
		consProps = #consProps CV.template
	      }
      end)

    open ViewBase

    structure Module =
      struct
	open ViewBase.Module

	local
	  fun getModName (prop, suffix) modId = (
		case View.getOptValue prop (view, View.Module modId)
		 of NONE => getName modId ^ suffix
		  | SOME[name] => name
		  | _ => raise Fail("unexpected multiple values for "^Atom.toString prop)
		(* end case *))
	in
        val getPickleSigName = getModName (PN.pickler_name, "Pickle")
	val getPickleName = getModName (PN.memory_pickler_name, "MemoryPickle")
	val getIOName = getModName (PN.file_pickler_name, "FilePickle")
	val getSExpName = getModName (PN.sexp_pickle_name, "SExpPickle")
	end (* local *)

      end

  (* the default header template *)
    val header =
	  "(* @FILENAME@\n\
	  \ *\n\
	  \ * Generated from @SRCFILE@ by asdlgen.\n\
	  \ *)\n"

  (* set the default header property *)
    val () = let
	  val SOME prop = View.findProp(view, View.File, Atom.atom "header")
	  in
	    View.Prop.setValue(prop, header)
	  end

  (* set the default properties for the ASDL primitive types *)
    val () = let
	    fun set (id, propName, name) = let
		  val SOME prop = View.findProp(view, View.Type id, propName)
		  in
		    View.Prop.setValue(prop, name)
		  end
	    in
	      List.app set [
		  (PTy.boolTyId,	PN.name,	"bool"),
		  (PTy.boolTyId,	PN.reader,	"readBool"),
		  (PTy.boolTyId,	PN.writer,	"writeBool"),
		  (PTy.intTyId,		PN.name,	"int"),
		  (PTy.intTyId,		PN.reader,	"readInt"),
		  (PTy.intTyId,		PN.writer,	"writeInt"),
		  (PTy.uintTyId,	PN.name,	"word"),
		  (PTy.uintTyId,	PN.reader,	"readUInt"),
		  (PTy.uintTyId,	PN.writer,	"writeUInt"),
		  (PTy.integerTyId,	PN.name,	"IntInf.int"),
		  (PTy.integerTyId,	PN.reader,	"readInteger"),
		  (PTy.integerTyId,	PN.writer,	"writeInteger"),
		  (PTy.identifierTyId,	PN.name,	"Atom.atom"),
		  (PTy.identifierTyId,	PN.reader,	"readIdentifier"),
		  (PTy.identifierTyId,	PN.writer,	"writeIdentifier"),
		  (PTy.stringTyId,	PN.name,	"string"),
		  (PTy.stringTyId,	PN.reader,	"readString"),
		  (PTy.stringTyId,	PN.writer,	"writeString"),
		  (PTy.tag8TyId,	PN.reader,	"readTag8"),
		  (PTy.tag8TyId,	PN.writer,	"writeTag8"),
		  (PTy.tagTyId,		PN.reader,	"readTag16"),
		  (PTy.tagTyId,		PN.writer,	"writeTag16")
		]
	    end

  (* set the default names for the ASDL primitive-types module *)
    val () = let
	    val primMod = View.Module PrimTypes.primTypesId
	    fun set (propName, name) = let
		  val SOME prop = View.findProp(view, primMod, propName)
		  in
		    View.Prop.setValue(prop, name)
		  end
	    in
	      List.app set [
		  (PN.name,			"ASDL"),
		  (PN.memory_pickler_name,	"ASDLMemoryPickle"),
		  (PN.file_pickler_name,	"ASDLFilePickle"),
		  (PN.sexp_pickle_name,		"ASDLSExpPickle")
		]
	    end

  end
