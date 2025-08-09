(* sml-view.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * The "Sml" view.
 *)

structure SMLView : sig

    val view : View.t

    structure File : sig
        include VIEW_FILE_BASE
      (* get the name of the sharing-context structure name *)
        val getShareContextName : unit -> string
      end

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

    structure Type : sig
        include VIEW_TYPE_BASE
        (* get the name of the equivalent datatype *)
        val getIsDatatype : AST.TypeId.t -> string option
      end

    structure Constr : VIEW_CONSTR_BASE

  end = struct

    structure CV = CommonView
    structure PN = PropNames
    structure BT = BaseTypes

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
                typeProps =
                  CV.prop(PN.is_datatype, false) ::
                  #typeProps CV.template,
                consProps = #consProps CV.template
              }
      end)

    val view = ViewBase.view

    structure File =
      struct
        open ViewBase.File
(* TODO: add view support *)
        fun getShareContextName () = "ShareContext"
      end

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
        val getPickleSigName = getModName (PN.pickler_name, "PICKLE")
        val getPickleName = getModName (PN.memory_pickler_name, "MemoryPickle")
        val getIOName = getModName (PN.file_pickler_name, "FilePickle")
        val getSExpName = getModName (PN.sexp_pickle_name, "SExpPickle")
        end (* local *)

      end

    structure Type =
      struct
        open ViewBase.Type
        fun getIsDatatype tyId = (
              case View.getOptValue PN.is_datatype (view, View.Type tyId)
               of NONE => NONE
                | SOME[name] => SOME name
                | _ => raise Fail("unexpected multiple values for 'is_datatype'")
              (* end case *))
      end

    structure Constr = ViewBase.Constr

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
                  (BT.boolTyId,        PN.name,        "bool"),
                  (BT.boolTyId,        PN.reader,      "readBool"),
                  (BT.boolTyId,        PN.writer,      "writeBool"),
                  (BT.intTyId,         PN.name,        "int"),
                  (BT.intTyId,         PN.reader,      "readInt"),
                  (BT.intTyId,         PN.writer,      "writeInt"),
                  (BT.uintTyId,        PN.name,        "word"),
                  (BT.uintTyId,        PN.reader,      "readUInt"),
                  (BT.uintTyId,        PN.writer,      "writeUInt"),
                  (BT.integerTyId,     PN.name,        "IntInf.int"),
                  (BT.integerTyId,     PN.reader,      "readInteger"),
                  (BT.integerTyId,     PN.writer,      "writeInteger"),
                  (BT.identifierTyId,  PN.name,        "Atom.atom"),
                  (BT.identifierTyId,  PN.reader,      "readIdentifier"),
                  (BT.identifierTyId,  PN.writer,      "writeIdentifier"),
                  (BT.stringTyId,      PN.name,        "string"),
                  (BT.stringTyId,      PN.reader,      "readString"),
                  (BT.stringTyId,      PN.writer,      "writeString"),
                  (BT.tag8TyId,        PN.reader,      "readTag8"),
                  (BT.tag8TyId,        PN.writer,      "writeTag8"),
                  (BT.tagTyId,         PN.reader,      "readTag16"),
                  (BT.tagTyId,         PN.writer,      "writeTag16")
                ]
            end

  (* set the default names for the ASDL primitive-types module *)
    val () = let
            val primMod = View.Module BT.asdlTypesId
            fun set (propName, name) = let
                  val SOME prop = View.findProp(view, primMod, propName)
                  in
                    View.Prop.setValue(prop, name)
                  end
            in
              List.app set [
                  (PN.name,                     "ASDL"),
                  (PN.memory_pickler_name,      "ASDLMemoryPickle"),
                  (PN.file_pickler_name,        "ASDLFilePickle"),
                  (PN.sexp_pickle_name,         "ASDLSExpPickle")
                ]
            end

  end
