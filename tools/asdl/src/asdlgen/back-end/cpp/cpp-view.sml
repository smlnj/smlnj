(* cpp-view.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * The C++ view.
 *)

structure CppView : sig

    val view : View.t

    structure File : sig
        include VIEW_FILE_BASE
        val getArenaAlloc : unit -> bool
      end

    structure Module : VIEW_MODULE_BASE

    structure Type : sig
        include VIEW_TYPE_BASE
        val getPublicCode : AST.TypeId.t -> string list
        val getProtectedCode : AST.TypeId.t -> string list
        val getPrivateCode : AST.TypeId.t -> string list
        val getBoxed : AST.TypeId.t -> bool
(* TODO: base_type *)
      end

    structure Constr : sig
        include VIEW_CONSTR_BASE
        val getPublicCode : AST.ConstrId.t -> string list
        val getProtectedCode : AST.ConstrId.t -> string list
        val getPrivateCode : AST.ConstrId.t -> string list
(* TODO: enum_value *)
      end

  end = struct

    structure CV = CommonView
    structure PN = PropNames
    structure BT = BaseTypes

    structure ViewBase = ViewBaseFn (
      struct
        val viewName = "Cpp"
        val template =  {
                fileProps =
                  CV.prop(PN.arena_alloc, false) ::
                  #fileProps CV.template,
                moduleProps = #moduleProps CV.template,
                typeProps =
                  CV.prop(PN.boxed, false) ::
                  CV.prop(PN.base_type, false) ::
                  CV.prop(PN.public_code, true) ::
                  CV.prop(PN.protected_code, true) ::
                  CV.prop(PN.private_code, true) ::
                  #typeProps CV.template,
                consProps =
                  CV.prop(PN.public_code, true) ::
                  CV.prop(PN.protected_code, true) ::
                  CV.prop(PN.private_code, true) ::
                  CV.prop(PN.enum_value, false) ::
                  #consProps CV.template
              }
      end)

    open ViewBase

    structure File =
      struct
        open ViewBase.File
        fun getArenaAlloc () = View.getBoolValue' PN.arena_alloc (view, View.File)
      end

    structure Type =
      struct
        open ViewBase.Type

        fun getCode prop id = View.getValues prop (view, View.Type id)

        val getPublicCode = getCode PN.public_code
        val getProtectedCode = getCode PN.protected_code
        val getPrivateCode = getCode PN.private_code
        fun getBoxed id = (case View.getBoolValue PN.boxed (view, View.Type id)
               of SOME b => b
                | NONE => if AST.TypeId.isPrim id
                    then false (* the default assumes primitive types are unboxed *)
                    else raise Fail(concat[
                        "getBoxed '", AST.TypeId.nameOf id, "' is undefined"
                      ])
              (* end case *))
(* TODO: base_type *)
      end

    structure Constr =
      struct
        open ViewBase.Constr

        fun getCode prop id = View.getValues prop (view, View.Constr id)

        val getPublicCode = getCode PN.public_code
        val getProtectedCode = getCode PN.protected_code
        val getPrivateCode = getCode PN.private_code
(* TODO: enum_value *)
      end

(* FIXME: it should be possible to override this default! *)
  (* the default header template *)
    val header =
          "// @FILENAME@\n\
          \//\n\
          \// Generated from @SRCFILE@ by asdlgen.\n\
          \//\n"

  (* set the default header property *)
    val () = let
          val SOME prop = View.findProp(view, View.File, PN.header)
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
                (* primitive type bool *)
                  (BT.boolTyId,         PN.name,        "bool"),
                  (BT.boolTyId,         PN.boxed,       "false"),
                (* primitive type int *)
                  (BT.intTyId,          PN.name,        "int"),
                  (BT.intTyId,          PN.boxed,       "false"),
                (* primitive type uint *)
                  (BT.uintTyId,         PN.name,        "unsigned int"),
                  (BT.uintTyId,         PN.boxed,       "false"),
                (* primitive type integer *)
                  (BT.integerTyId,      PN.name,        "asdl::integer"),
                  (BT.integerTyId,      PN.boxed,       "false"),
                (* primitive type identifier *)
                  (BT.identifierTyId,   PN.name,        "asdl::identifier"),
                  (BT.identifierTyId,   PN.boxed,       "false"),
                (* primitive type string *)
                  (BT.stringTyId,       PN.name,        "std::string"),
                  (BT.stringTyId,       PN.boxed,       "false"),
                (* internal type tag8 *)
                  (BT.tag8TyId,         PN.name,        "unsigned int"),
                  (BT.tag8TyId,         PN.boxed,       "false"),
                (* internal type tag *)
                  (BT.tagTyId,          PN.name,        "unsigned int"),
                  (BT.tagTyId,          PN.boxed,       "false")
                ]
            end

  (* set the default name for the ASDL primitive-types module *)
    val () = let
            val asdlMod = View.Module BaseTypes.asdlTypesId
            fun set (propName, name) = let
                  val SOME prop = View.findProp(view, asdlMod, propName)
                  in
                    View.Prop.setValue(prop, name)
                  end
            in
              List.app set [
                  (PN.name,             "asdl")
                ]
            end

  end
