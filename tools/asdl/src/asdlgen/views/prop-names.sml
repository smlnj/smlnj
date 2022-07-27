(* prop-names.sml
 *
 * COPYRIGHT (c) 2021 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * The view-property names.
 *)

structure PropNames =
  struct

    val arena_alloc = Atom.atom "arena_alloc"
    val base_class = Atom.atom "base_class"
    val base_type = Atom.atom "base_type"
    val boxed = Atom.atom "boxed"
    val doc_string = Atom.atom "doc_string"
    val enum_value = Atom.atom "enum_value"
    val file_pickler_name = Atom.atom "file_pickler_name"
    val header = Atom.atom "header"
    val implementation_epilogue = Atom.atom "implementation_epilogue"
    val implementation_prologue = Atom.atom "implementation_prologue"
    val interface_epilogue = Atom.atom "interface_epilogue"
    val interface_prologue = Atom.atom "interface_prologue"
    val memory_pickler_name = Atom.atom "memory_pickler_name"
    val name = Atom.atom "name"
    val natural_type = Atom.atom "natural_type"
    val natural_type_con = Atom.atom "natural_type_con"
    val pickler_name = Atom.atom "pickler_name"
    val private_code = Atom.atom "private_code"
    val protected_code = Atom.atom "protected_code"
    val public_code = Atom.atom "public_code"
    val reader = Atom.atom "reader"
    val sexp_pickle_name = Atom.atom "sexp_pickle_name"
    val suppress = Atom.atom "suppress"
    val unwrapper = Atom.atom "unwrapper"
    val user_attribute = Atom.atom "user_attribute"
    val user_init = Atom.atom "user_init"
    val wrapper = Atom.atom "wrapper"
    val writer = Atom.atom "writer"

  end
