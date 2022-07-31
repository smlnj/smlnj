(* Copyright (c) 1998 by Lucent Technologies *)

(* Configuration for ANSIC *)

structure Config : CONFIG =
struct

  val DFLAG = false

  structure ParseControl : PARSECONTROL =
  struct
    val symbolLength 		= 256
    val typedefsScoped		= true
    val prototypesAllowed 	= true
    val templatesAllowed 	= false
    val trailingCommaInEnum 	= {error=false,warning=true}
    val newFundefsAllowed 	= true
    val voidAllowed		= true
    val voidStarAllowed		= true
    val constAllowed		= true
    val volatileAllowed		= true
    fun violation str =
        TextIO.output(TextIO.stdOut,"\nERROR: in ANSI C " ^ str ^ "\n")
    val Dkeywords               = false
    val parseDirective = true   (* Chandra, 6/21/99 *)
    val underscoreKeywords = SOME true (* Blume *)
  end

  (* see type-check-control-sig.sml for description of these flags *)
  structure TypeCheckControl : TYPECHECKCONTROL = 
  struct
    val don't_convert_SHORT_to_INT = false             (* not doing dsp *)
    val don't_convert_DOUBLE_in_usual_unary_cnv = true (* ansic *)
    val enumeration_incompatibility = true             (* ansic *)
    val pointer_compatibility_quals = true             (* ansic *)
    val undeclared_id_error = true                     (* ansic *)
    val undeclared_fun_error = true                    (* ansic *)
    val convert_function_args_to_pointers = true       (* ansic *)
    val storage_size_check = true                      (* ansic *)
    val perform_type_checking = true                   (* do type checking *)
    val ISO_bitfield_restrictions = false              (* allow char, short, long in bitfields *)
    val allow_enum_bitfields = true                    (* allow enums in bitfields *)
    val allow_non_constant_local_initializer_lists = false (* ansic *)
    val partial_enum_error = false                     (* permissive *)
    val partial_enums_have_unknown_size = false        (* permissive *)
  end
end (* structure Config *)
