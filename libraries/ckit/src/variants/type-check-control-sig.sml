signature TYPECHECKCONTROL =
sig
   
  (* these flags are used in type-util.sml *)
  val don't_convert_SHORT_to_INT: bool
                  (* In ANSI C, usual unary converstion converts
		     SHORT to INT; for DSP code, we want to
		     keep SHORT as SHORT.
		     Default: true for ANSI C behavior *)

  val don't_convert_DOUBLE_in_usual_unary_cnv: bool
                  (* In ANSI, FLOAT is not converted to DOUBLE during
		     usual unary converstion; in old style compilers
		     FLOAT *is* converted to DOUBLE.
		     Default: true for ANSI behavior *)

  val enumeration_incompatibility: bool
                  (* ANSI says that different enumerations are incomptible
		     (although all are compatible with int);
		     older style compilers say that different enumerations
		     are compatible.
		     Default: true for ANSI behavior *)

  val pointer_compatibility_quals: bool
                  (* ANSI says that pointers to differently qualified types
		     are different; some compilers vary.
		     Default: true for ANSI behavior *)

  (* used in build-ast.sml *)
  val undeclared_id_error:bool
                  (* In ANSI C, an undeclared id is an error;
 		     in older versions of C, undeclared ids are assumed integer.
		     Default value: true (for ANSI behavior) *)

  val undeclared_fun_error:bool
                  (* In ANSI C, an undeclared fun is an error;
 		     in older versions of C, undeclared funs are assumed to return integer.
		     Default value: true (for ANSI behavior) *)

  val convert_function_args_to_pointers:bool
                  (* In ANSI C, arguments of functions goverened by prototype
		     definitions that have type function or array are not
		     promoted to pointer type; however many compilers do this
		     promotion.
		     Default value: true (to get standard behavior) *)

  val storage_size_check:bool
                  (* Declarations and structure fields must have known storage
		     size; maybe you want to turn this check off?
		     Default value: true (to get ANSI behavior). *)

  val allow_non_constant_local_initializer_lists: bool
                  (* Allow non constant local inializers for aggregates and unions.
                      e.g. int x, y, z;
                           int a[] = {x, y, z};
                     This is allowed gcc *)
  val perform_type_checking:bool
                  (* true = do type checking; false = disable type checking;
		     Note: with type checking off, there is still some
		           rudimentary type processing, but no
			   usual unary conversions, usual binary conversions, etc. *)

  (* used by sizeof *)
  val ISO_bitfield_restrictions: bool
                  (* In ANSI/ISO, types of bitfields must be qualified or unqualified version of
		   int, unsigned int or signed int (ISO spec, section 6.5.2.1, p60);
		   however most compilers allow chars, shorts and longs as well.
		   Default value: false (to get std permissive behavior) *)

  val allow_enum_bitfields: bool 
                  (* Allow bitfields involving enum
		     e.g. enum x y : 8;
		   Default value: true (permissive behavior e.g. gcc) *)

  val partial_enum_error: bool
                  (* Prohibit partial enums.
		     i.e. enum x *y;
                          enum x {a, b, c};
                     Default value: false.
		     (set to true to get strict behaviour)
		   *)
		     
  val partial_enums_have_unknown_size: bool
                  (* Treat partial enums as having unknown size.
		     e.g. 
                          enum x y;
                          enum x {a, b, c};
                     Default value: false.
		   *)
end



