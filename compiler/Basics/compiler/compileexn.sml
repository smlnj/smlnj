(* compileexn.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *)
structure CompileExn = struct
    (* An exception to be raised by the compiler when compilation fails. *)
    exception Compile of string
end
