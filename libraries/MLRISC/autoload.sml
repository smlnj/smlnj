(*
 * Make Compiler.version visible, so that we can decide what to do.  
 * Unfortunately, how to do this is completely version specific, 
 * so we have to try a few alternatives.
 *)
CM.autoload "$smlnj/compiler.cm" handle _ => false;
CM.autoload "host-compiler.cm" handle _ => false;
