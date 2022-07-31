(* minimal-compiler.sml
 * 
 * (C) 2001 Lucent Technologies, Bell Labs
 *
 * This defines a minimal version of structure Compiler
 * for backward compatibility with code that wants to test
 * Compiler.version or Compiler.architecture.
 *)
structure MinimalCompiler = struct
    val version = SMLNJVersion.version
    val architecture = Backend.architecture
end

structure Compiler = MinimalCompiler
