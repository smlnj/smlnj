(* code-obj.sig
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *
 * An interface for manipulating code objects.
 *)

signature CODE_OBJ =
  sig

    type t

    type executable = Unsafe.Object.object -> Unsafe.Object.object

    (* raised by input when there are insufficient bytes *)
    exception FormatError

    (* given the target, source-file name, and a CFG pickle, generate machine
     * code using the LLVM code generator
     *)
    val generate : {
            target : string,
            src : string,
            pkl : Word8Vector.vector,
            verifyLLVM : bool
          } -> t

    (* Allocate a code object of the given size and initialize it
     * from the input stream.  The third argument is the entrypoint
     * offset.
     *)
    val input : (BinIO.instream * int * int) -> t

    (* Output a code object to the given output stream *)
    val output : (BinIO.outstream * t) -> unit

    (* View the code object as an executable.  This has the side-effect
     * of flushing the instruction cache.
     *)
    val exec : t -> executable

    (* return the size of the code object *)
    val size : t -> int

    (* return the offset of the entry point of the code object *)
    val entrypoint : t -> int

    (* use the run-time system interpreter to generate a literals
     * vector from a literal bytecode program.
     *)
    val mkLiterals : Word8Vector.vector -> Unsafe.Object.object

  end
