(* code-obj.sig
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * An interface for manipulating code objects.
 *)

signature CODE_OBJ =
  sig

    type code_object

    type csegments = {
	code : code_object,
	data : Word8Vector.vector
      }

    type executable = Unsafe.Object.object -> Unsafe.Object.object

  (* raised by input when there are insufficient bytes *)
    exception FormatError

  (* given the target, source-file name, and a CFG pickle, generate machine
   * code using the LLVM code generator
   *)
    val generate : {
            target : string,
            src : string,
            pkl : Word8Vector.vector
          } -> code_object

  (* Allocate an unintialized code object of the given number of bytes. *)
    val alloc : int -> code_object

  (* Allocate a code object of the given size and initialize it
   * from the input stream.
   *)
    val input : (BinIO.instream * int) -> code_object

  (* Output a code object to the given output stream *)
    val output : (BinIO.outstream * code_object) -> unit

  (* View the code object as an updatable array of bytes. *)
    val bytes : code_object -> Word8Array.array

  (* Set the offset of the entrypoint of the code object (default: 0). *)
    val set_entrypoint : code_object * int -> unit

  (* View the code object as an executable.  This has the side-effect
   * of flushing the instruction cache.
   *)
    val exec : code_object -> executable

  (* return the size of the code object *)
    val size : code_object -> int

  (* return the offset of the entry point of the code object *)
    val entrypoint : code_object -> int

  (* use the run-time system interpreter to generate a literals
   * vector from a literal bytecode program.
   *)
    val mkLiterals : Word8Vector.vector -> Unsafe.Object.object

  end;
