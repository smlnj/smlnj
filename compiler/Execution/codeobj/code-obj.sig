(* code-obj.sig
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *
 * An interface for manipulating the various representations of
 * executable code.
 *)

signature CODE_OBJ =
  sig

    type cfg_pickle = Word8Vector.vector

    type native_code

    (* the representations of the executable *)
    datatype t
      = NoCode
      | CFGPickle of cfg_pickle
      | NativeCode of native_code

    type literals = Word8Vector.vector

    type executable = Unsafe.Object.object -> Unsafe.Object.object

    (* raised by input when there are insufficient bytes *)
    exception FormatError

    (* given the target, source-file name, and a CFG pickle, generate
     * native-machine code using the LLVM code generator
     *)
    val generate : {
(* TODO: the target field will go away once we are generating CFG in binfiles *)
            target : string,
            src : string,
            pkl : cfg_pickle,
            verifyLLVM : bool
          } -> native_code

    (* return the size of the code object *)
    val size : t -> int

    (* Allocate a native-code object of the given size and initialize it
     * from the input stream.  The third argument is the entrypoint
     * offset.
     *)
    val input : (BinIO.instream * int * int) -> native_code

    (* Output a native code object to the given output stream *)
    val output : (BinIO.outstream * native_code) -> unit

    (* return the size of a native-code object *)
    val sizeOfNativeCode : native_code -> int

    (* View the code object as an executable.  This has the side-effect
     * of flushing the instruction cache.
     *)
    val exec : native_code -> executable

    (* return the offset of the entry point of the code object *)
    val entrypoint : native_code -> int

    (* use the run-time system interpreter to generate a literals
     * vector from a literal bytecode program.
     *)
    val mkLiterals : literals -> Unsafe.Object.object

  end
