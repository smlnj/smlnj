(* code-obj.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ https://www.smlnj.org)
 * All rights reserved.
 *
 * An interface for manipulating code objects.
 *)

structure CodeObj :> CODE_OBJ =
  struct

    structure W8A = Word8Array
    structure W8V = Word8Vector
    type object = Unsafe.Object.object

    type cfg_pickle = Word8Vector.vector

    datatype native_code = C of {
(* QUESTION: it appears that the entrypoint is always going to be zero, so we might not
 * need it!
 *)
	entrypoint : int,
        isExec : bool ref,
	obj : W8V.vector ref
      }

    (* the representations of the executable *)
    datatype t
      = NoCode
      | CFGPickle of cfg_pickle
      | NativeCode of native_code

    type literals = Word8Vector.vector

    type executable = object -> object

    (* raised by input when there are insufficient bytes *)
    exception FormatError

    local
      structure CI = Unsafe.CInterface
    in
    (* set the target architecture for the code generator *)
    val setTarget : string option -> bool = CI.c_function "CodeGen" "setTarget"
    (* interface to the LLVM code generator *)
    val codegen : string * W8V.vector * bool -> W8V.vector * int
          = CI.c_function "CodeGen" "generate"
    (* build the in-heap literal data from a literal-byte-code program *)
    val mkLiterals : W8V.vector -> object = CI.c_function "SMLNJ-RunT" "mkLiterals"
    (* allocate and initialize a code object in the heap *)
    val mkCodeObj : W8V.vector -> W8V.vector = CI.c_function "SMLNJ-RunT" "mkCodeObj"
    (* package a code object as an executable function *)
    val mkExec : W8V.vector * int -> executable = CI.c_function "SMLNJ-RunT" "mkExec"
    end (* local *)

    fun newObj (code, offset) = C{
            entrypoint = offset,
            isExec = ref false,
            obj = ref code
          }

    fun generate {target, src, pkl, verifyLLVM} =
          if setTarget (SOME target)
            then raise Fail(concat[
                "Internal error: ", target, " is not a supported codegen target"
              ])
            else newObj (codegen (src, pkl, verifyLLVM))

    fun input (inStrm, sz, offset) = let
	  val data = BinIO.inputN (inStrm, sz)
	  in
	    if (W8V.length data < sz)
	      then (
		Control_Print.say(concat[
		    "Bin file format error: expected ", Int.toString sz,
		    " bytes, but only found ", Int.toString(W8V.length data)
		  ]);
		raise FormatError)
	      else ();
            newObj(data, offset)
	  end

    fun output (outStrm, C{obj, ...}) = (
	  BinIO.output(outStrm, !obj);
	  BinIO.flushOut outStrm)

    fun exec (C{obj, isExec, entrypoint}) = (
          (* ensure that the code object is in executable memory *)
          if not(!isExec)
            then (obj := mkCodeObj(!obj); isExec := true)
            else ();
          mkExec (!obj, entrypoint))

    fun sizeOfNativeCode (C{obj, ...}) = W8V.length(!obj)

    fun size NoCode = 0
      | size (CFGPickle pkl) = Word8Vector.length pkl
      | size (NativeCode cd) = sizeOfNativeCode cd

    fun entrypoint (C c) = (#entrypoint c)

  end
