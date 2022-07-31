structure SMLNJPrimCCall =
  struct

    structure DL = DynLinkage
    structure W = VarargConstants.W

    fun dynLink s = let
	val lh = DL.open_lib
		     { name = "./vararg", global = true, lazy = true }
        in 
	    DL.lib_symbol (lh, s)
        end

  (* make the primitive call to the interpereter *)
    fun applyInterp (varargFnPtr, startLocdArgs, endLocdArgs) = let
	    val callInterp = RawMemInlineT.rawccall :
		      W.word * (W.word * W.word * W.word) * 
		      (unit * W.word * W.word * W.word -> W.word) list
		      -> W.word
	    val vararg_h = dynLink VarargConstants.varargInterpreter
	    val x = callInterp (DL.addr vararg_h, (DL.addr varargFnPtr, startLocdArgs, endLocdArgs), [])
            in
	        Marshal.freeStrs();
	        Marshal.free startLocdArgs;
	        x
	    end


  end
