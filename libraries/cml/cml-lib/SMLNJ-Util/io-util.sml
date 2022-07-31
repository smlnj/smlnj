(* io-util.sml
 *
 * COPYRIGHT (c) 1997 AT&T Labs Research.
 *)

structure IOUtil : IO_UTIL =
  struct

    type instream = TextIO.instream
    type outstream = TextIO.outstream

    fun swapInstrm (s, s') =
	  TextIO.getInstream s before TextIO.setInstream(s, s')

    fun withInputFile (s, f) x = let
	  val oldStrm = swapInstrm(TextIO.stdIn, TextIO.getInstream(TextIO.openIn s))
	  fun cleanUp () =
		TextIO.StreamIO.closeIn(swapInstrm(TextIO.stdIn, oldStrm))
	  val res = (f x) handle ex => (cleanUp(); raise ex)
	  in
	    cleanUp();
	    res
	  end

    fun withInstream (strm, f) x = let
	    val oldStrm = swapInstrm(TextIO.stdIn, TextIO.getInstream strm)
	    fun cleanUp () =
		  TextIO.setInstream(strm, swapInstrm(TextIO.stdIn, oldStrm))
	    val res = (f x) handle ex => (cleanUp(); raise ex)
	    in
	      cleanUp();
	      res
	    end

    fun swapOutstrm (s, s') =
	  TextIO.getOutstream s before TextIO.setOutstream(s, s')

    fun withOutputFile (s, f) x = let
	  val oldStrm = swapOutstrm(TextIO.stdOut, TextIO.getOutstream(TextIO.openOut s))
	  fun cleanUp () =
		TextIO.StreamIO.closeOut(swapOutstrm(TextIO.stdOut, oldStrm))
	  val res = (f x) handle ex => (cleanUp(); raise ex)
	  in
	    cleanUp();
	    res
	  end

    fun withOutstream (strm, f) x = let
	    val oldStrm = swapOutstrm(TextIO.stdOut, TextIO.getOutstream strm)
	    fun cleanUp () =
		  TextIO.setOutstream(strm, swapOutstrm(TextIO.stdOut, oldStrm))
	    val res = (f x) handle ex => (cleanUp(); raise ex)
	    in
	      cleanUp();
	      res
	    end

  end (* IOUtil *)
