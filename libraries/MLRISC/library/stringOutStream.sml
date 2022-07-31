(* 
 * The basis seems to be missing a string (out)stream type.
 * This is it.
 *
 * -- Allen.
 *)
structure StringOutStream :> STRING_OUTSTREAM =
struct

   structure TextIO = TextIO
   structure TextPrimIO = TextPrimIO

   type streambuf = string list ref

   fun mkStreamBuf ()    = ref [] : streambuf
   fun getString (ref s) = String.concat(List.rev s)
   fun setString (r,s)   = r := [s]     

   fun openStringOut buffer =
   let fun writeVec sl =
	   (buffer := CharVectorSlice.vector sl :: !buffer;
	    CharVectorSlice.length sl)
       fun writeArr sl =
	   (buffer := CharArraySlice.vector sl :: !buffer;
	    CharArraySlice.length sl)
       val writer =
           TextPrimIO.WR 
                { name       = "string stream",
                chunkSize  = 512,
                writeVec   = SOME writeVec,
                writeArr   = SOME writeArr,
                writeVecNB = SOME (SOME o writeVec),
                writeArrNB = SOME (SOME o writeArr),
                block      = NONE,
                canOutput  = NONE,
                getPos     = NONE,
                setPos     = NONE,
                endPos     = NONE,
                verifyPos  = NONE,
                close      = fn () => (),
                ioDesc     = NONE
              }
       val outstream = TextIO.mkOutstream 
              (TextIO.StreamIO.mkOutstream (writer,IO.NO_BUF))
   in  outstream
   end

end

