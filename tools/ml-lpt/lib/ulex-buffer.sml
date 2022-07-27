(* ulex-buffer.sml
 *
 * COPYRIGHT (c) 2006
 * John Reppy (http://www.cs.uchicago.edu/~jhr)
 * Aaron Turon (http://www.cs.uchicago.edu/~adrassi)
 * All rights reserved.
 *
 * Forward-chained buffers for lexing
 *)

structure ULexBuffer : sig

    type stream

    exception Incomplete	(* raised by getu on an incomplete multi-byte character *)

    val mkStream : (AntlrStreamPos.pos * (unit -> string)) -> stream
    val getc : stream -> (char * stream) option
    val getu : stream -> (word * stream) option
    val getpos : stream -> AntlrStreamPos.pos
    val subtract : stream * stream -> Substring.substring
    val eof : stream -> bool
    val lastWasNL : stream -> bool

  end = struct

    structure W = Word

    datatype stream = S of (buf * int * bool) 
    and buf = B of { 
      data : string,
      basePos : AntlrStreamPos.pos,
      more : more ref,
      input : unit -> string
    }
    and more = UNKNOWN | YES of buf | NO
        
    fun mkStream (pos, input) = 
	  (S (B {data = "", basePos = pos, 
		 more = ref UNKNOWN,
		 input = input},
	      0, true))

  (* advance the stream to the next block of input *)
    fun advance (data, input, basePos, more) = (case !more
	   of UNKNOWN => (case input()
		 of "" => (more := NO; NO)
		  | data' => let 
		      val buf' = B {
			  data = data',
			  basePos = AntlrStreamPos.forward (basePos, String.size data),
			  more = ref UNKNOWN,
			  input = input
			}
		      in
			more := YES buf';
			YES buf'
		      end
		(* end case *))
	    | m => m
	  (* end case *))

    fun getc (S(buf as B{data, basePos, more, input}, pos, lastWasNL)) = 
	  if pos < String.size data
	    then let
	      val c = String.sub (data, pos)
	      in
		SOME (c, S (buf, pos+1, c = #"\n"))
	      end
	    else (case advance(data, input, basePos, more)
	       of NO => NONE
		| YES buf' => getc (S (buf', 0, lastWasNL))
		| UNKNOWN => raise Fail "impossible"
	      (* end case *))

    exception Incomplete

  (* get the next UTF8 character represented as a word *)
    fun getu (S(buf as B{data, basePos, more, input}, pos, _)) =
	  if pos < String.size data
	    then let
	      val c = W.fromInt(Char.ord(String.sub(data, pos)))
	      in
		if (c < 0w128)
		  then SOME(c, S(buf, pos+1, c = 0w10))  (* ord #"\n" = 10 *)
		  else let (* multibyte character *)
		    fun getByte (S(buf as B{data, basePos, more, input}, pos, _)) = 
			  if pos < String.size data
			    then let
			      val c = W.fromInt(Char.ord(String.sub(data, pos)))
			      in
				SOME (c, S (buf, pos+1, false))
			      end
			    else (case advance(data, input, basePos, more)
			       of NO => NONE
				| YES buf' => getByte (S (buf', 0, false))
				| UNKNOWN => raise Fail "impossible"
			      (* end case *))
		    fun getContByte (wc, strm) = (case getByte strm
			   of NONE => raise Incomplete
			    | SOME(b, strm') => if (W.andb(0wxc0, b) = 0wx80)
				then (W.orb(W.<<(wc, 0w6), W.andb(0wx3f, b)), strm')
				else raise Incomplete
			  (* end case *))
		    val strm = S(buf, pos+1, false)
		    in
		      case (W.andb(0wxe0, c))
		       of 0wxc0 => SOME(getContByte (W.andb(0wx1f, c), strm))
			| 0wxe0 => SOME(getContByte(getContByte(W.andb(0wx0f, c), strm)))
			| _ => raise Incomplete
		      (* end case *)
		    end
	      end
	  (* advance buffer *)
	    else (case advance(data, input, basePos, more)
	       of NO => NONE
		| YES buf' => getu (S(buf', 0, false))
		| UNKNOWN => raise Fail "impossible"
	      (* end case *))

    fun getpos (S (B {basePos, ...}, pos, _)) = AntlrStreamPos.forward (basePos, pos)

    fun subtract (new, old) = let
	  val (S (B {data = ndata, basePos = nbasePos, ...}, npos, _)) = new
	  val (S (B {data = odata, basePos = obasePos, 
		     more, input}, opos, _)) = old
	  in
	    if nbasePos = obasePos then
	      Substring.substring (ndata, opos, npos-opos)
	    else case !more
		  of NO =>      raise Fail "BUG: ULexBuffer.subtract, but buffers are unrelated"
		   | UNKNOWN => raise Fail "BUG: ULexBuffer.subtract, but buffers are unrelated"
		   | YES buf => 
		       Substring.extract (
			 Substring.concat [
			   Substring.extract (odata, opos, NONE),
			   subtract (new, S (buf, 0, false))],
			 0, NONE)
	  end

    fun eof s = not (isSome (getc s))

    fun lastWasNL (S (_, _, lastWasNL)) = lastWasNL

  end