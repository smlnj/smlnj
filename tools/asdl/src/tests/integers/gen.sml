(* generate some integer pickles *)

CM.make "../../lib/sml/asdl-lib.cm";

fun toBytes (n : IntInf.int) =
      ASDLMemoryPickle.toVector ASDLMemoryPickle.writeInteger n;

fun mkTest outS n = let
      val bytes = Word8Vector.toList (toBytes n)
      fun pr s = TextIO.output(outS, String.concat s)
      fun b2s byte = "0x" ^ StringCvt.padLeft #"0" 2 (Word8.toString byte)
      in
	pr [
	    "{ \"", if n < 0 then "-" else "", IntInf.toString(IntInf.abs n),
	    "\", ", Int.toString(List.length bytes), ", { ",
	    String.concatWithMap ", " b2s bytes,
	    "} },\n"
	  ]
      end;

fun mkTests outS = List.app (mkTest outS) [
	~1, 0, 1, 15, 0x3f, 0x40, 0x7f, 0x80, 1000, 0x7fffffff
      ];
